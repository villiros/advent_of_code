with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;

with Harness; use Harness;
with utils; use utils;

package body p18 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(p18a'(Name => "p18a"));
        D.Append(p18b'(Name => "p18b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------
    use type InputStr; use InputStrPkg;

    ShouldNotGetHere: exception;

    subtype Number is ResultType;
    subtype NormNumber is Number range 0..9;

    type NumType is (NumNode, PairNode);

    type Node;
    type NodePtr is access Node;

    type Node(ntype: NumType) is record
        case ntype is
            when NumNode =>
                num: Number;
            when PairNode =>
                left, right: not null NodePtr;
        end case;
    end record;

    -- Is the number well-formed?
    function IsWellFormed(n: not null NodePtr; requireNormalized: Boolean) return Boolean is
        maxDepth: constant Natural := (if requireNormalized then 4 else 5);

        function IsNormalInner(n: not null NodePtr; depth: Natural) return Boolean is
        begin
            case n.ntype is
                when NumNode =>
                    return depth <= maxDepth and then (if requireNormalized then n.num in NormNumber);
                when PairNode =>
                    return depth <= maxDepth and then
                            (IsNormalInner (n.left, depth + 1) and IsNormalInner (n.right, depth + 1));
            end case;
        end IsNormalInner;
    begin
        return IsNormalInner (n, 0);
    end IsWellFormed;

    type VisitType is (NodeEnter, NumberNodeVisit, NodeExit);
    generic
        with procedure VisitNode(n: not null NodePtr; parent: NodePtr; vtype: VisitType; depth: Positive);
                -- GNAT bug detected :(
                -- with Pre => (case vtype is
                --                 when NodeEnter | NodeExit => n.ntype = PairNode,
                --                 when NumberNodeVisit => n.ntype = NumNode)
                --             (if parent /= null then parent.ntype = PairNode) and
                --             (if depth > 1 then parent /= null);
    procedure Visitor(root: not null NodePtr; parent: NodePtr := null; depth: Positive := 1);

    procedure Visitor(root: not null NodePtr; parent: NodePtr := null; depth: Positive := 1) is
    begin
        if root.ntype = PairNode then
            VisitNode (root, parent, NodeEnter, depth);
            Visitor (root.left, root, depth + 1);
            Visitor (root.right, root, depth + 1);
            VisitNode (root, parent, NodeExit, depth);
        elsif root.ntype = NumNode then
            VisitNode (root, parent, NumberNodeVisit, depth);
        end if;
    end Visitor;

    function FromString(s: InputStr) return not null NodePtr
    with Post => IsWellFormed(FromString'Result, True)
    is
        -- Modified by GetNode
        pos: Positive := 1;

        function GetNode return NodePtr
        with Pre => pos < Length(s),
             Post => GetNode'Result /= null
        is
            c: Character := Element (s, pos);
            left, right: NodePtr := null;
        begin
            pos := pos + 1;
            case c is
                when '[' =>
                    left := GetNode;
                    Assert (Element(s, pos) = ',');
                    pos := pos + 1;
                    right := GetNode;
                    Assert (Element(s, pos) = ']');
                    pos := pos + 1;

                    return new Node'(ntype => PairNode, left => left, right => right);
                when '0'..'9' =>
                    return new Node'(ntype => NumNode, num => Number'Value("" & c));
                when others =>
                    raise ShouldNotGetHere;
            end case;
        end GetNode;

        result: not null NodePtr := GetNode;
    begin
        Assert (pos = Length(s) + 1);
        return result;
    end FromString;

    function ToString(root: not null NodePtr) return String is
        result: InputStr;

        procedure VisitNode(n: not null NodePtr; parent: NodePtr; vtype: VisitType; depth: Positive) is
        begin
            case vtype is
                when NodeEnter =>
                    Append (result, '[');
                when NodeExit =>
                    Append (result, ']');
                when NumberNodeVisit =>
                    Append (result, n.num'Image & " ");
            end case;
        end VisitNode;

        procedure ToStringVisitor is
            new Visitor(VisitNode => VisitNode);
    begin
        ToStringVisitor(root);
        return To_String(result);
    end ToString;

    function AddNums(left, right: not null NodePtr) return not null NodePtr
    with Pre => IsWellFormed (left, true) and
                IsWellFormed (right, true),
         Post => IsWellFormed (AddNums'Result, false)
    is
    begin
        return new Node'(ntype => PairNode, left => left, right => right);
    end AddNums;

    procedure Reduce(root: not null NodePtr)
    with Pre => IsWellFormed (root, false),
         Post => IsWellFormed (root, true)
    is

        -- Explode searcher
        type ExplodeSearchState is record
            toLeft, target, targetParent, toRight: NodePtr := null;
        end record
        with Dynamic_Predicate => (if target /= null
                                   then targetParent /= null and
                                        target.ntype = PairNode and
                                        target.left.ntype = NumNode and
                                        target.right.ntype = NumNode);

        function SearchExplode return ExplodeSearchState is
            explodeState: ExplodeSearchState := (others => <>);
            procedure ExplodeSearchVisit(n: not null NodePtr; parent: NodePtr; vtype: VisitType; depth: Positive)
            is
            begin
                if vtype = NumberNodeVisit and explodeState.target = null then
                    explodeState.toLeft := n;
                elsif vtype = NumberNodeVisit and explodeState.target /= null and
                      explodeState.toRight = null and parent /= explodeState.target then
                    explodeState.toRight := n;
                elsif vtype = NodeEnter and explodeState.target = null and depth = 5 then
                    explodeState.target := n;
                    explodeState.targetParent := parent;
                else
                    null;
                end if;
            end ExplodeSearchVisit;

            procedure ExplodeVisitor is new Visitor(VisitNode => ExplodeSearchVisit);
        begin
            ExplodeVisitor(root);
            return explodeState;
        end SearchExplode;

        -- Split searcher
        type SplitSearchState is record
            target, targetParent: NodePtr := null;
        end record
        with Dynamic_Predicate => (if target /= null
                                   then targetParent /= null and
                                        target.ntype = NumNode);

        function SearchSplit return SplitSearchState is
            splitState: SplitSearchState := (others => <>);
            procedure SplitSearchVisit(n: not null NodePtr; parent: NodePtr; vtype: VisitType; depth: Positive)
            is
            begin
                if vtype = NumberNodeVisit and then (splitState.target = null and n.num >= 10) then
                    splitState.target := n;
                    splitState.targetParent := parent;
                else
                    null;
                end if;
            end SplitSearchVisit;

            procedure SplitVisitor is new Visitor(VisitNode => SplitSearchVisit);
        begin
            SplitVisitor(root);
            return splitState;
        end SearchSplit;
    begin
        loop
            declare
                explodeTarget: ExplodeSearchState := SearchExplode;
                splitTarget: SplitSearchState := SearchSplit;
            begin
                if explodeTarget.target /= null then
                    if explodeTarget.toLeft /= null then
                        explodeTarget.toLeft.num := explodeTarget.toLeft.num + explodeTarget.target.left.num;
                    end if;
                    if explodeTarget.toRight /= null then
                        explodeTarget.toRight.num := explodeTarget.toRight.num + explodeTarget.target.right.num;
                    end if;
                    if explodeTarget.targetParent.left = explodeTarget.target then
                        explodeTarget.targetParent.left := new Node'(ntype => NumNode, num => 0);
                    else
                        Assert(explodeTarget.targetParent.right = explodeTarget.target);
                        explodeTarget.targetParent.right := new Node'(ntype => NumNode, num => 0);
                    end if;
                elsif splitTarget.target /= null then
                    declare
                        newPair: NodePtr := new Node'(ntype => PairNode,
                                                      left => new Node'(ntype => NumNode, num => splitTarget.target.num / 2),
                                                      right => new Node'(ntype => NumNode, num => splitTarget.target.num - (splitTarget.target.num / 2)));
                    begin
                        if splitTarget.targetParent.left = splitTarget.target then
                            splitTarget.targetParent.left := newPair;
                        else
                            Assert(splitTarget.targetParent.right = splitTarget.target);
                            splitTarget.targetParent.right := newPair;
                        end if;
                    end;
                else
                    exit;
                end if;
                if PrintDebug then Put_Line ("After reduce step: " & ToString (root)); end if;
            end;
        end loop;
    end Reduce;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    package InputPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => InputStr);
    use InputPkg;
    subtype InputType is InputPkg.Vector;

    type Solution is tagged record
        Input : InputType := InputPkg.Empty_Vector;
    end record;

    procedure ReadInput(self : in out Solution'class; InData : File_Type)
    with
        Post => not self.Input.Is_Empty
    is
    begin
        while not End_Of_File(InData) loop
            -- Basically the whole line
            self.Input.Append (GetAtom(InData));
            AdvanceLine (InData);
        end loop;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    function Magnitude(n: not null NodePtr) return ResultType is
    begin
        if n.ntype = PairNode then
            return Magnitude (n.left) * 3 + Magnitude (n.right) * 2;
        else
            return n.num;
        end if;
    end Magnitude;

    type SolutionA is new Solution with record
        null;
    end record;

    function Solve (SDisp : p18a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionA;
        cur: NodePtr := null;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        cur := FromString(s.Input.First_Element);
        if PrintDebug then Put_Line (ToString(cur)); end if;

        for i in (s.Input.First_Index+1)..s.Input.Last_Index loop
            cur := AddNums (cur, FromString(s.Input.Element(i)));
            Reduce (cur);
            if PrintDebug then Put_Line (ToString(cur)); end if;
        end loop;
        return Magnitude (cur);
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p18b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionB;
        maxMag: ResultType := 0;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        for n1 in s.Input.First_Index..s.Input.Last_Index loop
            for n2 in s.Input.First_Index..s.Input.Last_Index loop
                if n1 /= n2 then
                    declare
                        left: NodePtr := FromString(Element(s.Input, n1));
                    begin
                        left := AddNums (left, FromString (Element(s.Input, n2)));
                        Reduce (left);
                        if Magnitude (left) > maxMag then
                            maxMag := Magnitude (left);
                        end if;
                    end;
                end if;
            end loop;
        end loop;

        return maxMag;
    end Solve;

end p18;
