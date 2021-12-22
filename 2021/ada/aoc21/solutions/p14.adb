with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;

with Harness; use Harness;
with utils; use utils;

package body p14 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(p14a'(Name => "p14a"));
        D.Append(p14b'(Name => "p14b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    type ACharOrNull is ('_', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L',
                         'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z');
    subtype AChar is ACharOrNull range 'A'..'Z';

    type PairCounter is array(ACharOrNull, ACharOrNull) of ResultType;
    use type ResultType;

    type Rule is record
        First, Second: AChar;
        Insert: AChar;
    end record;

    function ToString(pairs: PairCounter) return String is
        use InputStrPkg;
        result: InputStr;
    begin
        for i in ACharOrNull loop
            for j in ACharOrNull loop
                if pairs(i, j) > 0 then
                    result := result & " " & i'Image & j'Image & "=" & pairs(i, j)'Image;
                end if;
            end loop;
        end loop;

        return To_String (result);
    end ToString;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------
    use InputStrPkg;

    package RulesPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Rule);
    use RulesPkg;
    subtype RulesType is RulesPkg.Vector;

    type Solution is tagged record
        StartStr: InputStr;
        Rules : RulesType := RulesPkg.Empty_Vector;
    end record;

    function ToAChar(c: Character) return AChar is
    begin
        return AChar'Value("'" & c & "'");
    end ToAChar;

    procedure ReadInput(self : in out Solution'class; InData : File_Type)
    with
        Post => (not self.Rules.Is_Empty) and
                (self.StartStr /= "")
    is
    begin
        self.StartStr := GetAtom (InData);
        AdvanceLine (InData);
        AdvanceLine (InData);

        while not End_Of_File(InData) loop
            declare
                Left: InputStr := GetAtom(InData);
            begin
                SkipChar (InData, '-'); SkipChar (InData, '>');
                self.Rules.Append(Rule'(First => ToAChar(Element(Left, 1)),
                                        Second => ToAChar(Element(Left, 2)),
                                        Insert => ToAChar(GetChar (InData))));
                AdvanceLine (InData);
            end;
        end loop;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    type SolutionA is new Solution with record
        null;
    end record;

    function ApplyRules (pairs: PairCounter; rules: RulesType) return PairCounter is
        result: PairCounter := pairs;
    begin
        for r of rules loop
            if pairs(r.First, r.Second) > 0 then
                result(r.First, r.Second) := result(r.First, r.Second) - pairs(r.First, r.Second);
                result(r.First, r.Insert) := result(r.First, r.Insert) + pairs(r.First, r.Second);
                result(r.Insert, r.Second) := result(r.Insert, r.Second) + pairs(r.First, r.Second);
            end if;
        end loop;
        return result;
    end ApplyRules;

    procedure LoadString(pairs: in out PairCounter; s: InputStr) is
    begin
        pairs('_', ToAChar(Element(s, 1))) := 1;
        pairs(ToAChar(Element(s, Length(s))), '_') := 1;

        for i in 1..(Length(s) - 1) loop
            declare
                a: AChar := ToAChar(Element(s, i));
                b: AChar := ToAChar(Element(s, i+1));
            begin
                pairs(a, b) := pairs(a, b) + 1;
            end;
        end loop;
    end LoadString;

    type CharCounts is array(ACharOrNull) of ResultType;

    function ToCharCounts(pairs: PairCounter) return CharCounts is
        result: CharCounts := (others => 0);
    begin
        for a in pairs'Range loop
            for b in pairs'Range(2) loop
                result(a) := result(a) + pairs(a, b);
                result(b) := result(b) + pairs(b, a);
            end loop;
        end loop;

        -- Above double-counts.
        for i in result'Range loop
            result(i) := result(i) / 2;
        end loop;

        -- Don't return '_'
        result('_') := 0;

        return result;
    end ToCharCounts;

    function RunSolution(s: Solution'Class; numSteps: Integer) return ResultType is
        counts: PairCounter := (others => (others => 0));
    begin
        LoadString (counts, s.StartStr);
        if PrintDebug then Put_Line ("Initial pairs: " & ToString (counts)); end if;
        for i in 1..numSteps loop
            counts := ApplyRules (counts, s.Rules);
            if PrintDebug then
                Put_Line ("Step" & i'Image & ToString(counts));
                declare
                    ccounts: CharCounts := ToCharCounts(counts);
                begin
                    if PrintDebug then Put_Line ("    charcounts: " & i'Image & " " & ccounts'Image); end if;
                end;
            end if;
        end loop;

        declare
            ccounts: CharCounts := ToCharCounts (counts);
            maximum: ResultType := 0;
            minimum: ResultType := ResultType'Last;
        begin
            if PrintDebug then Put_Line (ccounts'Image); end if;

            for i in AChar loop
                if ccounts(i) > maximum then
                    maximum := ccounts(i);
                end if;
                if ccounts(i) > 0 and ccounts(i) < minimum then
                    minimum := ccounts(i);
                end if;
            end loop;

            return ResultType(maximum - minimum);
        end;
    end RunSolution;

    function Solve (SDisp : p14a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionA;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        return RunSolution (s, 10);

    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p14b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionB;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        return RunSolution (s, 40);
    end Solve;

end p14;
