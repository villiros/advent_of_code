with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;

with Harness; use Harness;
with utils; use utils;

package body p07 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(new p07a'(Name => "p07a"));
        D.Append(new p07b'(Name => "p07b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    -- Horizontal position (range from observing input).
    type HPosType is range 0..2000;
    -- Number of crabs. Contstrained by the size of the input.
    subtype CrabCountType is ResultType range 0..2000;
    -- How many crabs are at a position?
    -- \A i \in DOMAIN CrabCountsType: CrabCounts(i) = SUM(SelectSeq(Input, LAMBDA x: x = i))
    type CrabCountsType is array(HPosType) of CrabCountType;
    -- Direction around a position.
    type Half is (Left, Right);

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    subtype InputPkgIndex is Positive;
    package InputPkg is new
        Ada.Containers.Vectors(Index_Type => InputPkgIndex, Element_Type => HPosType);
    use InputPkg;
    subtype InputType is InputPkg.Vector;

    -- Only used for assertions
    -- Count the number of crabs at position Loc
    function CountAt(L: InputType; Loc: HPosType) return CrabCountType
    is
        result : CrabCountType := 0;
    begin
        for E of L loop
            if E = Loc then
                result := result + 1;
            end if;
        end loop;
        return result;
    end CountAt;

    -- Only used for assertions
    -- Count crabs to the left of pivot (including at pivot) or to the right.
    function CountPart(L: CrabCountsType; Pivot: HPosType; H: Half) return CrabCountType
    is
        result : CrabCountType := 0;
    begin
        case H is
            when Left =>
                for I in HPosType'First..Pivot loop
                    result := result + L(I);
                end loop;
            when Right =>
                if Pivot < HPosType'Last then
                    for I in HPostype'Succ(Pivot)..HPosType'Last loop
                        result := result + L(I);
                    end loop;
                else
                    result := result + L(HPosType'Last);
                end if;
        end case;

        return result;
    end CountPart;

    -- Only used for assertions
    -- Calculate fuel (part A) needed to the left or right of pivot
    function FuelTo(L: CrabCountsType; Pivot: HPosType; H: Half) return ResultType
    is
        result : ResultType := 0;
    begin
        case H is
            when Left =>
                for I in L'First..Pivot loop
                    result := result + L(I) * ResultType(Pivot - I);
                end loop;
            when Right =>
                if Pivot < HPosType'Last then
                    for I in HPosType'Succ(Pivot)..HPosType'Last loop
                        result := result + L(I) * ResultType(I - Pivot);
                    end loop;
                end if;
        end case;
        return result;
    end FuelTo;

    type Solution is tagged record
        Input : InputType := InputPkg.Empty_Vector;
        Counts : CrabCountsType := (others => 0);
    end record
    with
        Dynamic_Predicate =>
            -- Check that counts are calculated correctly.
            (for all i in Counts'First..Counts'Last =>
                Counts(i) = CountAt(Input, i));

    procedure ReadInput(Input : in out InputType; InData : File_Type)
    with
        Post => not Input.Is_Empty
    is
    begin
        loop
            Input.Append(HPosType(GetInt(InData)));
            exit when PeekChar(InData) /= ',';
            SkipChar(InData, ',');
        end loop;
    end ReadInput;

    function MakeCounts(Input: InputType) return CrabCountsType
    -- Correctness of result is checked by the Solution contract
    is
        result : CrabCountsType := (others => 0);
    begin
        for E of Input loop
            result(E) := result(E) + 1;
        end loop;
        return result;
    end MakeCounts;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    -- For position, how many crabs are to the left (incl. pos) and to the right
    -- And how much fuel is needed by left and right crabs.
    type SearchCursor is tagged record
        Pos : HPosType := HPosType'First;
        LeftNCrabs, RightNCrabs : CrabCountType := 0;
        LeftFuel, RightFuel : ResultType := 0;
    end record;

    -- Used for assertions
    function IsCursorValid(Cur: SearchCursor; Counts: CrabCountsType) return Boolean is
    begin
        return (Cur.LeftNCrabs = CountPart (Counts, Cur.Pos, Left)) and
               (Cur.LeftFuel = FuelTo (Counts, Cur.Pos, Left)) and
               (Cur.RightNCrabs = CountPart (Counts, Cur.Pos, Right)) and
               (Cur.RightFuel = FuelTo (Counts, Cur.Pos, Right));
    end IsCursorValid;

    function MakeSearchCursor(Counts : CrabCountsType) return SearchCursor
    with
        Post => IsCursorValid(MakeSearchCursor'Result, Counts)
    is
        result : SearchCursor := (others => <>);
    begin
        result.LeftNCrabs := Counts(result.Pos);
        
        for i in HPosType'Succ(result.Pos)..HPosType'Last loop
            result.RightNCrabs := result.RightNCrabs + Counts(i);
            result.RightFuel := result.RightFuel + Counts(i) * CrabCountType(i - result.Pos);
        end loop;

        return result;
    end MakeSearchCursor;

    -- Move search cursor one to the right.
    -- Returns false if it's at the rightmost position.
    function MoveSearchRight(Counts: CrabCountsType; Cur: in out SearchCursor) return Boolean
    with
        Pre => IsCursorValid(Cur, Counts),
        Post => IsCursorValid(Cur, Counts)
    is begin
        if Cur.Pos = HPosType'Last then
            return False;
        end if;

        Cur.LeftFuel := Cur.LeftFuel + Cur.LeftNCrabs;
        Cur.LeftNCrabs := Cur.LeftNCrabs + Counts(HPosType'Succ(Cur.Pos));

        Cur.RightFuel := Cur.RightFuel - Cur.RightNCrabs;
        Cur.RightNCrabs := Cur.RightNCrabs - Counts(HPosType'Succ(Cur.Pos));

        Cur.Pos := HPosType'Succ(Cur.Pos);

        return True;
    end MoveSearchRight;


    type SolutionA is new Solution with record
        Cur : SearchCursor;
        LeastFuel : ResultType;
    end record;

    function Solve (SDisp : p07a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        Input : InputType;
    begin
        ReadInput(Input, InData);
        StartTs := Ada.Real_Time.Clock;

        -- Starting from the first position, move right one-by-one
        -- Once we know left&right crab counts and fueld cost, cursor move is a simple shift
        -- of crab counts at the new cursor position to the right.

        declare
            Counts : CrabCountsType := MakeCounts (Input);
            Cur : SearchCursor := MakeSearchCursor (Counts);
            s : SolutionA := (Input => Input,
                              Counts => Counts,
                              Cur => Cur,
                              LeastFuel => Cur.LeftFuel + Cur.RightFuel);
        begin
            loop
                exit when not MoveSearchRight (s.Counts, s.Cur);

                if (s.Cur.LeftFuel + s.Cur.RightFuel) < s.LeastFuel then
                    s.LeastFuel := s.Cur.LeftFuel + s.Cur.RightFuel;
                end if;
            end loop;

            return s.LeastFuel;
        end;
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    -- Sum(1..n)
    function SumProg(N:ResultType) return ResultType
    is begin
        if N > 0 then
            return (N*(N+1))/2;
        else
            return 0;
        end if;
    end SumProg;

    function FuelToB(L: CrabCountsType; Pivot: HPosType; H: Half) return ResultType
    is
        result : ResultType := 0;
    begin
        case H is
            when Left =>
                for I in L'First..Pivot loop
                    result := result + L(I) * SumProg(ResultType(Pivot - I));
                end loop;
            when Right =>
                if Pivot < HPosType'Last then
                    for I in HPosType'Succ(Pivot)..HPosType'Last loop
                        result := result + L(I) * SumProg(ResultType(I - Pivot));
                    end loop;
                end if;
        end case;
        return result;
    end FuelToB;

    type SolutionB is new Solution with record
        LeastFuel : ResultType;
    end record;

    function Solve (SDisp : p07b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        Input : InputType;
    begin
        ReadInput(Input, InData);
        StartTs := Ada.Real_Time.Clock;

        -- Naive solution: for each target position, calculate fuel cost.
        declare
            Counts : CrabCountsType := MakeCounts (Input);
            s : SolutionB := (Input => Input,
                              Counts => Counts,
                              LeastFuel => FuelToB (Counts, HPosType'First, Right));
        begin
            for I in HPosType'First..HPosType'Last loop
                declare
                    F : ResultType := FuelToB(s.counts, I, left) + FuelToB(s.counts, I, right);
                begin
                    if F < s.LeastFuel then
                        s.LeastFuel := F;
                    end if;
                end;
            end loop;

            return s.LeastFuel;
        end;
    end Solve;

end p07;
