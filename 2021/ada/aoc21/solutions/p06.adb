with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;

with utils; use utils;

package body p06 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(new p06a'(Name => "p06a"));
        D.Append(new p06b'(Name => "p06b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------
    type FishTimerType is range 0..8;
    subtype FishTimerAfterSpawn is FishTimerType range 6..6;
    subtype FishTimerBorn is FishTimerType range 8..8;

    -- PartA: 2**(80/10) is ~1k; input is ~300, so Integer is plenty
    -- PartB: need 64-bit integer for this. Fits into signed one though.
    subtype FishCount is ResultType;

    -- On a day, keep a count of fishes for each possible remaining timer
    -- This is start of day, FishCounts(0) will spawn that day.
    type FishCounts is array(FishTimerType) of FishCount;

    -- Generic Sum() function. Note, refuses to work on empty array (sum of nothing is not defined)
    generic
        type InTR is (<>);
        type T is range <>;
        type InT is array(InTR) of T;
    function Sum(Input : InT) return T
    with Pre => Input'Length > 0;

    function Sum(Input : InT) return T
    is
        R : T := Input(InTR'First);
    begin
        for I in (InTR'Succ(InTR'First))..InTR'Last loop
            R := R + Input(I);
        end loop;
        return R;
    end Sum;

    function SumFish is new Sum(-- Can these be taken from FishCounts?
                                T => FishCount,
                                InTr => FishTimerType,
                                InT => FishCounts);
    

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    package InputPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => FishTimerType);
    use InputPkg;
    subtype InputType is InputPkg.Vector;

    type Solution is tagged record
        Input : InputType := InputPkg.Empty_Vector;
        Counts : FishCounts := (others => 0);
    end record;

    procedure ReadInput(self : in out Solution'class; InData : File_Type)
    with
        Post => not self.Input.Is_Empty
    is
    begin
        loop
            self.Input.Append(FishTimerType(GetInt(InData)));
            exit when PeekChar(InData) /= ',';
            SkipChar (InData, ',');
        end loop;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    procedure ApplyInput(s : in out Solution'Class) is
    begin
        for I of s.Input loop
            s.Counts(I) := s.Counts(I) + 1;
        end loop;
    end ApplyInput;

    procedure DoDay(C : in out FishCounts)
    with
        Post =>
            -- Number of fishes cannot decrease
            (SumFish(C'Old) <= SumFish(C)) and
            -- All at 0 spawn new ones at 8 (previous 8 are now 7)
            (C(8) = (C(0)'Old)) and
            -- All at 0 move to 6 plus whatever moved from 7
            (C(0)'Old + C(7)'Old = C(FishTimerType(6)))
    is
        SpawnCount : FishCount := C(0);
    begin
        C(FishTimerType'First..FishTimerType'Pred(FishTimerType'Last)) :=
            C(FishTimerType'Succ(FishTimerType'First)..FishTimerType'Last);
        
        C(FishTimerBorn'First) := SpawnCount;
        C(FishTimerAfterSpawn'First) := C(FishTimerAfterSpawn'First) + SpawnCount;
    end DoDay;

    type SolutionA is new Solution with record
        null;
    end record;

    function Solve (SDisp : p06a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionA;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        s.ApplyInput;

        for DayN in 1..80 loop
            DoDay (s.Counts);
        end loop;

        return SumFish(s.Counts);
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p06b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionB;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        s.ApplyInput;

        for DayN in 1..256 loop
            DoDay (s.Counts);
        end loop;

        return SumFish(s.Counts);
    end Solve;

end p06;
