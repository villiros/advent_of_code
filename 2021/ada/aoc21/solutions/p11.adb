with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;

with Harness; use Harness;
with utils; use utils;

package body p11 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(new p11a'(Name => "p11a"));
        D.Append(new p11b'(Name => "p11b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    -- Range 10 to allow incrementing those that are at 9.
    subtype Energy is Integer range 0..10;
    type Row is range 1..10;
    type Col is range 1..10;

    -- Ranges for adjacents.
    subtype DRow is Integer range -1..1;
    subtype DCol is Integer range -1..1;

    type Energies is array(Row, Col) of Energy;
    type BitMap is array(Row, Col) of Boolean;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    type Solution is tagged record
        Input : Energies := (others => (others => 0));
    end record;

    procedure ReadInput(self : in out Solution'class; InData : File_Type) is
    begin
        for i in Row loop
            for j in Col loop
                self.Input(i, j) := Energy'Value("" & GetChar (InData));
            end loop;
            AdvanceLine (InData);
        end loop;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    function InBounds(r: Row; c: Col; dr: DRow; dc: DCol) return Boolean is
    begin
        return (Integer'Pos(Integer(r) + dr) in Row and
                Integer'Pos(Integer(c) + dc) in Col);
    end InBounds;

    -- Increment adjacent cells.
    procedure IncrAdj(m: in out Energies; r: Row; c: Col) is
    begin
        for dr in DRow loop
            for dc in DCol loop
                if dr = 0 and dc = 0 then
                    null;
                elsif InBounds(r, c, dr, dc) then
                    declare
                        cell: Energy renames m(Row(Integer(r) + dr), Col(Integer(c) + dc));
                    begin
                        if cell < 10 then
                            cell := cell + 1;
                        end if;
                    end;
                end if;
            end loop;
        end loop;
    end IncrAdj;

    function RunStep(m: in out Energies; allFlashed: out Boolean) return ResultType
    with Pre => (for all i in Row =>
                    (for all j in Col => m(i, j) < 10)),
         Post => (for all i in Row =>
                    (for all j in Col => m(i, j) < 10))
    is
        DidFlash: BitMap := (others => (others => False));

        numFlashes: ResultType := 0;
    begin
        -- First add energies to all.
        for i in Row loop
            for j in Col loop
                m(i, j) := m(i, j) + 1;
            end loop;
        end loop;

        -- Only 100 things in the map, so a simple loop over until it's stable is OK.
        loop
            declare
                anyFlashed: Boolean := False;
            begin
                for i in Row loop
                    for j in Col loop
                        if m(i, j) = 10 and not DidFlash(i, j) then
                            anyFlashed := True;
                            numFlashes := numFlashes + 1;

                            DidFlash(i, j) := True;
                            IncrAdj (m, i, j);
                        end if;
                    end loop;
                end loop;
                exit when not anyFlashed;
            end;
        end loop;

        -- And reset those that did flash.
        for i in Row loop
            for j in Col loop
                if m(i, j) = 10 then
                    m(i, j) := 0;
                end if;
            end loop;
        end loop;

        allFlashed := (for all i in Row =>
                        (for all j in Col => DidFlash(i, j)));

        return numFlashes;
    end RunStep;

    type SolutionA is new Solution with record
        null;
    end record;

    function Solve (SDisp : p11a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionA;
        result: ResultType := 0;
        junk: Boolean;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        for step in 1..100 loop
            result := result + RunStep (s.Input, junk);
        end loop;

        return result;
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p11b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionB;
        allFlashed: Boolean := false;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        for stepNumber in 1..Integer'Last loop
            declare
                junk: ResultType := RunStep(s.Input, allFlashed);
            begin
                if allFlashed then
                    return ResultType(stepNumber);
                end if;
            end;
        end loop;

        return 0;
    end Solve;

end p11;
