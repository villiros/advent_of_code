with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;

with utils; use utils;

package body p02 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(new p02a'(Name => "p02a"));
        D.Append(new p02b'(Name => "p02b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    ------------------------------------
    -- Input data definitions
    ------------------------------------

    type OpType is (Forward, Up, Down);
    function GetOp is new GetEnum(OpType);
    -- Component of a coordinate
    subtype NumberLineType is ResultType range 0..ResultType'Last;

    type Coordinate is record
        X, Y : NumberLineType := 0;
    end record;

    -- Number of steps
    subtype CountType is ResultType range 1..ResultType'Last;
    -- Step description
    type Step is record
        Op :  OpType;
        Count : CountType;
    end record;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    package InputPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Step);
    use InputPkg;
    subtype InputType is InputPkg.Vector;

    type Solution is tagged record
        Input : InputType := InputPkg.Empty_Vector;
    end record;

    procedure ReadInput(InData : File_Type; S : in out Solution'class) is
    begin
        while not End_Of_File(InData) loop
            declare
                M : OpType := GetOp(InData);
                C : CountType := CountType(GetInt(InData));
            begin
                s.Input.Append(Step'(Op => M, Count => C));
            end;
            AdvanceLine (InData);
        end loop;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------
    
    type SolutionA is new Solution with record
        Pos : Coordinate;
    end record;

    function Solve (SDisp : p02a;
                    InData : in Ada.Text_IO.File_type) return ResultType is
        s : SolutionA;
        Input : InputType renames s.Input;
        Pos : Coordinate renames S.Pos;
    begin
        ReadInput(InData, s);

        for E of Input loop
            case E.Op is
                when Forward => Pos.X := Pos.X + E.Count;
                when Up => Pos.Y := Pos.Y - E.Count;
                when Down => Pos.Y := Pos.Y + E.Count;
            end case;
        end loop;

        return Pos.X * Pos.Y;
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------
    subtype AimAmount is ResultType;

    type SolutionB is new SolutionA with record
        Aim : AimAmount := 0;
    end record;

    function Solve (SDisp : p02b;
                    InData : in Ada.Text_IO.File_type) return ResultType is
        s : SolutionB;
        Input : InputType renames s.Input;
        Pos : Coordinate renames S.Pos;
    begin
        ReadInput(InData, s);

        for E of Input loop
            case E.Op is
                when Forward =>
                    Pos.X := Pos.X + E.Count;
                    Pos.Y := Pos.Y + s.Aim * E.Count;
                when Up =>
                    s.Aim := s.Aim - E.Count;
                when Down =>
                    s.Aim := s.Aim + E.Count;
            end case;
        end loop;

        return Pos.X * Pos.Y;
    end Solve;

end p02;
