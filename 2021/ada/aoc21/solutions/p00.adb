with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;

with utils; use utils;

package body p00 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(new p00a'(Name => "p00a"));
        D.Append(new p00b'(Name => "p00b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    ------------------------------------
    -- Input data definitions
    ------------------------------------

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    package InputPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Integer);
    use InputPkg;
    subtype InputType is InputPkg.Vector;

    type Solution is tagged record
        Input : InputType := InputPkg.Empty_Vector;
    end record;

    procedure ReadInput(InData : File_Type; S : in out Solution'class) is
    begin
        while not End_Of_File(InData) loop
            s.Input.Append (GetInt(InData));
            AdvanceLine (InData);
        end loop;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    type SolutionA is new Solution with record
        null;
    end record;

    function Solve (SDisp : p00a;
                    InData : in Ada.Text_IO.File_type) return ResultType is
        s : SolutionA;
        Input : InputType renames s.Input;
    begin
        ReadInput(InData, s);

        -- do it here

        return 0;
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p00b;
                    InData : in Ada.Text_IO.File_type) return ResultType is
        s : SolutionB;
        Input : InputType renames s.Input;
    begin
        ReadInput(InData, s);

        -- do it here

        return 0;
    end Solve;

end p00;
