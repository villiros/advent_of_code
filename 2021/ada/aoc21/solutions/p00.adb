with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;

with utils; use utils;

package body p00 is
    --
    -- Dispatching stuff
    --
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(new p00a'(Name => "p00a"));
        D.Append(new p00b'(Name => "p00b"));
    end GetDispatchers;
    
    --
    -- Solution A
    --
    package InputPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Integer);
    use InputPkg;
    subtype InputType is InputPkg.Vector;

    type Solution is record
        Input : InputType := InputPkg.Empty_Vector;
    end record;

    function Solve (SDisp : p00a;
                    InData : in Ada.Text_IO.File_type) return ResultType is
        s : Solution;
        Input : InputType renames s.Input;

        Result : ResultType := 0;
    begin
        while not End_Of_File(InData) loop
            Input.Append (GetInt(InData));
            AdvanceLine (InData);
        end loop;

        return Result;
    end Solve;

    --
    -- Solution B
    --
    function Solve (SDisp : p00b;
                    InData : in Ada.Text_IO.File_type) return ResultType is
        s : Solution;
        Input : InputType renames s.Input;

        Result : ResultType := 0;
    begin
        while not End_Of_File(InData) loop
            Input.Append (GetInt(InData));
            AdvanceLine (InData);
        end loop;

        return Result;
    end Solve;

end p00;
