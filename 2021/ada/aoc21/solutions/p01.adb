with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;

with utils; use utils;

package body p01 is
    --
    -- Dispatching stuff
    --
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(new p01a'(Name => "p01a"));
        D.Append(new p01b'(Name => "p01b"));
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

    function Solve (SDisp : p01a;
                    InData : in Ada.Text_IO.File_type) return ResultType is
        s : Solution;
        Input : InputType renames s.Input;

        NIncr : ResultType := 0;
    begin
        while not End_Of_File(InData) loop
            Input.Append (GetInt(InData));
            AdvanceLine (InData);
        end loop;

        for i in (Input.First_Index+1)..Input.Last_Index loop
            if Input(i) > Input(i-1) then
                NIncr := NIncr + 1;
            end if;
        end loop;
        return NIncr;
    end Solve;

    --
    -- Solution B
    --
    function Solve (SDisp : p01b;
                    InData : in Ada.Text_IO.File_type) return ResultType is
        s : Solution;
        Input : InputType renames s.Input;

        NIncr : Natural := 0;
        WinSum1, WinSum2 : Natural := 0;
    begin
        while not End_Of_File(InData) loop
            Input.Append (GetInt(InData));
            AdvanceLine (InData);
        end loop;

        for i in Input.First_Index..(Input.First_Index + 2) loop
            WinSum1 := WinSum1 + Input(i);
        end loop;

        for i in (Input.First_Index + 1)..(Input.First_Index + 1 + 2) loop
            WinSum2 := WinSum2 + Input(i);
        end loop;

        for i in (Input.First_Index+1)..(Input.Last_Index - 3) loop
            if WinSum2 > WinSum1 then
                NIncr := NIncr + 1;
            end if;
            -- Next window becomes previous
            WinSum1 := WinSum2;
            -- Next window slides down
            WinSum2 := WinSum2 - Input(i) + Input(i + 3);
        end loop;

        -- Last window (did Last_index - 3, so the last window was not checked in the loop)
        if WinSum2 > WinSum1 then
            NIncr := NIncr + 1;
        end if;

        return ResultType(NIncr);
    end Solve;

end p01;
