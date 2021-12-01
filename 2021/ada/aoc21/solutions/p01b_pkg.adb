with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;

with utils; use utils;
with Harness; use Harness;

package body p01b_pkg is
    function Make return SolutionAcc is
    begin
        return new p01b;
    end;

    procedure Solve (Self : in out p01b;
                     InData : in Ada.Text_IO.File_type;
                     Result : out ResultType) is
        Input : InputType renames Self.Input;

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

        Result := ResultType(NIncr);
    end Solve;

end p01b_pkg;
