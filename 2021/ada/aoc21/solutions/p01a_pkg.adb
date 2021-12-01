with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;

with utils; use utils;

package body p01a_pkg is
    function Make return SolutionAcc is
    begin
        return new p01a;
    end;

    procedure Solve (Self : in out p01a;
                     InData : in Ada.Text_IO.File_type;
                     Result : out ResultType) is
        Input : InputType renames Self.Input;

        NIncr : Natural := 0;
    begin
        while not End_Of_File(InData) loop
            declare
                n : Integer;
                did_read : Boolean;
            begin
                GetInt(InData, n, did_read);
                Assert(did_read);
                Input.Append(n);
                exit when not SkipNl (InData);
            end;
        end loop;

        for i in (Input.First_Index+1)..Input.Last_Index loop
            if Input(i) > Input(i-1) then
                NIncr := NIncr + 1;
            end if;
        end loop;

        Result := ResultType(NIncr);

        -- for i in Input.First_Index..Input.Last_Index loop
        --     --Put_Line (i'Image);
        --     declare
        --         n : Integer := Input(i);
        --         t : Integer := Target - n;
        --         el : InputPkg.Cursor := Input.Find (t, Input.To_Cursor(i + 1));
        --     begin
        --         if el /= InputPkg.No_Element then
        --             Result := ResultType(n * Element(el));
        --             return;
        --         end if;
        --     end;
        -- end loop;

        -- raise NoSolution;

    end Solve;

end p01a_pkg;
