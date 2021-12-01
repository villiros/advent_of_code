with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;

with utils; use utils;

package body p01b_pkg is
    function Make return SolutionAcc is
    begin
        return new p01b;
    end Make;

    procedure Solve (Self : in out p01b;
                     InData : in Ada.Text_IO.File_type;
                     Result : out ResultType) is
        Target : constant Integer := 2020;

        Input : InputType renames Self.Input;
    begin
        while not End_Of_File(InData) loop
            declare
                n : Integer;
                did_read : Boolean;
            begin
                GetInt(InData, n, did_read);
                if did_read then
                    Input.Append(n);
                end if;

                exit when not SkipNl (InData);
            end;
        end loop;

        for i in Input.First_Index..Input.Last_Index loop
            --Put_Line (i'Image);
            declare
                n : Integer := Input(i);
            begin
                for j in (i+1)..Input.Last_Index loop
                    declare
                        n2 : Integer := Input(j);
                        t : Integer := Target - n - n2;
                        el : InputPkg.Cursor := Input.Find (t, Input.To_Cursor(j + 1));
                    begin
                        if el /= InputPkg.No_Element then
                            Result := ResultType(n * n2 * Element(el));
                            return;
                        end if;
                    end;
                end loop;
            end;
        end loop;

        raise NoSolution;

    end Solve;

end p01b_pkg;