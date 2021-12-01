with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;

with utils; use utils;

package body p02a_pkg is
    function Make return SolutionAcc is
    begin
        return new p02a;
    end Make;

    procedure Solve (Self : in out p02a;
                     InData : in Ada.Text_IO.File_type;
                     Result : out ResultType) is
        Input : InputType renames Self.Input;
        NumValid : Natural := 0;

        did_read : Boolean;
    begin
        while not End_Of_File(InData) loop
            declare
                L, H : Integer;
                C : Character;
                Pwd : InputStr;
            begin
                GetInt(InData, L, did_read);
                Assert (GetChar(InData) = '-');
                GetInt(InData, H, did_read);
                Assert(SkipWs (InData));
                C := GetChar (InData);
                Assert (GetChar(InData) = ':');
                Pwd := GetAtom (InData);
                Assert(SkipNl (InData));

                Input.Append (IEntry'(Lowest => L, Highest => H, Letter => "" & C, Password => Pwd));
            end;
        end loop;

        if PrintDebug then Put_Line("Input is" & Input'Image); end if;

        for E of Input loop
            declare
                ccount : Natural := InputStrPkg.Count (E.Password, E.Letter);
            begin
                if E.Lowest <= ccount and ccount <= E.Highest then
                    NumValid := NumValid + 1;
                end if;
            end;
        end loop;

        Result := ResultType(NumValid);
    end Solve;
end p02a_pkg;