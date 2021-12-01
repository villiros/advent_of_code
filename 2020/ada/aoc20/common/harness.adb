with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Utils; use Utils;

package body Harness is
package CMD renames Ada.Command_Line;

function CmdGetSolutionsFilter return ProblemNames is
    BadArgument : exception;
    Result : ProblemNames;

    procedure addParam(Val : String) is
    begin
        if Val'Length /= 4 then
            Put_Line ("ERROR: -p argument " & Val & " is too not pXX[ab]");
            raise BadArgument with ("ERROR: -p argument " & Val & " is too not pXX[ab]");
        end if;

        Result.Append (Val);
    end addParam;

begin
    for i in 1..CMD.Argument_Count loop
        if CMD.Argument (i) = "-p" and i < CMD.Argument_Count then
            addParam (Cmd.Argument(i+1));
        elsif Index(CMD.Argument(i), "-p") = 1 then
            addParam (Cmd.Argument(i)(3..(Cmd.Argument(i)'Length)));
        end if;
    end loop;
    return Result;
end CmdGetSolutionsFilter;

function GetAnswers(Filter : ProblemNames := ProblemNamesPkg.Empty_Vector) return Answers is
    File : File_Type;
    Result : Answers;
begin
    Open (File => File, Mode => In_File, Name => "../../answers");

    Main:
    loop
        while SkipNl (File) loop
            null;
        end loop;

        exit when End_Of_File(File);

        declare
            should_produce : Boolean := True;
            solution_name : String := GetAtom (File);
            fname : String := GetAtom(File);
            resultStr : String := GetAtom (File);
        begin
            if not Filter.Is_Empty then
                should_produce := False;
                for i of Filter loop
                    if solution_name = i then
                        should_produce := True;
                        exit;
                    end if;
                end loop;
            end if;
            if (not should_produce) or resultStr = "SKIP" or solution_name(1) = '#' then
                null;
            elsif resultStr = "?" then
                Result.Append (Answer'(Problem => solution_name,
                                    Name => InputNamePkg.To_Bounded_String("../../input/" & fname),
                                    ResultKnown => False,
                                    Result => 0));
            else
                Result.Append (Answer'(Problem => solution_name,
                                        Name => InputNamePkg.To_Bounded_String ("../../input/" & fname),
                                        ResultKnown => True,
                                        Result => ResultType'Value(resultStr)));
            end if;

        end;
    end loop Main;

    return Result;
end GetAnswers;

end harness;