with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Utils; use Utils;
with Solutions;

package body Harness is
    package CMD renames Ada.Command_Line;

    procedure CmdSetup is
    begin
        for i in 1..CMD.Argument_Count loop
            if CMD.Argument(i) = "-d" then
                Put_Line("  # Printing debug logs");
                PrintDebug := True;
            end if;
        end loop;
    end CmdSetup;

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
        use InputStrPkg;
        use InputNamePkg;

        File : File_Type;
        Result : Answers;
    begin
        Open (File => File, Mode => In_File, Name => "../../answers");

        Main:
        loop
            -- Skip empty lines
            loop
                begin
                    exit when not AdvanceLine (File);
                exception
                    when ReadFailed => exit;
                end;
            end loop;

            SkipWs (File);

            -- Are we done with the file?
            exit when End_Of_File(File);

            -- At this point we *should* be at a line.
            -- But check if it's a comment
            if PeekChar (File) = '#' then
                SkipAll (File);
            else
                -- Read in fields
                declare
                    problem_name : ProblemName := To_String(GetAtom (File));
                    input_fname : InputName := To_Bounded_String(To_String("../../input/" & GetAtom (File)));
                    result_known : Boolean := PeekChar (File, DoSkipWs => True) /= '?';
                    expected_result : ResultType := (if result_known then ResultType(GetInt(File)) else 0);
                begin
                    -- If result is not known, we didn't actually read the last field
                    if not result_known then
                        SkipAll (File);
                    end if;
                    if not Filter.Is_Empty and then Filter.Find_Index (problem_name) = ProblemNamesPkg.No_Index then
                        -- Not empty and problem name not in the filter. Skip;
                        null;
                    else
                        Result.Append (Answer'(Dispatcher => Solutions.GetSolution (problem_name),
                                               Name => input_fname,
                                               ResultKnown => result_known,
                                               Result => expected_result));
                    end if;
                end;
            end if;
        end loop Main;

        return Result;
    end GetAnswers;

end harness;