with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.Command_Line;   use GNAT.Command_Line;

with Utils; use Utils;
with Solutions;

package body Harness is
    package CMD renames Ada.Command_Line;

    function CmdSetup return Boolean is
    begin
        loop
            case Getopt ("d p: -notest h") is
                when 'd' =>
                    Put_Line("  # Printing debug logs");
                    PrintDebug := True;
                when 'p' =>
                    declare
                        param: String := Parameter;
                    begin
                        if param'Length /= 4 then
                            Put_Line ("ERROR: -p argument " & Param & " is too not pXX[ab]");
                            return False;
                        else
                            CmdProblemNames.Append(param);
                        end if;
                    end;
                when '-' =>
                    if Full_Switch = "-notest" then
                        SkipTests := True;
                    else
                        Put_Line ("Invalid argument: " & Full_switch);
                        return False;
                    end if;
                when 'h'  =>
                    Put_Line ("advent [-d] [-p pXXX] [-notest]");
                    Put_Line ("  -d: Print debug logs.");
                    Put_Line ("  -p: Run only specified problems. Ex: p01b");
                    Put_Line ("  --notest: Don't run _test cases");
                    return False;
                when ASCII.nul =>
                    exit;
                when others =>
                    Put_Line ("Invalid argument: " & Full_switch);
                    return False;
            end case;
        end loop;
        return True;
    end CmdSetup;

    function GetAnswers return Answers is
        use InputStrPkg;
        use InputNamePkg;

        Filter : ProblemNames renames CmdProblemNames;

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
                    input_fname : InputName := To_Bounded_String("../../input/" & To_String(GetAtom (File)));
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
                    elsif SkipTests and Index(input_fname, "_test") /= 0 then
                        -- Skipping test.
                        null;
                    else
                        Result.Append (Answer'(Dispatcher => SolutionDispatcherHolderPkg.To_Holder(Solutions.GetSolution (problem_name)),
                                               Name => input_fname,
                                               ResultKnown => result_known,
                                               Result => expected_result));
                    end if;
                end;
            end if;
        end loop Main;

        Close (File);

        return Result;
    end GetAnswers;

end harness;