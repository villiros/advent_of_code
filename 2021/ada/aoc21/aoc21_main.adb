with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Command_Line;
with Harness; use Harness;
with solutions;
with p01a_pkg;

procedure aoc21_main is
    Ans : Answers;
    WasOk : Boolean := True;
begin
    CmdSetup;
    Ans := GetAnswers(CmdGetSolutionsFilter);
    Put_Line ("   # Running " & Integer'Image(Integer(Ans.Length)) & " cases");
    for E of Ans loop
        Put("   # " & E.Problem & " on " & InputNamePkg.To_String (E.Name) & " ");
        if E.ResultKnown then
            Put_Line ("expect result " & E.Result'Image);
        else
            Put_Line ("");
        end if;

        declare
            SolAcc : SolutionAcc := solutions.GetSolution (E.Problem);
            InData : File_Type;
            Result : ResultType;
        begin
            Open (InData, In_File, InputNamePkg.To_String(E.Name));

            SolAcc.Solve(InData, Result);

            if E.ResultKnown then
                if Result = E.Result then
                    Put_Line ("OK");
                else
                    WasOk := False;
                    Put_Line ("FAIL: Got " & Result'Image &
                              " Expected " & E.Result'Image &
                              " Diff " & ResultTypeDiff'Image(ResultTypeDiff(Result - E.Result)));
                end if;
            else
                Put_Line ("???: Got " & Result'Image);
            end if;

            Close (InData);
        end;
    end loop;

    Put_Line ("");
    if WasOk then
        Put_Line ("ALL OK");
        Ada.Command_Line.Set_Exit_Status (0);
    else
        Put_Line ("FAILURES");
        Ada.Command_Line.Set_Exit_Status (1);
    end if;
exception
    when Error: Others =>
        Put("ERROR: Unexpected exception: ");
        Put_Line (Exception_Information(Error));
end aoc21_main;
