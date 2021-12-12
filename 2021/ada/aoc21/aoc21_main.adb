with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Command_Line;
with Harness; use Harness;
with Ada.Real_Time; use Ada.Real_Time;

procedure aoc21_main is
    package Int_IO is new Integer_IO(Integer); use Int_IO;
    package Fl_IO is new Float_IO(Float); use Fl_IO;

    Ans : Answers;
    WasOk : Boolean := True;

    TotalReadInputTimeUs : Integer := 0;
    TotalSolveTimeUs : Integer := 0;
begin
    if not CmdSetup then
        return;
    end if;

    Ans := GetAnswers;
    Put_Line ("# Running " & Integer'Image(Integer(Ans.Length)) & " cases");
    for E of Ans loop
        Put("# " & E.Dispatcher.Name & " on " & InputNamePkg.To_String (E.Name) & " ");
        if E.ResultKnown then
            Put_Line ("expect result " & E.Result'Image);
        else
            Put_Line ("");
        end if;

        declare
            InData : File_Type;
            Result : ResultType;
            ReadInputTimeUs : Integer;
            SolveTimeUs : Integer;
        begin
            Open (InData, In_File, InputNamePkg.To_String(E.Name));

            declare
                ProblemStartTime: Time := Clock;
                SolutionStartTime : Time;
            begin
                Result := Solve (E.Dispatcher.all, InData, SolutionStartTime);
                SolveTimeUs := Integer((Clock - SolutionStartTime) / Microseconds(1));
                TotalSolveTimeUs := TotalSolveTimeUs + SolveTimeUs;
                ReadInputTimeUs := Integer((SolutionStartTime - ProblemStartTime) / Microseconds (1));
                TotalReadInputTimeUs := TotalReadInputTimeUs + ReadInputTimeUs;
            end;

            if E.ResultKnown then
                if Result = E.Result then
                    Put (" OK: " & E.Dispatcher.Name & " solve in ");
                    Put (Float(SolveTimeUs) / 1000.0, 4, 3, 0);
                    Put ("ms; read input in ");
                    Put (Float(ReadInputTimeUs) / 1000.0, 4, 3, 0);
                    put ("ms. Total: ");
                    Put (Float(SolveTimeUs + ReadInputTimeUs) / 1000.0, 4, 3, 0);
                    Put_Line ("ms");
                else
                    WasOk := False;
                    Put_Line (" FAIL: Got " & Result'Image &
                              " Expected " & E.Result'Image &
                              " Diff " & ResultType'Image(ResultType(Result - E.Result)) &
                              " In " & SolveTimeUs'Image & "us");
                end if;
            else
                Put_Line ("???: Got " & Result'Image);
            end if;

            Close (InData);
        end;
    end loop;

    Put_Line ("");
    Put ("Total: ");
    Put (Float(TotalSolveTimeUs + TotalReadInputTimeUs) / 1000.0, 7, 3, 0);
    Put ("ms; read ");
    Put (Float(TotalReadInputTimeUs) / 1000.0, 7, 3, 0);
    put ("ms. solve ");
    Put (Float(TotalSolveTimeUs) / 1000.0, 7, 3, 0);
    Put_Line ("ms");

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
