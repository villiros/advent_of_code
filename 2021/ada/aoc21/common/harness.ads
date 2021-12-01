with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Bounded;
with Ada.Containers.Vectors;

package Harness is
    -- 64-bit unsigned
    type ResultType is range 0 .. (2 ** 64 - 1);
    type ResultTypeDiff is range -(ResultType'Last)..(ResultType'Last);
    -- p\d\d[ab]
    subtype ProblemName is String(1..4);
    package ProblemNamesPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => ProblemName);
    subtype ProblemNames is ProblemNamesPkg.Vector;

    NoSolution : exception;

    type Solution is abstract tagged null record;

    type SolutionAcc is access Solution'Class;

    --function Name (Self : Solution) return ProblemName is abstract;

    procedure Solve (Self : in out Solution;
                     InData : in Ada.Text_IO.File_type;
                     Result : out ResultType) is abstract;

    --
    -- Command line and helpers;
    --
    PrintDebug : Boolean := False;

    function CmdGetSolutionsFilter return ProblemNames;

    -- Checks for standard command-line options
    procedure CmdSetup;

    --
    -- Answers db
    --
    package InputNamePkg is new Ada.Strings.Bounded.Generic_Bounded_Length(160);
    subtype InputName is InputNamePkg.Bounded_String;

    type InputFile is access File_Type;

    type Answer is record
        Problem : ProblemName;
        -- Name of the file containing the input
        Name : InputName;
        -- Expected result information;
        ResultKnown : Boolean;
        Result : ResultType := 0;
    end record;
    package AnswersPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Answer);
    subtype Answers is AnswersPkg.Vector;

    -- Return expected and unknown answers. If problem name list is specified, filtered by that.
    function GetAnswers(Filter : ProblemNames := ProblemNamesPkg.Empty_Vector) return Answers;

end Harness;