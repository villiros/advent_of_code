with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Bounded;
with Ada.Containers.Vectors;

package Harness is
    --
    -- Common types
    --

    -- 64-bit signed
    type ResultType is range (-2**63) .. (2 ** 63 - 1);

    -- p\d\d[ab]
    subtype ProblemName is String(1..4);
    package ProblemNamesPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => ProblemName);
    subtype ProblemNames is ProblemNamesPkg.Vector;

    --
    -- Solution dispatching
    --

    -- Individual solution packages override this.
    -- Then Solve is dispatched to the correct thing.
    -- solutions package has a function with a list of all the solutions.
    type SolutionDispatcher is abstract tagged record
        Name : ProblemName;
    end record;
    type SolutionDispatcherAcc is access SolutionDispatcher'Class;

    function Solve(SDisp : SolutionDispatcher;
                   InData : in Ada.Text_IO.File_Type) return ResultType is abstract;

    -- Registered dispatchers
    package DispatchersPkg is new Ada.Containers.Vectors(Index_Type => Positive, Element_Type => SolutionDispatcherAcc);
    subtype Dispatchers is DispatchersPkg.Vector;

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

    type Answer is record
        Dispatcher : SolutionDispatcherAcc;

        -- Name of the file containing the input
        Name : InputName;

        -- Expected result information;
        ResultKnown : Boolean;
        Result : ResultType := 0;
    end record
    with
        Dynamic_Predicate =>
            (Answer.Dispatcher /= null) and
            (InputNamePkg.Length(Answer.Name) > 0) and
            (if not Answer.ResultKnown then Answer.Result = 0 else True);

    package AnswersPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Answer);
    subtype Answers is AnswersPkg.Vector;

    -- Return expected and unknown answers. If problem name list is specified, filtered by that.
    function GetAnswers(Filter : ProblemNames := ProblemNamesPkg.Empty_Vector) return Answers;

end Harness;