with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Bounded;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Indefinite_Holders;
with Ada.Real_Time;

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

    function Solve(SDisp : SolutionDispatcher;
                   InData : in Ada.Text_IO.File_Type;
                   StartTs : out Ada.Real_Time.Time) return ResultType is abstract;

    package SolutionDispatcherHolderPkg is new
        Ada.Containers.Indefinite_Holders(Element_Type => SolutionDispatcher'Class);
    subtype SolutionDispatcherHolder is SolutionDispatcherHolderPkg.Holder;

    -- Registered dispatchers
    package DispatchersPkg is new
        Ada.Containers.Indefinite_Vectors(Index_Type => Positive, Element_Type => SolutionDispatcher'Class);
    subtype Dispatchers is DispatchersPkg.Vector;

    --
    -- Command line and helpers;
    --
    PrintDebug : Boolean := False;
    CmdProblemNames : ProblemNames;
    SkipTests : Boolean := False;

    -- Reads command line options
    -- Returns False if the program should abort
    function CmdSetup return Boolean;

    --
    -- Answers db
    --
    package InputNamePkg is new Ada.Strings.Bounded.Generic_Bounded_Length(160);
    subtype InputName is InputNamePkg.Bounded_String;

    type Answer is record
        Dispatcher : SolutionDispatcherHolder;

        -- Name of the file containing the input
        Name : InputName;

        -- Expected result information;
        ResultKnown : Boolean;
        Result : ResultType := 0;
    end record
    with
        Dynamic_Predicate =>
            (not Answer.Dispatcher.Is_Empty) and
            (InputNamePkg.Length(Answer.Name) > 0) and
            (if not Answer.ResultKnown then Answer.Result = 0 else True);

    package AnswersPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Answer);
    subtype Answers is AnswersPkg.Vector;

    -- Return expected and unknown answers. If problem name list is specified, filtered by that.
    function GetAnswers return Answers;

end Harness;