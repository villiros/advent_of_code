with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;

with Harness; use Harness;
with utils; use utils;

package body p10 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(new p10a'(Name => "p10a"));
        D.Append(new p10b'(Name => "p10b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    CodeError: exception;

    -- Using characters with dynamic constraint for simplicity.
    subtype Char is Character
        with Static_Predicate => Char in '(' | ')' | '[' | ']' | '{' | '}' | '<' | '>';
    
    subtype OpenChar is Char
        with Static_Predicate => OpenChar in '(' | '[' | '{' | '<';
    
    subtype CloseChar is Char
        with Static_Predicate => CloseChar in ')' | ']' | '}' | '>';
    
    -- Convert opening character to a matching closing one
    function ToClose(c: OpenChar) return CloseChar is
    begin
        case c is
            when '(' => return ')';
            when '[' => return ']';
            when '{' => return '}';
            when '<' => return '>';
        end case;
    end ToClose;
    
    -- Stack of opening characters
    package StackPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => OpenChar);
    use StackPkg;
    subtype StackType is StackPkg.Vector;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    package LinePkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Char);
    use LinePkg;
    subtype LineType is LinePkg.Vector;

    package InputPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => LineType);
    use InputPkg;
    subtype InputType is InputPkg.Vector;

    type Solution is tagged record
        Input : InputType := InputPkg.Empty_Vector;
    end record;

    procedure ReadInput(self : in out Solution'class; InData : File_Type)
    with
        Post => not self.Input.Is_Empty
    is
    begin
        while not End_Of_File(InData) loop
            declare
                line: LineType;
            begin
                while not (PeekChar (InData) in ASCII.LF | ASCII.nul) loop
                    declare
                        c: Char := GetChar(InData);
                    begin
                        line.Append(c);
                    end;
                end loop;
                self.Input.Append (Line);
                AdvanceLine (InData);
            end;
        end loop;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    function Score(c: CloseChar) return ResultType is
    begin
        return (case c is
                    when ')' => 3,
                    when ']' => 57,
                    when '}' => 1197,
                    when '>' => 25137);
    end Score;


    type SolutionA is new Solution with record
        null;
    end record;

    function Solve (SDisp : p10a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionA;
        result: ResultType := 0;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        for line of s.Input loop
            declare
                stack: StackType;
            begin
                char_loop:
                for c of line loop
                    case c is
                        when OpenChar =>
                            stack.Append(c);
                        when others =>
                            if (stack = StackPkg.Empty_Vector) or (ToClose(stack.Last_Element) /= c) then
                                result := result + Score(c);
                                exit char_loop;
                            else
                                stack.Delete_Last(1);
                            end if;
                        end case;
                end loop char_loop;
            end;
        end loop;

        return result;
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    type SolutionB is new Solution with record
        null;
    end record;

    function Scoreb(c: CloseChar) return ResultType is
    begin
        return (case c is
                    when ')' => 1,
                    when ']' => 2,
                    when '}' => 3,
                    when '>' => 4);
    end ScoreB;

    function Solve (SDisp : p10b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionB;
        package ResultPkg is new
            Ada.Containers.Vectors(Index_Type => Positive, Element_Type => ResultType);
        use ResultPkg;
        package ResultSorting is new ResultPkg.Generic_Sorting;

        result: ResultPkg.Vector;

        DiscardLine: exception;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        for line of s.Input loop
            declare
                stack: StackType;
                lineScore: ResultType := 0;
            begin
                char_loop:
                for c of line loop
                    case c is
                        when OpenChar =>
                            stack.Append(c);
                        when others =>
                            if (stack = StackPkg.Empty_Vector) or (ToClose(stack.Last_Element) /= c) then
                                raise DiscardLine;
                            else
                                stack.Delete_Last(1);
                            end if;
                        end case;
                end loop char_loop;

                while not stack.Is_Empty loop
                    lineScore := lineScore * 5 + ScoreB(ToClose(stack.Last_Element));
                    stack.Delete_Last (1);
                end loop;

                -- Non-empty stack once the line is read will always produce a positive score.
                if lineScore > 0 then
                    result.Append (lineScore);
                end if;
            exception
                when DiscardLine => null;
            end;
        end loop;

        ResultSorting.Sort (result);

        return result(Result.First_Index + (Result.Last_Index - Result.First_Index) / 2);
    end Solve;

end p10;
