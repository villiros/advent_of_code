with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;

with utils; use utils;

package body p04 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(new p04a'(Name => "p04a"));
        D.Append(new p04b'(Name => "p04b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    ------------------------------------
    -- Input data definitions
    ------------------------------------
    subtype BNumType is ResultType range 0..99;
    subtype RowRange is ResultType range 1..5;
    subtype ColRange is ResultType range 1..5;

    type BCell is record
        Num : BNumType;
        Marked : Boolean := False;
    end record;

    type Board is Array(RowRange, ColRange) of BCell;

    package BoardsPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Board);
    use BoardsPkg;
    subtype BoardsType is BoardsPkg.Vector;

    package NumsPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => BNumType);
    use NumsPkg;
    subtype NumsType is NumsPkg.Vector;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    type Solution is tagged record
        Nums : NumsType;
        Boards : BoardsType;
    end record;

    procedure ReadInput(self : in out Solution'class; InData : File_Type)
    with
        Post => (not self.Nums.Is_Empty) and
                (not self.Boards.Is_Empty)
    is
    begin
        loop
            self.Nums.Append (BNumType(GetInt(InData)));
            declare
                c : Character := PeekChar (InData);
            begin
                exit when c /= ',';
                c := GetChar (InData);
            end;
        end loop;

        AdvanceLine (InData); AdvanceLine (InData);

        while not End_Of_File(InData) loop
            declare
                B : Board;
            begin
                for Row in RowRange loop
                    for Col in ColRange loop
                        B(Row, Col) := (Num => BNumType(GetInt (InData)),
                                        others => <>);
                    end loop;
                    AdvanceLine (InData);
                end loop;

                self.Boards.Append (B);
                AdvanceLine (InData);
            end;
        end loop;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    procedure PrintBoard(B : in Board) is
    begin
        if PrintDebug then
            for R in RowRange loop
                for C in ColRange loop
                    Put(" " & (if B(R, C).Marked then "X" else B(R, C).Num'Image));
                end loop;
                Put_Line ("");
            end loop;
            Put_Line ("");
        end if;
    end PrintBoard;

    function SumUnmarked(B : in Board) return ResultType is
        Result : ResultType := 0;
    begin
        for R in RowRange loop
            for C in ColRange loop
                if not B(R, C).Marked then
                    Result := Result + ResultType(B(R, C).Num);
                end if;
            end loop;
        end loop;
        return Result;
    end SumUnmarked;

    function IsBingo(B : in Board; Row : RowRange; Col : ColRange) return Boolean is
        ColMarked, RowMarked : Boolean := True;
    begin
        for C in ColRange loop
            ColMarked := ColMarked and B(Row, C).Marked;
        end loop;

        for R in RowRange loop
            RowMarked := RowMarked and B(R, Col).Marked;
        end loop;

        return ColMarked or RowMarked;
    end IsBingo;

    function MarkAndCheck(B : in out Board; N : BNumType) return Boolean
    with
        Pre =>
            -- Boards with bingo shouldn't be checked again.
            (for all R in RowRange =>
                (for all C in ColRange =>
                    not IsBingo(B, R, C))),
        Post =>
            -- Below code assumes there are no duplicate numbers
            -- This assertion would fail if there are duplicate matching numbers though.
            (for all R in RowRange =>
                (for all C in ColRange =>
                    (if B(R, C).Num = N then B(R, C).Marked else True)))
    is
    begin
        for Row in RowRange loop
            for Col in ColRange loop
                declare
                    Cell : BCell renames B(Row, Col);
                begin
                    if Cell.Num = N and not Cell.Marked then
                        Cell.Marked := True;
                        if IsBingo(B, Row, Col) then
                            return True;
                        end if;
                    end if;
                end;
            end loop;
        end loop;
        return False;
    end MarkAndCheck;

    type SolutionA is new Solution with record
        null;
    end record;

    function Solve (SDisp : p04a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionA;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        for I of s.Nums loop
            if PrintDebug then Put_Line ("After " & I'Image); end if;
            
            for B of S.Boards loop
                if MarkAndCheck (B, I) then
                    return SumUnmarked (B) * ResultType(I);
                end if;
            end loop;
        end loop;

        return 0;
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p04b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionB;
        LastResult : ResultType := 0;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        declare
            DoneBoards : array (0..s.Boards.Last_Index) of Boolean := (others => False);
        begin
            for I of s.Nums loop
                for BI in s.Boards.First_Index..s.Boards.Last_Index loop
                    declare
                        B : Board renames s.Boards(BI);
                    begin
                        if (not DoneBoards(BI)) and then MarkAndCheck (B, I) then
                            LastResult := SumUnmarked (B) * ResultType(I);
                            DoneBoards(BI) := True;
                        end if;
                    end;
                end loop;
            end loop;
        end;

        return LastResult;
    end Solve;

end p04;
