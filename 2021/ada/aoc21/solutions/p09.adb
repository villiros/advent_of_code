with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;

with Harness; use Harness;
with utils; use utils;

package body p09 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(new p09a'(Name => "p09a"));
        D.Append(new p09b'(Name => "p09b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    -- Quick-n-dirty solution.

    type Height is range 0..9;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    package RowPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Height);
    use RowPkg;
    subtype RowType is RowPkg.Vector;

    subtype HeightVec is RowType;

    package InputPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => RowType);
    use InputPkg;
    subtype InputType is InputPkg.Vector;

    type Solution is tagged record
        Input : InputType := InputPkg.Empty_Vector;
    end record;

    procedure ReadInput(self : in out Solution'class; InData : File_Type)
    with
        Post => (not self.Input.Is_Empty)
    is
    begin
        while not End_Of_File(InData) loop
            declare
                r: RowType;
                c: Character := PeekChar (InData);
            begin
                while (PeekChar (InData) /= ASCII.nul) and (PeekChar(InData) /= ASCII.LF) loop
                    r.Append (Height'Value("" & GetChar(InData)));
                end loop;
                self.Input.Append(r);
                AdvanceLine (InData);
            end;
        end loop;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    function InBounds(M:InputType; X, Y: Integer) return Boolean
    is begin
        return (X in Positive) and (Y in Positive) and
                (X <= M.Last_Index) and (Y <= M.First_Element.Last_Index);
    end InBounds;

    type Heights is array(Natural range <>) of Height;
    function GetAdjacent(M: InputType; X: Integer; Y: Integer) return Heights is
        res: array(0..10) of Height := (others => 0);
        numRes: Natural := 0;
    begin
        if InBounds (M, X - 1, Y) then
            res(numRes) := M(X - 1)(Y);
            numRes := numRes + 1;
        end if;
        if InBounds (M, X + 1, Y) then
            res(numRes) := M(X + 1)(Y);
            numRes := numRes + 1;
        end if;
        if InBounds (M, X, Y - 1) then
            res(numRes) := M(X)(Y - 1);
            numRes := numRes + 1;
        end if;
        if InBounds (M, X, Y + 1) then
            res(numRes) := M(X)(Y + 1);
            numRes := numRes + 1;
        end if;

        declare
            result: Heights := Heights(res(0..(numRes-1)));
        begin
            return result;
        end;
    end GetAdjacent;

    type SolutionA is new Solution with record
        null;
    end record;

    function Solve (SDisp : p09a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionA;
        result: ResultType := 0;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        for X in s.Input.First_Index..s.Input.Last_Index loop
            for Y in s.Input.First_Element.First_Index..s.Input.First_Element.Last_Index loop
                declare
                    adj: Heights := GetAdjacent(s.Input, X, Y);
                begin
                    if (for all i in adj'Range => adj(i) > s.Input(X)(Y)) then
                        result := result + ResultType(Height'(s.Input(X)(Y))) + 1;
                    end if;
                end;
            end loop;
        end loop;

        return result;
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    type SolutionB is new Solution with record
        null;
    end record;

    function IsLowest(Input: InputType; X, Y: Positive) return Boolean is
        adj: Heights := GetAdjacent(Input, X, Y);
    begin
        return (for all i in adj'Range => adj(i) > Input(X)(Y));
    end IsLowest;

    function DoIt(Input: InputType) return ResultType is
        subtype BasinNumber is Integer;

        package BasinSizePkg is new
            Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Integer);
        use BasinSizePkg;
        subtype BasinSizes is BasinSizePkg.Vector;
        use type BasinSizes;

        package SizeSorting is new BasinSizePkg.Generic_Sorting;
        use SizeSorting;

        -- 0: don't know, otherwise basin number
        type BasMap is array(Input.First_index..Input.Last_Index,
                             Input.First_element.First_index..Input.Last_element.Last_index) of BasinNumber;
        Basins: BasMap := (others => (others => 0));

        numBasins: Natural := 0;
        -- Track size of each discovered basin here
        -- Basin currently being explored is last element, also sizes(numBasins)
        sizes: BasinSizes;

        result: ResultType := 1;

        procedure Fill(X, Y: Integer; PrevHeight: Integer) is
        begin
            -- Point must be in bounds, not explored already,
            -- not a 9, and heigher than previous.
            if not InBounds (Input, X, Y) then
                return;
            end if;

            if Basins(X, Y) /= 0 then
                return;
            end if;

            if Input(X)(Y) = 9 or Integer(Height'(Input(X)(Y))) <= PrevHeight then
                return;
            end if;

            -- Ok, new point for the basin. Mark it and increase the size.
            Basins(X, Y) := numBasins;
            sizes(numBasins) := sizes(numBasins) + 1;

            -- Try nearby points.
            Fill(X-1, Y, Integer(Height'(Input(X)(Y))));
            Fill(X+1, Y, Integer(Height'(Input(X)(Y))));
            Fill(X, Y-1, Integer(Height'(Input(X)(Y))));
            Fill(X, Y+1, Integer(Height'(Input(X)(Y))));
        end Fill;
    begin
        for X in Input.First_Index..Input.Last_Index loop
            for Y in Input.First_Element.First_Index..Input.First_Element.Last_Index loop
                if Basins(X, Y) = 0 and Input(X)(Y) < 9 and IsLowest(Input, X, Y) then
                    numBasins := numBasins + 1;
                    sizes.Append(0);
                    -- Pass -1 for height to get the fill to accept this.
                    Fill(X, Y, -1);
                end if;
            end loop;
        end loop;

        Sort (sizes);
        Reverse_Elements (sizes);

        for i in sizes.First_Index..(sizes.First_Index + 2) loop
            result := result * ResultType(Integer'(sizes(i)));
        end loop;

        return result;
    end DoIt;

    function Solve (SDisp : p09b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionB;
        --lowPoints: HeightVec;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        return DoIt (s.Input);
    end Solve;

end p09;
