with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;

with utils; use utils;

package body p05 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(new p05a'(Name => "p05a"));
        D.Append(new p05b'(Name => "p05b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------
    type XCoord is range 0..1000;
    type YCoord is range 0..1000;
    type NumLines is range 0..10000;

    type MapType is Array(XCoord, YCoord) of NumLines;

    type Coord is record
        X : XCoord;
        Y : YCoord;
    end record;

    function Image(self : Coord) return String is
        ("(" & self.X'Image & ", " & self.Y'Image & ")");

    type CoordsArr is array(Positive range <>) of Coord;

    package LinePkg is
        type AxisDirection is range -1..1;

        -- Only tagged to make foo.Bar() work
        type LineType is tagged record
            From, To : Coord;
        end record
        with
            Dynamic_Predicate =>
                From /= To;

        function Image(self : LineType) return String is
            ("(" & Image (self.From) & " -> " & Image(self.To) & ")");

        function DX(self: LineType) return XCoord is
            (XCoord(abs (Integer(self.From.X) - Integer(self.To.X))));

        function DY(self: LineType) return YCoord is
            (YCoord(abs (Integer(self.From.Y) - Integer(self.To.Y))));
        
        function DirX(self: LineType) return AxisDirection is
            (if self.To.X > self.From.X then 1
             elsif self.From.X > self.To.X then -1
             else 0);

        function DirY(self: LineType) return AxisDirection is
            (if self.To.Y > self.From.Y then 1
             elsif self.From.Y > self.To.Y then -1
             else 0);
        
        function IsHoriz(self: LineType) return Boolean is
            (self.DY = 0);

        function IsVert(self: LineType) return Boolean is
            (self.DX = 0);

        function IsDiag(self: LineType) return Boolean is
            (Natural(self.DX) = Natural(self.DY));
        
        function IsValidLine(self: LineType) return Boolean is
            (self.IsHoriz or self.IsVert or self.IsDiag);
        
        -- Number of points on the line
        -- LineType does not allow points, so this returns at least 2
        function NumPoints(self: LineType) return Positive
        with
            Pre => self.IsValidLine,
            Post => NumPoints'Result >= 2;
        
        -- Is the coordinate on the line?
        function IsOn(self: LineType; C: Coord) return Boolean;

        function CoordAlong(self: LineType; PointN: Natural) return Coord
        with
            Pre => self.IsValidLine and
                   (PointN < self.NumPoints),
            Post =>
                self.IsOn(CoordAlong'Result);
    end LinePkg;
    use LinePkg;

    package body LinePkg is
        function NumPoints(self: LineType) return Positive
        is
        begin
            return Positive'Max(Natural(self.DX) + 1, Natural(self.DY) + 1);
        end NumPoints;

        function IsOn(self: LineType; C: Coord) return Boolean
        is
        begin 
            if C = self.From or C = self.To then
                return True;
            elsif not IsValidLine((From => self.From, To => C)) then
                return False;
            else
                -- From->C is a valid line
                -- Just check that C is within self's bounds
                return ((XCoord'Min(self.From.X, self.To.X) <= C.X) and
                        (XCoord'Max(self.From.X, self.To.X) >= C.X) and
                        (YCoord'Min(self.From.Y, self.To.Y) <= C.Y) and
                        (YCoord'Max(self.From.Y, self.To.Y) >= C.Y));
            end if;
        end IsOn;

        function CoordAlong(self: LineType; PointN: Natural) return Coord is
        begin
            return (X => XCoord(Integer(self.From.X) + Integer(self.DirX) * PointN),
                    Y => YCoord(Integer(self.From.Y) + Integer(self.DirY) * PointN));
        end CoordAlong;

    end LinePkg;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

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
        use type InputStr; use InputStrPkg;
    begin
        while not End_Of_File(InData) loop
            declare
                X1 : XCoord := XCoord(GetInt(InData));
                junk : Character := GetChar (InData);
                Y1 : YCoord := YCoord(GetInt(InData));
                junk2 : InputStr := GetAtom (InData);
                X2 : XCoord := XCoord(GetInt(InData));
                junk3 : Character := GetChar (InData);
                Y2 : YCoord := YCoord(GetInt(InData));
            begin
                self.Input.Append (LineType'(From => (X1, Y1), To => (X2, Y2)));
            end;
            AdvanceLine (InData);
        end loop;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    type SolutionA is new Solution with record
        null;
    end record;

    function Solve (SDisp : p05a;
                    InData : in Ada.Text_IO.File_type) return ResultType is
        s : SolutionA;

        m : MapType := (others => (others => 0));

        Result : ResultType := 0;
    begin
        ReadInput(s, InData);

        for L of s.Input loop
            if IsHoriz (L) or IsVert (L) then
                for D in 0..(L.NumPoints-1) loop
                    declare
                        p : Coord := CoordAlong(L, D);
                    begin
                        m(p.X, p.Y) := m(p.X, p.Y) + 1;
                    end;
                end loop;
            end if;
        end loop;

        for X in XCoord loop
            for Y in YCoord loop
                if m(X, Y) > 1 then
                    Result := Result + 1;
                end if;
            end loop;
        end loop;

        return Result;
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    procedure PrintMap(m : MapType) is
    begin
        if PrintDebug then
            for Y in 0..YCoord(15) loop
                for X in 0..XCoord(15) loop
                    declare
                        t : Integer := Integer(m(x, y));
                    begin
                        Put(t'image);
                        null;
                    end;
                end loop;
                Put_Line ("");
            end loop;
        end if;
    end PrintMap;

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p05b;
                    InData : in Ada.Text_IO.File_type) return ResultType is
        s : SolutionB;
        m : MapType := (others => (others => 0));
        Result : ResultType := 0;
    begin
        ReadInput(s, InData);

        for L of s.Input loop
            for D in 0..(L.NumPoints-1) loop
                declare
                    p : Coord := CoordAlong(L, D);
                begin
                    m(p.X, p.Y) := m(p.X, p.Y) + 1;
                end;
            end loop;
            PrintMap (m);
        end loop;

        for Y in YCoord loop
            for X in XCoord loop
                if m(X, Y) > 1 then
                    Result := Result + 1;
                end if;
            end loop;
        end loop;

        return Result;
    end Solve;

end p05;
