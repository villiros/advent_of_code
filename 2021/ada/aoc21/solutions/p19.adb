with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;
with Ada.Containers.Hashed_Sets;

with Harness; use Harness;
with utils; use utils;

package body p19 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(p19a'(Name => "p19a"));
        D.Append(p19b'(Name => "p19b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    -- Coordinates
    subtype Axis is Integer;
    subtype AxisIndex is Integer range 1..3;
    -- Global coordinate
    type Coord is array(AxisIndex) of Axis;

    -- Translation vector
    type Vec is array(AxisIndex) of Axis;

    function "-"(a, b: Coord) return Vec is
        (1 => a(1) - b(1),
         2 => a(2) - b(2),
         3 => a(3) - b(3));
    
    function "+"(a: Coord; b: Vec) return Coord is
        (1 => a(1) + b(1),
         2 => a(2) + b(2),
         3 => a(3) + b(3));

    -- Axis rotations
    type RotAxis is (L0, L90, L180, L270);
    type RotCoord is array(AxisIndex) of RotAxis;
    -- type RotCoords is array(1..(3 * 4)) of RotCoord;
    
    -- function GetAllRotations return RotCoords is
    --     result: RotCoords;
    --     i: Integer := 1;
    -- begin
    --     for rx in RotAxis loop
    --         for ry in RotAxis loop
    --             for rz in RotAxis loop
    --                 result(i) := (rx, ry, rz);
    --                 i := i + 1;
    --             end loop;
    --         end loop;
    --     end loop;
    --     return result;
    -- end GetAllRotations;
    -- -- All possible rotations
    -- allRotations: constant RotCoords := GetAllRotations;

    subtype RotVal is Integer with Static_Predicate => RotVal in -1 | 0 | 1;
    subtype MatrixIndex is Integer range 1..3;
    type RotMatrix is array(MatrixIndex, MatrixIndex) of RotVal;

    -- General rotation matrix
    function GetRotMatrix(rot: RotCoord) return RotMatrix is
        function cos(a: RotAxis) return RotVal is
            (case a is
                when L0 => 1,
                when L90 => 0,
                when L180 => -1,
                when L270 => 0);
        
        function sin(a: RotAxis) return RotVal is
            (case a is
                when L0 => 0,
                when L90 => 1,
                when L180 => 0,
                when L270 => -1);

        -- a: z, b: y, c: x
        cosA: RotVal := cos(rot(3));
        sinA: RotVal := sin(rot(3));

        cosB: RotVal := cos(rot(2));
        sinB: RotVal := sin(rot(2));

        cosC: RotVal := cos(rot(1));
        sinC: RotVal := sin(rot(1));
    begin
        return (1 => (1 => cosA * cosB,     2 => cosA * sinB * sinC - sinA * cosC,      3 => cosA * sinB * cosC + sinA * sinC),
                2 => (1 => sinA * cosB,     2 => sinA * sinB * sinC + cosA * cosC,      3 => sinA * sinB * cosC - cosA * sinC),
                3 => (1 => -sinB,           2 => cosB * sinC,                           3 => cosB * cosC));
    end GetRotMatrix;

    -- All possible rotations
    -- We know there are 24 for such rotations
    subtype AllRotationsIndex is Integer range 1..24;
    type AllRotationsType is array(AllRotationsIndex) of RotMatrix;

    function GetAllRotations return AllRotationsType is
        result: AllRotationsType := (others => (others => (others => 0)));
        i: Integer := AllRotationsIndex'First;
    begin
        for rx in RotAxis loop
            for ry in RotAxis loop
                for rz in RotAxis loop
                    declare
                        m: RotMatrix := GetRotMatrix((rx, ry, rz));
                        isNew: Boolean := True;
                    begin
                        for j in AllRotationsIndex'First..AllRotationsIndex'Pred(i) loop
                            isNew := isNew and result(j) /= m;
                        end loop;

                        if isNew then
                            result(i) := m;
                            i := i + 1;
                        end if;
                    end;
                end loop;
            end loop;
        end loop;
        return result;
    end GetAllRotations;

    allRotations: constant AllRotationsType := GetAllRotations;



    -- function "*"(left, right: RotMatrix) return RotMatrix is
    --     result: RotMatrix;
    -- begin
    --     for i in RotMatrix'Range(1) loop
    --         for j in RotMatrix'Range(2) loop
    --             declare
    --                 t: Integer := 0;
    --             begin
    --                 for ii in RotMatrix'Range(1) loop
    --                     t := t + left(i, ii) * right(ii, j);
    --                 end loop;
    --                 result(i, j) := t;
    --             end;
    --         end loop;
    --     end loop;

    --     return result;
    -- end "*";

    function "*"(left: RotMatrix; right: Coord) return Coord is
        result: Coord;
    begin
        for i in RotMatrix'Range loop
            declare
                t: Integer := 0;
            begin
                for ii in RotMatrix'Range(1) loop
                    t := t + left(i, ii) * right(ii);
                end loop;
                result(i) := t;
            end;
        end loop;
        return result;
    end "*";

    package CoordsPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Coord);
    use CoordsPkg;
    subtype CoordsType is CoordsPkg.Vector;

    function Translate(a: CoordsType; b: Vec) return CoordsType is
        result: CoordsType;
    begin
        for E of a loop
            result.Append(E + b);
        end loop;
        return result;
    end Translate;

    function Rotate(a: CoordsType; b: RotMatrix) return CoordsType is
        result: CoordsType;
    begin
        for E of a loop
            result.Append(b * E);
        end loop;
        return result;
    end Rotate;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    subtype ScannerIndex is Natural;

    type Scanner is record
        index: ScannerIndex;
        points: CoordsType;
    end record;

    package ScannersPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => Scanner);
    use ScannersPkg;
    subtype ScannersType is ScannersPkg.Vector;
    use type ScannersType;
    use type Ada.Containers.Count_Type;

    type Solution is tagged record
        Input : ScannersType;
    end record;

    procedure ReadInput(self : in out Solution'class; InData : File_Type)
    with
        Post => not self.Input.Is_Empty
    is
    begin
        while not End_Of_File(InData) loop
            SkipString(InData, "--- scanner");
            declare
                ind: ScannerIndex := ScannerIndex(GetInt(InData));
                points: CoordsType;
            begin
                SkipString(InData, "---");
                AdvanceLine(InData);

                while (PeekChar(InData) not in ASCII.LF | ASCII.nul) loop
                    declare
                        x: Axis := Axis(GetInt(InData));
                        y, z: Axis;
                    begin
                        SkipChar(InData, ',');
                        y := Axis(GetInt(InData));
                        SkipChar(InData, ',');
                        z := Axis(GetInt(InData));
                        AdvanceLine(InData);
                        points.Append(Coord'(x, y, z));
                    end;
                end loop;
                
                self.Input.Append(Scanner'(index => ind, points => points, others => <>));
                AdvanceLine(InData);
            end;
        end loop;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    function CoordHash(a: Coord) return Ada.Containers.Hash_Type is
        use Ada.Containers;
    begin
        return Hash_Type(abs a(1)) xor Hash_Type(abs a(2)) xor Hash_Type(abs a(3));
    end CoordHash;

    package CoordSetPkg is new
        Ada.Containers.Hashed_Sets(Element_Type => Coord,
                                   Hash => CoordHash,
                                   Equivalent_Elements => "=");
    use CoordSetPkg;
    subtype CoordSet is CoordSetPkg.Set;

    function FromVector(a: CoordsType) return CoordSet is
        result: CoordSet;
    begin
        for e of a loop
            result.Insert (e);
        end loop;
        return result;
    end FromVector;

    -- function CountMatches(a, b: CoordsType) return Natural is
    --     aset, bset: CoordSet;
    -- begin
    --     for i of a loop
    --         aset.Insert (i);
    --     end loop;
    --     for i of b loop
    --         bset.Insert (i);
    --     end loop;

    --     return Natural(Length((aset and bset)));
    -- end CountMatches;

    type ResolvedScanner is record
        index: ScannerIndex;
        -- These points are translated into global coords
        points: CoordsType;

        -- Global position of the scanner
        pos: Coord;
        -- Rotation matrix of the scanner
        rot: RotMatrix;
    end record;

    package ResolvedScannersPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => ResolvedScanner);
    use ResolvedScannersPkg;
    subtype ResolvedScannersType is ResolvedScannersPkg.Vector;
    use type ResolvedScannersType;

    function SolveA(scanners: ScannersType) return ResolvedScannersType is
        fixed: ResolvedScannersType;
        nextToConsider: Positive;

        function IsResolved(ind: ScannerIndex) return Boolean is
            (for some E of fixed => e.index = ind);
        
        procedure Consider(e: Scanner) is
            consFixed: constant ResolvedScanner := fixed(nextToConsider);
            consPointSet: constant CoordSet := FromVector (consFixed.points);
        begin
            -- Try all possible rotations
            for rot of allRotations loop
                declare
                    points: CoordsType := Rotate(e.points, rot);
                begin
                    -- Each point in the thing we are considering (after rotation) *might* coincide
                    -- with some point in the fixed one.
                    -- For simplicity, try all pairs.
                    -- Simple ptimization: no need to keep trying if less than 12 points remaining
                    for p1 of consFixed.points loop
                        --for p2 of points loop
                        for p2index in points.First_Index..(points.Last_Index - 12) loop
                            declare
                                p2: Coord := points(p2index);
                                translation: Vec := p1 - p2;
                                numMatching: Natural := 0;
                            begin
                                for p of points loop
                                    numMatching := numMatching + (if consPointSet.Contains (p + translation) then 1 else 0);
                                end loop;
                                if numMatching >= 12 then
                                    fixed.Append(ResolvedScanner'(index => e.index,
                                                                  points => Translate(points, translation),
                                                                  pos => (0, 0, 0) + (p1 - p2),
                                                                  rot => rot));
                                    return;
                                end if;
                            end;
                        end loop;
                    end loop;
                end;
            end loop;
        end Consider;
    begin
        declare
            f: Scanner renames scanners.First_Element;
        begin
            fixed.Append(ResolvedScanner'(index => f.index,
                                          points => f.points,
                                          pos => (0, 0, 0),
                                          rot => GetRotMatrix ((L0, L0, L0))));
            nextToConsider := 1;
        end;

        while fixed.Length < scanners.Length loop
            for E of scanners loop
                if not IsResolved(E.index) then
                    Consider (E);
                end if;
            end loop;
            -- Above must find new scanners to fix
            Assert(fixed.Last_Index > nextToConsider);
            nextToConsider := nextToConsider + 1;
        end loop;

        return fixed;
    end SolveA;

    type SolutionA is new Solution with record
        null;
    end record;

    function Solve (SDisp : p19a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionA;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        declare
            resolved: ResolvedScannersType := SolveA(s.Input);
            result: CoordSet;
        begin
            for S of resolved loop
                for p of s.points loop
                    result.Include (p);
                end loop;
            end loop;

            return ResultType(result.Length);
        end;
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    function ManhattanDist(a, b: Coord) return ResultType is
    begin
        return (abs ResultType(a(1) - b(1))) + (abs ResultType(a(2) - b(2))) + (abs ResultType(a(3) - b(3)));
    end ManhattanDist;

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p19b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionB;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        declare
            resolved: ResolvedScannersType := SolveA(s.Input);
            maxDist: ResultType := 0;
        begin
            for a of resolved loop
                for b of resolved loop
                    if a /= b and ManhattanDist (a.pos, b.pos) > maxDist then
                        maxDist := ManhattanDist (a.pos, b.pos);
                    end if;
                end loop;
            end loop;

            return maxDist;
        end;
    end Solve;

end p19;
