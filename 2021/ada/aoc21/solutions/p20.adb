with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;
with Ada.Containers.Indefinite_Holders;

with Harness; use Harness;
with utils; use utils;

package body p20 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(new p20a'(Name => "p20a"));
        D.Append(new p20b'(Name => "p20b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    ShouldNotGetHere: exception;

    type Pixel is (Dark, Lit);
    type MapIndex is range 0..511;
    -- NB: 0-based index
    type MapType is array(MapIndex) of Pixel;
    type ImageSize is new Positive;

    type ImageData is array(ImageSize range <>, ImageSize range <>) of Pixel;

    type Image(size: ImageSize) is record
        data: ImageData(1..size, 1..size);
        -- State of all pixels not in data
        outOfBounds: Pixel := Dark;
    end record;

    package ImagePkg is new
        Ada.Containers.Indefinite_Holders(Element_Type => Image);
    use ImagePkg;
    subtype ImageHolder is ImagePkg.Holder;

    function ToPixel(c: Character) return Pixel is
    begin
        case c is
            when '#' => return Lit;
            when '.' => return Dark;
            when others => raise ShouldNotGetHere;
        end case;
    end ToPixel;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    type Solution is tagged record
        map: MapType;
        img: ImageHolder;
    end record;

    procedure ReadInput(self : in out Solution'class; InData : File_Type)
    with
        Post => not self.img.Is_Empty
    is
        use InputStrPkg;
    begin
        declare
            mapStr: InputStr := GetAtom(InData);
        begin
            Assert(Integer(Length(mapStr) - 1) = Integer(MapIndex'Last), "Got " & Length(mapStr)'image);
            for i in 1..Length(mapStr) loop
                self.map(MapIndex(i - 1)) := ToPixel(Element(mapStr, i));
            end loop;
        end;
        AdvanceLine (InData);
        AdvanceLine (InData);

        declare
            curLine: InputStr := GetAtom(InData);
            img: Image(ImageSize(Length(curLine)));

            procedure SetLine(lineNo: ImageSize; line: InputStr)
            with Pre => ImageSize(Length(line)) = img.size and lineNo <= img.size
            is
            begin
                for i in 1..Length(line) loop
                    img.data(lineNo, ImageSize(i)) := ToPixel (Element(line, i));
                end loop;
            end SetLine;
        begin
            img.outOfBounds := Dark;
            AdvanceLine (InData);
            SetLine (1, curLine);

            for i in 2..img.size loop
                curLine := GetAtom (inData);
                AdvanceLine (InData);

                SetLine (i, curLine);
            end loop;

            self.img.Replace_Element (img);
        end;
    end ReadInput;

    function GetIndex(img: Image; x, y: Integer) return MapIndex
    is
        -- First coordinate is row
        offsetY: constant array (Positive range <>) of Integer := (-1,  0,  1,  -1, 0, 1,  -1, 0, 1);
        offsetX: constant array (Positive range <>) of Integer := (-1, -1, -1,   0, 0, 0,   1, 1, 1);

        function GetPixel(x, y: Integer) return MapIndex is
        begin
            if (x >= Integer(ImageSize'First) and x <= Integer(img.size)) and
               (y >= Integer(ImageSize'first) and y <= Integer(img.size)) then
                return (if (img.data(ImageSize(x), ImageSize(y))) = Lit then 1 else 0);
            else
                return (if img.outOfBounds = Lit then 1 else 0);
            end if;
        end GetPixel;

        result: MapIndex := 0;
    begin
        for i in offsetX'Range loop
            result := result * 2 + GetPixel (x + offsetX(i), y + offsetY(i));
        end loop;

        return result;
    end GetIndex;

    function ApplyStep(img: Image; map: MapType) return Image is
        result: Image(img.size + 3*2);
    begin
        for x in result.data'Range(1) loop
            for y in result.data'Range(2) loop
                result.data(x, y) := map(GetIndex(img, Integer(x) - 3, Integer(y) - 3));
            end loop;
        end loop;

        -- :)
        result.outOfBounds := map(GetIndex(img, -100, -100));

        return result;
    end ApplyStep;

    procedure PrintImage(img: Image) is
    begin
        if not PrintDebug then
            return;
        end if;

        Put_Line ("--- Outside: " & (if img.outOfBounds = Lit then '#' else '.'));
        for row in 1..img.size loop
            for col in 1..img.size loop
                Put("" & (if img.data(row, col) = Lit then '#' else '.'));
            end loop;
            Put_Line ("");
        end loop;
    end PrintImage;

    function CountPixels(img: Image) return ResultType
    with Pre => img.outOfBounds = Dark
    is
        result: ResultType := 0;
    begin
        for i in img.data'Range(1) loop
            for j in img.data'Range(2) loop
                result := result + (if img.data(i, j) = Lit then 1 else 0);
            end loop;
        end loop;
        return result;
    end CountPixels;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    type SolutionA is new Solution with record
        null;
    end record;

    function Solve (SDisp : p20a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionA;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        declare
            cur: ImageHolder := s.img;
        begin
            PrintImage (cur.Element);
            cur.Replace_Element(ApplyStep (cur.Element, s.map));
            PrintImage (cur.Element);
            cur.Replace_Element(ApplyStep (cur.Element, s.map));
            PrintImage (cur.Element);

            return CountPixels (cur.Element);
        end;
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p20b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionB;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        declare
            cur: ImageHolder := s.img;
        begin
            for i in 1..50 loop
                cur.Replace_Element (Applystep(cur.Element, s.map));
            end loop;
            return CountPixels (cur.Element);
        end;
    end Solve;

end p20;
