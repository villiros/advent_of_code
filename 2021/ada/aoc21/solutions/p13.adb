with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Maps;

with Harness; use Harness;
with utils; use utils;

package body p13 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(new p13a'(Name => "p13a"));
        D.Append(new p13b'(Name => "p13b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    type FoldKind is (Vertical, Horizontal);
    type XCoord is range 0..2000;
    type YCoord is range 0..2000;

    type Coord is record
        X : XCoord;
        Y : YCoord;
    end record;

    type FoldType (Kind: FoldKind) is record
        case Kind is
            when Vertical =>
                AlongY: YCoord;
            when Horizontal =>
                AlongX: XCoord;
        end case;
    end record;

    function CoordHash (Element : Coord) return Ada.Containers.Hash_Type is
    begin
        return Ada.Containers.Hash_Type(Integer(Element.X) + Integer(Element.Y));
    end CoordHash;

    package DotsPkg is new
        Ada.Containers.Hashed_Sets(Element_Type => Coord,
                                   Hash => CoordHash,
                                   Equivalent_Elements => "=");
    use DotsPkg;
    subtype DotsType is DotsPkg.Set;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    package FoldsPkg is new
        Ada.Containers.Indefinite_Vectors(Index_Type => Positive, Element_Type => FoldType);
    use FoldsPkg;
    subtype Foldstype is FoldsPkg.Vector;

    type Solution is tagged record
        Dots: DotsType;
        Folds: Foldstype;
    end record;

    procedure ReadInput(self : in out Solution'class; InData : File_Type)
    with
        Post => (not self.Dots.Is_Empty) and
                (not self.Folds.Is_Empty)
    is
    begin
        while not End_Of_File(InData) loop
            declare
                noInt: constant ResultType := -1;
                x: ResultType := GetInt (InData, False, noInt);
            begin
                exit when x = NoInt;
                SkipChar (InData, ',');
                self.Dots.Insert((X => XCoord(x), Y => YCoord(GetInt(InData))));
                AdvanceLine (InData);
            end;
        end loop;

        -- Empty line
        AdvanceLine (InData);

        while not End_Of_File (InData) loop
            declare
                junkFold: InputStr := GetAtom(InData);
                junkAlong: InputStr := GetAtom(InData);
                foldAxis: String := InputStrPkg.To_String(GetAtom (InData, Ada.Strings.Maps.To_Set("=")));
            begin
                SkipChar (InData, '=');
                if foldAxis = "x" then
                    self.Folds.Append (FoldType'(Horizontal, XCoord(GetInt (InData))));
                elsif foldAxis = "y" then
                    self.Folds.Append (FoldType'(Vertical, YCoord(GetInt(InData))));
                else
                    raise ReadFailed;
                end if;
                AdvanceLine(InData);
            end;
        end loop;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    procedure DoFold (d: in out DotsType; f: FoldType)
    with Pre =>
        -- No dots along the line
        (if f.Kind = Horizontal then
            (for all Y in YCoord => not d.Contains((X => f.AlongX, Y => Y)))
        else
            (for all X in XCoord => not d.Contains ((X => X, Y => f.AlongY))))
    is
        DotsToRemove: DotsType;
        DotsToAdd: DotsType;
    begin
        for E of d loop
            case f.Kind is
                when Horizontal =>
                    if E.X > f.AlongX then
                        DotsToRemove.Insert (E);
                        DotsToAdd.Insert ((X => f.AlongX - (E.X - f.AlongX),
                                           Y => E.Y));
                    end if;
                when Vertical =>
                    if E.Y > f.AlongY then
                        DotsToRemove.Insert (E);
                        DotsToAdd.Insert ((X => E.X,
                                           Y => f.AlongY - (E.Y - f.AlongY)));
                    end if;
            end case;
        end loop;

        d := (d - DotsToRemove) or DotsToAdd;
    end DoFold;

    type SolutionA is new Solution with record
        null;
    end record;

    function Solve (SDisp : p13a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionA;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        DoFold (s.Dots, s.Folds.First_Element);

        return ResultType(s.Dots.Length);
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    procedure Print(d: DotsType)
    with Pre => not d.Is_Empty and 
                (for all E of d => E.X < 50 and E.Y <= 8)
    is
    begin
        -- Bounds checked by precondition
        for Y in 0..8 loop
            for X in 0..50 loop
                if d.Contains((X => XCoord(X), Y => YCoord(Y))) then
                    Put ("#");
                else
                    Put (".");
                end if;
            end loop;
            Put_Line ("");
        end loop;
    end Print;

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p13b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionB;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        for F of s.Folds loop
            DoFold (s.Dots, F);
        end loop;

        if PrintDebug then Print(s.Dots); end if;

        return ResultType(s.Dots.Length);
    end Solve;

end p13;
