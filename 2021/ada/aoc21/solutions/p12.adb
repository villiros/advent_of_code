with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Hash;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;

with Harness; use Harness;
with utils; use utils;

package body p12 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(new p12a'(Name => "p12a"));
        D.Append(new p12b'(Name => "p12b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    subtype NameLen is Integer range 1..5;
    subtype NameType is String(NameLen);

    StartName: constant NameType := "start     ";
    EndName  : constant NameType := "end       ";
    NoCaveName : constant NameType := "NOTACAVE  ";

    package ConnectsPkg is
        new Ada.Containers.Vectors(Index_type => Positive, Element_Type => NameType);
    use ConnectsPkg;
    subtype ConnectsType is ConnectsPkg.Vector;

    
    type CaveType is (Cstart, Cend, Cbig, Csmall);

    type Cave is record
        name: NameType;
        ct: CaveType;
        conn: ConnectsType;
    end record;

    package CavesPkg is new
        Ada.Containers.Indefinite_Hashed_Maps(
            Key_Type => NameType,
            Element_type => Cave,
            Hash => Ada.Strings.Hash,
            Equivalent_Keys => "="
            );
    use CavesPkg;
    subtype CavesType is CavesPkg.Map;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    package VisitedPkg is new
        Ada.Containers.Indefinite_Hashed_Sets(
            Element_Type => NameType,
            hash => Ada.Strings.Hash,
            Equivalent_Elements => "=");
    use VisitedPkg;
    subtype VisitedType is VisitedPkg.Set;

    function ToCaveType(name: NameType) return CaveType is
        use Ada.Characters.Handling;
    begin
        if name = StartName then
            return CStart;
        elsif name = EndName then
            return CEnd;
        elsif Is_Upper(name(Name'First)) then
            return Cbig;
        else
            return Csmall;
        end if;
    end ToCaveType;

    type Solution is tagged record
        Input : CavesType;
    end record;

    procedure ReadInput(self : in out Solution'class; InData : File_Type)
    with
        Post =>
            (not self.Input.Is_Empty) and
            (self.Input.Contains(StartName)) and
            (self.Input.Contains(EndName)) and
            (for all E of self.Input =>
                not E.conn.Is_Empty and
                (for all E2 of E.conn =>
                    self.Input.Contains(E2) and
                    self.Input.Element(E2).conn.Contains(E.name)))
    is
        use Ada.Strings.Fixed;
    begin
        while not End_Of_File(InData) loop
            declare
                from: NameType;
                to: NameType;
            begin
                Move(InputStrPkg.To_String(GetAtom (InData, Ada.Strings.Maps.To_Set("-"))), from);
                SkipChar (InData, '-');
                Move(InputStrPkg.To_String(GetAtom (InData)), to);

                if not self.Input.Contains (from) then
                    self.Input.Include (from, (name => from,
                                               ct => ToCaveType (from),
                                               others => <>));
                end if;
                if not self.Input.Contains (to) then
                    self.Input.Include (to, (name => to,
                                             ct => ToCaveType (to),
                                             others => <>));
                end if;

                self.Input(from).conn.Append (to);
                self.Input(to).conn.Append (from);
            end;
            AdvanceLine (InData);
        end loop;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    function Visitor(m: CavesType; v: in out VisitedType;
                     curName: NameType;
                     numPaths: in out ResultType) return Boolean is
        cur: Cave renames m.Element(curName);

        hadPaths: Boolean := False;
    begin
        if cur.ct = Cend then
            numPaths := numPaths + 1;
            return True;
        end if;

        for NName of cur.conn loop
            if not v.Contains (NName) then
                if m.Element(NName).ct /= Cbig then
                    v.Include (NName);
                end if;

                hadPaths := hadPaths or Visitor(m, v, NName, numPaths);

                v.Exclude (NName);
            end if;
        end loop;

        return hadPaths;
    end Visitor;

    type SolutionA is new Solution with record
        null;
    end record;

    function Solve (SDisp : p12a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionA;

        v: VisitedType;
        result: ResultType := 0;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        v.Include(StartName);
        declare
            junk: Boolean := Visitor(s.Input, v, StartName, result);
        begin
            null;
        end;

        return result;
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    function VisitorB(m: CavesType; v: in out VisitedType; twiceVisit: NameType;
                      curName: NameType;
                      numPaths: in out ResultType) return Boolean is
        cur: Cave renames m.Element(curName);

        hadPaths: Boolean := False;
    begin
        if cur.ct = Cend then
            numPaths := numPaths + 1;
            return True;
        end if;

        for NName of cur.conn loop
            if (not v.Contains (NName)) or else
               (twiceVisit = NoCaveName and NName /= StartName) then
                declare
                    alreadyVisited: Boolean := v.Contains (NName);
                    -- If already visited, then twiceVisit must've been NoCaveName
                    twiceVisitNew: NameType := (if alreadyVisited then NName else twiceVisit);
                begin
                    if m.Element(NName).ct /= Cbig then
                        v.Include (NName);
                    end if;

                    hadPaths := hadPaths or VisitorB(m, v, twiceVisitNew, NName, numPaths);

                    if not alreadyVisited then
                        v.Exclude (NName);
                    end if;
                end;
            end if;
        end loop;

        return hadPaths;
    end VisitorB;

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p12b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionB;
        v: VisitedType;
        result: ResultType := 0;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        v.Include(StartName);
        declare
            junk: Boolean := VisitorB(s.Input, v, NoCaveName, StartName, result);
        begin
            null;
        end;

        return result;
    end Solve;

end p12;
