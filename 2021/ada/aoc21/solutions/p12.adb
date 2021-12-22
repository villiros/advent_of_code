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
        D.Append(p12a'(Name => "p12a"));
        D.Append(p12b'(Name => "p12b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    ShouldNotGetHere: exception;

    -- Original solution used indefinite hashed maps and sets and was pretty
    -- slow (400ms on B).
    --
    -- There are very few caves, so instead of using string-based maps,
    -- the cave names are converted into numbers and the whole thing can be
    -- kept in arrays.
    --
    -- Each node contains a (CaveID) -> IsConnected array; simply
    -- iterating over the whole thing on each step is fairly fast.
    -- *Could* be optimized to keep connected as a bounded array, but meh.

    -- IDs for caves. One of them is not a valid cave.
    type CaveId is range 1..20;
    -- Actual caves
    subtype CaveNum is CaveId range 1..(CaveId'Pred(CaveId'Last));
    -- Null marker
    subtype NotACaveNum is CaveId range CaveId'Last..CaveId'Last;
    NotACave: constant CaveId := NotACaveNum'Last;

    -- Still need to track the actual name
    subtype NameLen is Integer range 1..5;
    subtype NameType is String(NameLen);

    StartName: constant NameType := "start";
    EndName  : constant NameType := "end  ";

    type CaveType is (Cstart, Cend, Cbig, Csmall, NoCave);
    -- Connections from one cave to another (index)
    type Connects is array(CaveNum) of Boolean;

    type Cave is record
        name: NameType;
        ct: CaveType := NoCave;
        conn: Connects := (others => False);
    end record;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    type VisitedType is array(CaveNum) of Boolean;
    type CavesType is array(CaveNum) of Cave;

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

    -- Returns index of next empty cell if name not known
    function GetIndex(s: Solution'Class; name: NameType) return CaveNum is
    begin
        for I in CaveNum loop
            if s.Input(I).name = name or
                s.Input(I).ct = NoCave then
                return I;
            end if;
        end loop;
        raise ShouldNotGetHere;
    end GetIndex;

    procedure ReadInput(self : in out Solution'class; InData : File_Type)
    with
        Post =>
            -- Start and end loaded
            (for some I in CaveNum => self.Input(I).ct = Cstart) and
            (for some I in CaveNum => self.Input(I).ct = Cend) and
            -- No overflow
            (self.Input(CaveNum'Last).ct = NoCave) and
            -- Check connectivity
            (for all I in CaveNum =>
                (if self.Input(I).ct /= NoCave then
                    -- Must connect to something
                    (for some J in CaveNum => self.Input(I).conn(J)) and
                    -- All connections are bidirectional
                    (for all J in CaveNum =>
                        (if self.Input(I).conn(J) then
                            self.Input(J).ct /= NoCave and
                            self.Input(J).conn(I)))))
    is
        use Ada.Strings.Fixed;
    begin
        while not End_Of_File(InData) loop
            declare
                from, to: NameType;
                fromIndex, toIndex: CaveNum;
            begin
                Move(InputStrPkg.To_String(GetAtom (InData, Ada.Strings.Maps.To_Set("-"))), from);
                SkipChar (InData, '-');
                Move(InputStrPkg.To_String(GetAtom (InData)), to);

                fromIndex := GetIndex (self, from);
                if self.Input(fromIndex).ct = NoCave then
                    self.Input(fromIndex).name := from;
                    self.Input(fromIndex).ct := ToCaveType (from);
                end if;

                toIndex := GetIndex (self, to);
                if self.Input(toIndex).ct = NoCave then
                    self.Input(toIndex).name := to;
                    self.Input(toIndex).ct := ToCaveType (to);
                end if;

                self.Input(fromIndex).conn(toIndex) := True;
                self.Input(toIndex).conn(fromIndex) := True;
            end;
            AdvanceLine (InData);
        end loop;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    procedure Visitor(m: CavesType; v: in out VisitedType;
                      curId: CaveNum;
                      numPaths: in out ResultType) is
        cur: Cave renames m(curId);
    begin
        if cur.ct = Cend then
            numPaths := numPaths + 1;
            return;
        end if;

        for NextId in CaveNum loop
            if cur.conn(NextId) and not v(NextId) then
                if m(NextId).ct /= Cbig then
                    v(NextId) := True;
                end if;

                Visitor(m, v, NextId, numPaths);

                v(NextId) := False;
            end if;
        end loop;
    end Visitor;

    type SolutionA is new Solution with record
        null;
    end record;

    function Solve (SDisp : p12a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionA;

        v: VisitedType := (others => False);
        result: ResultType := 0;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        v(GetIndex(s, StartName)) := True;
        Visitor(s.Input, v, GetIndex(s, StartName), result);

        return result;
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    procedure VisitorB(m: CavesType; v: in out VisitedType; twiceVisit: CaveId;
                       curId: CaveNum;
                       numPaths: in out ResultType) is
        cur: Cave renames m(curId);
    begin
        if cur.ct = Cend then
            numPaths := numPaths + 1;
            return;
        end if;

        for NextId in CaveNum loop
            if cur.conn(NextId) and
               ((not v(NextId)) or else
                (twiceVisit = NotACave and m(NextId).ct /= Cstart)) then

                declare
                    alreadyVisited: Boolean := v(NextId);
                    twiceVisitNew: CaveId := (if alreadyVisited then NextId else twiceVisit);
                begin
                    if m(NextId).ct /= Cbig then
                        v(NextId) := True;
                    end if;

                    VisitorB(m, v, twiceVisitNew, NextId, numPaths);

                    if not alreadyVisited then
                        v(NextId) := False;
                    end if;
                end;
            end if;
        end loop;
    end VisitorB;

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p12b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionB;
        v: VisitedType := (others => False);
        result: ResultType := 0;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        v(GetIndex(s, StartName)) := True;
        VisitorB(s.Input, v, NotACave, GetIndex(s, StartName), result);

        return result;
    end Solve;

end p12;
