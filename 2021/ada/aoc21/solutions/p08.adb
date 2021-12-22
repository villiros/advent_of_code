with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;

with Harness; use Harness;
with utils; use utils;

package body p08 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(p08a'(Name => "p08a"));
        D.Append(p08b'(Name => "p08b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    -- This is *not* a good way to solve this. It's more of a special-purpose and
    -- extremely slow constraint solver approach.
    -- Yes, this is coded to be very slow (lots of redundant loops and copies),
    -- and the main solver doesn't try to be smart: it simply recursively tries
    -- to put connections into the map.

    -- Unsat
    ShouldNotGetHere : exception;

    -- Segment states, including "don't know"
    type SegState is (On, Off, Unknown);
    subtype SegStateConcrete is SegState range On..Off;
    -- A connection: connected, not connected, or don't know.
    type SegConn is (Conn, Unconn, Unknown);

    type Digit is range 0..9;

    -- Individual segments. Subtyped into onces produces by instruments (InstSeg) and once seen in the input.
    type SegmentNum is (Sa, Sb, Sc, Sd, Se, Sf, Sg);
    subtype InstSeg is SegmentNum;
    subtype SeenSeg is SegmentNum;

    type SegmentCount is range 0..(SegmentNum'Pos(SegmentNum'Last) + 1);

    -- Definition of valid digits
    type DigSegments is array(SegmentNum) of SegStateConcrete;
    type DigToSegType is array(Digit) of DigSegments;
    AllDigits: constant DigToSegType :=
        (0 => (Sa | Sb | Sc | Se | Sf | Sg => On, others => Off),
         1 => (Sc | Sf => On, others => Off),
         2 => (Sa | Sc | Sd | Se | Sg => On, others => Off),
         3 => (Sa | Sc | Sd | Sf | Sg => On, others => Off),
         4 => (Sb | Sc | Sd | Sf => On, others => Off),
         5 => (Sa | Sb | Sd | Sf | Sg => On, others => Off),
         6 => (Sa | Sb | Sd | Se | Sf | Sg => On, others => Off),
         7 => (Sa | Sc | Sf => On, others => Off),
         8 => (others => On),
         9 => (Sa | Sb | Sc | Sd | Sf | Sg => On, others => Off));

    -- Generic counter of El in M
    generic
        type ArrEl is (<>);
        type ArrRange is (<>);
        type Arr is array(ArrRange) of ArrEl;
    function Count(M: Arr; El: ArrEl) return SegmentCount;
    function Count(M: Arr; El: ArrEl) return SegmentCount is
        result : SegmentCount := SegmentCount'First;
    begin
        for I in Arr'Range loop
            if M(I) = El then
                result := result + 1;
            end if;
        end loop;
        return result;
    end Count;

    -- Which segments we observe. These are concrete.
    type SeenInd is array(SeenSeg) of SegStateConcrete;
    function CountSeen is new Count(ArrEl => SegStateConcrete, ArrRange => SeenSeg, Arr => SeenInd);

    -- Guess for instrument segments state.
    type InstInd is array(InstSeg) of SegState;

    -- For a single seen segment, what we know about the instrument segments it can connect to?
    type SegMap is array(InstSeg) of SegConn;
    function CountSegMap is new Count(ArrEl => SegConn, ArrRange => InstSeg, Arr => SegMap);

    -- Our knowledge of how segments connect
    -- SegsMap(I)(J): how seen I is connected to instrument J
    type SegsMap is array(SeenSeg) of SegMap;
    InitSegsMap : constant SegsMap := (others => SegMap'(others => Unknown));

    -- For an instrument segment, what do we know about what seen segments it connects to?
    -- It's just a transpose of the SegsMap matrix.
    type RevSegMap is array(SeenSeg) of SegConn;
    function CountRevSegMap is new Count(ArrEl => SegConn, ArrRange => SeenSeg, Arr => RevSegMap);

    -- For instrument segment Seg, what do we know about connections to SeenSegs?
    -- Transposes M but returns only information on what connects to instrument segment Seg
    function RevForSeg(M: SegsMap; Seg: InstSeg) return RevSegMap is
        result: RevSegMap;
    begin
        for I in SeenSeg loop
            result(I) := M(I)(Seg);
        end loop;
        return result;
    end RevForSeg;

    -- SegMap invariant used by assertions
    function SegsMapValid(M: SegsMap) return Boolean is
        ok: Boolean := True;
    begin
        -- Each seen seg can connect at at most to one instrument seg, and must have a connection (or don't know)
        -- If connection exists, the rest should be unconnected
        -- Same applies in the other direction
        for I in SeenSeg loop
            ok := ok and then (CountSegMap(M(I), Conn) <= 1);
            ok := ok and then (CountSegMap(M(I), Conn) + CountSegMap(M(I), Unknown) > 0);
            ok := ok and then (if CountSegMap(M(I), Conn) = 1 then CountSegMap(M(I), Unknown) = 0);
        end loop;
        for I in InstSeg loop
            declare
                rev: RevSegMap := RevForSeg(M, I);
            begin
                ok := ok and then (CountRevSegMap(rev, Conn) <= 1);
                ok := ok and then (CountRevSegMap(rev, Conn) + CountRevSegMap(rev, Unknown) > 0);
                ok := ok and then (if CountRevSegMap(rev, Conn) = 1 then CountRevSegMap(rev, Unknown) = 0);
            end;
        end loop;

        return ok;
    end SegsMapValid;

    -- Map seen input to guess for an instrument. Doesn't validate if a particular guess makes sense.
    function MapInput(M: SegsMap; Seen: SeenInd) return InstInd
    with Pre => SegsMapValid (M)
    is
        result: InstInd := (others => Unknown);
    begin
        for I in SeenSeg loop
            for J in InstSeg loop
                case M(I)(J) is
                    when Conn =>
                        Assert(result(J) = Unknown);
                        result(J) := Seen(I);
                    when others =>
                        null;
                end case;
            end loop;
        end loop;

        return result;
    end MapInput;

    -- Turn on a connection From->To.
    function SetConnected(M: SegsMap; From: SeenSeg; To: InstSeg) return SegsMap
    with
        Pre =>
            SegsMapValid (M) and
            -- This implies that the rest of the column or row does not have Conn via SegsMapValid
            M(From)(To) = Unknown,
         Post =>
            SegsMapValid(SetConnected'Result)
    is
        result : SegsMap := M;
    begin
        for S in SeenSeg loop
            result(S)(To) := Unconn;
        end loop;
        for S in InstSeg loop
            result(From)(S) := Unconn;
        end loop;
        result(From)(To) := Conn;
        return Result;
    end SetConnected;

    -- Is this instrument segments assignment concrete (no unknowns)
    function IsConcrete(Inst: InstInd) return Boolean is
    begin
        return (for all I in InstInd'Range => Inst(I) /= Unknown);
    end IsConcrete;
    
    -- Is this guess at instrument segments viable?
    function IsPossible(Inst: InstInd) return Boolean
    is
        anyOk: Boolean := False;
    begin
        for D in Digit loop
            declare
                Segs: constant DigSegments := AllDigits(D);
                matches: Boolean := True;
            begin
                for S in SegmentNum loop
                    matches := matches and then (case Inst(S) is
                                                    when On | Off => Segs(S) = Inst(S),
                                                    when Unknown => true);
                end loop;
                anyOk := anyOk or matches;
            end;
        end loop;
        return anyOk;
    end IsPossible;

    function ToDigit(Inst: InstInd) return Digit
    with Pre =>
        -- Implied: concrete and possible inst matches one digit.
        (IsConcrete (Inst)) and
        (IsPossible (Inst)),
        Post => (for all I in SegmentNum => Inst(I) = AllDigits(ToDigit'Result)(I))
    is
    begin
        for I in Digit loop
            if (for all S in SegmentNum => AllDigits(I)(S) = Inst(S)) then
                return I;
            end if;
        end loop;
        raise ShouldNotGetHere;
    end ToDigit;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    package SeenPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => SeenInd);
    use SeenPkg;
    subtype SeenVec is SeenPkg.Vector;

    type WantedType is array(1..4) of SeenInd;

    -- Input line
    type InputL is record
        Seen: SeenVec;
        Wanted: WantedType;
    end record;

    package InputPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => InputL);
    use InputPkg;
    subtype InputType is InputPkg.Vector;

    type Solution is tagged record
        Input : InputType := InputPkg.Empty_Vector;
    end record;

    function ReadDigit(InData : File_Type) return SeenInd
    with Post => (for some I in SeenSeg => ReadDigit'Result(I) = On)
    is
        result : SeenInd := (others => Off);
    begin
        SkipWs (InData);
        result := (others => Off);
        loop
            declare
                c: Character := PeekChar(InData);
            begin
                case c is
                    when 'a' => result(Sa) := On;
                    when 'b' => result(Sb) := On;
                    when 'c' => result(Sc) := On;
                    when 'd' => result(Sd) := On;
                    when 'e' => result(Se) := On;
                    when 'f' => result(Sf) := On;
                    when 'g' => result(Sg) := On;
                    when ' ' | ASCII.LF | ASCII.nul => return result;
                    when others => raise ReadFailed with "Unexpected '" & c'Image & "'";
                end case;
                c := GetChar (InData); -- skip the character
            end;
        end loop;
    end ReadDigit;

    procedure ReadInput(self : in out Solution'class; InData : File_Type)
    with
        Post => not self.Input.Is_Empty
    is
    begin
        while not End_Of_File(InData) loop
            declare
                current: InputL;
            begin
                -- 10 inputs
                for i in 1..10 loop
                    current.Seen.Append(ReadDigit(InData));
                end loop;

                SkipChar (InData, '|');

                -- 4 wanted
                for i in WantedType'Range loop
                    current.Wanted(i) := ReadDigit (InData);
                end loop;
                self.Input.Append(Current);
            end;
            AdvanceLine (InData);
        end loop;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    -- Only used for part A.
    -- Can this input be converted to ouput unambiguously?
    function IsUnambigous(M: SeenInd) return Boolean is
    begin
        case CountSeen(M, On) is
            when 2 | 4 | 3 | 7 => return True; -- 1, 4, 7, 8
            when others => return False;
        end case;
    end IsUnambigous;

    type SolutionA is new Solution with record
        null;
    end record;

    function Solve (SDisp : p08a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionA;
        result : ResultType := 0;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        for E of s.Input loop
            for J of E.Wanted loop
                if IsUnambigous (J) then
                    result := result + 1;
                end if;
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

    -- Main solver.
    function DoAttempt(Input: InputL; InMap: SegsMap;
                       PrevI: SeenSeg; PrevJ: InstSeg;
                       concreteMap: out SegsMap) return Boolean
    with Pre => SegsMapValid (InMap),
         Post => (if DoAttempt'Result then SegsMapValid(concreteMap))
    is
    begin
        -- Starting from the lowest row (no need to try rows already tried),
        -- find Unknown positions. For each, try changing it to connected,
        -- check the constraints, and recurse if there are still unknowns.
        for I in PrevI..SeenSeg'Last loop
            -- Yes, this re-does 1..prevJ. But all of those will be set to Unconnected, so that's OK.
            for J in InstSeg loop
                if InMap(I)(J) = Unknown then
                    declare
                        m: SegsMap := InMap;
                    begin
                        m := SetConnected (M, I, J);

                        -- Is this mapping viable for all inputs?
                        if (for all Inp of Input.Seen =>
                                IsPossible(MapInput(m, Inp))) and then
                            (for all K in Input.Wanted'Range =>
                                IsPossible(MapInput(m, Input.Wanted(K)))) then
                            
                            -- All inputs and wanted are possible.
                            -- If all are concrete, we have our solution
                            -- Technically, only need Wanted to be concrete.
                            if (for all Inp of Input.Seen =>
                                    IsConcrete(MapInput(m, Inp))) and then
                                (for all K in Input.Wanted'Range =>
                                    IsConcrete(MapInput(m, Input.Wanted(K)))) then
                                    concreteMap := m;
                                    return True;
                            else
                                -- Keep going. If solution found, we are done,
                                -- otherwise backtrack and try the next position.
                                if DoAttempt (Input, m, I, J, concreteMap) then
                                    return True;
                                end if;
                            end if;
                        end if;
                    end;
                end if;
            end loop;
        end loop;

        return False;
    end DoAttempt;

    function Solve (SDisp : p08b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionB;
        result: ResultType := 0;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        for L of s.Input loop
            declare
                m: SegsMap := InitSegsMap;
                resMap: SegsMap;
            begin
                if DoAttempt (L, m, SeenSeg'First, InstSeg'First, resMap) then
                    if PrintDebug then Put_Line ("On line " & L'Image & " found solution."); end if;
                    declare
                        lres: ResultType := 0;
                    begin
                        -- Range is 1..4
                        for i in L.Wanted'Range loop
                            lres := lres + ResultType(ToDigit (MapInput(resMap, L.Wanted(i))))*(10**(l.Wanted'Last - i));
                        end loop;

                        if PrintDebug then Put_Line (lres'Image); end if;

                        result := result + lres;
                    end;
                else
                    raise ShouldNotGetHere;
                end if;
            end;
        end loop;

        return result;
    end Solve;

end p08;
