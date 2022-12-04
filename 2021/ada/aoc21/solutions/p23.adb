with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Hash;

with Harness; use Harness;
with utils; use utils;

package body p23 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(p23a'(Name => "p23a"));
        D.Append(p23b'(Name => "p23b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    ShouldNotGetHere: exception;

    subtype CostType is ResultType;

    -- What is in a particular spot on the map
    type SpotContent is (AmA, AmB, AmC, AmD, Empty, Wall, KeepEmpty);
    subtype AmType is SpotContent range AmA..AmD;
    subtype RoomSpot is SpotContent range AmA..Wall;

    --   123456789ab
    --  #############
    --  #...........#
    -- 1###.#.#.#.###
    -- 2  #.#.#.#.#
    -- 3  #########
    type HallCoord is range 1..11;
    type RoomCoord is range 1..4;

    -- To accommodate both parts
    type RoomSize is (PartA, PartB);

    function RoomMaxCoord(size: RoomSize) return RoomCoord is
        (if size = PartA then 2 else 4);

    type HallContent is array(HallCoord) of SpotContent
        with Dynamic_Predicate =>
            (for all i in HallContent'Range =>
                (HallContent(i) in AmA..Empty | KeepEmpty) and
                (if i in 3 | 5 | 7 | 9 then HallContent(i) = KeepEmpty else HallContent(i) in AmA..Empty));
            
    
    type RoomType is (RoomA, RoomB, RoomC, RoomD);
    type RoomContent is array(RoomCoord) of RoomSpot;
    type RoomsContent is array(RoomType) of RoomContent;
    
    function ToAmType(r: RoomType) return AmType is
        ((case r is
            when RoomA => AmA,
            when RoomB => AmB,
            when RoomC => AmC,
            when RoomD => AmD));
    
    function ToRoomType(a: AmType) return RoomType is
        ((case a is
            when AmA => RoomA,
            when AmB => RoomB,
            when AmC => RoomC,
            when AmD => RoomD));

    function ToHallCoord(r: RoomType) return HallCoord is
        ((case r is
            when RoomA => 3,
            when RoomB => 5,
            when RoomC => 7,
            when RoomD => 9));
    
    function EnergyMult(t: AmType) return CostType is
        ((case t is
            when AmA => 1,
            when AmB => 10,
            when AmC => 100,
            when AmD => 1000));

    generic
        type R is (<>);
        type T is (<>);
        type A is array(R) of T;
    function GenCount(c: A; etype: T) return Integer;

    function GenCount(c: A; etype: T) return Integer
    is
        result: Integer := 0;
    begin
        for e of c loop
            result := result + (if e = etype then 1 else 0);
        end loop;
        return result;
    end GenCount;

    function Count is new GenCount(R => HallCoord, T => SpotContent, A => HallContent);
    function Count is new GenCount(R => RoomCoord, T => RoomSpot, A => RoomContent);

    type BurrowState is record
        size: RoomSize;
        hall: HallContent; -- := (3 => KeepEmpty, 5 => KeepEmpty, 7 => KeepEmpty, 9 => KeepEmpty, others => Empty);
        rooms: RoomsContent;
    end record
    with Dynamic_Predicate =>
        -- Make sure expected walls stay in the rooms
        (if size = PartA then (for all r of rooms => r(3) = Wall and r(4) = Wall)
         else (for all r of rooms => Count(r, Wall) = 0)) and
        -- Allow the thing to be completely empty
        -- This is to allow EmptyBurrow function to return this.
        (((for all e of hall => e in Empty | KeepEmpty) and
          (for all r in rooms'Range => Count(rooms(r), Empty) + Count(rooms(r), Wall) = 4))
          or
         -- Remaining for non-empty burrow       
            -- Make sure the number of amphis stays correct
            (for all a in AmType =>
                (Count(hall, a) + Count(rooms(RoomA), a) + Count(rooms(RoomB), a) +
                Count(rooms(RoomC), a) + Count(rooms(RoomD), a)) = (if size = PartA then 2 else 4)));

    function EmptyBurrow(size: RoomSize) return BurrowState is
        ((size => size,
          hall => (3 => KeepEmpty, 5 => KeepEmpty, 7 => KeepEmpty, 9 => KeepEmpty, others => Empty),
          rooms => (others =>
                        (3 => (if size = PartA then Wall else Empty),
                         4 => (if size = PartA then Wall else Empty),
                         others => Empty))));
          

    function GenTarget(size: RoomSize) return BurrowState is
        subtype C is RoomCoord range 1..3;
        result: BurrowState := EmptyBurrow (size);
    begin
        for i in RoomCoord'First..RoomMaxCoord (size) loop
            result.rooms(RoomA)(i) := AmA;
            result.rooms(RoomB)(i) := AmB;
            result.rooms(RoomC)(i) := AmC;
            result.rooms(RoomD)(i) := AmD;
        end loop;
        return result;
    end GenTarget;

    function IsTarget(b: BurrowState) return Boolean is
        target: constant BurrowState := GenTarget(b.size);
    begin
        return b = target;
    end IsTarget;

    type ToStateMove is record
        s: BurrowState;
        cost: CostType;
    end record;
    type NextStates is array(Positive range <>) of ToStateMove;

    function GetNextStates(b: BurrowState) return NextStates is
        -- Can someone move into a room?
        canMoveIn: array(RoomType) of Boolean;
        -- Space for the result. 4 possible moves out of rooms to every hall location, about 5 out of the hallway
        result: NextStates(1..(5 + 4 * 7));
        numResult: Natural := 0;

        -- Move into a room
        procedure AddMove(from: HallCoord; to: RoomType)
        with
            Pre =>
                (b.hall(from) in AmType) and
                (if from < ToHallCoord(to) then
                    (for all i in (from + 1)..ToHallCoord(to) => b.hall(i) in Empty | KeepEmpty)) and
                (if from > ToHallCoord(to) then
                    (for all i in ToHallCoord(to)..(from - 1) => b.hall(i) in Empty | KeepEmpty))
        is
            typ: constant AmType := b.hall(from);
            t: BurrowState := b;
            dist: CostType := CostType(if from > ToHallCoord(to) then from - ToHallCoord(to) else ToHallCoord(to) - From);
        begin
            for r in reverse 1..RoomMaxCoord(b.size) loop
                if b.rooms(to)(r) = Empty then
                    dist := dist + CostType(r);
                    t.rooms(to)(r) := typ;
                    t.hall(from) := Empty;
                    exit;
                end if;
                
                Assert(b.rooms(to)(r) in Wall | ToAmType (to));
            end loop;

            Assert(t /= b);

            declare
                e: ToStateMove := (s => t, cost => dist * EnergyMult (typ));
            begin
                numResult := numResult + 1;
                result(numResult) := e;
            end;
        end AddMove;

        -- Move out of a room
        procedure AddMove(from: RoomType; fromCoord: RoomCoord; to: HallCoord)
        with Pre =>
            (for all i in 1..RoomCoord'Pred(fromCoord) => b.rooms(from)(i) = Empty) and
            ((b.rooms(from)(fromCoord) /= ToAmType(from)) or
             (for some i in RoomCoord'Succ(fromCoord)..RoomCoord'Last => b.rooms(from)(i) /= ToAmType(from) and b.rooms(from)(i) /= Wall))
        is
            typ: constant AmType := b.rooms(from)(fromCoord);
            t: BurrowState := b;
            dist: constant CostType := CostType(fromCoord) + CostType((if to > ToHallCoord(from) then to - ToHallCoord(from) else ToHallCoord(from) - to));
        begin
            t.hall(to) := typ;
            t.rooms(from)(fromCoord) := Empty;
            numResult := numResult + 1;
            result(numResult) := (s => t, cost => dist * EnergyMult (typ));
        end AddMove;
    begin
        for r in RoomType loop
            canMoveIn(r) := (for all e of b.rooms(r) => (e = ToAmType (r)) or e in Empty | Wall);
        end loop;

        -- Moves out of the hall
        for i in HallCoord loop
            if (b.hall(i) not in Empty | KeepEmpty) and then (canMoveIn(ToRoomType(b.hall(i)))) then
                declare
                    rc: constant HallCoord := ToHallCoord (ToRoomType(b.hall(i)));
                begin
                    if (i < rc and then (for all j in HallCoord'Succ(i)..rc => b.hall(j) in Empty | KeepEmpty)) or
                       (i > rc and then (for all j in rc..HallCoord'Pred(i) => b.hall(j) in Empty | KeepEmpty)) then
                        AddMove (i, ToRoomType(b.hall(i)));
                    end if;
                end;
            end if;
        end loop;

        -- Moves out of rooms
        for r in RoomType loop
            if not canMoveIn(r) then
                for i in 1..RoomCoord'Last loop
                    if b.rooms(r)(i) in AmType then
                        for j in reverse 1..ToHallCoord (r) loop
                            if b.hall(j) = Empty then
                                AddMove (r, i, j);
                            end if;

                            exit when b.hall(j) not in Empty | KeepEmpty;
                        end loop;

                        for j in ToHallCoord (r)..HallCoord'Last loop
                            if b.hall(j) = Empty then
                                AddMove (r, i, j);
                            end if;

                            exit when b.hall(j) not in Empty | KeepEmpty;
                        end loop;

                    end if;

                    exit when b.rooms(r)(i) /= Empty;
                end loop;
            end if;
        end loop;

        return result(1..numResult);
    end GetNextStates;

    procedure PrintState(s: BurrowState) is
        function ElToChar(e: SpotContent) return Character is
            (case e is
                when AmA => 'A', when AmB => 'B', when AmC => 'C', when AmD => 'D',
                when Empty => '.', when KeepEmpty => ' ', when Wall => '#');
    begin
        if not PrintDebug then
            return;
        end if;

        Put_line("#############");
        Put("#");
        for e of s.hall loop
            Put(ElToChar(e));
        end loop;
        Put_Line ("");

        for i in RoomCoord'First..RoomCoord'Last loop
            if i = 1 then Put("###"); else Put("  #"); end if;
            for e of s.rooms loop
                Put(ElToChar(e(i))); Put("#");
            end loop;
            if i = 1 then Put_Line("##"); else Put_Line (""); end if;
        end loop;
    end PrintState;


    -- -- x 123456789ab
    -- -- y#############
    -- -- 1#...........#
    -- -- 2###.#.#.#.###
    -- -- 3  #.#.#.#.#
    -- --    #########
    -- type CoordX is range 1..11;
    -- type CoordY is range 1..3;

    -- type Coord is record
    --     x: CoordX;
    --     y: CoordY;
    -- end record;

    -- invalidCoord: constant Coord := (11, 3);

    -- type SpaceContent is (A, B, C, D, Empty, KeepEmpty, Invalid);
    -- subtype AmType is SpaceContent range A..D;

    -- type AmMoveEnergy is array(AmType) of ResultType;
    -- amMoveEnergies: constant AmMoveEnergy := (A => 1, B => 10, C => 100, D => 1000);

    -- -- Valid locations where someone can be
    -- validLocations: constant array(Positive range <>) of Coord :=
    --     ((1, 1), (2, 1), (4, 1), (6, 1), (8, 1), (10, 1), (11, 1),
    --      (3, 2), (5, 2), (7, 2), (9, 2),
    --      (3, 3), (5, 3), (7, 3), (9, 3));
    -- -- Locations that must be empty
    -- emptyLocations: constant array(Positive range <>) of Coord :=
    --     ((3, 1), (5, 1), (7, 1), (9, 1));
    
    -- -- Room coordinates
    -- roomCoords: constant array(SpaceContent range A..D, 1..2) of Coord :=
    --     (A => ((3, 2), (3, 3)),
    --      B => ((5, 2), (5, 3)),
    --      C => ((7, 2), (7, 3)),
    --      D => ((9, 2), (9, 3)));

    -- type MapType is array(CoordX, CoordY) of SpaceContent
    -- with Dynamic_Predicate =>
    --     -- Ignore fully invalid (during initialization)
    --     (for all x in MapType'Range(1) => (for all y in MapType'Range(2) => MapType(x, y) = Invalid)) or
    --     (for all x in MapType'Range(1) =>
    --         (for all y in MapType'Range(2) =>
    --             (case Maptype(x, y) is
    --                 when Invalid => not (for some t of validLocations => t.x = x and t.y = y),
    --                 when KeepEmpty => (for some t of emptyLocations => t.x = x and t.y = y),
    --                 when others => (for some t of validLocations => t.x = x and t.y = y))));

    -- function Count(map: MapType; c: SpaceContent) return Positive is
    --     result: Natural := 0;
    -- begin
    --     for x in map'Range(1) loop
    --         for y in map'Range(2) loop
    --             result := result + (if map(x, y) = c then 1 else 0);
    --         end loop;
    --     end loop;
    --     return result;
    -- end Count;

    -- function GenEmptyMap return MapType is
    --     result: Maptype := (others => (others => Invalid));
    -- begin
    --     for c of validLocations loop
    --         result(c.x, c.y) := Empty;
    --     end loop;
    --     for c of emptyLocations loop
    --         result(c.x, c.y) := KeepEmpty;
    --     end loop;
    --     return result;
    -- end GenEmptyMap;

    -- emptyMap: constant MapType := GenEmptyMap;

    -- type BurrowState is record
    --     map: MapType := emptyMap;
    --     --cost: ResultType := 0;
    -- end record
    -- with
    --     -- Sanity check
    --     Dynamic_Predicate => 
    --         (BurrowState.map = emptyMap) or 
    --         (for all typ in A..D => Count(BurrowState.map, typ) = 2);

    -- procedure PrintState(s: BurrowState) is
    --     function ElToChar(e: SpaceContent) return Character is
    --         (case e is
    --             when A => 'A', when B => 'B', when C => 'C', when D => 'D',
    --             when Empty => '.', when KeepEmpty => ' ', when Invalid => '#');
    -- begin
    --     if not PrintDebug then
    --         return;
    --     end if;

    --     Put_line("#############");
    --     for y in CoordY loop
    --         if y < 3 then
    --             Put("#");
    --             for x in CoordX loop
    --                 Put(ElToChar(s.map(x, y)));
    --             end loop;
    --             Put_line("#");
    --         else
    --             Put("  ");
    --             for x in CoordX(2)..10 loop
    --                 Put(ElToChar(s.map(x, y)));
    --             end loop;
    --             Put_Line ("");
    --         end if;
    --     end loop;
    -- end PrintState;

    -- function GenTargetMap return Maptype is
    --     map: MapType := emptyMap;
    -- begin
    --     map(3, 2) := A; map(3, 3) := A;
    --     map(5, 2) := B; map(5, 3) := B;
    --     map(7, 2) := C; map(7, 3) := C;
    --     map(9, 2) := D; map(9, 3) := D;

    --     return map;
    -- end GenTargetMap;

    -- function IsTarget(s: BurrowState) return Boolean is
    --     target: constant MapType := GenTargetMap;
    -- begin
    --     return s.map = target;
    -- end IsTarget;

    -- type NextStateType is record
    --     s: BurrowState;
    --     moveCost: ResultType;
    -- end record;

    -- type NextStatesArray is array(Positive range <>) of NextStateType;

    -- -- All possible next state
    -- function GetNextStates(s: BurrowState) return NextStatesArray
    -- with Pre => not IsTarget(s)
    -- is
    --     res: NextStatesArray(1..9*7);
    --     nextIndex: Positive := 1;

    --     procedure AddMove(from, to: Coord; numMoves: ResultType)
    --     with Pre => s.map(to.x, to.y) = Empty
    --     is
    --         r: BurrowState := s;
    --         typ: AmType := s.map(from.x, from.y);
    --     begin
    --         r.map(to.x, to.y) := typ;
    --         r.map(from.x, from.y) := Empty;
    --         res(nextIndex) := (s => r, moveCost => amMoveEnergies(typ) * numMoves);
    --         nextIndex := nextIndex + 1;
    --     end AddMove;
            
    -- begin
    --     for roomTyp in roomCoords'Range(1) loop
    --         -- Moves from room to hallway
    --         check_room_out:
    --         for i in roomCoords'Range(2) loop
    --             declare
    --                 c: constant Coord := roomCoords(roomTyp, i);
    --                 moves: ResultType := 0;
    --             begin
    --                 if s.map(c.x, c.y) /= Empty then
    --                     -- If 2nd position, only move if does not belong there.
    --                     exit check_room_out when i = 2 and s.map(c.x, c.y) = roomTyp;
    --                     -- If 1st position, only move if does not belong there OR the 2nd position does not belong
    --                     exit check_room_out when i = 1 and then (s.map(c.x, c.y) = roomTyp and s.map(c.x, c.y + 1) = roomTyp);

    --                     moves := moves + ResultType(c.y) - 1;

    --                     -- Going left
    --                     for x in reverse 1..(c.x-1) loop
    --                         exit when s.map(x, 1) not in Empty | KeepEmpty;
    --                         if s.map(x, 1) = Empty then
    --                             AddMove(c, (x, 1), moves + ResultType(c.x - x));
    --                         end if;
    --                     end loop;

    --                     -- And right
    --                     for x in (c.x+1)..CoordX'Last loop
    --                         exit when s.map(x, 1) not in Empty | KeepEmpty;
    --                         if s.map(x, 1) = Empty then
    --                             AddMove(c, (x, 1), moves + ResultType(x - c.x));
    --                         end if;
    --                     end loop;

    --                     -- If 1st position is non-empty, further ones cannot move.
    --                     exit check_room_out;
    --                 end if;                    
    --             end;
    --         end loop check_room_out;

    --         -- Moves from hallway into the room
    --         check_room_in:
    --         for i in roomCoords'Range(2) loop
    --             declare
    --                 c: constant Coord := roomCoords(roomTyp, i);
    --             begin
    --                 exit check_room_in when s.map(c.x, c.y) /= Empty;
    --                 -- -- If this is the first empty space in the room, check that the other one matches the room type,
    --                 -- -- otherwise noone can move into it
    --                 -- exit check_room_in when i = 1 and then s.map(c.x, c.y + 1) not in Empty | roomTyp;

    --                 -- If this is the first empty space in the room, but the second one is empty, don't use this one
    --                 -- XXX: Doesn't make sense, but it seems to be what the example is doing?
    --                 if (i = 2) or (i = 1 and then s.map(c.x, c.y + 1) = roomTyp) then
    --                     -- From left of the room
    --                     for x in reverse 1..(c.x - 1) loop
    --                         if s.map(x, 1) = roomTyp then
    --                             AddMove((x, 1), c, ResultType(c.x - x) + ResultType(c.y - 1));
    --                             exit;
    --                         elsif s.map(x, 1) not in Empty | KeepEmpty then
    --                             exit;
    --                         end if;
    --                     end loop;

    --                     -- From right of the room
    --                     for x in (c.x + 1)..CoordX'Last loop
    --                         if s.map(x, 1) = roomTyp then
    --                             AddMove((x, 1), c, ResultType(x - c.x) + ResultType(c.y - 1));
    --                             exit;
    --                         elsif s.map(x, 1) not in Empty | KeepEmpty then
    --                             exit;
    --                         end if;
    --                     end loop;
    --                 end if;
    --             end;
               
    --         end loop check_room_in;
    --     end loop;

    --     return res(1..(nextIndex - 1));
    -- end GetNextStates;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    type Solution is tagged record
        Input : BurrowState := EmptyBurrow (PartA); -- Will get replaced
    end record;
    -- procedure ReadInput(self : in out Solution'class; InData : File_Type)
    -- is
    --     use InputStrPkg;
    --     function ToAmType(cx: Character) return AmType is
    --         (case cx is
    --             when 'A' => A,
    --             when 'B' => B,
    --             when 'C' => C,
    --             when 'D' => D,
    --             when others => raise ShouldNotGetHere);
    -- begin
    --     SkipString(InData, "#############"); AdvanceLine(InData);
    --     SkipString(InData, "#...........#"); AdvanceLine(InData);
    --     declare
    --         row: InputStr := GetAtom(InData);
    --     begin
    --         AdvanceLine(InData);
    --         self.input.map(3, 2) := ToAmType(Element(row, 4));
    --         self.input.map(5, 2) := ToAmType(Element(row, 6));
    --         self.input.map(7, 2) := ToAmType(Element(row, 8));
    --         self.input.map(9, 2) := ToAmType(Element(row, 10));
    --     end;
    --     declare
    --         row: InputStr := GetAtom(InData);
    --     begin
    --         AdvanceLine(InData);
    --         self.input.map(3, 3) := ToAmType(Element(row, 2));
    --         self.input.map(5, 3) := ToAmType(Element(row, 4));
    --         self.input.map(7, 3) := ToAmType(Element(row, 6));
    --         self.input.map(9, 3) := ToAmType(Element(row, 8));
    --     end;
    -- end ReadInput;

    procedure ReadInput(self: in out Solution'Class; InData: File_Type)
    is
        use InputStrPkg;
        function ToAmType(cx: Character) return AmType is
            (case cx is
                when 'A' => AmA,
                when 'B' => AmB,
                when 'C' => AmC,
                when 'D' => AmD,
                when others => raise ShouldNotGetHere);
    begin
        SkipString(InData, "#############"); AdvanceLine(InData);
        SkipString(InData, "#...........#"); AdvanceLine(InData);
        for rownum in RoomCoord'First..2 loop
            declare
                row: InputStr := GetAtom(File => InData, ShouldSkipWs => False);
            begin
                AdvanceLine(InData);
                for rt in RoomType loop
                    self.Input.rooms(rt)(rownum) := ToAmType (Element(row, Integer(ToHallCoord(rt) + 1)));
                end loop;
            end;
        end loop;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    function Hash(key: BurrowState) return Ada.Containers.Hash_Type is
        use Ada.Containers;
        result: Hash_Type := 0;
    begin
        for e of key.hall loop
            result := (result * 2) xor (result / (2 ** (Hash_Type'Size - 1)));
            result := result xor SpotContent'Pos(e);
        end loop;
        
        for e of key.rooms loop
            for j of e loop
                result := (result * 2) xor (result / (2 ** (Hash_Type'Size - 1)));
                result := result xor RoomSpot'Pos(j);
            end loop;
        end loop;

        return result;
    end Hash;

    package StateCostPkg is new
        Ada.Containers.Hashed_Maps(Key_Type => BurrowState,
                                   Element_Type => ResultType,
                                   Hash => Hash,
                                   Equivalent_Keys => "=");
    use StateCostPkg;
    subtype StateCostType is StateCostPkg.Map;

    function "<"(left, right: ToStateMove) return Boolean
    with Pre => left.s.size = right.s.size
    is
    begin
        if left.cost = right.cost then
            for i in left.s.hall'Range loop
                if left.s.hall(i) < right.s.hall(i) then
                    return True;
                elsif left.s.hall(i) > right.s.hall(i) then
                    return False;
                end if;
            end loop;

            for i in left.s.rooms'Range loop
                for j in left.s.rooms(i)'Range loop
                    if left.s.rooms(i)(j) < right.s.rooms(i)(j) then
                        return True;
                    elsif left.s.rooms(i)(j) > right.s.rooms(i)(j) then
                        return False;
                    end if;
                end loop;
            end loop;

            Assert(left.s = right.s);
            return False;
        elsif left.cost < right.cost then
            return True;
        else
            return False;
        end if;
    end "<";

    package UnvisitedPkg is new
        Ada.Containers.Ordered_Sets(Element_Type => ToStateMove);
    use UnvisitedPkg;
    subtype UnvisitedSet is UnvisitedPkg.Set;

    type SolutionA is new Solution with record
        null;
    end record;

    function FindLowestCost(start: BurrowState) return ResultType is
        visited: StateCostType;
        unvisited: UnvisitedSet;
        target: constant BurrowState := GenTarget(start.size);
    begin
        visited.Insert (start, 0);
        unvisited.Insert ((start, 0));

        loop
            declare
                cur: ToStateMove := unvisited.First_Element;
                next: NextStates := GetNextStates (cur.s);
            begin
                unvisited.Delete (cur);

                for e of next loop
                    if not visited.Contains (e.s) then
                        visited.Insert (e.s, cur.cost + e.cost);
                        unvisited.Insert ((e.s, cur.cost + e.cost));
                    elsif visited.Element(e.s) > (cur.cost + e.cost) then
                        unvisited.Delete((e.s, visited.Element(e.s)));
                        visited.Replace (e.s, cur.cost + e.cost);
                        unvisited.Insert((e.s, cur.cost + e.cost));
                    end if;

                end loop;

                if PrintDebug then
                    Put_Line (ASCII.LF & "From with cost " & cur.cost'Image & ASCII.LF);
                    PrintState (cur.s);
                    for e of next loop
                        PrintState (e.s);
                        Put_Line ("cost: " & e.cost'Image);
                    end loop;
                    -- for e in visited.Iterate loop
                    --     PrintState (Key(e));
                    --     Put_Line ("cost: " & Element(e)'Image);
                    -- end loop;
                end if;

                exit when IsTarget (cur.s);
            end;
        end loop;

        return visited.Element(target);
    end FindLowestCost;

    function Solve (SDisp : p23a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionA;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        PrintState (s.Input);
        return FindLowestCost (s.Input);
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p23b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionB;
        inputB : BurrowState := EmptyBurrow(PartB);
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        inputB.hall := s.Input.hall;
        for r in RoomType loop
            inputB.rooms(r)(1) := s.Input.rooms(r)(1);
            inputB.rooms(r)(4) := s.Input.rooms(r)(2);
        end loop;
        inputB.rooms(RoomA)(2..3) := (AmD, AmD);
        inputB.rooms(RoomB)(2..3) := (AmC, AmB);
        inputB.rooms(RoomC)(2..3) := (AmB, AmA);
        inputB.rooms(RoomD)(2..3) := (AmA, AmC);

        PrintState (s.Input);
        return FindLowestCost (inputB);
    end Solve;

end p23;
