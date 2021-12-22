with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;
with Ada.Containers.Ordered_Sets;

with Harness; use Harness;
with utils; use utils;

package body p15 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(p15a'(Name => "p15a"));
        D.Append(p15b'(Name => "p15b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    type Coord is range 1..501;
    type Matrix is array(Coord, Coord) of Integer;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    type Solution is tagged record
        Input : Matrix := (others => (others => 0));
        size: Coord := 1;
    end record;

    procedure ReadInput(self : in out Solution'class; InData : File_Type)
    with Post => self.size > 1
    is
        row, col: Coord := 1;
    begin
        while not End_Of_File(InData) loop

            while  PeekChar (InData) not in ASCII.LF | ASCII.nul loop
                self.Input(row, col) := Integer'Value("" & GetChar (InData));
                col := col + 1;
            end loop;
            row := row + 1;
            col := 1;
            AdvanceLine (InData);
        end loop;
        self.size := row - 1;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    type SolutionA is new Solution with record
        null;
    end record;

    function Solve (SDisp : p15a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionA;

        -- Use a BFS of sorts, but with an unordered queue of nodes
        -- to visit: that's fast enough.
        -- Unlike better algos, this can visit nodes multiple times
        -- as it is not prioritizing visiting shortest distance paths first.
        --
        -- However, to speed it up, pick an arbitrary path initially
        -- (below just goes all the way down and then left) and use
        -- that as an upper bound to when to stop exploring paths.

        lowest: Matrix := (others => (others => Integer'Last));

        type Pair is record
            x, y: Coord := 1;
        end record;

        type QArr is array(Integer range 1..(100*100/2)) of Pair;
        queue: QArr;
        queueSize: Integer := 0;

        procedure Push(x, y: Coord) is
        begin
            queueSize := queueSize + 1;
            queue(queueSize) := (x, y);
        end Push;
        
        procedure Check(curLowest: Integer; x, y: Coord) is
            next: constant Integer := s.Input(x, y) + curLowest;
        begin
            if next < lowest(x, y) and next < lowest(s.size, s.size) then
                lowest(x, y) := next;
                Push(x, y);
            end if;
        end Check;

    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        Push(1, 1);
        lowest(1, 1) := 0;

        declare
            badPathTotal: Integer := 0;
        begin
            for x in 2..s.size loop
                badPathTotal := badPathTotal + s.Input(x, 1);
            end loop;
            for y in 2..s.size loop
                badPathTotal := badPathTotal + s.Input(s.size, y);
            end loop;

            lowest(s.size, s.size) := badPathTotal;
        end;

        while queueSize > 0 loop
            declare
                x: Coord := queue(queueSize).x;
                y: Coord := queue(queueSize).y;
                cur: Integer := lowest(x, y);
            begin
                queueSize := queueSize - 1;
                if x < s.size then
                    Check (cur, x + 1, y);
                end if;
                if x > 1 then
                    Check (cur, x - 1, y);
                end if;
                if y < s.size then
                    Check (cur, x, y + 1);
                end if;
                if y > 1 then
                    Check (cur, x, y - 1);
                end if;

            end;
        end loop;

        return ResultType(lowest(s.size, s.size));
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p15b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionB;

        -- Use dijkstra here, naive approach from A is a bit too slow.

        lowest: Matrix := (others => (others => Integer'Last));

        type Pair is record
            x, y: Coord := 1;
            dist: Integer := Integer'Last;
        end record;

        function "<"(a, b: Pair) return Boolean is
        begin
            if a.dist = b.dist then
                return (if a.x = b.x then a.y < b.y
                        else a.x < b.x);
            else
                return a.dist < b.dist;
            end if;
        end "<";

        package QueuePkg is new
            Ada.Containers.Ordered_Sets(Element_Type => Pair);
        subtype QueueType is QueuePkg.Set;
        use QueuePkg;
        use type QueueType;

        queue: QueueType;

        procedure Check(curLowest: Integer; x, y: Coord) is
            next: constant Integer := s.Input(x, y) + curLowest;
        begin
            if lowest(x, y) > next then
                declare
                    tnew: Pair := (x, y, next);
                    told: Pair := (x, y, lowest(x, y));                    
                begin
                    lowest(x, y) := next;
                    queue.Exclude (told);
                    queue.Insert (tnew);
                end;
            end if;
        end Check;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        -- Copy across one row (or is it column?)
        for x in 1..s.size loop
            for y in 1..s.size loop
                declare
                    t : Integer := s.input(x, y);
                begin
                    for i in 1..4 loop
                        t := (if t /= 9 then t + 1 else 1);
                        s.Input(x, y + s.size * Coord(i)) := t;
                    end loop;
                end;
            end loop;
        end loop;

        -- Now in the other direction
        for x in 1..s.size loop
            for y in 1..(s.size * 5) loop
                declare
                    t : Integer := s.Input(x, y);
                begin
                    for i in 1..4 loop
                        t := (if t /= 9 then t + 1 else 1);
                        s.Input(x + s.size * Coord(i), y) := t;
                    end loop;
                end;
            end loop;
        end loop;

        s.size := s.size * 5;

        queue.Insert((1, 1, 0));
        lowest(1, 1) := 0;
 
        while (not queue.Is_Empty) loop -- and (not queue.Contains ((s.size, s.size, 0))) loop
            declare
                curPoint: constant Pair := Element(queue.First);
                x: constant Coord := curPoint.x;
                y: constant Coord := curPoint.y;
                cur: constant Integer := lowest(x, y);
            begin
                Assert (curPoint.dist = cur, "Distance mismatch");
                queue.Delete_First; 
                if x < s.size then
                    Check (cur, x + 1, y);
                end if;
                if x > 1 then
                    Check (cur, x - 1, y);
                end if;
                if y < s.size then
                    Check (cur, x, y + 1);
                end if;
                if y > 1 then
                    Check (cur, x, y - 1);
                end if;

            end;
        end loop;

        return ResultType(lowest(s.size, s.size));
    end Solve;

end p15;
