with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;
with Ada.Numerics.Elementary_Functions;

with Harness; use Harness;
with utils; use utils;

package body p17 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(new p17a'(Name => "p17a"));
        D.Append(new p17b'(Name => "p17b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    ------------------------------------
    -- Input data definitions
    ------------------------------------

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------
    type Axis is new Integer;
    -- X can only be non-negative
    subtype PosAxis is Axis range 0..Axis'Last;
    type Coord is record
        x: PosAxis;
        y: Axis;
    end record;

    type Solution is tagged record
        xmin, xmax, ymin, ymax: Axis;
    end record;

    procedure ReadInput(self : in out Solution'class; InData : File_Type)
    is
    begin
        SkipString (InData, "target area: x=");
        self.xmin := Axis(GetInt (InData));
        SkipString (InData, "..");
        self.xmax := Axis(GetInt (InData));

        SkipString (InData, ", y=");

        self.ymin := Axis(GetInt(InData));
        SkipString (InData, "..");
        self.ymax := Axis(GetInt (InData));
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    function InTarget(s: Solution'Class; p: Coord) return Boolean is
        ((p.x >= s.xmin) and (p.x <= s.xmax) and (p.y >= s.ymin and p.y <= s.ymax));

    procedure Step(pos: in out Coord; vel: in out Coord) is
    begin
        pos.x := pos.x + vel.x;
        pos.y := pos.y + vel.y;

        vel.x := vel.x - (if vel.x /= 0 then 1 else 0);
        vel.y := vel.y - 1;
    end Step;
                  

    type SolutionA is new Solution with record
        null;
    end record;

    function Solve (SDisp : p17a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        use Ada.Numerics.Elementary_Functions;
        s : SolutionA;
        result: Axis := 0;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        -- Maximum X velocity is such that we don't overshoot the target after the first step
        -- Minimum X velocity is such that Sum(1..vx) >= xmin
        -- => vx*(vx+1) >= xmin * 2
        -- approx. vx >= sqrt(xmin * 2)
        -- Here we assume xmin is at least 2
        for vx in Axis(Sqrt(Float(s.xmin) * 2.0) - 1.0)..s.xmax loop
            -- As with vx, min vy cannot be such that we overshoot the target after the first step.
            -- Assuming target is below x axis:
            -- For y velocity vy, it'll take t = 2*vy steps in order to arc back down to 0
            -- Then we'll have -vy velocity, which cannot let us overshoot the target on the first step.
            Assert(s.ymin < 0);
            for vy in s.ymin..-s.ymin loop
                declare
                    pos: Coord := (0, 0);
                    vel: Coord := (Axis(vx), Axis(vy));
                    maxy: Axis := 0;
                begin
                    inner:
                    for snum in 1..1000 loop
                        Step(pos, vel);
                        maxy := (if pos.y > maxy then pos.y else maxy);

                        if InTarget (s, pos) then
                            if maxy > result then
                                result := maxy;
                            end if;
                            exit inner;
                        end if;

                        if pos.x > s.xmax or pos.y < s.ymin then
                            exit inner;
                        end if;

                        -- If we haven't reached the target x and velocity is zero, we won't reach it ever
                        if vel.x = 0 and pos.x < s.xmin then
                            exit inner;
                        end if;

                    end loop inner;
                end;
            end loop;
        end loop;

        return ResultType(result);
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p17b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        use Ada.Numerics.Elementary_Functions;

        s : SolutionB;
        result: ResultType := 0;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        -- Maximum X velocity is such that we don't overshoot the target after the first step
        -- Minimum X velocity is such that Sum(1..vx) >= xmin
        -- => vx*(vx+1) >= xmin * 2
        -- approx. vx >= sqrt(xmin * 2)
        -- Here we assume xmin is at least 2
        Assert(s.xmin > 2);
        for vx in Axis(Sqrt(Float(s.xmin) * 2.0) - 1.0)..s.xmax loop
            -- Assuming target is below x axis:
            -- As with vx, min vy cannot be such that we overshoot the target after the first step.
            -- For y velocity vy, it'll take t = 2*vy steps in order to arc back down to 0
            -- Then we'll have -vy velocity, which cannot let us overshoot the target on the next step.
            -- XXX: vy range is actually dependent on vx here, but meh.
            Assert(s.ymin < 0);
            for vy in s.ymin..-s.ymin loop
                declare
                    pos: Coord := (0, 0);
                    vel: Coord := (Axis(vx), Axis(vy));
                begin
                    inner:
                    for snum in 1..1000 loop
                        Step(pos, vel);

                        if InTarget (s, pos) then
                            result := result + 1;
                            exit inner;
                        end if;

                        if pos.x > s.xmax or pos.y < s.ymin then
                            exit inner;
                        end if;
                        
                        -- If we haven't reached the target x and velocity is zero, we won't reach it ever
                        if vel.x = 0 and pos.x < s.xmin then
                            exit inner;
                        end if;

                    end loop inner;
                end;
            end loop;
        end loop;

        return result;
    end Solve;

end p17;
