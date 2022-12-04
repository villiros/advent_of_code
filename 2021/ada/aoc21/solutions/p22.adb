with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;

with Harness; use Harness;
with utils; use utils;

package body p22 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(p22a'(Name => "p22a"));
        D.Append(p22b'(Name => "p22b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    type AxisName is (AxX, AxY, AxZ);
    type SwitchState is (On, Off);

    -- Important: the "to" here is exclusive, so it's +1 from what the problem says
    type AxisRange(isNull: Boolean) is record
        case isNull is
            when True =>
                null;
            when False =>
                from, to: ResultType;
        end case;
    end record
    with Dynamic_Predicate => (if not isNull then to > from);

    type Cuboid(isNull: Boolean) is record
        case isNull is
            when True =>
                null;
            when False =>
                rangeX, rangeY, rangeZ: AxisRange(False);
                sw: SwitchState;
        end case;
    end record;

    function GetIsNull(c: Cuboid) return Boolean is
        (c.isNull);

    nullCuboid: constant Cuboid := (isNull => True);

    -- Array of non-null cuboids.
    type Cuboids is array(Positive range <>) of Cuboid(false);

    function Volume(c: Cuboid) return ResultType is
        (if c.isNull then 0
         else (c.rangeX.to - c.rangeX.from) * (c.rangeY.to - c.rangeY.from) * (c.rangeZ.to - c.rangeZ.from));

    function SumVolume(c: Cuboids) return ResultType is
        result: ResultType := 0;
    begin
        for e of c loop
            result := result + Volume(e);
        end loop;
        return result;
    end SumVolume;

    function ToCuboid(fromX, toX: ResultType; fromY, toY: ResultType; fromZ, toZ: ResultType; sw: SwitchState) return Cuboid
    with Pre => (toX >= fromX) and (toY >= fromY) and (toZ >= fromZ),
         Post => not GetIsNull(ToCuboid'Result)
    is
    begin
        return (isNull => False,
                sw => sw,
                rangeX => (isNull => False, from => fromX, to => toX + 1),
                rangeY => (isNull => False, from => fromY, to => toY + 1),
                rangeZ => (isNull => False, from => fromZ, to => toZ + 1));
    end ToCuboid;

    -- Intersect. Takes switch state from a
    function "and"(a, b: Cuboid) return Cuboid
    with Pre => (not a.isNull) and (not b.isNull),
                -- Sanity check: resulting volume should not be greater than either input.
         Post => Volume("and"'Result) <= Volume(a) and
                 Volume("and"'Result) <= Volume(b)
    is
        function IntersectRange(from, to: AxisRange) return AxisRange
        is
            left: ResultType := (if to.from < from.from then from.from
                                 elsif to.from < from.to then to.from
                                 else ResultType'Last);
            right: ResultType := (if to.to < from.from then ResultType'First
                                  elsif to.to < from.to then to.to
                                  else from.to);
        begin
            if left < right then
                return (isNull => False, from => left, to => right);
            else
                return (isNull => True);
            end if;
        end IntersectRange;

        rangeX: AxisRange := IntersectRange(a.rangeX, b.rangeX);
        rangeY: AxisRange := IntersectRange(a.rangeY, b.rangeY);
        rangeZ: AxisRange := IntersectRange(a.rangeZ, b.rangeZ);
    begin
        if rangeX.isNull or rangeY.isNull or rangeZ.isNull then
            return nullCuboid;
        else
            return (isNull => False, sw => a.sw, rangeX => rangeX, rangeY => rangeY, rangeZ => rangeZ);
        end if;
    end "and";

    -- Slice cuboid A on axis plane located at pos
    -- Returns 1 or two cuboids, in a particular order ("left" is first)
    function Slice(a: Cuboid; plane: AxisName; pos: ResultType) return Cuboids
    with Pre => (not a.isNull),
         Post => (Slice'Result'Length in 1 | 2) and
                -- If 1 is returned, then the plane is outside or just touches a
                -- Otherwise, two returned cuboids should have the same volume as the input
                -- (If there was a union operator, union of results should be a)
                 (if Slice'Result'Length = 1
                  then Slice'Result(1) = a
                  else Volume(Slice'Result(1)) + Volume(Slice'Result(2)) = Volume(a))
    is
        r: AxisRange := (case plane is
                            when AxX => a.rangeX,
                            when AxY => a.rangeY,
                            when AxZ => a.rangeZ);
    begin
        if pos > r.from and pos < r.to then
            -- Make a copy of a and adjust their axis ranges.
            declare
                left, right: Cuboid := a;
            begin
                case plane is
                    when AxX =>
                        left.rangeX.to := pos;
                        right.rangeX.from := pos;
                    when AxY =>
                        left.rangeY.to := pos;
                        right.rangeY.from := pos;
                    when AxZ =>
                        left.rangeZ.to := pos;
                        right.rangeZ.from := pos;
                end case;
                return (1 => left, 2 => right);
            end;
        else
            -- Slice plane does not go through the cuboid, so return it as-is.
            return (1 => a);
        end if;
    end Slice;
    
    -- Subtract: given cuboids A and B, return parts of A that are not in B
    function "-"(a, b: Cuboid) return Cuboids
    with Pre => (not a.isNull) and (not b.isNull),
         Post =>
            -- If they don't intersect, then should just return a
            -- otherwise, all results should not intersect with b
            (if GetIsNull(a and b)
             then "-"'Result'Length = 1 and "-"'Result(1) = a
             else (for all i of "-"'Result =>
                     GetIsNull(i and b))) and
            -- If they do intersect, then total volume of returned plus intersection should be the same as original
            (if not GetIsNull(a and b)
             then SumVolume("-"'Result) + Volume(a and b) = Volume(a))
    is
        inter: Cuboid := a and b;
    begin
        if inter.isNull then
            -- They don't intersect
            return (1 => a);
        else
            declare
                -- First slice along left-side of the intersection.
                -- Then slice the everything to the right ('Last) along
                -- the right-side of the intersection cube.
                -- Take the left-side of 2nd slice (which should intersect with inter)
                -- and continue slicing along other axes.
                --
                -- In the declare block ignore that slices may not produce
                -- anything to the left or right of inter: it takes 'First or 'Last,
                -- which work either way.
                --
                -- But figure out numResults (purely to avoid mucking around with vectors)
                -- by noticing that we only take results from slices that produced 2 cuboids:
                -- slices that produced 1 cuboid had that cuboid further sliced in the chain.
                --
                -- The result is then constructed as the opposite of the slice*Next used in the chain.

                -- Slice along the left face of inter
                sliceX: Cuboids := Slice(a, AxX, inter.rangeX.from);
                -- And take the rhs of the result to the next step.
                -- As (a and inter) is not empty, the rhs is never empty.
                sliceXNext: Cuboid := sliceX(sliceX'last);

                -- Taking everything to the right from the left face of inter,
                -- slice by the right face of inter.
                sliceX2: Cuboids := Slice(sliceXNext, AxX, inter.rangeX.to);
                -- This time, the left side of the slice is known to intersect
                -- with inter.
                sliceX2Next: Cuboid := sliceX2(1);

                -- Etc...
                sliceY: Cuboids := Slice(sliceX2Next, AxY, inter.rangeY.from);
                sliceYNext: Cuboid := sliceY(sliceY'Last);

                sliceY2: Cuboids := Slice(sliceYNext, AxY, inter.rangeY.to);
                sliceY2Next: Cuboid := sliceY2(1);

                sliceZ: Cuboids := Slice(sliceY2Next, AxZ, inter.rangeZ.from);
                sliceZNext: Cuboid := sliceZ(sliceZ'Last);

                sliceZ2: Cuboids := Slice(sliceZnext, AxZ, inter.rangeZ.to);
                sliceZ2Next: Cuboid := sliceZ2(1);

                -- Each slice either produces 2 cuboids (of which we need to return 1),
                -- or the "non-intersecting" side was null, so there was only 1 result.
                numResults: Natural :=
                    (Integer(SliceX'Length) - 1) + (Integer(SliceX2'Length) - 1) +
                    (Integer(SliceY'Length) - 1) + (Integer(SliceY2'Length) - 1) +
                    (Integer(sliceZ'Length) - 1) + (Integer(SliceZ2'Length) - 1);
                
                result: Cuboids(1..numResults);
                i: Positive := 1;
            begin
                -- If none of the slices had things to return, that means that
                -- inter = a (the b cuboid fully contains a), so return nothing.
                if NumResults = 0 then
                    Assert(inter = a);
                    return result;
                end if;

                -- Finally copy relevant results from slices.
                if sliceX'Length > 1 then
                    result(i) := sliceX(1); i := i + 1;
                end if;
                if sliceX2'Length > 1 then
                    result(i) := sliceX2(2); i := i + 1;
                end if;
                if sliceY'Length > 1 then
                    result(i) := sliceY(1); i := i + 1;
                end if;
                if sliceY2'Length > 1 then
                    result(i) := sliceY2(2); i := i + 1;
                end if;
                if sliceZ'Length > 1 then
                    result(i) := sliceZ(1); i := i + 1;
                end if;
                if sliceZ2'Length > 1 then
                    result(i) := sliceZ2(2); i := i + 1;
                end if;

                return result;
            end;
        end if;
    end "-";

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    subtype NonNullCuboid is Cuboid(false);

    package InputPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => NonNullCuboid);
    use InputPkg;
    subtype InputType is InputPkg.Vector;

    type Solution is tagged record
        Input : InputType := InputPkg.Empty_Vector;
    end record;

    procedure ReadInput(self : in out Solution'class; InData : File_Type)
    with
        Post => not self.Input.Is_Empty
    is
        use InputStrPkg;
    begin
        while not End_Of_File(InData) loop
            declare
                sw: SwitchState := SwitchState'Value(To_String(GetAtom(InData)));
                fromX, toX, fromY, toY, fromZ, toZ: ResultType;
            begin
                -- FIXME: Probably should add a helper to read such formats. Or use the GNAT awk-like thing?
                SkipString(InData, "x="); fromX := GetInt(InData); SkipString(InData, ".."); toX := GetInt(InData);
                SkipString(InData, ",y="); fromY := GetInt(InData); SkipString(InData, ".."); toY := GetInt(InData);
                SkipString(InData, ",z="); fromZ := GetInt(InData); SkipString(InData, ".."); toZ := GetInt(InData);
                AdvanceLine(InData);

                self.Input.Append(ToCuboid(fromX => fromX, toX => toX, fromY => fromY, toY => toY, fromZ => fromZ, toZ => toZ, sw => sw));
            end;

        end loop;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    subtype Actives is InputType;

    type ProblemPart is (PartA, PartB);

    function ToPart(c: Cuboid; part: ProblemPart) return Cuboid is
        -- PartA range
        validRange: constant Cuboid := (isNull => false, sw => Off,
                                        rangeX => (isNull => false, from => -50, to => 51),
                                        rangeY => (isNull => false, from => -50, to => 51),
                                        rangeZ => (isNull => false, from => -50, to => 51));
    begin
        return (if part = PartA then c and validRange else c);
    end ToPart;

    type SolutionA is new Solution with record
        null;
    end record;

    function ActuallySolve(s: Solution'Class; part: ProblemPart) return ResultType is
        cur: Actives;
    begin
        cur.Append(s.Input.First_element);

        -- For each input cuboid, cut it out (subtract) from all ones we are already tracking:
        -- those parts are unaffected.
        -- Then just add it with the new switch.
        -- XXX: dunno how to detect first element with "of" iterator, so just using index.
        for ei in 2..s.input.Last_Index loop
            declare
                e2: Cuboid := ToPart(s.Input(ei), part);
                cur2: Actives;
            begin
                if not e2.isNull then
                    for c of cur loop
                        declare
                            untouched: Cuboids := c - e2;
                        begin
                            for j of untouched loop
                                cur2.Append(j);
                            end loop;
                        end;
                    end loop;

                    -- Above should make sure that everything in cur does not intersect with e2
                    Assert(for all x of cur2 => GetIsNull(x and e2));
                    cur2.Append(e2);
                    cur := cur2;
                end if;
            end;
        end loop;

        declare
            result: ResultType := 0;
        begin
            for e of cur loop
                if e.sw = On then
                    result := result + Volume(e);
                end if;
            end loop;
            return result;
        end;
    end ActuallySolve;

    function Solve (SDisp : p22a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionA;
        
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;
        return ActuallySolve(s, PartA);
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p22b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionB;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;
        return ActuallySolve(s, PartB);
    end Solve;

end p22;
