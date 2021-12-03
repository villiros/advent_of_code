with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Bounded;

with utils; use utils;

package body p03 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(new p03a'(Name => "p03a"));
        D.Append(new p03b'(Name => "p03b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    ------------------------------------
    -- Input data definitions
    ------------------------------------
    subtype BitNumberType is ResultType range 0..63;
    package EntryPkg is new Ada.Strings.Bounded.Generic_Bounded_Length(Max => 64); -- VLAD: ???
    use EntryPkg;
    subtype EntryType is EntryPkg.Bounded_String;

    subtype GammaRateType is ResultType range 0..ResultType'Last;
    subtype EpsRateType is ResultType range 0..ResultType'Last;
    subtype BitType is ResultType range 0..1;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    package InputPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => EntryType);
    use InputPkg;
    subtype InputType is InputPkg.Vector;

    type Solution is tagged record
        Input : InputType := InputPkg.Empty_Vector;
    end record
    with
        Dynamic_Predicate => (for all I of Solution.Input=> EntryPkg.Length(I) = EntryPkg.Length(Solution.Input.First_Element));

    -- function GetBitN(self : Solution; pos : InputPkg.Extended_Index; bitn : BitNumberType) return BitType
    -- with Pre => (pos >= self.Input.First_Index) and
    --             (pos <= self.Input.Last_Index) and
    --             (bitn <= InputStrPkg.Length(self.Input.First_Element))
    -- is
    -- begin
    --     return 0;
    -- end GetBitN;

    procedure ReadInput(InData : File_Type; S : in out Solution'class) is
    begin
        while not End_Of_File(InData) loop
            s.Input.Append (EntryPkg.To_Bounded_String(InputStrPkg.To_String(GetAtom(InData))));
            AdvanceLine (InData);
        end loop;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    function CountBits(Input : InputType; bitn : BitNumberType; target : Character) return ResultType
    with
        Pre => (bitn <= BitNumberType(EntryPkg.Length(Input.First_Element))) and
               (target = '0' or target = '1')
    is
        result : ResultType := 0;
    begin
        for I of Input loop
            if Element(I, 1 + Natural(bitn)) = target then
                result := result + 1;
            end if;
        end loop;
        return ResultType(Length(Input)) - result;
    end CountBits;

    type SolutionA is new Solution with record
        inputBitLen : BitNumberType := 0;

        Gamma : GammaRateType := 0;
        Eps : EpsRateType := 0;
    end record;

    function Solve (SDisp : p03a;
                    InData : in Ada.Text_IO.File_type) return ResultType is
        s : SolutionA;
        Input : InputType renames s.Input;
        Gamma : GammaRateType renames s.Gamma;
        Eps : EpsRateType renames s.Eps;
    begin
        ReadInput(InData, s);

        for bitn in 0..(Length(Input.First_Element) - 1) loop
            Gamma := Gamma * 2;
            Eps := Eps * 2;
            if CountBits (Input, BitNumberType(bitn), '0') > ResultType(Input.Length) / 2 then
                Gamma := Gamma + 1;
            else
                Eps := Eps + 1;
            end if;
        end loop;

        return Gamma * Eps;
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    procedure RemoveBits(Input : in out InputType; bitn : BitNumberType; matching : Character)
    with
        Pre => (bitn <= BitNumberType(EntryPkg.Length(Input.First_Element))) and
               (matching = '0' or matching = '1')
    is
        result : InputType;
    begin
        for I of Input loop
            if Element(I, 1 + Natural(bitn)) /= matching then
                result.Append (I);
            end if;
        end loop;

        Input := result;
    end RemoveBits;

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p03b;
                    InData : in Ada.Text_IO.File_type) return ResultType is
        use type InputType;
        s : SolutionB;
        Input : InputType renames s.Input;

        oxresult : ResultType := 0;
        co2result : ResultType := 0;
    begin
        ReadInput(InData, s);

        declare
            ox : InputType := Input;
        begin
            for bitn in 0..(Length(ox.First_Element) - 1) loop
                exit when Integer(ox.Length) = 1;
                declare
                    num0 : ResultType := CountBits (ox, BitNumberType(bitn), '1');
                    num1 : ResultType := CountBits (ox, BitNumberType(bitn), '0');
                begin
                    if num0 = num1 then
                        RemoveBits(ox, BitNumberType(bitn), '0');
                    elsif num0 > num1 then
                        RemoveBits(ox, BitNumberType(bitn), '1');
                    else
                        RemoveBits(ox, BitNumberType(bitn), '0');
                    end if;
                end;
            end loop;

            for i in 1..(EntryPkg.Length(ox.First_Element)) loop
                oxresult := (oxresult * 2) + (if Element(ox.First_Element, i) = '1' then 1 else 0);
            end loop;

            if PrintDebug then Put_Line ("xx" & ox.First_Element'image & ASCII.LF); end if;
        end;

        declare
            co2 : InputType := Input;
        begin
            for bitn in 0..(Length(co2.First_Element) - 1) loop
                exit when Integer(co2.Length) = 1;
                declare
                    num0 : ResultType := CountBits (co2, BitNumberType(bitn), '1');
                    num1 : ResultType := CountBits (co2, BitNumberType(bitn), '0');
                begin
                    if num0 = num1 then
                        RemoveBits(co2, BitNumberType(bitn), '1');
                    elsif num0 > num1 then
                        RemoveBits(co2, BitNumberType(bitn), '0');
                    else
                        RemoveBits(co2, BitNumberType(bitn), '1');
                    end if;
                end;
            end loop;

            for i in 1..(EntryPkg.Length(co2.First_Element)) loop
                co2result := (co2result * 2) + (if Element(co2.First_Element, i) = '1' then 1 else 0);
            end loop;

            if PrintDebug then Put_Line ("xx" & co2.First_Element'image & ASCII.LF); end if;
        end;

        return oxresult * co2result;
    end Solve;

end p03;
