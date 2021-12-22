with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Bounded;
with Ada.Assertions;
with Ada.Real_Time;

with utils; use utils;

package body p03 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(p03a'(Name => "p03a"));
        D.Append(p03b'(Name => "p03b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------
    ------------------------------------
    -- Input data definitions
    ------------------------------------
    type EntryType is mod 2**(ResultType'Size - 1);
    subtype BitNumberType is Integer range 0..(EntryType'Size - 1);

    subtype GammaRateType is EntryType;
    subtype EpsRateType is EntryType;
    type BitType is mod 2;

    ------------------------------------
    -- Bit fiddling
    ------------------------------------

    -- Returns a bitmask with NumBits lowest set to 1
    function Mask(NumBits : BitNumberType) return EntryType is
    begin
        return 2**(NumBits) - 1;
    end Mask;

    -- Left shift by count bits
    function LShift(E : EntryType; Count : BitNumberType) return EntryType is
    begin
        return E * (2 ** Count);
    end LShift;

    -- Right shift by count bits
    function RShift(E : EntryType; Count : BitNumberType) return EntryType
    with
        Post => LShift(RShift'Result, Count) = (E and (not Mask(Count)))
    is
    begin
        return E / (2**Count);
    end RShift;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    package InputPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => EntryType);
    use InputPkg; use type Ada.Containers.Count_Type;
    subtype InputType is InputPkg.Vector;

    type Solution is tagged record
        Input : InputType := InputPkg.Empty_Vector;
        EntryBits : BitNumberType := 0;
    end record
    with
        Dynamic_Predicate =>
            (if Solution.EntryBits = 0
                then
                    Input = InputPkg.Empty_Vector
                else
                    Input.Length > 0 and
                    (for all I of Solution.Input =>
                        (Mask(Solution.EntryBits) >= I)) and
                    (for some I of Solution.Input =>
                        ((I and LShift(1, Solution.EntryBits - 1)) > 0)));

    procedure ReadInput(self : in out Solution'class; InData : File_Type)
    with
        Post => self.Input.Length > 0
    is
    begin
        while not End_Of_File(InData) loop
            declare
                EStr : String := InputStrPkg.To_String(GetAtom(InData));
                E : EntryType := EntryType'Value("2#" & EStr & "#");
                NBits : BitNumberType := BitNumberType(EStr'Length);
            begin
                if self.Input.Length = 0 then
                    self.EntryBits := NBits;
                else
                    Assert(self.EntryBits = NBits);
                end if;
                self.Input.Append(E);
            end;
            AdvanceLine (InData);
        end loop;
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    -- Count number of 1 bits at position bitn in Entries
    function CountBits(self : Solution'Class; Entries : InputType; bitn : BitNumberType) return ResultType
    with
        Pre => (bitn < self.EntryBits) -- bitn is 0-indexed
    is
        result : ResultType := 0;
    begin
        for I of Entries loop
            result := result + ResultType(RShift(I, bitn) and 1);
        end loop;
        return result;
    end CountBits;

    type SolutionA is new Solution with record
        Gamma : GammaRateType := 0;
        Eps : EpsRateType := 0;
    end record;

    function Solve (SDisp : p03a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.time) return ResultType is
        s : SolutionA;
        Gamma : GammaRateType renames s.Gamma;
        Eps : EpsRateType renames s.Eps;
    begin
        s.ReadInput(InData);
        StartTs := Ada.Real_Time.Clock;

        for bitn in 0..(s.EntryBits-1) loop
            if s.CountBits (s.Input, bitn) > ResultType(s.Input.Length) / 2 then
                Gamma := Gamma or GammaRateType(2**bitn);
            end if;
        end loop;

        Eps := EpsRateType(Gamma) xor Mask (s.EntryBits);

        return ResultType(Gamma) * ResultType(Eps);
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    -- Remove entries from CurInput where bitn is not IfNotMatches
    function RemoveAtBit(CurInput : InputType; bitn : BitNumberType; IfNotMatches : BitType) return InputType
    with
        Post => -- Only matching remain
                (for all I of RemoveAtBit'Result =>
                    BitType(RShift(I and (2**bitn), bitn)) = IfNotMatches) and 
                -- Non-matching are not present
                (for all I of CurInput =>
                    (if not (BitType(RShift(I and (2**bitn), bitn)) = IfNotMatches) then
                        not Contains(RemoveAtBit'Result, I)))
    is
        result : InputType;
    begin
        for I of CurInput loop
            if BitType(RShift(I, bitn) and 1) = IfNotMatches then
                result.Append(I);
            end if;
        end loop;
        return result;
    end RemoveAtBit;

    type SolutionB is new Solution with record
        null;
    end record;

    -- Part 2 value extraction process with generic bit criteria
    generic
        with function BitCriteria(MostCommon : BitType; IsEqual : Boolean) return BitType is <>;
    function RunProcess(self: Solution'Class) return EntryType;

    function RunProcess(self: Solution'Class) return EntryType is
        CurInput : InputType := self.Input;
    begin
        for bitn in reverse 0..(self.EntryBits-1) loop
            exit when CurInput.Length = 1;
            Assert(CurInput.Length >= 1);
            
            declare
                Num1 : ResultType := self.CountBits(CurInput, bitn);
                KeepBit : BitType := BitCriteria ((if Num1 > ResultType(CurInput.Length) / 2 then 1 else 0),
                                                  Num1 = ResultType(CurInput.Length) - Num1);
            begin
                CurInput := RemoveAtBit (CurInput, bitn, KeepBit);
            end;
        end loop;

        return CurInput.First_Element;
    end RunProcess;

    function Solve (SDisp : p03b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.time) return ResultType is
        s : SolutionB;

        function OxCriteria(MostCommon : BitType; IsEqual : Boolean) return BitType is
        begin
            if IsEqual then return 1;
            else return MostCommon;
            end if;
        end OxCriteria;

        function O2Criteria(MostCommon : BitType; IsEqual : Boolean) return BitType is
        begin
            if IsEqual then return 0;
            else return not MostCommon;
            end if;
        end O2Criteria;

        function OxProcess is new RunProcess(BitCriteria => OxCriteria);
        function O2Process is new RunProcess(BitCriteria => O2Criteria);
    begin
        s.ReadInput(InData);
        StartTs := Ada.Real_Time.Clock;
        
        return ResultType(OxProcess(s) * O2Process(s));
    end Solve;

end p03;
