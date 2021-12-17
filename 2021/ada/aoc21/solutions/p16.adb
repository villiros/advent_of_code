with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;
with Ada.Containers.Vectors;

with Harness; use Harness;
with utils; use utils;

package body p16 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(new p16a'(Name => "p16a"));
        D.Append(new p16b'(Name => "p16b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------
    use InputStrPkg;

    ------------------------------------
    -- Bit stream handling
    -- Not using Ada streams, as they seem to want to be on a per-byte basis.
    ------------------------------------
    package BitStreamPkg is 
        InputFormatError: exception;
        type Bit is mod 2;
        type Nibble is mod 2**4;

        type BitStream is tagged;

        function ToBitStream(s: InputStr) return BitStream;
        function CanRead(s: BitStream; numBits: Natural) return Boolean;

        function NumBits(s: InputStr) return Natural is
            (InputStrPkg.Length(s) * 4);

        type BitStream is tagged record
            input: InputStr;

            readCursor: Natural := 0;
        end record
        with Dynamic_Predicate =>
            BitStream.readCursor <= NumBits(BitStream.input);

        function Read(s: in out BitStream; numBits: Positive) return ResultType
        with Pre => CanRead (s, numBits),
            Post => s'Old.readCursor + numBits = s.readCursor;

        function ReadVarint(s: in out BitStream) return ResultType
        with Pre => CanRead (s, 5),
             Post => s'Old.readCursor + 5 <= s.readCursor;

    private

    end BitStreamPkg;

    package body BitStreamPkg is
        function ToNibble(c: Character) return Nibble is
        begin
            case c is
                when '0'..'9' => return Nibble(Character'Pos(c) - Character'Pos('0'));
                when 'A'..'F' => return Nibble(10 + Character'Pos(c) - Character'Pos('A'));
                when 'a'..'f' => return Nibble(10 + Character'Pos(c) - Character'Pos('a'));
                when others => raise InputFormatError;
            end case;
        end ToNibble;

        function BitMask(n: Natural) return Nibble
        with Pre => n < 4
        is begin
            return 2**(3 - n);
        end BitMask;

        function ToBitStream(s: InputStr) return BitStream is
        begin
            return (input => s, others => <>);
        end ToBitStream;

        function Length(s: BitStream) return Natural is
        begin
            return InputStrPkg.Length(s.input) * 4;
        end Length;

        function CanRead(s: BitStream; numBits: Natural) return Boolean is
        begin
            return s.readCursor + numBits <= Length(s);
        end CanRead;

        function CurNibble(s: BitStream) return Nibble is
        begin
            return ToNibble (Element(s.input, 1 + s.readCursor / 4));
        end CurNibble;

        function ReadBit(s: in out BitStream) return Bit
        with Pre => CanRead(s, 1),
            Post => s'Old.readCursor + 1 = s.readCursor
        is
            result: constant Bit := (if ((s.CurNibble and BitMask (s.readCursor mod 4))) > 0 then 1 else 0);
        begin
            s.readCursor := s.readCursor + 1;
            return result;
        end ReadBit;

        function Read(s: in out BitStream; numBits: Positive) return ResultType
        is
            result: ResultType := 0;
        begin
            -- FIXME: This *could* be optimized
            for i in 1..numBits loop
                result := result * 2 + ResultType(ReadBit (s));
            end loop;
            return result;
        end Read;

        function ReadVarint(s: in out BitStream) return ResultType
        is
            result: ResultType := 0;
        begin
            while s.CanRead (5) loop
                declare
                    shouldTerminate: Boolean := s.ReadBit = 0;
                begin
                    result := result * (2**4) + Read(s, 4);
                    exit when shouldTerminate;
                end;
            end loop;

            return result;
        end ReadVarint;
    end BitStreamPkg;
    subtype BitStream is BitStreamPkg.BitStream;
    use BitStreamPkg;

    ------------------------------------
    -- Data packets
    ------------------------------------

    type PktVersion is range 0..(2**3 - 1);

    -- Packet type handling
    type PktType is (Sum, Product, Minimum, Maximum, Literal, GreaterThan, LessThan, EqualTo);
    subtype ListOp is PktType with Static_Predicate => ListOp in Sum | Product | Minimum | Maximum;
    subtype BinOp is PktType with Static_Predicate => BinOp in GreaterThan | LessThan | EqualTo;

    type PktTypeID is range 0..(2**3 - 1);
    type PktTypeIDToType is array(PktTypeID) of PktType;
    ToPktType: constant PktTypeIDToType := (
        0 => Sum, 1 => Product, 2 => Minimum, 3 => Maximum, 4 => Literal, 5 => GreaterThan,
        6 => LessThan, 7 => EqualTo);

    type Packet;
    type PacketPtr is access Packet;

    -- List of packets (points to them, to make the compiler happy)
    package PacketListPkg is new
        Ada.Containers.Vectors(Index_Type => Positive, Element_Type => PacketPtr);
    subtype PacketListType is PacketListPkg.Vector;
    use PacketListPkg;
    use type PacketListType; use type Ada.Containers.Count_Type;

    type Packet(ptype: PktType) is record
        vsn: PktVersion;
        case ptype is
            when Literal =>
                val: ResultType;
            when ListOp =>
                vals: PacketListType;
            when BinOp =>
                left: not null PacketPtr;
                right: not null PacketPtr;
        end case;
    end record
    with Dynamic_Predicate =>
        -- These must be non-empty
        (if Packet.ptype in Sum | Product | Minimum | Maximum then not Packet.vals.Is_Empty);

    function ReadPacket(s: in out BitStream) return Packet;

    function ReadLenStream(s: in out BitStream; numBits: Positive) return PacketListType
    with Pre => s.CanRead(numBits),
         Post => (s'Old.readCursor + numBits = s.readCursor) and
                  not ReadLenStream'Result.Is_Empty
    is
        endAt: Positive := s.readCursor + numBits;
        result: PacketListType;
    begin
        loop
            exit when s.readCursor >= endAt;
            result.Append (new Packet'(ReadPacket(s)));
        end loop;
        return result;
    end ReadLenStream;

    function ReadCountStream(s: in out BitStream; count: Positive) return PacketListType
    is
        result: PacketListType;
    begin
        for i in 1..count loop
            result.Append (new Packet'(ReadPacket(s)));
        end loop;
        return result;
    end ReadCountStream;

    function ReadPacket(s: in out BitStream) return Packet is
        vsn: PktVersion := PktVersion(Read(s, 3));
        ptype: PktType := ToPktType(PktTypeID(Read(s, 3)));

        vals: PacketListType;

        procedure ReadVals is
        begin
            case Bit(s.Read(1)) is
                when 0 =>
                    vals := s.ReadLenStream(Integer(s.Read(15)));
                when 1 =>
                    vals := s.ReadCountStream (Integer(s.Read(11)));
            end case;
        end ReadVals;
    begin
        case ptype is
            when Literal =>
                return (ptype => Literal,
                        vsn => vsn,
                        val => s.ReadVarint);
            when ListOp =>
                ReadVals;
                return (ptype => ListOp(ptype),
                        vsn => vsn,
                        vals => vals);
            when BinOp =>
                ReadVals;
                Assert (Length(vals) = 2, "Invalid vals for binary op");
                return (ptype => BinOp(ptype),
                        vsn => vsn,
                        left => vals.First_Element,
                        right => vals.Last_Element);
        end case;
    end ReadPacket;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------
    use InputStrPkg;

    type Solution is tagged record
        Input : InputStr;
    end record;

    procedure ReadInput(self : in out Solution'class; InData : File_Type)
    with
        Post => Length(self.Input) > 0
    is
    begin
        self.Input := GetAtom (InData);
    end ReadInput;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    function SumVsn(p: Packet) return ResultType is
    begin
        case p.ptype is
            when Literal =>
                return ResultType(p.vsn);
            when ListOp =>
                declare
                    result: ResultType := ResultType(p.vsn);
                begin
                    for e of p.vals loop
                        result := result + ResultType(SumVsn (e.all));
                    end loop;
                    return result;
                end;
            when BinOp =>
                return ResultType(p.vsn) + ResultType(SumVsn (p.left.all)) + ResultType(SumVsn (p.right.all));
        end case;
    end SumVsn;


    type SolutionA is new Solution with record
        null;
    end record;

    function Solve (SDisp : p16a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        self : SolutionA;

    begin
        ReadInput(self, InData);
        StartTs := Ada.Real_Time.Clock;

        declare
            stream: BitStream := ToBitStream (self.Input);
            pkt: Packet := ReadPacket (stream);
        begin
            return SumVsn (pkt);
        end;
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    function Eval(p: Packet) return ResultType is
        type ListOptFun is access function(left, right: ResultType) return ResultType;

        function ListRunner(op: ListOptFun; starting: ResultType) return ResultType is
            result: ResultType := starting;
        begin
            for e of p.vals loop
                result := op(result, eval(e.all));
            end loop;
            return result;
        end ListRunner;

        function Sum(left, right: ResultType) return ResultType is (left + right);
        function Product(left, right: ResultType) return ResultType is (left * right);
        function Min(left, right: ResultType) return ResultType is (if left < right then left else right);
        function Max(left, right: ResultType) return ResultType is (if left > right then left else right);

    begin
        case p.ptype is
            when Sum =>
                return ListRunner(Sum'Access, 0);
            when Product =>
                return ListRunner (Product'Access, 1);
            when Minimum =>
                return ListRunner (Min'Access, ResultType'Last);
            when Maximum =>
                return ListRunner (Max'Access, 0);
            when GreaterThan =>
                return (if eval(p.left.all) > eval(p.right.all) then 1 else 0);
            when LessThan =>
                return (if eval(p.left.all) < eval(p.right.all) then 1 else 0);
            when EqualTo =>
                return (if eval(p.left.all) = eval(p.right.all) then 1 else 0);
            when Literal =>
                return p.val;
        end case;
    end Eval;

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p16b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        self : SolutionB;
    begin
        ReadInput(self, InData);
        StartTs := Ada.Real_Time.Clock;

        declare
            stream: BitStream := ToBitStream (self.Input);
            pkt: Packet := ReadPacket (stream);
        begin
            return eval(pkt);
        end;
    end Solve;

end p16;
