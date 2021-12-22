with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Ada.Real_Time;
with Ada.Containers.Hashed_Maps;

with Harness; use Harness;
with utils; use utils;

package body p21 is
    ------------------------------------------------------------------------
    -- Dispatching stuff
    ------------------------------------------------------------------------
    procedure GetDispatchers(D : in out Dispatchers) is
    begin
        D.Append(new p21a'(Name => "p21a"));
        D.Append(new p21b'(Name => "p21b"));
    end GetDispatchers;
    
    ------------------------------------------------------------------------
    -- Common stuff
    ------------------------------------------------------------------------

    ShouldNotGetHere: exception;

    -- 0-based, unlike the problem description
    type BoardPos is mod 10;

    type DetDice is record
        numRolls: Natural := 0;
        nextRoll: Natural := 1;
    end record;

    function Roll(d: in out DetDice) return Natural is
        result: Natural := d.nextRoll;
    begin
        d.numRolls := d.numRolls + 1;
        if d.nextRoll = 100 then
            d.nextRoll := 1;
        else
            d.nextRoll := d.nextRoll + 1;
        end if;
        return result;
    end Roll;

    type Player is record
        score: Natural := 0;
        position: BoardPos;
    end record;

    ------------------------------------
    -- Base solution and data reading
    ------------------------------------

    type Solution is tagged record
        player1, player2: Player;
    end record;

    procedure ReadInput(self : in out Solution'class; InData : File_Type)
    is
    begin
        SkipString(InData, "Player 1 starting position:");
        self.player1.position := BoardPos(GetInt(InData) - 1);
        AdvanceLine(InData);

        SkipString(InData, "Player 2 starting position:");
        self.player2.position := BoardPos(GetInt(InData) - 1);
    end ReadInput;

    function PlayTurn(p: in out Player; dice: in out DetDice) return Boolean
    with Pre => p.score < 1000,
         Post => (if p.score >= 1000 then PlayTurn'Result)
    is
        step: Natural := 0;
    begin
        for i in 1..3 loop
            step := step + Roll(dice);
        end loop;

        p.position := p.position + BoardPos(step rem 10);
        p.score := p.score + Natural(p.position) + 1;

        if PrintDebug then Put_line("Player rolled " & step'Image & " pos " & p.position'Image & " score " & p.score'Image); end if;

        return p.score >= 1000;

    end PlayTurn;

    ------------------------------------------------------------------------
    -- Part A
    ------------------------------------------------------------------------

    type SolutionA is new Solution with record
        null;
    end record;

    function Solve (SDisp : p21a;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionA;
        dice: DetDice;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        loop
            if PlayTurn(s.player1, dice) then
                return ResultType(s.player2.score * dice.numRolls);
            end if;
            if PlayTurn(s.player2, dice) then
                return ResultType(s.player2.score * dice.numRolls);
            end if;
        end loop;
    end Solve;

    ------------------------------------------------------------------------
    -- Part B
    ------------------------------------------------------------------------

    -- Possible sums of rolling the dice 3 times. This contains duplicates: each one is a separate universe
    type QDiceRollsType is array(Positive range <>) of Positive;
    function GenQDiceRolls return QDiceRollsType is
        result: QDiceRollsType(1..3*3*3);
        resultI: Positive := 1;
    begin
        for a in 1..3 loop
            for b in 1..3 loop
                for c in 1..3 loop
                    result(resultI) := a + b +c;
                    resultI := resultI + 1;
                end loop;
            end loop;
        end loop;
        return result;
    end GenQDiceRolls;
    qDiceRolls: constant QDiceRollsType := GenQDiceRolls;

    type WinCount is record
        player1, player2: ResultType := 0;
    end record;

    function "+"(a, b: WinCount) return WinCount is
    begin
        return (a.player1 + b.player1, a.player2 + b.player2);
    end "+";

    type Universe is record
        player1, player2: Player;
        nextIs2: Boolean;
    end record
    with Dynamic_Predicate => player1.score <= 21 and player2.score <= 21;

    function UniverseHash(u: Universe) return Ada.Containers.Hash_Type is
        subtype H is Ada.Containers.Hash_Type;
        function Shift_Left (Value  : H; Amount : Natural) return H;
        pragma Import(Intrinsic, Shift_Left);

        use type H;
        
        result: H := 0;
    begin
        result := H(u.player1.position);
        result := Shift_Left (result, 4) xor H(u.player2.position);
        result := Shift_Left (result, 4) xor H(u.player1.score);
        result := Shift_Left (result, 5) xor H(u.player2.score);
        result := Shift_Left (result, 1) xor (if u.nextIs2 then 1 else 0);
        return result;
    end UniverseHash;

    package UniverseMapPkg is new
        Ada.Containers.Hashed_Maps(Key_Type => Universe,
                                   Element_Type => WinCount,
                                   hash => UniverseHash,
                                   Equivalent_Keys => "=");
    use UniverseMapPkg;
    subtype UniverseMap is UniverseMapPkg.Map;

    function PlayTurnB(p: in out Player; step: Positive) return Boolean
    with Pre => p.score < 22,
         Post => (if p.score >= 21 then PlayTurnB'Result)
    is
    begin
        p.position := p.position + BoardPos(step);
        p.score := p.score + Natural(p.position) + 1;

        if PrintDebug then Put_line("Player rolled " & step'Image & " pos " & p.position'Image & " score " & p.score'Image); end if;

        return p.score >= 21;
    end PlayTurnB;


    function PlayB(map: in out UniverseMap; u: Universe) return WinCount is
        result: WinCount;
    begin
        if map.Contains (u) then
            return map.Element (u);
        end if;

        for roll of qDiceRolls loop
            if u.nextIs2 then
                declare
                    p: Player := u.player2;
                begin
                    if PlayTurnB (p, roll) then
                        result.player2 := result.player2 + 1;
                    else
                        result := result + PlayB(map, (player1 => u.player1,
                                                       player2 => p,
                                                       nextIs2 => False));
                    end if;
                end;
            else
                declare
                    p: Player := u.player1;
                begin
                    if PlayTurnB (p, roll) then
                        result.player1 := result.player1 + 1;
                    else
                        result := result + PlayB (map, (player1 => p,
                                                        player2 => u.player2,
                                                        nextIs2 => True));
                    end if;
                end;
            end if;
        end loop;

        map.Insert (u, result);
        return result;
    end PlayB;

    type SolutionB is new Solution with record
        null;
    end record;

    function Solve (SDisp : p21b;
                    InData : in Ada.Text_IO.File_type;
                    StartTs : out Ada.Real_Time.Time) return ResultType is
        s : SolutionB;
        map: UniverseMap;
    begin
        ReadInput(s, InData);
        StartTs := Ada.Real_Time.Clock;

        declare
            result: WinCount := PlayB(map, (player1 => s.player1, player2 => s.player2, nextIs2 => False));
        begin
            if PrintDebug then Put_Line ("Player 1 wins " & result.player1'Image & " and 2 wins " & result.player2'Image); end if;

            return (if result.player1 > result.player2 then result.player1 else result.player2);
        end;
    end Solve;

end p21;
