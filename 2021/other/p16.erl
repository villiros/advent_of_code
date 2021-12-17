-module(p16).
-export([run/0, test/0]).
-compile([export_all]).

run() ->
    io:format("Test 1 expect 16: ~p~n", [add_vsn("8A004A801A8002F478")]),
    io:format("Test 2 expect 12: ~p~n", [add_vsn("620080001611562C8802118E34")]),
    io:format("Test 2 expect 23: ~p~n", [add_vsn("C0015000016115A2E0802F182340")]),
    io:format("Test 2 expect 31: ~p~n", [add_vsn("A0016C880162017C3686B18A3D4780")]),

    io:format("~n", []),

    Input =  "420D74C3088043390499ED709E6EB49A5CC4A3A3898B7E0F44011C4CC"
             "48AC0119D049B0C500265EB8F615900180910C88129B2F0007C61C4B7F"
             "74ED7396B20020A44A4C014D005E5A72E274B4E5C4B96CC3793410078C"
             "01D82F1DA08180351661AC1920042A3CC578BA6008F802138D93352B9C"
             "FCEF61D3009A7D2268D254925569C02A92D62BF108D52C1B3E4B257B57"
             "FAE5C54400A84840267880311D23245F1007A35C79848200C4288FF0E8"
             "C01194A4E625E00A4EFEF5F5996486C400C5002800BFA402D3D00A9C4027B98093D602231C00F001D38C009500258057E601324"
             "C00D3003D400C7003DC00A20053A6F1DBDE2D4600A6802B37C4B9E872B0E44CA5FF0BFB116C3004740119895E6F7312BCDE25E"
             "F077700725B9F2B8F131F333005740169A7F92EFEB3BC8A21998027400D2CDF30F927880B4C62D6CDFFD88EB0068D2BF019A8DA"
             "AF3245B39C9CFA1D2DF9C3DB9D3E50A0164BE2A3339436993894EC41A0D10020B329334C62016C8E7A5F27C97D0663982D8EB23"
             "C5282529CDD271E8F100AE1401AA80021119E3A4511006E1E47689323585F3AEBF900AEB2B6942BD91EE8028000874238AB0C00"
             "010B8D913220A004A73D789C4D54E24816301802538E940198880371AE15C1D1007638C43856C00954C25CD595A471FE9D90056"
             "D60094CEA61933A9854E9F3801F2BBC6131001F792F6796ACB40D036605C80348C005F64F5AC374888CA42FD99A98025319EB95"
             "0025713656F202200B767AB6A30E802D278F81CBA89004CD286360094FC03A7E01640245CED5A3C010100660FC578B60008641C"
             "8B105CC017F004E597E596E633BA5AB78B9C8F840C029917C9E389B439179927A3004F003511006610C658A200084C2989D0AE6"
             "7BD07000606154B70E66DC0C01E99649545950B8AB34C8401A5CDA050043D319F31CB7EBCEE14",

    io:format("Part A (expect 895): ~p~n", [add_vsn(Input)]),
    io:format("Part B (expect 1148595959144): ~p~n", [eval1(get_one(Input))]),

    ok.

hex_to_bits(Str) ->
    Int = list_to_integer(Str, 16),
    NBits = length(Str) * 4,
    <<(Int):(NBits)/unsigned-big>>.

% Take a bitstring containing a single packet and return {Packet, Rest :: bitstring()}
% Can take binstring or parsed input
get_one(T = [_|_]) ->
    get_one(hex_to_bits(T));

get_one(<<Vsn:3, 4:3, Rest/bitstring>>) ->
    {Val, R2} = literal(Rest, 0),
    {{literal, Vsn, Val}, R2};
get_one(<<Vsn:3, Op:3, Rest/bitstring>>) ->
    {Val, R2} = packet(Rest),
    {{packet, Vsn, Op, Val}, R2}.

literal(<<0:1, Last:4, Rest/bitstring>>, Acc) ->
    {Acc * 16 + Last, Rest};
literal(<<1:1, Last:4, Rest/bitstring>>, Acc) ->
    literal(Rest, Acc * 16 + Last).

packet(<<0:1, BitLen:15, Data:BitLen/bitstring, Rest/bitstring>>) ->
    {Res, <<>>} = packet_bitstream(Data, []),
    {Res, Rest};
packet(<<1:1, NumPackets:11, Rest/bitstring>>) ->
    packet_1(Rest, NumPackets, []).

packet_bitstream(Bin, Acc) when bit_size(Bin) < 8 ->
    {Acc, <<>>};
packet_bitstream(Bin, Acc) ->
    {Val, Rest} = get_one(Bin),
    packet_bitstream(Rest, Acc ++ [Val]).

packet_1(BinRest, 0, Acc) ->
    {Acc, BinRest};
packet_1(Bin, N, Acc) ->
    {V, BinRest} = get_one(Bin),
    packet_1(BinRest, N - 1, Acc ++ [V]).

% Part A: add version numbers
% Can take the input hex string
add_vsn([], Acc) ->
    Acc;
add_vsn([{literal, V, _} | R], Acc) ->
    add_vsn(R, Acc + V);
add_vsn([{packet, V, _, PR} | R], Acc) ->
    add_vsn(R, Acc + V + add_vsn(PR, 0)).
% Input string
add_vsn(Input) ->
    {Pkt, _} = get_one(hex_to_bits(Input)),
    add_vsn([Pkt], 0).

%
% Part B: ealuate a single packet to a value.

% Can take output of get_one here
eval1({Packet, _ = <<_/bitstring>>}) ->
    eval1(Packet);

eval1({packet, _, 0, D}) ->
    lists:sum(eval_list(D));
eval1({packet, _, 1, D}) ->
    lists:foldl(fun erlang:'*'/2, 1, eval_list(D));
eval1({packet, _, 2, D}) ->
    lists:min(eval_list(D));
eval1({packet, _, 3, D}) ->
    lists:max(eval_list(D));
eval1({packet, _, CompOp, [P1, P2]}) when CompOp == 5; CompOp == 6; CompOp == 7 ->
    V1 = eval1(P1),
    V2 = eval1(P2),

    if CompOp == 5, V1 > V2 -> 1;
       CompOp == 6, V1 < V2 -> 1;
       CompOp == 7, V1 == V2 -> 1;
       true -> 0
    end;
eval1({literal, _, X}) ->
    X.

eval_list(D) ->
    [eval1(X) || X <- D].

%
%
%

test() ->
    io:format("Literal 2021: ~p~n", [get_one(hex_to_bits("D2FE28"))]),
    io:format("Packet case 1: ~p~n", [get_one(hex_to_bits("38006F45291200"))]),
    io:format("Packet case 2: ~p~n", [get_one(hex_to_bits("EE00D40C823060"))]),

    io:format("B case 1 expect 3: ~p~n", [eval1(get_one("C200B40A82"))]),
    io:format("B case 2 expect 54: ~p~n", [eval1(get_one("04005AC33890"))]),
    io:format("B case 3 expect 7: ~p~n", [eval1(get_one("880086C3E88112"))]),
    io:format("B case 4 expect 9: ~p~n", [eval1(get_one("CE00C43D881120"))]),
    io:format("B case 5 expect 0: ~p~n", [eval1(get_one("F600BC2D8F"))]),
    io:format("B case 6 expect 0: ~p~n", [eval1(get_one("9C005AC2F8F0"))]),
    io:format("B case 7 expect 1: ~p~n", [eval1(get_one("9C0141080250320F1802104A08"))]),
    ok.




