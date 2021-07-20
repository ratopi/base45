%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2021, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2021 22:11
%%%-------------------------------------------------------------------
-module(base45).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

%% API
-export([encode/1, decode/1]).

%%%===================================================================
%%% API
%%%===================================================================

encode(<<>>) ->
	<<>>;

encode(<<N>>) ->
	C = encode_part(N rem 45),
	D = encode_part((N div 45) rem 45),
	<<C, D>>;

encode(<<N:16, Rest/binary>>) ->
	C = encode_part(N rem 45),
	D = encode_part((N div 45) rem 45),
	E = encode_part((N div 45 div 45)),
	X = encode(Rest),
	<<C, D, E, X/binary>>.


decode(<<>>) ->
	<<>>;

decode(<<C, D>>) ->
	N = decode_part(C) + decode_part(D) * 45,
	<<N>>;

decode(<<C, D, E, Rest/binary>>) ->
	N = decode_part(C) + decode_part(D) * 45 + decode_part(E) * 45 * 45,
	X = decode(Rest),
	<<N:16, X/binary>>.

%%%===================================================================
%%% Internal functions
%%%===================================================================

special_chars() ->
	" $%*+-./:".

decode_alphabet() ->
	[
		{36, $ },
		{37, $$},
		{38, $%},
		{39, $*},
		{40, $+},
		{41, $-},
		{42, $.},
		{43, $/},
		{44, $:}
	].


encode_part(N) when N >= 0, N =< 9 ->
	N + $0;

encode_part(N) when N >= 10, N =< 35 ->
	N - 10 + $A;

encode_part(N) when N >= 36, N =< 45 ->
	lists:nth(N - 35, special_chars()).


decode_part(Char) when Char >= $0, Char =< $9 ->
	Char - $0;

decode_part(Char) when Char >= $A, Char =< $Z ->
	Char - $A + 10;

decode_part(Char) ->
	case
		lists:filtermap(
			fun({N, C}) ->
				case C == Char of
					true -> {true, N};
					false -> false
				end
			end,
			decode_alphabet()
		)
	of
		[N] -> N
	end.
