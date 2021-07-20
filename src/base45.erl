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

alphabet() ->
	"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:".

encode_part(N) when N >= 0, N =< 45 ->
	lists:nth(N + 1, alphabet()).

decode_part(Char) ->
	decode_part(Char, alphabet()).

decode_part(Char, []) ->
	erlang:error({illegal_char, Char});

decode_part(Char, [Char | X]) ->
	44 - length(X);

decode_part(Char, [_ | X]) ->
	decode_part(Char, X).
