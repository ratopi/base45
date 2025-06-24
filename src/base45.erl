%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2021, Ralf Thomas Pietsch
%%% @doc base45 is module for encoding and decoding binary data into the (somewhat strange) base45 format.
%%%
%%% The base45 format is used in QR codes, e.g. for the EU Digital COVID Certificate.
%%% Implementing encoding as defined in RFC 9285 (https://datatracker.ietf.org/doc/rfc9285/).
%%%
%%% The base45 format encodes 16 bit values into 2 or 3 characters, depending on the value.
%%% The module provides functions to encode and decode binary data in this format.
%%%
%%% encode/1 encodes a binary value into base45.
%%%
%%% decode/1 decodes a base45 encoded binary value.
%%%
%%% The base45 format uses the following 45 characters:
%%%
%%% 0-9, A-Z, ' ' (space), $ (dollar), % (percent), * (asterisk), + (plus), - (minus), . (dot), / (slash), : (colon).
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

%%%@doc Encode a binary value into base45.
-spec encode(binary()) -> binary().
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


%%% @doc Decode a base45 encoded binary value.
%%% Will fail with an exception, if the input is not a valid base45 encoded binary value with
%%% <ul>
%%% <li>`{illegal_encoding, <<_, _, _>>}' if a tripple of chars result to a value above 65335, or</li>
%%% <li>`{illegal_character, <<Char>>}' if the binary contains an illegal character.</li>
%%% </ul>
%%% @end
-spec decode(binary()) -> binary().
decode(<<>>) ->
	<<>>;

decode(<<C>>) ->
	N = decode_part(C),
	<<N>>;

decode(<<C, D>>) ->
	N = decode_part(C) + 45 * decode_part(D),
	<<N>>;

decode(<<C, D, E, Rest/binary>>) ->
	N = decode_part(C) + 45 * (decode_part(D) + 45 * decode_part(E)),
	case N > 65535 of
		true ->
			erlang:error({illegal_encoding, <<C, D, E>>});
		false ->
			X = decode(Rest),
			<<N:16, X/binary>>
	end.

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
		[N] -> N;
		_ -> erlang:error({illegal_character, <<Char>>})
	end.

