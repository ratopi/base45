%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2021, Ralf Thomas Pietsch
%%% @doc A Base45 encoder/decoder for binary data.
%%%
%%% Implements the Base45 encoding scheme as defined in
%%% <a href="https://datatracker.ietf.org/doc/rfc9285/">RFC 9285</a>.
%%%
%%% == Examples ==
%%%
%%% Encoding:
%%% ```
%%% > base45:encode(<<"Hello!!">>).
%%% <<"%69 VD92EX0">>
%%%
%%% > base45:encode(<<"AB">>).
%%% <<"BB8">>
%%%
%%% > base45:encode(<<100>>).
%%% <<"A2">>
%%% '''
%%%
%%% Decoding:
%%% ```
%%% > base45:decode(<<"%69 VD92EX0">>).
%%% <<"Hello!!">>
%%%
%%% > base45:decode(<<"BB8">>).
%%% <<"AB">>
%%%
%%% > base45:decode(<<"A2">>).
%%% <<100>>
%%% '''
%%%
%%% Invalid input raises an error:
%%% ```
%%% > base45:decode(<<"GGW">>).
%%% ** exception error: {illegal_encoding,<<"GGW">>}
%%%
%%% > base45:decode(<<"::">>).
%%% ** exception error: {illegal_encoding,<<"::">>}
%%%
%%% > base45:decode(<<"A">>).
%%% ** exception error: {illegal_encoding,<<"A">>}
%%%
%%% > base45:decode(<<"===">>).
%%% ** exception error: {illegal_character,<<"=">>}
%%% '''
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

%%% @doc Encode a binary value into Base45.
%%%
%%% Two input bytes are encoded into 3 characters, a single remaining byte
%%% into 2 characters. An empty binary returns an empty binary.
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


%%% @doc Decode a Base45 encoded binary value.
%%%
%%% The input length must be divisible by 3, or have a remainder of 2
%%% (for a trailing single-byte encoding).
%%%
%%% Raises an `error' exception if the input is not valid Base45:
%%% <ul>
%%% <li>`{illegal_encoding, <<_>>}' if a single character remains (invalid length)</li>
%%% <li>`{illegal_encoding, <<_, _>>}' if a pair of characters results in a value above 255</li>
%%% <li>`{illegal_encoding, <<_, _, _>>}' if a triplet of characters results in a value above 65535</li>
%%% <li>`{illegal_character, <<Char>>}' if the input contains a character outside the Base45 alphabet</li>
%%% </ul>
%%% @end
-spec decode(binary()) -> binary().
decode(<<>>) ->
	<<>>;

decode(<<C>>) ->
	erlang:error({illegal_encoding, <<C>>});

decode(<<C, D>>) ->
	N = decode_part(C) + 45 * decode_part(D),
	case N > 255 of
		true ->
			erlang:error({illegal_encoding, <<C, D>>});
		false ->
			<<N>>
	end;

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

