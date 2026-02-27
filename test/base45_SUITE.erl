%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2021, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2021 22:08
%%%-------------------------------------------------------------------
-module(base45_SUITE).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

-include_lib("common_test/include/ct.hrl").

%% API
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2, all/0]).
-export([encode/1, decode/1, decode_fail/1, decode_fail_illegal_character/1]).
-export([encode_boundary/1, decode_boundary/1]).
-export([decode_fail_single_char/1, decode_fail_pair_overflow/1]).

all() -> [
	encode,
	decode,
	decode_fail,
	decode_fail_illegal_character,
	encode_boundary,
	decode_boundary,
	decode_fail_single_char,
	decode_fail_pair_overflow
].

%%%===================================================================
%%% Common Tests API
%%%===================================================================

init_per_suite(Config) ->
	Config.


end_per_suite(_Config) ->
	ok.

init_per_testcase(_, Config) ->
	Config.


end_per_testcase(_, _Config) ->
	ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

encode(_Config) ->
	lists:foreach(fun encode_test/1, in_out()).

decode(_Config) ->
	lists:foreach(fun decode_test/1, in_out()).

decode_fail(_Config) ->
	lists:foreach(decode_fail_test(illegal_encoding), illegal_values()).

decode_fail_illegal_character(_Config) ->
	lists:foreach(decode_fail_test(illegal_character), illegal_characters()).

%% Test encoding of boundary values
encode_boundary(_Config) ->
	lists:foreach(fun encode_test/1, boundary_in_out()).

%% Test decoding of boundary values
decode_boundary(_Config) ->
	lists:foreach(fun decode_test/1, boundary_in_out()).

%% A single character is not a valid base45 encoding (length mod 3 == 1)
decode_fail_single_char(_Config) ->
	lists:foreach(
		fun(In) ->
			try base45:decode(In) of
				_ ->
					io:fwrite(standard_error, "decode of ~p should not succeed~n", [In]),
					erlang:error(decode_fail)
			catch
				error:_ -> ok
			end
		end,
		single_char_inputs()
	).

%% A pair of characters that decodes to a value > 255 is not valid
decode_fail_pair_overflow(_Config) ->
	lists:foreach(
		fun({In, FailingPart}) ->
			try base45:decode(In) of
				_ ->
					io:fwrite(standard_error, "decode of ~p should not succeed~n", [In]),
					erlang:error(decode_fail)
			catch
				error:{illegal_encoding, FailingPart} -> ok
			end
		end,
		pair_overflow_values()
	).

%%%===================================================================
%%% Internal functions
%%%===================================================================

encode_test({In, Out}) ->
	Out = base45:encode(In).

decode_test({Out, In}) ->
	Out = base45:decode(In).


in_out() ->
	[
		{<<>>, <<>>},
		{<<"AB">>, <<"BB8">>},
		{<<"Hello!!">>, <<"%69 VD92EX0">>},
		{<<"base-45">>, <<"UJCLQE7W581">>},
		{<<"ietf!">>, <<"QED8WEX0">>},
		{<<"Neben der Evidenz der formalen Gestaltung.">>, <<":+9YJCM-D6VCBJE7Z8PED1$CYJF6VCBJEX.C/KEAEC1$CS346$C3WE:VD2%E:1D">>}
	].

% --- for must fail tests ---

decode_fail_test(Error) ->
	fun({In, FailingPart}) ->
		try base45:decode(In) of
			_ ->
				io:fwrite(standard_error, "should not succeed~n"),
				erlang:error(decode_fail)
		catch
			error:{Error, FailingPart} ->
				ok
		end
	end.


illegal_values() ->
	[
		{<<"GGW">>, <<"GGW">>},
		{<<"000GGW">>, <<"GGW">>}
	].

illegal_characters() ->
	[
		{<<"==">>,  <<"=">>},
		{<<"AB=">>, <<"=">>},
		{<<"ABa">>, <<"a">>}
	].

%% Boundary values for encode/decode roundtrip
boundary_in_out() ->
	[
		%% Single byte: minimum value (0) -> 0 mod 45 = 0, 0 div 45 = 0
		{<<0>>, <<"00">>},
		%% Single byte: maximum value (255) -> 255 mod 45 = 30 (U), 255 div 45 = 5 (5)
		{<<255>>, <<"U5">>},
		%% Two bytes: minimum value (0x0000) -> [0, 0, 0]
		{<<0, 0>>, <<"000">>},
		%% Two bytes: maximum value (0xFFFF = 65535) -> "FGW" as per RFC 9285
		{<<255, 255>>, <<"FGW">>},
		%% Two bytes: 0x0001 -> n=1 -> [1, 0, 0]
		{<<0, 1>>, <<"100">>},
		%% Two bytes: 0x0100 -> n=256 -> 256 mod 45=31 (V), (256 div 45)=5 mod 45=5 (5), 5 div 45=0 (0)
		{<<1, 0>>, <<"V50">>}
	].

%% Single character inputs (length mod 3 == 1) — invalid base45 encoding
single_char_inputs() ->
	[
		<<"0">>,
		<<"A">>,
		<<" ">>,
		%% 4 chars = triplet + single char remainder
		<<"0000">>
	].

%% Pair of characters decoding to a value > 255 — invalid for a single byte
pair_overflow_values() ->
	[
		%% "::" = 44 + 44*45 = 2024
		{<<"::">>  , <<"::">>},
		%% "ZZ" = 35 + 35*45 = 1610
		{<<"ZZ">>  , <<"ZZ">>},
		%% "U6" = 30 + 6*45 = 300 > 255, as last pair after a valid triplet
		{<<"000U6">>, <<"U6">>}
	].

