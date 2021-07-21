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
-export([encode/1, decode/1, decode_fail/1]).

all() -> [
	encode,
	decode,
	decode_fail
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
	lists:foreach(fun decode_fail_test/1, illegal_values()).

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

decode_fail_test({In, FailingPart}) ->
	try base45:decode(In) of
		_ ->
			io:fwrite(standard_error, "should not succeed~n"),
			erlang:error(decode_fail)
	catch
		error:{illegale_encoding, FailingPart} ->
			ok
	end.


illegal_values() ->
	[
		{<<"GGW">>, <<"GGW">>},
		{<<"000GGW">>, <<"GGW">>}
	].
