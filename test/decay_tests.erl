%% =============================================================================
%% EUnits for the deycay process - SyncFree
%% 
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================


-module(decay_tests).
-author('aas@trifork.co.uk').

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

-include_lib("eunit/include/eunit.hrl").


%% ====================================================================
%% Internal functions
%% ====================================================================
startStopDecay_test() ->
	% Initialise
	DecayTime = 100,
	% Send decay messages to us!
	{ok, Timer} = decay:startDecayTimer(DecayTime, self(), none),
	% Test that we are receiving decay notifications
	ok = repeat(DecayTime, Timer, 3),
	% Stop decay test and check that it is not running
	ok = decay:stopDecayTimer(Timer).


%%%%%%
repeat(_DelayTime, _Timer, 0) ->
	ok;
repeat(DelayTime, Timer, Num) when Num > 0 ->
	Result = receive
		{decay, _Pid, 0} ->
			repeat(DelayTime, Timer, Num-1);
		_ ->
			error
	after
		DelayTime + 100 ->
			error
	end,
	?assertEqual(ok, Result).
