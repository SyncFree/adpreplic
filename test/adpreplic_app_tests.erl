%% =============================================================================
%% EUnits for application - SyncFree
%% 
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================


-module(adpreplic_app_tests).


%% ====================================================================
%% API functions
%% ====================================================================
-export([]).


-include_lib("eunit/include/eunit.hrl").


%% ====================================================================
%% Test functions
%% ====================================================================
initialise_test() ->
    spawn(fun() ->
         {ok, Pid} = adpreplic_app:start(type, args),
         exit(Pid, shutdown)
         end),
    wait_for_death_or_get_event(),
    stop(). % I would expect this was not necessary

stop_test() ->
    Result = adpreplic_app:stop(fine),
    ?assertEqual(ok, Result).


%% ====================================================================
%% Support functions
%% ====================================================================
stop() ->
    io:format("(adprep_tests): Stopping servers~n", []),
    adprep:stop(),
    datastore:stop(),
    erlang:yield().

wait_for_death_or_get_event() ->
    receive
        Response ->
            ?assertEqual(wrong, Response)
    after
        1000 ->
            ?assert(whatever())
    end.

whatever() ->
    true.
