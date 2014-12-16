%% =============================================================================
%% EUnits for supervisour - SyncFree
%% 
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================


-module(adpreplic_sup_tests).


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
         {ok, Pid} = adpreplic_sup:start_link(),
         exit(Pid, shutdown)
         end),
    wait_for_death_or_get_event().


%% ====================================================================
%% Support functions
%% ====================================================================
wait_for_death_or_get_event() ->
    receive
        Response ->
            ?assertEqual(wrong, Response)
    after
        500 ->
            ?assertEqual(true, true)
    end.
