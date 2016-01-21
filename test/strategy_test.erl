%% =============================================================================
%% Adaptive Replication - SyncFree
%% 
%% @author Amadeo Asco, Annette Bieniusa, Adrian Vladu
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

%% Unit tests for Strategy module 

-module(strategy_test).
-author(['aas@trifork.co.uk','bieniusa@cs.uni-kl.de',
         'adrian.vladu21@gmail.com']).

%% =============================================================================
%% API functions
%% =============================================================================
-compile(export_all).
-export([]).

-include_lib("eunit/include/eunit.hrl").
-include("adprep.hrl").

%% =============================================================================
%% Internal functions
%% =============================================================================

local_read_test() ->
    Key = "test",
    Replicated = true,
    DecayTime     = 5000,
    ReplThreshold = 100,
    RmvThreshold  = 10,
    MaxStrength   = 200,
    DecayFactor   = 5,
    Rstrength      = 20,
    Wstrength      = 100,
    
    {ok, Pid} = strategy_adprep:init_strategy(Key, Replicated, [DecayTime, ReplThreshold, RmvThreshold, MaxStrength, DecayFactor, Rstrength, Wstrength]),
    ?assertNotEqual(undefined, whereis(list_to_atom(Key))),
    ok = strategy_adprep:stop(Pid),
    ?assertEqual(undefined, whereis(list_to_atom(Key))).

