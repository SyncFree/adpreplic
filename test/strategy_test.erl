%% =============================================================================
%% Adaptive Replication - SyncFree
%% 
%% @author Amadeo Asco, Annette Bieniusa
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

%% Unit tests for Strategy module 

-module(strategy_test).
-author(['aas@trifork.co.uk','bieniusa@cs.uni-kl.de']).

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
    Strategy = #strategy_params{
        decay_time     = 5000,
        repl_threshold = 100,
        rmv_threshold  = 10,
        max_strength   = 200,
        decay_factor   = 5,
        rstrength      = 20,
        wstrength      = 100
    },
    {ok, Pid} = strategy_adprep:init_strategy(Key, Replicated, Strategy),
    ok = strategy_adprep:stop(Pid).

