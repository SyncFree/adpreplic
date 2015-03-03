%% =============================================================================
%% Haedrer for Partial Replication Layer - SyncFree
%%
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

-record(replica,   {key :: key(), 
					value :: value(), 
					num_replicas = 1 :: integer(), 
					list_dcs_with_replicas}).

-record(strategy_params, {       
    decay_time       :: integer(),
    repl_threshold   :: float(),
    rmv_threshold    :: float(),
    max_strength     :: float(),
    decay_factor     :: float(),
    rstrength        :: float(),
    wstrength        :: float() 
}).

-record(strategy_state, {
    key         :: key(),
    strength    :: float(),
    replicated  :: boolean(),
    params      :: strategy_params(),
    timer       :: timer()
}).

-type key() :: string().
-type id() :: integer().
-type value() :: term().
-type strategy() :: string().
-type args() :: term().
-type reason() :: atom().
-type time() :: integer().
-type timer() :: timer:tref().
-type tcp_port() :: integer().
-type address() :: string().
-type dc() :: {address(), tcp_port()}.
-type strategy_state() :: #strategy_state{}.
-type strategy_params() :: #strategy_params{}.


-export_type([key/0, id/0, value/0, strategy/0, args/0, 
    reason/0, time/0, timer/0, dc/0, strategy_state/0, strategy_params/0]).

