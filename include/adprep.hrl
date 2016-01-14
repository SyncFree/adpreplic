%% =============================================================================
%% Header for Partial Replication Layer - SyncFree
%%
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

-record(replica, {
    key              :: key(),
    num_replicas = 1 :: integer(),
    value            :: value(),
    list_dcs_with_replicas,
    dcs              :: [dc()]
}).

-record(strategy_params, {
    decay_time       :: integer(),
    repl_threshold   :: float(),
    rmv_threshold    :: float(),
    max_strength     :: float(),
    decay_factor     :: float(),
    rstrength        :: float(),
    wstrength        :: float(),
    min_dcs_number   :: integer
}).

-record(data_item, {
    key       :: id(),
    value   :: value()
}).

-record(data_info_with_key, {
    key      :: id(),
    value :: data_info()
}).

-record(data_info, {
    replicated :: boolean(),
    strength :: float(),
    strategy :: strategy(),
    dcs      :: [datacenter()],
    timestamp:: value()
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
-type datacenter() :: string().
-type replica_info() :: #replica{}.
-type strategy_params() :: #strategy_params{}.
-type data_info() :: #data_info{}.

-export_type([key/0, id/0, value/0, strategy/0, args/0, 
    reason/0, time/0, timer/0, dc/0, datacenter/0, replica_info/0, strategy_params/0,
    data_info/0]).
