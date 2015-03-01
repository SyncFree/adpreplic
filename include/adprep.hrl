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

-type key() :: string().
-type id() :: integer().
-type value() :: term().
-type strategy() :: string().
-type args() :: term().
-type reason() :: atom().
-type time() :: timer:time().
-type timer() :: timer:tref().

-export_type([key/0, id/0, value/0, strategy/0, args/0, 
    reason/0, time/0, timer/0]).

