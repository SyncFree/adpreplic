%% =============================================================================
%% Haedrer for Partial Replication Layer - SyncFree
%%
%% @author Amadeo Asco
%% @version 1.0.0
%% @reference Project <a href="https://syncfree.lip6.fr/">SyncFree</a>
%% @reference More courses at <a href="http://www.trifork.com">Trifork Leeds</a>
%% @end
%% =============================================================================

%% @doc Record with all the data required by the Replication Layer.
-record(replica,   {key, % the key associated with the record/data
                    value, % the data stored in the replica/record
                    num_replicas = 1 :: integer(), % the number of replicas of this data
                    list_dcs_with_replicas}). % the set of DCs with replicas. There should be num_replicas - 1 of DCs in the set

-type key() :: term().

-export_type([key/0]).
