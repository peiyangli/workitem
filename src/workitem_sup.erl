%%%-------------------------------------------------------------------
%% @doc workitem top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(workitem_sup).

-behaviour(supervisor).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0, add_pool/3]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Pools} = application:get_env(workitem, pools),
    PoolSpec = lists:map(
        fun ({PoolName, SizeArgs, WorkerArgs}) ->
            PoolArgs = [{name, {local, PoolName}},
                {worker_module, workitem_worker}] ++ SizeArgs,
            poolboy:child_spec(PoolName, PoolArgs, WorkerArgs);
            (_)->ok
        end, Pools),
    {ok, { {one_for_one, 10, 10}, PoolSpec} }.



%% workitem_sup:add_pool(test_pool, [{name, {local, PoolName}}, {worker_module, workitem_worker}, {size, 5}, {max_size, 500}, {max_overflow, 200}], #{})
add_pool(Name, PoolArgs, WorkerArgs) ->
    ChildSpec = poolboy:child_spec(Name, PoolArgs, WorkerArgs),
    supervisor:start_child(?MODULE, ChildSpec).
