%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 八月 2018 10:21
%%%-------------------------------------------------------------------
-module(workitem).
-author("pei").

%% API
-export([create_pool/3, create_pool/2, async/1, async/2, start/0]).
-export([call/1, call/2, cast/1, cast/2, async/3, cast/3, call/3, call_raw/3]).
-export([call_on/1, call_on/2, call_on/3, call_on_raw/3]).
-export([cast_on/1,cast_on/2,cast_on/3]).

-include("wi.hrl").

start()->
  application:start(poolboy),
  application:start(workitem),
  create_pool([{size, 3}, {max_overflow, 5}], []).

create_pool(Settings, Args) ->
  create_pool(?DEFAULT_POOL_NAME, Settings, Args).

create_pool(PoolName, Settings0, Args) ->
  % {strategy, fifo}
  Settings = lists:append(Settings0, [{name, {local, PoolName}}, {worker_module, workitem_worker}]),
  workitem_sup:add_pool(PoolName, Settings, Args).


% lists:foldl(fun(N,_)-> spawn(fun()-> Rt = workitem:async(fun()-> receive after (N rem 6)*1000 -> N*2 end end), io:format("----->: ~p~n", [{N, Rt}]) end) end, ok, lists:seq(1, 5)).
% lists:foldl(fun(N,_)-> spawn(fun()-> Rt = workitem:queue(fun()-> receive after (N rem 6)*1000 -> N*2 end end), io:format("----->: ~p~n", [{N, Rt}]) end) end, ok, lists:seq(1, 5)).
% lists:foldl(fun(N,_)-> workitem:call(fun()-> receive after (N rem 10)*200 -> io:format("----->: ~p~n", [{N*2}]) end end) end, ok, lists:seq(1, 30)).
% lists:foldl(fun(N,_)-> workitem:async(fun()-> receive after (N rem 10)*200 -> io:format("----->: ~p~n", [{N*2}]) end end) end, ok, lists:seq(1, 30)).

%% No state
%% on Fun(), {M,F,A}: M:F(A)
async(Fun)->
  workitem_worker:async(?DEFAULT_POOL_NAME, Fun, infinity).

async(PoolName, Fun)->
  workitem_worker:async(PoolName, Fun, infinity).

async(PoolName, Fun, Timeout)->
  workitem_worker:async(PoolName, Fun, Timeout).

cast(Fun)->
  workitem_worker:cast(?DEFAULT_POOL_NAME, Fun, infinity).

cast(PoolName, Fun)->
  workitem_worker:cast(PoolName, Fun, infinity).

cast(PoolName, Fun, Timeout)->
  workitem_worker:cast(PoolName, Fun, Timeout).

call(Fun)->
  workitem_worker:call(?DEFAULT_POOL_NAME, Fun, infinity).

call(PoolName, Fun)->
  workitem_worker:call(PoolName, Fun, infinity).

call(PoolName, Fun, Timeout)->
  workitem_worker:call(PoolName, Fun, Timeout).

call_raw(PoolName, Fun, Timeout)->
  workitem_worker:call_raw(PoolName, Fun, Timeout).


%% with state
%% on Fun(State), {M,F,A}: M:F(A,State)
cast_on(Fun)->
  workitem_worker:cast_on(?DEFAULT_POOL_NAME, Fun, infinity).

cast_on(PoolName, Fun)->
  workitem_worker:cast_on(PoolName, Fun, infinity).

cast_on(PoolName, Fun, Timeout)->
  workitem_worker:cast_on(PoolName, Fun, Timeout).

call_on(Fun)->
  workitem_worker:call_on(?DEFAULT_POOL_NAME, Fun, infinity).

call_on(PoolName, Fun)->
  workitem_worker:call_on(PoolName, Fun, infinity).

call_on(PoolName, Fun, Timeout)->
  workitem_worker:call_on(PoolName, Fun, Timeout).

call_on_raw(PoolName, Fun, Timeout)->
  workitem_worker:call_on_raw(PoolName, Fun, Timeout).