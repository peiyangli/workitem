%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 八月 2018 10:24
%%%-------------------------------------------------------------------
-module(workitem_worker).
-author("pei").

-behaviour(gen_server).
-behaviour(poolboy_worker).

%% API
-export([start_link/1, cast/3, call/3, cast_on/3, call_on/3, async/3, call_raw/3, call_on_raw/3]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).



-define(SERVER, ?MODULE).

%%-record(state, {}).

-include("wi.hrl").


middle_man_transaction(Pool, Fun, Timeout) ->
  Tag = make_ref(),
  {Receiver, Ref} = erlang:spawn_monitor(
    fun() ->
      process_flag(trap_exit, true),
      Result = poolboy:transaction(Pool, Fun,
        Timeout),
      exit({self(),Tag,Result})
    end),
  receive
    {'DOWN', Ref, _, _, {Receiver, Tag, Result}} ->
      Result;
    {'DOWN', Ref, _, _, {timeout, _}} ->
      {error, timeout};
    {'DOWN', Ref, _, _, Reason} ->
      {error, Reason}
  end.

%%%===================================================================
%%% API
%%%===================================================================

% poolboy
start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_Args) ->
  process_flag(trap_exit, true),
  {ok, _Args}.


call_fun({M,F}) when is_atom(M)->
  M:F();
call_fun({M,F,A})when is_atom(M)->
  M:F(A);
call_fun(Fun)when is_function(Fun, 0)->
  Fun().

call_fun({M,F}, State) when is_atom(M)->
  M:F(State);
call_fun({M,F,A}, State)when is_atom(M)->
  M:F(A, State);
call_fun(Fun, State)when is_function(Fun, 1)->
  Fun(State).
%%
%%call_fun({M,F}) when is_atom(M)->
%%  try
%%    M:F()
%%  catch
%%    Class:Reason:ST ->
%%      lager:error("[workitem]Class: ~p, Reason: ~p, Stack: ~p~n", [Class, Reason, ST]),
%%      {error, crash}
%%  end;
%%call_fun({M,F,A})when is_atom(M)->
%%  try
%%    M:F(A)
%%  catch
%%    Class:Reason:ST ->
%%      lager:error("[workitem]Class: ~p, Reason: ~p, Stack: ~p~n", [Class, Reason, ST]),
%%      {error, crash}
%%  end;
%%call_fun(Fun)->
%%  try
%%    Fun()
%%  catch
%%    Class:Reason:ST ->
%%      lager:error("[workitem]Class: ~p, Reason: ~p, Stack: ~p~n", [Class, Reason, ST]),
%%      {error, crash}
%%  end.
%%
%%
%%call_fun({M,F}, State) when is_atom(M)->
%%  try
%%    M:F(State)
%%  catch
%%    Class:Reason:ST ->
%%      lager:error("[workitem]Class: ~p, Reason: ~p, Stack: ~p~n", [Class, Reason, ST]),
%%      {error, crash}
%%  end;
%%call_fun({M,F,A}, State)when is_atom(M)->
%%  try
%%    M:F(A, State)
%%  catch
%%    Class:Reason:ST ->
%%      lager:error("[workitem]Class: ~p, Reason: ~p, Stack: ~p~n", [Class, Reason, ST]),
%%      {error, crash}
%%  end;
%%call_fun(Fun, State)->
%%  try
%%    Fun(State)
%%  catch
%%    Class:Reason:ST ->
%%      lager:error("[workitem]Class: ~p, Reason: ~p, Stack: ~p~n", [Class, Reason, ST]),
%%      {error, crash}
%%  end.


handle_call(A,From,State)->
  try
    handle_call_1(A,From,State)
  catch
      E:W ->{reply, {error, {E, W}}, State}
  end.

handle_call_1({do, Fun}, _From, State) ->
  Reply = call_fun(Fun),
  {reply, Reply, State};
handle_call_1({on, Fun}, _From, State) ->
  case call_fun(Fun, State) of
    {reply, Reply}->
      {reply, Reply, State};
    {reply, Reply, NewState}->
      {reply, Reply, NewState};
    {error, Why}->
      {reply, {error, Why}, State};
    What when is_atom(What)->
      {reply, What, State};
    NewState ->
      {reply, noreply, NewState}
  end;
handle_call_1(Request, From, State = #{handle := MFA}) ->
  case handle_X(MFA, call, {Request, From}, State) of
    {ok, Reply, NewState}->
      {reply, Reply, NewState};
    {Reply, NewState}->
      {reply, Reply, NewState};
    ok->
      {reply, ok, State};
    What->What
  end;
handle_call_1(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(E, State)->
  try
    handle_cast_1(E,State)
  catch
    _E:_W ->{noreply, State}
  end.

handle_cast_1({do, Fun}, State) ->
  call_fun(Fun),
  {noreply, State};
handle_cast_1({on, Fun}, State) ->
  case call_fun(Fun, State) of
    {reply, _Reply}->
      {noreply, State};
    {reply, _Reply, NewState}->
      {noreply, NewState};
    {error, _Why}->
      {noreply , State};
    What when is_atom(What)->
      {noreply, State};
    NewState ->
      {noreply, NewState}
  end;
handle_cast_1(Info, State = #{handle := MFA}) ->
  case handle_X(MFA, cast, Info, State) of
    {ok, _Reply, NewState}->
      {noreply, NewState};
    {_Reply, NewState}->
      {noreply, NewState};
    ok->
      {noreply, State};
    What->What
  end;
handle_cast_1(_Request, State) ->
  {noreply, State}.


handle_info(Info, State)->
  try
    handle_info_1(Info,State)
  catch
    _E:_W ->{noreply, State}
  end.

handle_info_1(Info, State = #{handle := MFA}) ->
  case handle_X(MFA, info, Info, State) of
    {ok, _Reply, NewState}->
      {noreply, NewState};
    {_Reply, NewState}->
      {noreply, NewState};
    ok->
      {noreply, State};
    What->What
  end;
%%  M:F(info, Info, State, A);
%%  {noreply, State};
handle_info_1(_Info, State) ->
  {noreply, State}.


handle_X({M,F,A}, X, What, State)->
  M:F(X, What, State, A);
handle_X({M,F}, X, What, State)->
  M:F(X, What, State);
handle_X(_,_,_,_)->ok.


terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
% put all work to pool queue: the message queue of working process mostly is not empty
%
cast(PoolName, Fun, Timeout)->
  poolboy:transaction(PoolName,
    fun(W)->
      gen_server:cast(W, {do, Fun})
    end, Timeout).



%%%===================================================================
% put all work to pool queue: the message queue of working process mostly is not empty
%
cast_on(PoolName, Fun, Timeout)->
  poolboy:transaction(PoolName,
    fun(W)->
      gen_server:cast(W, {on, Fun})
    end, Timeout).

%%%===================================================================
% put to queue and wait for return: the message queue of working process must be empty
call(PoolName, Fun, Timeout)->
  middle_man_transaction(PoolName,
    fun (W) ->
      gen_server:call(W, {do, Fun}, Timeout)
    end, Timeout).

%%%===================================================================
% put to queue and wait for return: the message queue of working process must be empty
call_on(PoolName, Fun, Timeout)->
  middle_man_transaction(PoolName,
    fun (W) ->
      gen_server:call(W, {on, Fun}, Timeout)
    end, Timeout).



%%%===================================================================
% put to queue and no return: the message queue of working process must be empty
async(PoolName, Fun, Timeout)->
  poolboy:transaction(PoolName,
    fun(W)->
      gen_server:cast(W, {do, Fun})
    end, Timeout).


call_raw(PoolName, Func, Timeout)when is_function(Func, 0)->
  poolboy:transaction(PoolName, fun(W)->gen_server:call(W, {do, Func}, Timeout) end, Timeout).

call_on_raw(PoolName, Func, Timeout) when is_function(Func, 1)->
  poolboy:transaction(PoolName, fun(W)->gen_server:call(W, {on, Func}, Timeout) end, Timeout).
%%%===================================================================