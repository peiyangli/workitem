%%%-------------------------------------------------------------------
%%% @author pei
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 十二月 2018 14:14
%%%-------------------------------------------------------------------
-module(wi_test).
-author("pei").

%% API
-export([test/1]).

%lists:foldl(fun(N,_)-> workitem:async({wi_test, test, N}) end, ok, lists:seq(1, 30)).
test(Args)->
  io:format("~p~n", [{self(), Args}]),
  Args.