-module(proxy).
-author("Mochi Media <dev@mochimedia.com>").
%% only test
%% only test 2 master
%% only test 3 soso
-export([start/0, stop/0]).

ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.
%% nada
%% @spec start() -> ok
start() ->
  proxy_deps:ensure(),
  ensure_started(crypto),
  application:start(proxy).


%% @spec stop() -> ok
stop() ->
  application:stop(proxy).
