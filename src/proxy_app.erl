-module(proxy_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


start(_Type, _StartArgs) ->
  proxy_deps:ensure(),
  proxy_sup:start_link().

stop(_State) ->
  ok.
