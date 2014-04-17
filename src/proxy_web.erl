-module(proxy_web).
-author("Mochi Media <dev@mochimedia.com>").

-include("proxy.hrl").

-export([start/1, stop/0, loop/2, to_binary/1]).

%% External API  
start(Options) ->
  {DocRoot, Options1} = get_option(docroot, Options),
  Loop = fun (Req) ->
             ?MODULE:loop(Req, DocRoot)
         end,
  mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
  mochiweb_http:stop(?MODULE).

loop(Req, _DocRoot) ->
  "/" ++ Path = Req:get(path),
  try
    case Req:get(method) of
      Method when Method =:= 'GET'; Method =:= 'HEAD' ->
        QueryStringData = Req:parse_qs(),
        router_do(Path, QueryStringData, Req);
      'POST' ->
        QueryStringData = Req:parse_post(),
        router_do(Path, QueryStringData, Req);
      _ ->
        Json = make_response_json(?STATUS_FAILED, ?CODE_UNKOWN_HTTP_METHOD, ?MSG_UNKOWN_HTTP_METHOD, ""),
        Req:respond({200, [{"Content-Type", "text/json"}], Json})
    end
  catch
    Type:What ->
      Report = ["web request failed",
                {path, Path},
                {type, Type}, {what, What},
                {trace, erlang:get_stacktrace()}],
      error_logger:error_report(Report),
      Msg = proxy_route:format_msg(Type),
      Json1 = make_response_json(?STATUS_FAILED, ?CODE_INTERNAL_ERROR, Msg, ""),
      Req:respond({200, [{"Content-Type", "text/json"}], Json1})
  end.


router_do(Path, QueryStringData, Req) ->
  case proxy_route:get(Path, QueryStringData) of
    ok -> 
      Json = make_response_json(?STATUS_SUCCESS, ?CODE_SUCCESS, "",  <<"success">>),
      Req:respond({200, [{"Content-Type", "text/json"}], Json});
    {ok, Result} ->
      Json = make_response_json(?STATUS_SUCCESS, ?CODE_SUCCESS, "", Result),
      Req:respond({200, [{"Content-Type", "text/json"}], Json});
    {error, Code, Msg}-> 
      Json = make_response_json(?STATUS_FAILED, Code, Msg, ""),
      Req:respond({200, [{"Content-Type", "text/json"}], Json})
  end.
  
%% Internal API
get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


make_response_json(Status, ErrorCode, Msg, Response) ->
  jsx:encode([{status, to_binary(Status)}, {code, ErrorCode}, 
              {message, to_binary(Msg)}, {response, Response}]).

to_binary(V) when is_binary(V) ->
  V;
to_binary(V) when is_list(V) ->
  list_to_binary(V);
to_binary(V) when is_tuple(V) ->
  list_to_binary(lists:flatten(io_lib:format("~p", [V])));
to_binary(V) when is_atom(V) ->  
  atom_to_binary(V, utf8);
to_binary(V) when is_integer(V) ->
  list_to_binary(integer_to_list(V)).


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
  ?assertEqual(
     "No, but I will!",
     "Have you written any tests?"),
  ok.

-endif.
