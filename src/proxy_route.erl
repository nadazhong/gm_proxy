-module(proxy_route).

-export([get/2]).
-export([say_hello/1,list_all_servers/1, player_search/1, player_log/1]).
-export([format_msg/1, list_events/1, clear_server_users/1]).
-export([get_server_info/1, get_server/1]).

-include("proxy.hrl").
-define(SAY_HELLO,"sayhello").
-define(LIST_ALL_SERVERS, "servers/list").
-define(PLAYER_SEARCH, "player/search").
-define(PLAYER_DETAIL, "player/detail").
-define(CARD_INFO, "player/card_info").
-define(RUNE_INFO, "player/rune_info").
%%-define(FRIEND_INFO, "player/friend_info").
-define(BAN_PLAYER, "player/ban").
-define(UNBAN_PLAYER, "player/unban").
-define(MUTE_PLAYER, "player/mute").
-define(UNMUTE_PLAYER, "player/unmute").
-define(SEND_MAIL, "player/send_mail").
-define(ADD_MONEY, "player/add_money").
-define(ADD_GOLD, "player/add_gold").
-define(ADD_POWER, "player/add_power").
-define(ADD_MAGIC, "player/add_magic").
-define(GET_CHAT, "chat/get").
-define(SEND_CHAT, "chat/send").
-define(PLAYER_LOG, "player/log").
-define(LIST_EVENTS, "logs/event_list").
-define(CLEAR_SERVER_USERS, "servers/clear_users").


%%% -------------------------------------------------------------------
%%% 第一个gm指令，dump玩家数据情况
%%% -------------------------------------------------------------------

%% 对单个节点执行单个命令.
get(PATH, QueryStringData) ->
  case param(QueryStringData, PATH) of
    {error, Err} -> {error, Err};
    Params ->
      do(PATH, Params)
  end.

%% 获取rpc参数.
param(QueryStringData, PATH) ->
  case get_keys_by_path(PATH) of
    {error, ErrCode, Msg} -> {error, ErrCode, Msg};
    Keys ->
      List = lists:map(fun(Key) ->
                           {Key, get_key_value(QueryStringData, Key, 1)}
                       end, Keys),
      lists:filter(fun({_, V}) ->
                       V =/= undefined
                   end, List)
  end.

%% 对节点进行rpc调用.
do(PATH, Params) ->
  io:fwrite("test log,Params=~p~n PATH=~p~n",[Params, PATH]),
  case get_do_options(PATH) of
    {error, _, _} = Err-> Err;
    {do, Module, Func} ->
      apply(Module, Func, [Params]);
    {rpc_call, Module, Func, Action} ->
      UserId = list_to_integer(proplists:get_value("userid", Params, "0")),
      ServerId = list_to_integer(proplists:get_value("server_identifier", Params, "0")),
	  io:format("rpc_call check MFA : ~p ~n UserId ~p ServerId ~p ~n   ",[{Module, Func, Action},UserId,ServerId]),
      case get_server_node(UserId, ServerId) of
        {undefined, _} ->
			io:format("undefined why ~n"),
          {error, ?CODE_NOT_FOUND, ?MSG_NODE_NOT_FOUND};
        {Node, SId} ->
			io:format("Node ~p, SId ~p ~n",[Node, SId]),
          case rpc:call(Node, Module, Func, [Action, Params], 3000) of
            {badrpc, Error} -> 
				io:format("return badrpc ~n"),
              {error, ?CODE_INTERNAL_ERROR, format_msg(Error)};
            {error, Error} ->
				io:format("return error ~n"),
              {Code, Msg} = get_error_code(Error),
              {error, Code, Msg};
            ok -> ok;
            {ok, Res} -> {ok, deal_response(PATH, Res, SId)}
          end
      end
  end.

%% deal response
%% when get player detail info, add server_name into it
deal_response(?PLAYER_DETAIL, Res, ServerId) ->
  Server = get_server(ServerId),
  Name = proplists:get_value(<<"name">>, Server),
  [{<<"server_name">>, to_binary(Name)}|Res];
deal_response(_, Res, _) ->
  Res.

get_key_value(List, Key, N) ->
  case lists:keyfind(Key, N, List) of
    false ->
      undefined;
    {Key, Value} -> Value
  end.
get_keys_by_path(?SAY_HELLO)->
	["name"];
get_keys_by_path(?LIST_ALL_SERVERS) ->
  [];
get_keys_by_path(?CLEAR_SERVER_USERS) ->
  ["server_id"];
get_keys_by_path(?PLAYER_SEARCH) ->
  ["userid", "openudid", "username", "fuzzy_search", "count"];
get_keys_by_path(?PLAYER_DETAIL) ->
  ["userid"];
get_keys_by_path(?CARD_INFO) ->
  ["userid", "page", "per_page"];
get_keys_by_path(?RUNE_INFO) ->
  ["userid", "page", "per_page"];
%%get_keys_by_path(?FRIEND_INFO) ->
%%  ["userid"];
get_keys_by_path(?BAN_PLAYER) ->
  ["userid"];
get_keys_by_path(?UNBAN_PLAYER) ->
  ["userid"];
get_keys_by_path(?MUTE_PLAYER) ->
  ["userid", "time"];
get_keys_by_path(?UNMUTE_PLAYER) ->
  ["userid"];
get_keys_by_path(?ADD_MONEY) ->
  ["userid","money"];
get_keys_by_path(?ADD_GOLD) ->
  ["userid","gold"];
get_keys_by_path(?ADD_POWER) ->	
  ["userid","power"];
get_keys_by_path(?ADD_MAGIC) ->	
  ["userid","magic"];
get_keys_by_path(?SEND_MAIL) ->
  ["userid", "title", "content"];
get_keys_by_path(?GET_CHAT) ->
  ["server_identifier", "count", "startid"];
get_keys_by_path(?SEND_CHAT) ->
  ["server_identifier", "nick", "content"];
get_keys_by_path(?PLAYER_LOG) ->
  ["userid", "event", "page", "per_page"];
get_keys_by_path(?LIST_EVENTS) ->
  [];
get_keys_by_path(_) ->
  {error, ?CODE_URL_PATH_NOT_FOUND, ?MSG_URL_PATH_NOT_FOUND}.

get_do_options(?SAY_HELLO)->
	{do,?MODULE,say_hello};
get_do_options(?LIST_ALL_SERVERS) ->
  {do, ?MODULE, list_all_servers};
get_do_options(?CLEAR_SERVER_USERS) ->
  {do, ?MODULE, clear_server_users};
get_do_options(?PLAYER_SEARCH) ->
  {do, ?MODULE, player_search};
get_do_options(?PLAYER_LOG) ->
  {do, ?MODULE, player_log};
get_do_options(?LIST_EVENTS) ->
  {do, ?MODULE, list_events};
get_do_options(?ADD_MONEY) ->
  {rpc_call, gm_proxy, gm, add_res_money};
get_do_options(?ADD_GOLD) ->
  {rpc_call, gm_proxy, gm, add_res_gold};
get_do_options(?ADD_POWER) ->	
  {rpc_call, gm_proxy, gm, add_res_power};
get_do_options(?ADD_MAGIC) ->	
  {rpc_call, gm_proxy, gm, add_res_magic};
get_do_options(?SEND_MAIL) ->
  {rpc_call, gm_proxy, gm, send_mail};
get_do_options(?PLAYER_DETAIL) ->  
  {rpc_call, gm_proxy, gm, player_detail};
get_do_options(?CARD_INFO) -> 
  {rpc_call, gm_proxy, gm, card_info};
get_do_options(?RUNE_INFO) -> 
  {rpc_call, gm_proxy, gm, rune_info};
%%get_do_options(?FRIEND_INFO) -> 
%%  {rpc_call, gm_proxy, gm, friend_info};
get_do_options(?BAN_PLAYER) ->
  {rpc_call, gm_proxy, gm, ban_player};
get_do_options(?UNBAN_PLAYER) ->
  {rpc_call, gm_proxy, gm, unban_player};
get_do_options(?MUTE_PLAYER) ->
  {rpc_call, gm_proxy, gm, mute_player};
get_do_options(?UNMUTE_PLAYER) ->
  {rpc_call, gm_proxy, gm, unmute_player};
get_do_options(?GET_CHAT) ->  
  {rpc_call, gm_proxy, gm, get_chat};
get_do_options(?SEND_CHAT) -> 
  {rpc_call, gm_proxy, gm, send_chat};
get_do_options(_) ->
{error, ?CODE_URL_PATH_NOT_FOUND, ?MSG_URL_PATH_NOT_FOUND}.

say_hello(Params)->	
	io:format("Params ~p ~n",[Params]),
	case proplists:get_value("name", Params) of
    undefined ->
      {error, ?CODE_INVALID_PARAMETER, "name undefined"};
    _Name -> 
      {ok,hello}
  end.

list_all_servers(_) ->
  inets:start(),
  
  %%{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request("http://121.199.51.39:8080/servers"),
  {GatewayIp, GatewayPort}  = proxy_database:get_proxy_gateway_config(),	
  {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request("http://"++GatewayIp++":"++integer_to_list(GatewayPort)++"/servers"),
  [{_, Servers}] = jsx:decode(list_to_binary(Body)),
  Keys = [<<"name">>, <<"ip">>, <<"port">>, <<"server_id">>,
          <<"language">>, <<"category">>, <<"open_time">>],
  ServersNew = 
    lists:map(fun(Server) ->
                  lists:map(fun(Key) ->
                                {parse_key(Key), proplists:get_value(Key, Server, <<"undefined">>)}
                            end, Keys)
              end, Servers),
  inets:stop(),
  {ok, ServersNew}.

parse_key(<<"ip">>) ->
  <<"host">>;
parse_key(<<"server_id">>) ->
  <<"identifier">>;
parse_key(Key) ->
  Key.

clear_server_users(Params) ->
  proxy_database:clear_server_users(Params).

%% as cannot get which node from parameters, so loop all servers until get it
list_events(_) ->
  {ok, Servers} = list_all_servers([]),
  Events = list_events_from_server(Servers),
  {ok, lists:map(fun({Id, Event}) ->
                     [{<<"name">>, to_binary(Event)},
                      {<<"id">>, to_binary(Id)}]
                 end, Events)}.

list_events_from_server([]) ->
  [];
list_events_from_server([Server|Servers]) ->
  Sid = proplists:get_value(<<"identifier">>, Server),
  case get_server_node(0, Sid) of
    {undefined, undefined} ->
      list_events_from_server(Servers);
    {Node, _} ->
      case rpc:call(Node, gm_proxy, list_log_events, [], 3000) of 
        {ok, Event} -> Event;
        _  -> list_events_from_server(Servers)
      end
  end.

player_search(Params) ->
  case proxy_database:search_user(Params) of
    {error, _, _} = Res -> Res;
    {ok, []}->
      {ok, []};
    {ok, Players} ->
      {ok, lists:map(fun([UserId, Udid, UserName, ServerName, ServerId]) ->
                         [{<<"userid">>, UserId}, {<<"openudid">>, Udid},
                          {<<"username">>, UserName}, {<<"server_name">>, ServerName},
                          {<<"server_identifier">>, ServerId}]
                     end, Players)}
  end.
       
player_log(Params) ->
  Userid = proplists:get_value("userid", Params, undefined),
  Event = proplists:get_value("event", Params, undefined),
  Page = list_to_integer(proplists:get_value("page", Params, "1")),
  PerPage = list_to_integer(proplists:get_value("per_page", Params, "10")),
  case {Userid, Event} of
    {V1, V2} when V1 == undefined;
                  V2 == undefined ->
      {error, ?CODE_INVALID_PARAMETER, ?MSG_INVALID_PARAMETER};
    _ ->
      ServerId = list_to_integer(Userid) rem 1000,
      Database = "slg_log_" ++ integer_to_list(ServerId),
      {Host, User, Passwd} = get_log_database_config(ServerId),
      Table = get_table(Event),

      %% get total count
      Sql1 = "select count(*) from "++ Table ++ " where user_id=" ++ Userid,
      Cmd1 = cmd_gen({Host, User, Database, Passwd, Sql1}),
      Res1 = cmd_exec(Cmd1),
      [_, CntStr] = string:tokens(Res1, "\n"),
      Count = list_to_integer(CntStr),

      %% get total page and select page
      TotalPage = case Count rem PerPage of
                    0 -> Count div PerPage;
                    _ -> Count div PerPage + 1
                  end,
      PageNew = case TotalPage >= Page of
                  true -> Page;
                  false -> TotalPage
                end,
      
      %% get logs by offset and limit
      Offset = (PageNew-1) * PerPage,
      Sql2 = "select * from " ++ Table ++ " where user_id=" ++ Userid 
        ++ " limit " ++ integer_to_list(Offset) ++ "," ++ integer_to_list(PerPage),
      Cmd2 = cmd_gen({Host, User, Database, Passwd, Sql2}),
      Res2 = cmd_exec(Cmd2),
      Logs = parse_select_log_result(Res2, Event),

      {ok,[{<<"pages">>, TotalPage}, {<<"page">>, PageNew}, {<<"logs">>, Logs}]}
  end.

get_table("pve") ->
  "pve_log";
get_table("dungeon") ->
  "dungeon_log";
get_table("pvp") ->
  "pvp_log".


format_msg(Msg) ->
  Msg1 = io_lib:format("~p", [Msg]),
  lists:flatten(Msg1).

get_error_code(player_not_eixst) ->
  {?CODE_NOT_FOUND, ?MSG_PLAYER_NOT_FOUND};
get_error_code(url_not_found) ->
  {?CODE_URL_PATH_NOT_FOUND, ?MSG_URL_PATH_NOT_FOUND};
get_error_code(Msg) ->
  {?CODE_INTERNAL_ERROR, format_msg(Msg)}.


get_server_node(0, 0) -> 
  {undefined, undefined};
get_server_node(0, ServerId) ->
  case dets:lookup(?SLG_SERVERS, ServerId) of
    [{ServerId, {Node, _}}] -> {Node, ServerId};
    _ -> {undefined, undefined}
  end;
get_server_node(UserId, _) ->
  ServerId = UserId rem 1000,
  get_server_node(0, ServerId).

get_server_info(UserId) when is_integer(UserId) ->
  ServerId = UserId rem 1000,
  get_server(ServerId);
get_server_info(UserId) ->
  get_server_info(list_to_integer(UserId)).

get_server(ServerId) ->
  inets:start(),  
  %%{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request("http://121.199.51.39:8080/servers"),
  {GatewayIp, GatewayPort}  = proxy_database:get_proxy_gateway_config(),	
  {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request("http://"++GatewayIp++":"++integer_to_list(GatewayPort)++"/servers"),
  
  [{_, Servers}] = jsx:decode(list_to_binary(Body)),
  SerFilter = lists:filter(fun(Ser) ->
                               ServerId == proplists:get_value(<<"server_id">>, Ser, -1)
                           end, Servers),
  case SerFilter of
    [] -> [];
    [Server] -> Server
  end.

get_log_database_config(ServerId) ->
  [{ServerId, {_,Server}}] = dets:lookup(?SLG_SERVERS, ServerId),
  Server.

%% 生成基本的sql执行命令.
cmd_gen({HostName, UserName, DataBase, Password, Sql}) when length(Password) == 0 ->
  MetaCmd = io_lib:format("mysql -h~s -u~s ~s -e \"~s;\"",
                          [HostName, UserName, DataBase, Sql]),
  lists:flatten(MetaCmd);
cmd_gen({HostName, UserName, DataBase, Password, Sql}) when length(Password) > 0 ->
  MetaCmd = io_lib:format("mysql -h~s -u~s -p\"~s\" ~s -e \"~s;\"",
                          [HostName, UserName, Password, DataBase, Sql]),
  lists:flatten(MetaCmd).

%% 执行:os:cmd
cmd_exec(Cmd) ->
  io:format("exec ~p~n", [Cmd]),
  os:cmd(Cmd).


parse_select_log_result(Res, Event) when Event == "pve";
                                         Event == "dungeon" ->
  List = string:tokens(Res, "\n"),
  ValueList = lists:nthtail(1, List),
  lists:map(fun(Str) ->
                [Uid, Name, CreateAt, Udid, Story, Result, Clear, ServerId, Ip] = string:tokens(Str, "\t"),
                [{<<"user_id">>, list_to_integer(Uid)}, {<<"user_name">>, to_binary(Name)},
                 {<<"create_at">>, list_to_integer(CreateAt)}, {<<"open_udid">>, to_binary(Udid)},
                 {<<"level_id">>, list_to_integer(Story)}, {<<"result">>, to_boolean(Result)},
                 {<<"clear">>, to_boolean(Clear)}, {<<"server_id">>, list_to_integer(ServerId)},
                 {<<"ip">>, to_binary(Ip)}]
            end, ValueList);
parse_select_log_result(Res, "pvp") ->
  List = string:tokens(Res, "\n"),
  ValueList = lists:nthtail(1, List),
  lists:map(fun(Str) ->
                [Uid, AtkName, CreateAt, AtkUdid, AtkRankBef, AtkRankAft, AtkResult, AtkPointChange,
                 AtkPointTotal, AtkAlliance, AtkAlliancePointChange, AtkAlliancePointTotal,
                 AtkAllianceRankBef, AtkAllianceRankAft, AtkIp, DefUid, DefName, DefUdid, DefRankBef,
                 DefRankAft, DefPointChange, DefPointTotal, DefAlliance, DefAlliancePointChange,
                 DefAlliancePointTotal, DefAllianceRankBef, DefAllianceRankAft, ServerId] = string:tokens(Str, "\t"),
                [{<<"user_id">>, list_to_integer(Uid)}, {<<"atk_user_name">>, to_binary(AtkName)},
                 {<<"create_at">>, list_to_integer(CreateAt)}, {<<"atk_open_udid">>, to_binary(AtkUdid)},
                 {<<"atk_rank_before">>, list_to_integer(AtkRankBef)}, {<<"atk_rank_after">>, list_to_integer(AtkRankAft)},
                 {<<"atk_result">>, to_boolean(AtkResult)}, {<<"atk_point_change">>, list_to_integer(AtkPointChange)},
                 {<<"atk_point_total">>, list_to_integer(AtkPointTotal)}, {<<"atk_alliance">>, list_to_integer(AtkAlliance)},
                 {<<"atk_alliance_point_change">>, list_to_integer(AtkAlliancePointChange)}, 
                 {<<"atk_alliance_point_total">>, list_to_integer(AtkAlliancePointTotal)},
                 {<<"atk_alliance_rank_before">>, list_to_integer(AtkAllianceRankBef)}, 
                 {<<"atk_alliance_rank_after">>, list_to_integer(AtkAllianceRankAft)}, {<<"atk_ip">>, to_binary(AtkIp)}, 
                 {<<"def_user_id">>, list_to_integer(DefUid)}, {<<"def_user_name">>, to_binary(DefName)},
                 {<<"def_open_udid">>, to_binary(DefUdid)}, {<<"def_rank_before">>, list_to_integer(DefRankBef)},
                 {<<"def_rank_after">>, list_to_integer(DefRankAft)}, {<<"def_point_change">>, list_to_integer(DefPointChange)},
                 {<<"def_point_total">>, list_to_integer(DefPointTotal)}, {<<"def_alliance">>, list_to_integer(DefAlliance)},
                 {<<"def_alliance_point_change">>, list_to_integer(DefAlliancePointChange)}, 
                 {<<"def_alliance_point_total">>, list_to_integer(DefAlliancePointTotal)},
                 {<<"def_alliance_rank_before">>, list_to_integer(DefAllianceRankBef)}, 
                 {<<"def_alliance_rank_after">>, list_to_integer(DefAllianceRankAft)},
                 {<<"server_id">>, list_to_integer(ServerId)}]
            end, ValueList).


to_boolean("0") ->
  false;
to_boolean("1") ->
  true.

to_binary(V) when is_binary(V);
                  is_integer(V);
                  is_boolean(V)->
  V;
to_binary(V) when is_list(V) -> 
  list_to_binary(V);
to_binary(V) when is_tuple(V) ->
  list_to_binary(lists:flatten(io_lib:format("~p", [V])));
to_binary(V) when is_atom(V) -> 
  atom_to_binary(V, utf8).
