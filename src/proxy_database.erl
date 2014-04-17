%%% ==================================================================
%%% @author qianwangang
%%% @doc
%%%  gm_proxy's database server
%%%    1.handle update users' base infos when register/rename
%%%    2.handle writing all logs  
%%% @end
%%% ==================================================================    
-module(proxy_database).
-behavior(gen_server).

-include("proxy.hrl").

-export([start_link/0,
         register_info/3,
         rename_user/2,
         search_user/1,
         clear_server_users/1,
         start_server/3
        ]).
-export([init/1, handle_cast/2, handle_call/3,
         handle_info/2, terminate/2, code_change/3]).

-export([get_proxy_gateway_config/0]).

-define(DATABASE, gm_proxy).

%% 启动champoin进程                                                                                                                   
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
  init_config(),
  {Host, User, Passwd, Database, Port} = get_proxy_database_config(),
  mysql:start_link(?DATABASE, Host, Port, User, Passwd, Database, fun logger/4, utf8),
  mysql:connect(?DATABASE, Host, undefined, User, Passwd, Database, utf8, true),

  file:make_dir("./data"),
  case dets:open_file(?SLG_SERVERS, [{access,read_write}, {file, "./data/log_servers"}]) of
    {ok, _} -> ok;
    {error, Reason} ->
      io:format("opend dets failed. reason~p ~n", [Reason])
  end,
  {ok, {}}.

handle_call({register, UserId, OpenUdid, UserName}, _From, State) ->
  do_register_user(UserId, OpenUdid, UserName),
  {reply, ok,State};
handle_call({rename, UserId, UserName}, _From, State) ->
  do_rename_user(UserId, UserName),
  {reply, ok,State};
handle_call({start_server, Sid, Node, LogSer}, _From, State) ->
  Result =  do_start_server(Sid, Node, LogSer),
  {reply, Result, State};
handle_call(_Event, _From, State) ->
  {reply, undefined,State}.

handle_cast(_Event, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, _State) ->
  dets:sync(?SLG_SERVERS),
  io:format("gm_proxy shouldn't terminate reason~p ~n", [Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

logger(_, _, _Level, _Fun) ->
  pass.

init_config() ->
  ets:new(?PROXY_CONFIG, [named_table, public]),
  init_config_file().

init_config_file() ->
  case file:open("./config/proxy.conf", read) of
    {ok, File} ->
      do_parse_line(File),
      file:close(File);
    {error, Reason} ->
      io:format("open proxy.conf error, Reason ~p ~n", [Reason])
  end.

do_parse_line(File) ->
  case io:read(File, '') of
    eof ->
      ok;
    {error, Reason} -> 
      io:format("read proxy.conf error,  Reason ~p ~n", [Reason]);
    {ok, {Key, Value}} ->
      ets:insert(?PROXY_CONFIG, {Key, Value}),
      do_parse_line(File)
  end.

search_user(Params) ->
  UserIdstr = case proplists:get_value("userid", Params) of
                undefined -> "";
                Id -> "user_id="++ Id
              end,
  UdidStr = case proplists:get_value("openudid", Params) of
              undefined -> "";
              Udid -> "open_udid="++ Udid
            end, 
  FuzzySearch = case proplists:get_value("fuzzy_search", Params) of
                  undefined -> true;
                  V -> list_to_atom(V)
                end,
  UserNameStr = case proplists:get_value("username", Params) of
                  undefined -> "";
                  Name -> 
                    case FuzzySearch of 
                      true -> "user_name like '%" ++ Name ++ "%'";
                      false -> "user_name='" ++ Name ++ "'"
                    end
                end, 
  Cond = lists:foldl(fun(Str, C) ->
                         case Str == "" of
                           true -> C;
                           _ -> C ++ " AND " ++ Str
                         end
                     end, "", [UserIdstr, UdidStr, UserNameStr]),
  case Cond of
    "" -> {error, ?CODE_INVALID_PARAMETER, ?MSG_INVALID_PARAMETER};
    _ ->
      Count = proplists:get_value("count", Params, "20"),
      Cond1 = string:substr(Cond, 6),
      Sql = "select * from users where " ++ Cond1 ++ " limit " ++ Count,
      {ok, select_exec(Sql)}
  end.

clear_server_users(Params) ->
  case proplists:get_value("server_id", Params) of
    undefined ->
      {error, ?CODE_INVALID_PARAMETER, "server_id undefined"};
    Id -> 
      Sql = "delete from users where server_id=" ++ Id,
      mysql:fetch(?DATABASE, Sql),
      ok
  end.

%% LogSer:the log's database server info, include databases' {host, username, password}   
start_server(Sid, Node, LogSer) ->
  gen_server:call(?MODULE, {start_server, Sid, Node, LogSer}).

register_info(UserId, OpenUdid, UserName) ->
  gen_server:call(?MODULE, {register, UserId, OpenUdid, UserName}).

rename_user(UserId, UserName) ->
  gen_server:call(?MODULE, {rename, UserId, UserName}).  

do_start_server(Sid, Node, LogSer) ->
  dets:insert(?SLG_SERVERS, {Sid, {Node, LogSer}}),
  dets:sync(?SLG_SERVERS).

do_register_user(UserId, OpenUdid, UserName) ->
  Server = proxy_route:get_server_info(UserId),
  ServerId = proplists:get_value(<<"server_id">>, Server),
  ServerName = proplists:get_value(<<"server_name">>, Server),
  Sql = "insert into users values (" ++ to_list(UserId)
    ++ ",'" ++ to_list(OpenUdid) ++ "','" ++ to_list(UserName) ++ "','" ++
    to_list(ServerName) ++ "'," ++ to_list(ServerId) ++ ")",
  insert_exec(Sql),
  ok.

do_rename_user(UserId, UserName) ->
  Sql = "update users set user_name='" ++ to_list(UserName) ++ 
    "' where user_id=" ++ to_list(UserId),
  insert_exec(Sql),
  ok.

%% when select
select_exec(Cmd) ->
  io:fwrite("test log,Cmd=~p~n",[Cmd]),
  {data, Result} = mysql:fetch(?DATABASE, Cmd),
  mysql:get_result_rows(Result).
  
%% when insert/update
insert_exec(Cmd) ->
  {updated, _} = mysql:fetch(?DATABASE, Cmd),
  ok.
  
get_proxy_database_config() ->
  [{hostname,Host}] = ets:lookup(?PROXY_CONFIG, hostname),
  [{username, User}] = ets:lookup(?PROXY_CONFIG, username),
  [{password, Passwd}] = ets:lookup(?PROXY_CONFIG, password),
  [{database, Database}] = ets:lookup(?PROXY_CONFIG, database),
  [{port, Port}] = ets:lookup(?PROXY_CONFIG, port),
  {Host, User, Passwd, Database, Port}.

get_proxy_gateway_config() ->
  [{gate_way_ip,GatewayIp}] = ets:lookup(?PROXY_CONFIG, gate_way_ip),
  [{gate_way_port, GatewayPort}] = ets:lookup(?PROXY_CONFIG, gate_way_port), 
  {GatewayIp, GatewayPort}.
  
to_list(V) when is_list(V) ->
  V;
to_list(V) when is_integer(V) ->
  integer_to_list(V);
to_list(V) when is_atom(V) ->
  atom_to_list(V);
to_list(V) when is_binary(V) ->
  binary_to_list(V);
to_list(V) ->
  lists:flatten(io_lib:format("~p", [V])).
