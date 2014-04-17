-ifndef(__PROXY).
-define(__PROXY, true).

-define(GLOBAL_PROXY_SERVER, proxy_server).

-define(STATUS_SUCCESS, "success").
-define(STATUS_FAILED,"failed").

-define(CODE_SUCCESS, 0).
-define(CODE_INTERNAL_ERROR, 1001).
-define(CODE_URL_PATH_NOT_FOUND, 1002).
-define(CODE_UNKOWN_HTTP_METHOD, 1003).
-define(CODE_NOT_FOUND, 1004).
-define(CODE_INVALID_PARAMETER, 1005).

-define(MSG_URL_PATH_NOT_FOUND, "URL is wrong.").
-define(MSG_UNKOWN_HTTP_METHOD, "Unknown http method.").
-define(MSG_PLAYER_NOT_FOUND, "player not exist.").
-define(MSG_INVALID_PARAMETER, "invalid parameters.").
-define(MSG_NODE_NOT_FOUND, "slg_server node not found.").

%% ets of proxy config
-define(PROXY_CONFIG, proxy_config).

%% dets
-define(SLG_SERVERS, slg_servers).

-endif.
