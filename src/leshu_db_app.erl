-module(leshu_db_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-include("define_mysql_stat.hrl").
-include("leshu_db_common.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->    
    ets:new(?ETS_DB_STAT, [{keypos,#rec_db_stat.key}, named_table, public, set]),
    error_logger:info_msg("leshu_db_stated: StartArgs ~p~n", [_StartArgs]),
    error_logger:info_msg("leshu_db_env: ~p~n", [application:get_env(tcp_listener_ip)]),
    leshu_db_sup:start_link().

stop(_State) ->
    ok.
