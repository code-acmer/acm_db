-module(db_mysql_stat_tests).
-include_lib("eunit/include/eunit.hrl").
-include("leshu_db_common.hrl").
-include("define_mysql_stat.hrl").

start() ->
    hloglevel:set(6),
    application:start(mysql),
    application:start(hstdlib),    
    application:start(leshu_db),
    ?assertNot(undefined == whereis(hstdlib_sup)),
    ?assertNot(undefined == whereis(leshu_db_sup)).
stop() ->
    application:stop(mysql),
    application:stop(hstdlib),    
    application:stop(leshu_db).

stat_db_access_test() ->
    start(),
    ?assertEqual(db_mysql_stat:stat_db_access(player, select), ok),
    ?assertEqual(db_mysql_stat:stat_db_access(player, select_all), ok),
    ?assertEqual(db_mysql_stat:stat_db_access(player, delete), ok),
    ?assertEqual(db_mysql_stat:stat_db_access(player, select), ok),
    error_logger:error_msg("~p", [hetsutil:tab2list(?ETS_DB_STAT)]),
    stop().
    
