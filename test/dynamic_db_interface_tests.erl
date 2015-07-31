-module(dynamic_db_interface_tests).
-include_lib("eunit/include/eunit.hrl").
-include("leshu_db_common.hrl").
-include("leshu_db.hrl").
-define(setup(F), {setup, fun start/0, fun stop/1, F}).


start() ->
    hloglevel:set(6),
    application:start(mysql),
    application:start(hstdlib),    
    application:start(leshu_db),
    ?assertNot(undefined == whereis(hstdlib_sup)),
    ?assertNot(undefined == whereis(leshu_db_sup)),

    DbGameConf = #db_conf{
                    mod_name = "db_game",
                    host = "192.168.1.149",
                    poll = p01,
                    username = "shsg",
                    password = "shsg",
                    database = "shsg_game",
                    worker = 4
                   },
    DbBaseConf = #db_conf{
                    mod_name = "db_base",
                    host = "192.168.1.149",
                    poll = p02,
                    username = "shsg",
                    password = "shsg",
                    database = "shsg_base",
                    worker = 2
                   },
    #db_conf{
       poll = DbGamePoll,
       host = HostName,
       port = Port,
       username = UserName,
       password = Password,
       database = DbGame,
       worker = GameWorker
      } = DbGameConf,
    #db_conf{
       poll = DbBasePoll,
       database = DbBase,
       worker = BaseWorker       
      } = DbBaseConf,

    mysql_sup:start_link([[DbGamePoll, HostName, Port, UserName, Password, DbGame, GameWorker], [DbBasePoll, HostName, Port, UserName, Password, DbBase, BaseWorker]]),
    ?assertEqual(ok, dynamic_db_interface:start([DbGameConf, DbBaseConf])),
    error_logger:error_msg("test p1~p~n", [db_game:select_all(player, "COUNT(1)", [])]).

stop(_) ->
    application:stop(mysql),
    application:stop(hstdlib),    
    application:stop(leshu_db),
    mysql_sup:stop().


start_test_() ->
    ?setup(fun hah/1).



hah(_) ->
    [?_assertNotEqual(db_game:select_all(player, "COUNT(1)", []), [[0]]),
     ?_assertNotEqual(db_base:select_all(base_goods, "COUNT(1)", []), [[0]])].    



