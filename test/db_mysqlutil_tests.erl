-module(db_mysqlutil_tests).
-include_lib("eunit/include/eunit.hrl").
-include("leshu_db.hrl").
-include("leshu_db_common.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).
%%% SETUP FUNCTIONS %%%
%% start() ->
%%     hloglevel:set(6),
%%     application:start(mysql),
%%     application:start(hstdlib),    
%%     application:start(leshu_db),
%%     DbTestConf = #db_conf{
%%                     mod_name = "test",
%%                     host = "192.168.1.149",
%%                     poll = test,
%%                     username = "shsg",
%%                     password = "shsg",
%%                     database = "eunit_test",
%%                     worker = 1
%%                    },
%%     #db_conf{
%%        poll = DbPoll,
%%        host = HostName,
%%        port = Port,
%%        username = UserName,
%%        password = Password,
%%        database = DbGame,
%%        worker = Worker
%%       } = DbTestConf,
%%     mysql_sup:start_link([[DbPoll, HostName, Port, UserName, Password, DbGame, Worker]]),    
%%     ok = dynamic_db_interface:start([DbTestConf]).
start() ->
    hloglevel:set(6),
    application:start(emysql),
    application:start(hstdlib),    
    application:start(leshu_db),
    DbTestConf = #db_conf{
                    mod_name = "test",
                    host = "192.168.1.149",
                    poll = test,
                    username = "shsg",
                    password = "shsg",
                    database = "eunit_test",
                    worker = 1
                   },
    #db_conf{
       poll = DbPoll,
       host = HostName,
       port = Port,
       username = UserName,
       password = Password,
       database = DbGame,
       worker = Worker
      } = DbTestConf,
    emysql:add_pool(DbPoll, Worker, UserName, Password, HostName, Port, DbGame, utf8),
    ok = dynamic_db_interface:start([DbTestConf]).

stop(_) ->
    crypto:start(),
    application:stop(mysql),
    application:stop(emysql),
    application:stop(hstdlib),    
    application:stop(leshu_db),
    mysql_sup:stop().

%%% TESTS DESCRIPTIONS %%%
start_stop_test_() ->
    {"The application can be started, stopped and has a registered name",
     ?setup(fun is_start/1)}.

sql_run_test_() ->
    [{"The sql can run",
      ?setup(fun sql_run/1)}].

%%% ACTUAL TESTS %%%
is_start(_) ->
    [?_assertNot(undefined == whereis(hstdlib_sup)),
     ?_assertNot(undefined == whereis(leshu_db_sup))].

%% 不要随意调测试代码的顺序，数据之间前后是有逻辑关系的，除非知道其中的逻辑关系 By Roowe
sql_run(_) ->
    [%% 清理数据
     ?_assertMatch(V when is_integer(V), test:delete(user, [])),
     %% 测试insert
     ?_assertEqual(1, test:insert(user, 1, 
                                  [accid, accname, status, idcard_status, lastest_rolename], 
                                  [2222, "roowe", 0, 0, "神奇的三国"])),
     %% 测试db_mysqlutil:execute失败
     ?_assertEqual(db_error, test:insert(user, 1, 
                                  [accid, acc1name, status, idcard_status, lastest_rolename], 
                                  [2222, "roowe", 0, 0, "神奇的三国"])),
     %% 测试mul_insert
     ?_assertEqual(4, test:mul_insert(user, 
                                  [accid, accname, status, idcard_status, lastest_rolename], 
                                  [[2224, "oroowe", 0, 0, "神奇的三国"],
                                   [2225, "qroowe", 0, 0, "神奇的测试"],
                                   [2226, "proowe", 0, 0, "神奇的老板"],
                                   [2223, "iroowe", 0, 0, "拉风的神人"]])),
     ?_assertEqual(2223, test:select_one(user, "accid", [{accname, "iroowe"}])),
     %% 测试get_one失败
     ?_assertEqual(db_error, test:select_one(user, "acc1id", [{accname, "iroowe"}])),
     %% get_one null
     ?_assertEqual(null, test:select_one(user, "accid", [{accname, "mingan"}])),
     %% get_all
     ?_assertEqual(5, length(test:select_all(user, "*", []))),
     %% get_all with order and limit n
     ?_assertEqual(2, length(test:select_all(user, "*", [], [{id, desc}, {accname}], [2]))),
     %% get_all with order and limit n,m
     ?_assertEqual(3, length(test:select_all(user, "*", [], [{id, desc}, {accname}], [{0,3}]))),
     %% get_all error
     ?_assertEqual(db_error, test:select_all(user, "1*", [])),
     %% 带有超时的get_all
     ?_assertEqual(5, length(db_mysqlutil:get_all(test, "SELECT * FROM user", 10000))),
     %% get_all error
     %?_assertEqual(db_error, db_mysqlutil:get_all(te1st, "SELECT * FROM user", 10000)),
     %% get_row
     ?_assertMatch([_, 2223, <<"iroowe">>, 0, 0, <<"拉风的神人">>], test:select_row(user, "*", [{accname, "iroowe"}])),
     %% get_row 失败
     ?_assertEqual(db_error, test:select_row(user, "*", [{status1, 0}])),
     %% get_row 返回[]
     ?_assertEqual([], test:select_row(user, "*", [{id, 1111111}])),

     %% insert_or_update, run insert
     ?_assertEqual(1, test:insert_or_update(user,
                                            [id, accid, accname, status, idcard_status, lastest_rolename], 
                                            [111111, 2221, "uc", 0, 0, "拉风的UC"]
                                           )),
     %% insert_or_update, run update
     ?_assertEqual(2, test:insert_or_update(user,
                                            [id, accid, accname, status, idcard_status, lastest_rolename], 
                                            [111111, 2220, "uc2", 0, 0, "UC的三国"]
                                           )),
     %% test insert_or_update really update
     ?_assertEqual([111111, 2220, <<"uc2">>, 0, 0, <<"UC的三国">>], test:select_row(user, "*", [{id, 111111}])),
     %% test get_one多条的时候只返回一条
     ?_assertMatch([_, _, _, 0, 0, _], test:select_row(user, "*", [{status, 0}])),

     %% 测试update key值等于非字符串
     ?_assertEqual(1, test:update(user, ["accname"], ["生姜三国"], "id", 111111)),
     %% 测试update key值等于字符串
     ?_assertEqual(1, test:update(user, ["accname"], ["生姜的三国"], "accname", "生姜三国")),
     %% 测试update key值等于二进制字符串
     ?_assertEqual(0, test:update(user, ["accname"], ["三国渣"], "accname", <<"生姜de三国">>)),
     ?_assertEqual(0, test:update(user, [{"accname","三国渣"}], [{"accname", <<"生姜de三国">>}])),
     %% 覆盖update残余分支
     ?_assertEqual(1, test:update(user, [{lastest_rolename, "测.*试"},{status, 1, add}, {idcard_status, -1, sub}, {id, 211111}], [{"accname", <<"生姜的三国">>}])),
     
     %% 测试另外一种insert
     ?_assertEqual(1, test:insert(user, 1, 
                                  [{id, 1}, {accid, 222}, {accname, "set酱"}, {lastest_rolename, "觉醒的肥仔"}])),
     %% 测试另外一种insert_or_update
     ?_assertEqual(2, test:insert_or_update(user,
                                  [{id, 1}, {accid, 222}, {accname, "set酱么么哒"}, {lastest_rolename, "觉醒的肥仔"}])),
     %% 类似insert_or_update
     ?_assertEqual(2, test:replace(user,
                                   [{id, 1}, {accid, 222}, {accname, "么么哒"}, {lastest_rolename, "兵的肥仔"}])),
     %% 测试有效delete
     ?_assertEqual(1, test:delete(user, [{id, 1}])),
     %% 测试无效delete
     ?_assertEqual(0, test:delete(user, [{id, 1}], [{id, desc}], [2])),

     %% 测试get_where_sql
     ?_assertEqual(2223, test:select_one(user, "accid", [{accname, in, ["iroowe"]}])),
     ?_assertEqual(2223, test:select_one(user, "accid", [{accname, "=", "iroowe"}])),
     ?_assertEqual(null, test:select_one(user, "accid", [{accname, "=", "iroowe"}, {id, -1}])),
     ?_assertEqual(null, test:select_one(user, "accid", [{accname, "=", "iroowe", "and"}, {id, -1}])),
     ?_assertEqual(null, test:select_one(user, "accid", [{accname, "=", "iroowe", "and"}, {id, "=", -1, "and"}])),
     ?_assertEqual(null, test:select_one(user, "accid", [{accname, "=", "iroowe", "and"}, {id, "=", -1, "or"},  {id, "=", -2}])),
     ?_assertEqual(null, test:select_one(user, "accid", [{accname, "=", "iroowe", "and"}, {id, "=", -1, "or"},  {id, "=", -2}])),
     %% ?_assertEqual({error, stop}, test:transaction(
	 %%    		    fun() -> 
	 %%    			    test:delete(user, []),
	 %%    			    throw({error, stop})
	 %%    		    end)),
     ?_assertEqual(6, length(test:select_all(user, "*", []))),
     %% ?_assertEqual(6, test:transaction(
     %% 			fun() -> 
     %% 				test:delete(user, [])
     %% 			end)),
     %?_assertEqual(0, length(test:select_all(user, "*", []))),
     ?_assertMatch({1, Id} when Id > 0, test:new_insert(user,  
                                                        [accid, accname, status, idcard_status, lastest_rolename], 
                                                        [3333, "roowe", 0, 0, "拉风烽火"]))
    ].
%%% HELPER FUNCTIONS %%%



