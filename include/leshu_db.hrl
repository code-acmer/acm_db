%% 启动一个连接池的参数配置
-ifndef(LESHU_DB_HRL).
-define(LESHU_DB_HRL, true).

-record(db_conf, {
          mod_name="" :: list(),
          poll=pxx_game :: atom(),
          host="localhost" :: list(),
          port=3306 :: integer(),
          username="root" :: list(),
          password="root" :: list(),
          database="test" :: list(),
          worker=33 :: integer(),
          db_type=db_mysql :: atom()
         }).

%% Mnesia 统计结构
-record(mnesia_statistics, {key,
                            node,
                            table,
                            op,
                            count = 0,
                            total_time = 0
                           }).

-endif.
