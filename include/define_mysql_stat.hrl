-ifndef(DEFINE_MYSQL_STAT).
-define(DEFINE_MYSQL_STAT, true).
-define(ETS_DB_STAT, ets_db_stat).
-record(rec_db_stat, {
          key = "" :: list(),
          table = undefined :: atom(),
          operation = undefined :: atom(),
          last_operate_timestamp = 0 :: integer(),
          count = 1 :: integer()
         }).
-endif.
