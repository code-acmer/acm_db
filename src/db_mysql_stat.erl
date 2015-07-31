%%% @author Roowe <bestluoliwe@gmail.com>
%%% @copyright (C) 2013, Roowe
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2013 by Roowe <bestluoliwe@gmail.com>

-module(db_mysql_stat).
-include("define_mysql_stat.hrl").
-include("leshu_db_common.hrl").

-export([stat_db_access/2]).

%%统计数据表操作次数和频率
stat_db_access(TableName, Operation) 
  when is_atom(TableName)->
    try
        Key = lists:concat([TableName, "/", Operation]),
        NewDbStat = 
            case hetsutil:lookup_one(?ETS_DB_STAT, Key) of
                [] ->
                    #rec_db_stat{
                       key = Key,
                       table = TableName,
                       operation = Operation,
                       last_operate_timestamp = hmisctime:unixtime()
                      };
                #rec_db_stat{
                   count = Count
                  }=DbStat ->
                    DbStat#rec_db_stat{
                      count = Count + 1,
                      last_operate_timestamp = hmisctime:unixtime()
                     }
            end,        
        hetsutil:insert(?ETS_DB_STAT, NewDbStat),
        ok
    catch
        Type:Error -> 
            ?ERROR_MSG("stat_db_access failed, ~w, ~w, ~p~n", [Type, Error, erlang:get_stacktrace()]),
            no_stat
    end;
stat_db_access(_, _) ->
    no_stat.
