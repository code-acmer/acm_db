%%% @author Roowe <bestluoliwe@gmail.com>
%%% @copyright (C) 2013, Roowe
%%% @doc
%%%
%%% @end
%%% Created : 10 Dec 2013 by Roowe <bestluoliwe@gmail.com>

-module(emysql_wrap).
-include_lib("emysql/include/emysql.hrl").
-export([execute/2,
         execute_insert/2,
         transaction/2,
         get_one/2,
         get_row/2,
         get_all/2,
         get_all/3
        ]).


%% 执行一个SQL查询,返回影响的行数
execute(Server, Sql) ->
    case emysql:execute(Server, Sql) of
        Result when is_record(Result, ok_packet) ->
            emysql_util:affected_rows(Result);
        Result when is_record(Result, error_packet) ->
            mysql_halt([Server, Sql, Result])
    end.

execute_insert(Server, Sql) ->
    case emysql:execute(Server, Sql) of
        Result when is_record(Result, ok_packet) ->
            {emysql_util:affected_rows(Result), emysql_util:insert_id(Result)};
        Result when is_record(Result, error_packet) ->
            mysql_halt([Server, Sql, Result])
    end.

%% 事务处理
transaction(Server, _Fun) ->
    mysql_halt([Server, "emysql not support like mnesia transaction, please use stored_procedure, look emysql/samples/e_stored_procedure.erl"]).

%% 取出查询结果中的第一行第一列
%% 未找到时返回null
get_one(Server, Sql) ->
    case emysql:execute(Server, Sql) of
        #result_packet{
           rows = Result
          } ->
            case Result of
                [] ->
                    null;
                [[R|_]|_] ->
                    R
            end;
        Result when is_record(Result, error_packet) ->
            mysql_halt([Server, Sql, Result])
    end.

%% 取出查询结果中的第一行
get_row(Server, Sql) ->
    case emysql:execute(Server, Sql) of
        #result_packet{
           rows = Result
          } ->
            case Result of
                [] ->
                    [];
                [R|_] ->
                    R
            end;
        Result when is_record(Result, error_packet) ->
            mysql_halt([Server, Sql, Result])
    end.

%% 取出查询结果中的所有行
get_all(Server, Sql) ->
    case emysql:execute(Server, Sql) of
        #result_packet{
           rows = Result
          } ->
            Result;
        Result when is_record(Result, error_packet) ->
            mysql_halt([Server, Sql, Result])
    end.

get_all(Server, Sql, Timeout) ->
    case emysql:execute(Server, Sql, Timeout) of
        #result_packet{
           rows = Result
          } ->
            Result;
        Result when is_record(Result, error_packet) ->
            mysql_halt([Server, Sql, Result])
    end.

%% @doc 显示人可以看得懂的错误信息
mysql_halt([_Server, Sql, #error_packet{
                             code = Code,
                             status = Status,
                             msg = Msg
                            }]) ->
    logger:error_msg(?MODULE, ?LINE, "SQL: ~ts, Reason: ~ts, ErrCode: ~w, ErrSqlState: ~ts~n", [Sql, Msg, Code, Status]),
    db_error;
mysql_halt([_Server, Sql, Reason])->
    logger:error_msg(?MODULE, ?LINE, "SQL: ~ts, Reason: ~p~n", [Sql, Reason]),
    db_error.
