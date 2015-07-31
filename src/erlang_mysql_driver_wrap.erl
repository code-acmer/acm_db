%%% @author Roowe <bestluoliwe@gmail.com>
%%% @copyright (C) 2013, Roowe
%%% @doc
%%%
%%% @end
%%% Created : 10 Dec 2013 by Roowe <bestluoliwe@gmail.com>

-module(erlang_mysql_driver_wrap).
-include_lib("mysql/include/mysql.hrl").
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
    case mysql:fetch(Server, Sql) of
        {updated, Result} ->
            mysql:get_result_affected_rows(Result);
        {error, Result} ->
            mysql_halt([Server, Sql, Result])
    end.

execute_insert(Server, Sql) ->
    case mysql:fetch(Server, Sql) of
        {updated, Result} ->
            {mysql:get_result_affected_rows(Result), mysql:get_result_insert_id(Result)};
        {error, Result} ->
            mysql_halt([Server, Sql, Result])
    end.


%% 事务处理
transaction(Server, Fun) ->
    case mysql:transaction(Server, Fun) of
        {atomic, Result} ->
            Result;        
        {aborted, {Reason, {rollback_result, _Result}}} ->
	    Reason
    end.


%% 取出查询结果中的第一行第一列
%% 未找到时返回null
get_one(Server, Sql) ->
    case mysql:fetch(Server, Sql) of
        {data, Result} ->
            case mysql:get_result_rows(Result) of
                [] ->
                    null;
                [[R|_]|_] ->
                    R
            end;
        {error, Result} ->
            mysql_halt([Server, Sql, Result])
    end.

%% 取出查询结果中的第一行
get_row(Server, Sql) ->
    case mysql:fetch(Server, Sql) of
        {data, Result} ->
            case mysql:get_result_rows(Result) of
                [] ->
                    [];
                [R|_] ->
                    R
            end;
        {error, Result} ->
            mysql_halt([Server, Sql, Result])
    end.

%% 取出查询结果中的所有行
get_all(Server, Sql) ->
    case mysql:fetch(Server, Sql) of
        {data, Result} ->
            mysql:get_result_rows(Result);
        {error, Result} ->
            mysql_halt([Server, Sql, Result])
    end.

get_all(Server, Sql, Timeout) ->
    case mysql:fetch(Server, Sql, Timeout) of
        {data, Result} ->            
            mysql:get_result_rows(Result);
        {error, Result} ->
            mysql_halt([Server, Sql, Result])
    end.

%% @doc 显示人可以看得懂的错误信息
mysql_halt([_Server, Sql, Result]) when is_record(Result, mysql_result)->
    logger:error_msg(?MODULE, ?LINE, "SQL: ~ts, Reason: ~ts, ErrCode: ~w, ErrSqlState: ~ts~n",
                     [Sql, mysql:get_result_reason(Result), 
                      mysql:get_result_err_code(Result),
                      mysql:get_result_err_sql_state(Result)]),
    db_error;
mysql_halt([_Server, Sql, Reason])->
    logger:error_msg(?MODULE, ?LINE, "SQL: ~ts, Reason: ~p~n", [Sql, Reason]),
    db_error.
