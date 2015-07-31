%%% @author Roowe <bestluoliwe@gmail.com>
%%% @copyright (C) 2013, Roowe
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2013 by Roowe <bestluoliwe@gmail.com>

-module(db_mysqlutil).
-include_lib("mysql/include/mysql.hrl").
-compile([export_all]).
%-define(MYSQL_DRI, erlang_mysql_driver_wrap).
-define(MYSQL_DRI, emysql_wrap).
%% 执行一个SQL查询,返回影响的行数
execute(Server, Sql) ->
    ?MYSQL_DRI:execute(Server, Sql).

execute_insert(Server, Sql) ->
    ?MYSQL_DRI:execute_insert(Server, Sql).

%% 事务处理
transaction(Server, Fun) ->
    ?MYSQL_DRI:transaction(Server, Fun).

%% 取出查询结果中的第一行第一列
%% 未找到时返回null
get_one(Server, Sql) ->
    ?MYSQL_DRI:get_one(Server, Sql).

%% 取出查询结果中的第一行
get_row(Server, Sql) ->
    ?MYSQL_DRI:get_row(Server, Sql).

%% 取出查询结果中的所有行
get_all(Server, Sql) ->
    ?MYSQL_DRI:get_all(Server, Sql).

get_all(Server, Sql, Timeout) ->
    ?MYSQL_DRI:get_all(Server, Sql, Timeout).

%%组合mysql insert语句
%% 使用方式db_mysqlutil:make_insert_sql(test,["row","r"],["测试",123]) 相当 insert into `test` (row,r) values('测试','123')
%%Table:表名
%%Field：字段
%%Data:数据
make_insert_sql(Table_name, FieldList, ValueList) ->
    L = make_conn_sql(FieldList, ValueList, []),
    lists:concat(["insert into `", Table_name, "` set ", L]).

%%make_mul_insert_sql(player, [id, name], [[1,"aa"],[2,"bbb"]])
%%insert into `player` (id, name) VALUES (1, "2")()
make_mul_insert_sql(Table_name, FieldList, ValueList) ->
    V = inner_to_value_list_str(ValueList),
    F = inner_to_field_str(FieldList),
    lists:concat(["insert into `", Table_name, "` ", F, " VALUES ", V]).


inner_to_field_str(D) ->
    inner_to_field_str(D, []).
inner_to_field_str([D], L) ->
    L ++ ["`", hmisc:to_list(D), "`)"];
inner_to_field_str([D|Acc], []) ->
    inner_to_field_str(Acc, ["(`", hmisc:to_list(D), "`,"]);
inner_to_field_str([D|Acc], L) ->
    inner_to_field_str(Acc, L ++["`", hmisc:to_list(D), "`,"]).

inner_to_value_list_str(D) ->
    inner_to_value_list_str(D, []).
inner_to_value_list_str([D], L) ->
    L ++ inner_to_value_str(D);
inner_to_value_list_str([D|Acc], []) ->
    inner_to_value_list_str(Acc, inner_to_value_str(D)++[","]);
inner_to_value_list_str([D|Acc], L) ->
    inner_to_value_list_str(Acc, L ++ inner_to_value_str(D) ++ [","]).

inner_to_value_str(D) ->
    inner_to_value_str(D, []).
inner_to_value_str([D], L) ->
    L ++ ["'", get_sql_val(D), "')"];
inner_to_value_str([D|Acc], []) ->
    inner_to_value_str(Acc, ["('", get_sql_val(D), "',"]);
inner_to_value_str([D|Acc], L) ->
    inner_to_value_str(Acc, L ++["'", get_sql_val(D), "',"]).

%% 组合 mysql insert ignore on duplicate key update 语句
%% Table: 表名
%% Field: 字段
%% Value: 值
make_insert_or_update_sql(Table_name, FieldList, ValueList) ->
    L = make_conn_sql(FieldList, ValueList, []),
    lists:concat(["insert ignore into `", Table_name, "` set ", L,
                  "on duplicate key update", L]).

%% 组合mysql update语句
%% 使用方式db_mysqlutil:make_update_sql(test,["row","r"],["测试",123],"id",1) 相当
%% update `test` set row='测试', r = '123' where id = '1'
%%Table:表名
%%Field：字段
%%Data:数据
%%Key:键
%%Data:值
make_update_sql(Table_name, Field, Data, Key, Value) ->
    L = make_conn_sql(Field, Data, []),
    lists:concat(["update `", Table_name, "` set ", L, " where ", Key, "= '", hmisc:to_list(Value), "'"]).

make_conn_sql([], _, L ) ->
    L ;
make_conn_sql(_, [], L ) ->
    L ;
make_conn_sql([F | T1], [D | T2], []) ->
    %%  F1 = hmisc:to_list(F),
    %%  if 
    %%          F1 == "other" ->
    %%                  make_conn_sql(T1, T2, []);
    %%          true ->
    %%                  L  = ["`", F1, "`='",get_sql_val(D),"'"],
    %%              make_conn_sql(T1, T2, L)
    %%  end;
    L  = ["`", hmisc:to_list(F), "`='",get_sql_val(D),"'"],
    make_conn_sql(T1, T2, L);
make_conn_sql([F | T1], [D | T2], L) ->
    %%  F1 = hmisc:to_list(F),
    %%  if 
    %%          F1 == "other" ->
    %%                  make_conn_sql(T1, T2, L);
    %%          true ->
    %%                           L1  = L ++ [",`", hmisc:to_list(F),"`='",get_sql_val(D),"'"],
    %%                     make_conn_sql(T1, T2, L1)
    %%  end.
    L1  = L ++ [",`", hmisc:to_list(F),"`='",get_sql_val(D),"'"],
    make_conn_sql(T1, T2, L1).

get_sql_val(Val) ->
    case is_binary(Val) orelse is_list(Val) of 
        true ->
            re:replace(hmisc:to_list(Val),"'","''",[global,{return,list}]);
        _-> 
            hmisc:to_list(Val)
    end.

make_insert_sql(Table_name, Field_Value_List) ->
    %%  db_mysqlutil:make_insert_sql(player, 
    %%                         [{status, 0}, {online_flag,1}, {hp,50}, {mp,30}]).
    Len = length(Field_Value_List),
    {Vsql, _Count1} =
        lists:mapfoldl(
          fun(Field_value, Sum) ->      
                  Expr = 
                      case Field_value of
                          {Field, Val} -> 
                              case is_binary(Val) orelse is_list(Val) of 
                                  true ->
                                      io_lib:format(
                                        "`~s`='~s'",
                                        [Field, re:replace(Val, "'", "''", [global, {return, binary}])]);
                                  _->
                                      io_lib:format("`~s`='~p'",[Field, Val])
                              end
                      end,
                  S1 =
                      if
                          Sum == Len->
                              io_lib:format("~s ", [Expr]);
                          true ->
                              io_lib:format("~s,", [Expr])
                      end,
                  {S1, Sum+1}
          end,
          1, Field_Value_List),
    lists:concat(["insert into `", Table_name, "` set ", lists:flatten(Vsql)]).

make_insert_or_update_sql(Table_name, Field_Value_List) ->
    %%  db_mysqlutil:make_insert_sql(player, 
    %%                         [{status, 0}, {online_flag,1}, {hp,50}, {mp,30}]).
    Len = length(Field_Value_List),
    {Vsql, _Count1} =
        lists:mapfoldl(
          fun(Field_value, Sum) ->      
                  Expr =
                      case Field_value of
                          {Field, Val} -> 
                              case is_binary(Val) orelse is_list(Val) of 
                                  true ->
                                      io_lib:format(
                                        "`~s`='~s'",
                                        [Field, re:replace(Val, "'", "''", [global,{return,binary}])]);
                                  _->
                                      io_lib:format("`~s`='~p'", [Field, Val])
                              end
                      end,
                  S1 =
                      if
                          Sum == Len ->
                              io_lib:format("~s ",[Expr]);
                          true ->
                              io_lib:format("~s,",[Expr])
                      end,
                  {S1, Sum + 1}
          end,
          1, Field_Value_List),
    L = lists:flatten(Vsql),
    lists:concat(["insert ignore into `", Table_name, "` set ", L,
                  "on duplicate key update ", L]).

make_replace_sql(Table_name, Field_Value_List) ->
    %%  db_mysqlutil:make_replace_sql(player, 
    %%                         [{status, 0}, {online_flag,1}, {hp,50}, {mp,30}]).
    Len = length(Field_Value_List),
    {Vsql, _Count1} =
        lists:mapfoldl(
          fun(Field_value, Sum) ->
                  Expr =
                      case Field_value of
                          {Field, Val} -> 
                              case is_binary(Val) orelse is_list(Val) of 
                                  true ->
                                      io_lib:format(
                                        "`~s`='~s'",
                                        [Field, re:replace(Val,"'","''",[global,{return,binary}])]);
                                  _->
                                      io_lib:format("`~s`=~p",[Field, Val])
                              end
                      end,
                  S1 =
                      if 
                          Sum == Len ->
                              io_lib:format("~s ",[Expr]);
                          true ->
                              io_lib:format("~s,",[Expr])
                      end,
                  {S1, Sum + 1}
          end,
          1, Field_Value_List),
    lists:concat(["replace into `", Table_name, "` set ", lists:flatten(Vsql)]).

make_update_sql(Table_name, Field_Value_List, Where_List) ->
    %%  db_mysqlutil:make_update_sql(player, 
    %%                         [{status, 0}, {online_flag,1}, {hp,50, add}, {mp,30,sub}],
    %%                         [{id, 11}]).
    Len = length(Field_Value_List),
    {Vsql, _Count1} =
        lists:mapfoldl(
          fun(Field_value, Sum) ->      
                  Expr = 
                      case Field_value of
                          {Field, Val, add} ->
                              io_lib:format("`~s`=`~s`+~p", [Field, Field, Val]);
                          {Field, Val, sub} ->
                              io_lib:format("`~s`=`~s`-~p", [Field, Field, Val]);
                          {Field, Val} -> 
                              case is_binary(Val) orelse is_list(Val) of 
                                  true ->
                                      io_lib:format(
                                        "`~s`='~s'", [Field, re:replace(Val,"'","''",[global,{return,binary}])]);
                                  _->
                                      io_lib:format("`~s`='~p'",[Field, Val])
                              end
                      end,
                  S1 =
                      if
                          Sum == Len ->
                              io_lib:format("~s ",[Expr]);
                          true ->
                              io_lib:format("~s,",[Expr])
                      end,
                  {S1, Sum+1}
          end,
          1, Field_Value_List),
    {Wsql, Count2} = get_where_sql(Where_List),
    WhereSql = 
        if
            Count2 > 1 ->
                lists:concat(["where ", lists:flatten(Wsql)]);
            true ->
                ""
        end,
    lists:concat(["update `", Table_name, "` set ",
                  lists:flatten(Vsql), WhereSql, ""]).

make_delete_sql(TableName, WhereList) ->
    %% db_mysqlutil:make_delete_sql(player, [{id, "=", 11, "and"},{status, 0}]).
    make_delete_sql(TableName, WhereList, [], []).
%% {Wsql, Count2} = get_where_sql(Where_List),
%% WhereSql = 
%%      if
%%         Count2 > 1 ->
%%             lists:concat(["where ", lists:flatten(Wsql)]);
%%         true ->
%%             ""
%%      end,
%% lists:concat(["delete from `", Table_name, "` ", WhereSql, ""]).

make_delete_sql(TableName, WhereList, OrderList, LimitNum) ->
    %% db_mysqlutil:make_delete_sql(player, [{id, Uid}], [{id, desc}], []).
    {Wsql, Count1} = get_where_sql(WhereList),
    WhereSql =
        if
            Count1 > 1 ->
                lists:concat(["where ", lists:flatten(Wsql)]);
            true -> 
                ""
        end,
    {Osql, Count2} = get_order_sql(OrderList),
    OrderSql = 
        if
            Count2 > 1->
                lists:concat(["order by ", lists:flatten(Osql)]);
            true -> 
                ""
        end,
    LimitSql = case LimitNum of
                   [] ->
                       "";
                   [Num] ->
                       %% 限定指定数量的 n 调记录
                       lists:concat(["limit ", Num])
               end,
    lists:concat(["delete from `", TableName, "` ", WhereSql, OrderSql, LimitSql]).

make_select_sql(Table_name, Fields_sql, Where_List) ->
    make_select_sql(Table_name, Fields_sql, Where_List, [], []).

make_select_sql(Table_name, Fields_sql, Where_List, Order_List, Limit_num) ->
    %% db_mysqlutil:make_select_sql(player, "*", [{status, 1}], [{id,desc},{status}],[]).
    %% db_mysqlutil:make_select_sql(player, "id, status", [{id, 11}], [{id,desc},{status}],[]).
    {Wsql, Count1} = get_where_sql(Where_List),
    WhereSql = 
        if
            Count1 > 1 ->
                lists:concat(["where ", lists:flatten(Wsql)]);
            true ->
                ""
        end,
    {Osql, Count2} = get_order_sql(Order_List),
    OrderSql = 
        if
            Count2 > 1 ->
                lists:concat(["order by ", lists:flatten(Osql)]);
            true ->
                ""
        end,
    LimitSql =
        case Limit_num of
            [] ->
                "";
            [{Start, Num}] ->
                %% 从指定的位置开始的 n 条记录
                lists:concat(["limit ", Start, ", ", Num]);
            [Num] ->
                %% 限定指定数量的 n 调记录
                lists:concat(["limit ", Num])
        end,
    lists:concat(["select ", Fields_sql," from `", Table_name, "` ", WhereSql, OrderSql, LimitSql]).

get_order_sql(Order_List) ->
    %% 排序用列表方式：[{id, desc},{status}]
    Len = length(Order_List),
    lists:mapfoldl(
      fun(Field_Order, Sum) ->  
              Expr = 
                  case Field_Order of   
                      {Field, Order} ->
                          io_lib:format("~p ~p",[Field, Order]);
                      {Field} ->
                          io_lib:format("~p",[Field])
                  end,
              S1 = if Sum == Len -> io_lib:format("~s ",[Expr]);
                      true ->   io_lib:format("~s,",[Expr])
                   end,
              {S1, Sum+1}
      end,
      1, Order_List).

get_where_sql(Where_List) ->
    %%  条件用列表方式：[{},{},{}]
    %%  每一个条件形式(一共三种)：
    %%          1、{idA, "<>", 10, "or"}        <===> {字段名, 操作符, 值，下一个条件的连接符}
    %%      2、{idB, ">", 20}           <===> {idB, ">", 20，"and"}
    %%      3、{idB, 20}                                <===> {idB, "=", 20，"and"}             
    %%      4、{idA, in, [_, _, _]}     <===> {idA, in, "(X, Y, Z)"}
    Len = length(Where_List),
    lists:mapfoldl(
      fun(Field_Operator_Val, Sum) ->   
              [Expr, Or_And_1] = 
                  case Field_Operator_Val of   
                      {Field, in, List} when is_list(List)->
                          %% case is_list(List) of
                          %%     false ->
                          %%         [io_lib:format("`~s` in('~p')", [Field, List]),"and"];
                          %%     true ->
                          [_|Values] = lists:foldl(
                                         fun(L, Result) -> 
                                                 lists:concat([Result,",'",L,"'"]) 
                                         end, "", List),
                          [lists:concat([Field," in (",Values,")"]), "and"];

                      %%end;
                      {Field, Operator, Val, Or_And} ->
                          case is_binary(Val) orelse is_list(Val) of 
                              true ->
				  %% 对于下面几个try，我实在想不通正常为啥会被挂掉，先改了，挂了再fix bug
                                  %%try
				  [io_lib:format(
				     "`~s`~s'~s'",
				     [Field, Operator, 
				      re:replace(Val, "'", "''", [global, {return, binary}])]), 
				   Or_And];
                                  %% catch
                                  %%     _:_ ->
                                  %%         [io_lib:format("`~s`~s'~p'",[Field, Operator, Val]), Or_And]
                                  %% end;
                              _->
                                  [io_lib:format("`~s`~s'~p'",[Field, Operator, Val]), Or_And]
                          end;
                      {Field, Operator, Val} ->
                          case is_binary(Val) orelse is_list(Val) of 
                              true ->
                                  %%try 
				  [io_lib:format(
				     "`~s`~s'~s'", 
				     [Field, Operator, re:replace(Val,"'","''",[global,{return,binary}])]), "and"];
                                  %% catch
                                  %%     _:_ ->
                                  %%         [io_lib:format("`~s`~s'~p'",[Field, Operator, Val]),"and"]
                                  %% end;
                              _ ->
                                  [io_lib:format("`~s`~s'~p'",[Field, Operator, Val]),"and"]
                          end;
                      {Field, Val} ->  
                          case is_binary(Val) orelse is_list(Val) of 
                              true ->
                                  %try 
				  [io_lib:format(
				     "`~s`='~s'",
				     [Field, re:replace(Val,"'","''",[global,{return,binary}])]), "and"];
				  %catch
                                  %    _:_ ->
                                  %        [io_lib:format("`~s`='~p'",[Field, Val]), "and"]
                                  %end;
                              _ ->
                                  [io_lib:format("`~s`='~p'",[Field, Val]), "and"]
                          end
                  end,
              S1 =
                  if
                      Sum == Len ->
                          io_lib:format("~s ",[Expr]);
                      true ->
                          io_lib:format("~s ~s ",[Expr, Or_And_1])
                  end,
              {S1, Sum + 1}
      end,
      1, Where_List).

