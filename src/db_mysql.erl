%%% @author Roowe <bestluoliwe@gmail.com>
%%% @copyright (C) 2013, Roowe
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2013 by Roowe <bestluoliwe@gmail.com>

-module(db_mysql).
-export([
         insert/5,
         insert/4,
         mul_insert/4,
         insert_or_update/3,
         insert_or_update/4,
         replace/3,
         update/4,
         update/6,
         select_one/4,
         select_one/6,
         select_row/4,
         select_row/6,
         select_count/3,
         select_all/4,
         select_all/6,
         delete/3,
         delete/5,
         transaction/2,
         new_insert/4,
         new_insert/3
        ]).

%% 插入数据表
insert(Server, Table_name, _Server_no, FieldList, ValueList) ->
    Sql = db_mysqlutil:make_insert_sql(Table_name, FieldList, ValueList),
    db_mysqlutil:execute(Server, Sql).
insert(Server, Table_name, _Server_no, Field_Value_List) ->
    Sql = db_mysqlutil:make_insert_sql(Table_name, Field_Value_List),
    db_mysqlutil:execute(Server, Sql).

%% 插入数据表
new_insert(Server, Table_name, FieldList, ValueList) ->
    Sql = db_mysqlutil:make_insert_sql(Table_name, FieldList, ValueList),
    db_mysqlutil:execute_insert(Server, Sql).
new_insert(Server, Table_name, Field_Value_List) ->
    Sql = db_mysqlutil:make_insert_sql(Table_name, Field_Value_List),
    db_mysqlutil:execute_insert(Server, Sql).


mul_insert(_Server, _Table_name, _FieldList, []) ->
    0;
mul_insert(Server, Table_name, FieldList, ValueList) ->
    Sql = db_mysqlutil:make_mul_insert_sql(Table_name, FieldList, ValueList),
    db_mysqlutil:execute(Server, Sql).

%% 插入，如果有则更新
%% 使用ON DUPLICATE KEY UPDATE，如果数据行插入成功，则返回的受影响的行数为1；如果更新了老的数据行，返回的首影响行是2。 
insert_or_update(Server, TableName, FieldList, ValueList) ->
    Sql = db_mysqlutil:make_insert_or_update_sql(TableName, FieldList, ValueList),
    db_mysqlutil:execute(Server, Sql).
insert_or_update(Server, TableName, FieldValueList) ->
    Sql = db_mysqlutil:make_insert_or_update_sql(TableName, FieldValueList),
    db_mysqlutil:execute(Server, Sql).

%% 修改数据表(replace方式)
replace(Server, Table_name, Field_Value_List) ->
    Sql = db_mysqlutil:make_replace_sql(Table_name, Field_Value_List),
    db_mysqlutil:execute(Server, Sql).

%% 修改数据表(update方式)
update(Server, Table_name, Field_Value_List, Where_List) ->
    Sql = db_mysqlutil:make_update_sql(Table_name, Field_Value_List, Where_List),
    db_mysqlutil:execute(Server, Sql).

update(Server, Table_name, Field, Data, Key, Value) ->
    Sql = db_mysqlutil:make_update_sql(Table_name, Field, Data, Key, Value),
    db_mysqlutil:execute(Server, Sql).

%% 获取一个数据字段
select_one(Server, Table_name, Fields_sql, Where_List) ->
    Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List, [], [1]),
    db_mysqlutil:get_one(Server, Sql).

select_one(Server, Table_name, Fields_sql, Where_List, Order_List, Limit_num) ->
    Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List, Order_List, Limit_num),
    db_mysqlutil:get_one(Server, Sql).

%% 获取一条数据记录
select_row(Server, Table_name, Fields_sql, Where_List) ->
    Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List, [], [1]),
    db_mysqlutil:get_row(Server, Sql).

select_row(Server, Table_name, Fields_sql, Where_List, Order_List, Limit_num) ->
    Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List, Order_List, Limit_num),
    db_mysqlutil:get_row(Server, Sql).

%% 获取记录个数 
select_count(Server, Table_name, Where_List) ->
    select_row(Server, Table_name, "count(1)", Where_List).

%% 获取所有数据
select_all(Server, Table_name, Fields_sql, Where_List) ->
    Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List),
    db_mysqlutil:get_all(Server, Sql).
select_all(Server, Table_name, Fields_sql, Where_List, Order_List, Limit_num) ->
    Sql = db_mysqlutil:make_select_sql(Table_name, Fields_sql, Where_List, Order_List, Limit_num),
    db_mysqlutil:get_all(Server, Sql).

%% 删除数据
delete(Server, Table_name, Where_List) ->
    Sql = db_mysqlutil:make_delete_sql(Table_name, Where_List),
    db_mysqlutil:execute(Server, Sql).

%% 删除数据
delete(Server, TableName, WhereList, OrderList, LimitNum) ->
    Sql = db_mysqlutil:make_delete_sql(TableName, WhereList, OrderList, LimitNum),
    db_mysqlutil:execute(Server, Sql).

%% 事务处理
transaction(Server, Fun) ->
    db_mysqlutil:transaction(Server, Fun).


