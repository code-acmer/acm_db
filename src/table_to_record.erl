-module(table_to_record).
-define(CONFIG_FILE, "../config/server.app").
-define(TMP_TABLE_PATH, "./tmptable/").
-define(SRC_TABLE_PATH, "../src/table/").
-define(INCLUDE_PATH,   "../include/").
-define(RECORD_FILENAME, "../include/table_to_record.hrl").
-define(BEAM_PATH, "./"). 

-define(ETS_MYSQL_COLUMN, ets_mysql_column).
-define(ETS_MYSQL_TABLE, ets_mysql_table).

%% mysql information_schema库的columns表
-record(rec_mysql_column, {
          table_catalog,
          table_schema,
          table_name,
          column_name,
          ordinal_position,
          column_default,
          is_nullable,
          data_type,
          character_maximum_length,
          character_octet_length,
          numeric_precision,
          numeric_scale,
          character_set_name,
          collation_name,
          column_type,
          column_key,
          extra,
          privileges,
          column_comment
         }).
-record(rec_mysql_table, {
          table_catalog,
          table_schema,
          table_name,
          table_type,
          engine,
          version,
          row_format,
          table_rows,
          avg_row_length,
          data_length,
          max_data_length,
          index_length,
          data_free,
          auto_increment,
          create_time,
          update_time,
          check_time,
          table_collation,
          checksum,
          create_options,
          table_comment
         }).

%%
%% Exported Functions
%%
-export([start/0]). 

%%
%% API Functions
%%
init() ->
    hloglevel:set(6),
    ok = application:start(mysql),
    ok = application:start(crypto),
    ok = application:start(emysql),
    ok = application:start(hstdlib),
    ok = application:start(leshu_db),
    ets:new(?ETS_MYSQL_COLUMN, 
            [{keypos, #rec_mysql_column.table_name}, 
             named_table, public, bag]),
    ets:new(?ETS_MYSQL_TABLE, 
            [{keypos, #rec_mysql_table.table_name}, 
             named_table, public, bag]).

get_db_config() ->
    case code:priv_dir(leshu_db) of
        {error, _} ->
            catch error(not_priv_dir);
        PrivDir ->
            Filename = filename:join(PrivDir, "table_to_record.conf"),
            case file:consult(Filename) of
                {ok, [H, Tables|_]=_Result} ->
                    %%error_logger:error_msg("Result ~p~n", [Result]),
                    {H, Tables};
                _ ->
                    catch error(not_table_to_record_conf)
            end
    end.


start() ->    
    init(),
    {DbConf, Tables} = get_db_config(),
    Host = proplists:get_value(host, DbConf),
    Port = proplists:get_value(port, DbConf),
    User = proplists:get_value(user, DbConf),
    Password = proplists:get_value(password, DbConf),
    DbList = proplists:get_value(dblist, DbConf),
    IncludePath = proplists:get_value(include_path, DbConf),
    NewDbList = lists:map(fun(X) ->
                                  list_to_binary(lists:concat([X]))
                          end, DbList),
    emysql:add_pool(db001, 1, User, Password, Host, Port, "information_schema", utf8),
    %mysql:start_link(db001, Host, Port, User, Password, "information_schema", fun(_,_,_,_)->ok end, utf8),
    TableDbHash = lists:foldl(fun(Db, Acc) ->
                        %mysql:fetch(db001, lists:concat(["USE ", Db])),
                                      emysql:execute(db001, lists:concat(["USE ", Db])),
                                      case db_mysqlutil:get_all(db001, "SHOW TABLES") of
                                          [] ->
                                              error(not_db_info);
                                          TableList ->
                                              lists:map(fun([Table]) ->
                                                                {binary_to_atom(Table, latin1), Db}
                                                        end, TableList)
                                      end ++ Acc
                end, [], DbList),
    io:format("Begin table_to_record......~n"),            
    tables_to_record(Tables, IncludePath, TableDbHash),
    io:format("table_to_record,This is OK!~n"),

    application:stop(mysql),
    application:stop(hstdlib),    
    application:stop(leshu_db),
    halt(), 
    ok.

tables_to_record(Tables, IncludePath, TableDbHash) ->    
    lists:foreach(
      fun(Conf)-> 
              case Conf of 
                  {TableName, RecordName} -> 
                      case proplists:get_value(TableName, TableDbHash) of
                          undefined ->
                              io:format("can't find ~p, please add db~n", [TableName]);
                          Db ->
                              table_to_record(lists:concat([Db, ".", TableName]), RecordName, IncludePath)
                      end;                            
                  _-> 
                      no_action
              end    
      end, 
      Tables),
    io:format("finished!~n~n"),    
    ok.
%% Field | Type | Collation | Null | Key | Default | Extra | Privileges | Comment
% init_exp | int(11) | NULL | NO |     | NULL |  | select,insert,update,references | 初始拥有的经验
table_to_record(TableName, RecordName, IncludePath) ->
    [_, DbTableName] = string:tokens(TableName, "."),
    case db_mysqlutil:get_all(db001, lists:concat(["SHOW FULL FIELDS FROM  ", TableName])) of
        [] ->
            io:format("something wrong~n", []),
            ingore;
        ColumnList ->
            Len = length(ColumnList),
            {WriteList0, _} = 
                lists:foldl(
                  fun ([ColumnName, Type, Collation, Null, Key, ColumnDefault, Extra, Privileges, ColumnComment], {In, Sum}) ->
                          [DataType|_] = binary:split(Type, <<"(">>),
                          %io:format("~p~n", [DataType]),
                          OutTemp = 
                              if 
                                  Sum == 1 ->
                                      In ++
                                          [
                                           "%%%------------------------------------------------\t\n",
                                           io_lib:format("%%% File    : db_~s.hrl\t\n", [DbTableName]),
                                           "%%% Description: 从mysql表生成的record\t\n",
                                           "%%% Warning:  由程序自动生成，请不要随意修改！\t\n",
                                           "%%%------------------------------------------------    \t\n",
                                           "\t\n",
                                           io_lib:format("-ifndef(DB_~s_HRL).\t\n",
                                                         [string:to_upper(hmisc:to_list(DbTableName))]),
                                           io_lib:format("-define(DB_~s_HRL, true).\t\n",
                                                         [string:to_upper(hmisc:to_list(DbTableName))]),
                                           "\t\n",
                                           list_to_binary(
                                             io_lib:format("%% ~s\t\n", [""])),
                                           list_to_binary(
                                             io_lib:format("%% ~s ==> ~s \t\n", [DbTableName, RecordName])),
                                           list_to_binary(
                                             io_lib:format("-record(~s, {\t\n", [RecordName]))
                                          ];
                                  true -> 
                                      In
                              end,
                          %% io:format("Field_comment = ~p~n",[Field_comment]),
                          Default = 
                              case ColumnDefault of
                                  undefined ->
                                      '';
                                  <<>> -> 
                                      case mysql_data_type(DataType) of
                                          integer ->
                                              " = 0";
                                          binary ->
                                              " = \"\"";
                                          _ -> 
                                              '' 
                                      end;
                                  <<"[]">> ->
                                      lists:concat([" = ", binary_to_list(ColumnDefault)]);
                                  Val -> 
                                      case mysql_data_type(DataType) of
                                          binary -> 
                                              lists:concat([" = <<\"", binary_to_list(Val) ,"\">>"]);
                                          _ -> 
                                              lists:concat([" = ", binary_to_list(Val)])
                                      end
                              end,
                          T1 = 
                              if
                                  Sum == Len -> 
                                      '';
                                  true ->
                                      ','
                              end,
                          %% io:format("Field[~p] = ~p / ~p ~n",[Sum, Field, Field_comment]),
                          T2 = io_lib:format("~s~s~s",
                                             [ColumnName, Default, T1]),
                          %% io:format("T2_len= ~p/~p ~n",[length(T2), T2]),
                          T3 = lists:duplicate(40 - length(lists:flatten(T2)), " "),
                          {OutTemp ++
                               [list_to_binary(io_lib:format("      ~s~s%% ~s\t\n",
                                                             [T2, T3, ColumnComment]))],
                           Sum + 1}
                  end,
                  {[], 1},
                  ColumnList),           
            hfile:writelines_new(
              IncludePath ++ io_lib:format("db_~s.hrl", [DbTableName]),
              WriteList0 ++ 
                  [list_to_binary(io_lib:format("    }).\t\n",[]))] ++
                  ["\t\n-endif.\t\n"]),
            io:format("                 ~s ==> ~s ~n",
                      [TableName, RecordName]),
            ok

end.    

%% table_to_record(TableName, RecordName, IncludePath) ->
%%     case ets:lookup(?ETS_MYSQL_TABLE, TableName) of
%%         [] ->
%%             catch error({not_table_info, TableName});
%%         [#rec_mysql_table{
%%             table_comment = TableComment
%%            }|Acc] when Acc =:= [] ->
%%             case ets:lookup(?ETS_MYSQL_COLUMN, TableName) of
%%                 [] ->
%%                     catch error({not_column_info, TableName});
%%                 ColumnList ->
%%                     Len = length(ColumnList),
%%                     {WriteList0, _} = 
%%                         lists:foldl(
%%                           fun(#rec_mysql_column{
%%                                  column_name = ColumnName,
%%                                  column_default = ColumnDefault,
%%                                  data_type = DataType,
%%                                  column_comment = ColumnComment
%%                                 }, {In, Sum}) ->

%%                                   OutTemp = 
%%                                       if 
%%                                           Sum == 1 ->
%%                                               In ++
%%                                                   [
%%                                                    "%%%------------------------------------------------\t\n",
%%                                                    io_lib:format("%%% File    : db_~s.hrl\t\n", [TableName]),
%%                                                    "%%% Description: 从mysql表生成的record\t\n",
%%                                                    "%%% Warning:  由程序自动生成，请不要随意修改！\t\n",
%%                                                    "%%%------------------------------------------------    \t\n",
%%                                                    "\t\n",
%%                                                    io_lib:format("-ifndef(DB_~s_HRL).\t\n",
%%                                                                  [string:to_upper(hmisc:to_list(TableName))]),
%%                                                    io_lib:format("-define(DB_~s_HRL, true).\t\n",
%%                                                                  [string:to_upper(hmisc:to_list(TableName))]),
%%                                                    "\t\n",
%%                                                    list_to_binary(
%%                                                      io_lib:format("%% ~s\t\n", [TableComment])),
%%                                                    list_to_binary(
%%                                                      io_lib:format("%% ~s ==> ~s \t\n", [TableName, RecordName])),
%%                                                    list_to_binary(
%%                                                      io_lib:format("-record(~s, {\t\n", [RecordName]))
%%                                                   ];
%%                                           true -> 
%%                                               In
%%                                       end,
%%                                   %% io:format("Field_comment = ~p~n",[Field_comment]),
%%                                   Default = 
%%                                       case ColumnDefault of
%%                                           undefined ->
%%                                               '';
%%                                           <<>> -> 
%%                                               case mysql_data_type(DataType) of
%%                                                   integer ->
%%                                                       " = 0";
%%                                                   binary ->
%%                                                       " = \"\"";
%%                                                   _ -> 
%%                                                       '' 
%%                                               end;
%%                                           <<"[]">> ->
%%                                               lists:concat([" = ", binary_to_list(ColumnDefault)]);
%%                                           Val -> 
%%                                               case mysql_data_type(DataType) of
%%                                                   binary -> 
%%                                                       lists:concat([" = <<\"", binary_to_list(Val) ,"\">>"]);
%%                                                   _ -> 
%%                                                       lists:concat([" = ", binary_to_list(Val)])
%%                                               end
%%                                       end,
%%                                   T1 = 
%%                                       if
%%                                           Sum == Len -> 
%%                                               '';
%%                                           true ->
%%                                               ','
%%                                       end,
%%                                   %% io:format("Field[~p] = ~p / ~p ~n",[Sum, Field, Field_comment]),
%%                                   T2 = io_lib:format("~s~s~s",
%%                                                      [ColumnName, Default, T1]),
%%                                   %% io:format("T2_len= ~p/~p ~n",[length(T2), T2]),
%%                                   T3 = lists:duplicate(40 - length(lists:flatten(T2)), " "),
%%                                   {OutTemp ++
%%                                        [list_to_binary(io_lib:format("      ~s~s%% ~s\t\n",
%%                                                                      [T2, T3, ColumnComment]))],
%%                                    Sum + 1}
%%                           end,
%%                           {[], 1},
%%                           ColumnList),

%%                     hfile:writelines_new(
%%                       IncludePath ++ io_lib:format("db_~s.hrl", [TableName]),
%%                       WriteList0 ++ 
%%                           [list_to_binary(io_lib:format("    }).\t\n",[]))] ++
%%                           ["\t\n-endif.\t\n"]),
%%                     io:format("                 ~s ==> ~s ~n",
%%                               [TableName, RecordName]),
%%                     ok
%%             end
%%     end.    

mysql_data_type(<<"bigint">>) ->
    integer;
mysql_data_type(<<"smallint">>) ->
    integer;
mysql_data_type(<<"tinyint">>) ->
    integer;
mysql_data_type(<<"int">>) ->
    integer;
mysql_data_type(<<"double">>) ->
    integer;
mysql_data_type(<<"float">>) ->
    integer;
mysql_data_type(<<"decimal">>) ->
    integer;
mysql_data_type(<<"mediumint">>) ->
    integer;
mysql_data_type(<<"char">>) ->
    binary;
mysql_data_type(<<"varchar">>) ->
    binary;
mysql_data_type(<<"tinytext">>) ->
    binary;
mysql_data_type(<<"text">>) ->
    binary;
mysql_data_type(<<"mediumtext">>) ->
    binary;
mysql_data_type(<<"longtext">>) ->
    binary;
mysql_data_type(_) ->
    other_type.

