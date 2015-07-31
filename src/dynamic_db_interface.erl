%%% @author Roowe <bestluoliwe@gmail.com>
%%% @copyright (C) 2013, Roowe
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2013 by Roowe <bestluoliwe@gmail.com>

-module(dynamic_db_interface).
-include("leshu_db_common.hrl").
-include("leshu_db.hrl").

-export([
         start/1
        ]).

start(DbConfList) ->
    lists:foreach(
      fun(DbConf) ->
              [ModName, Src] = db_src(DbConf),
              %%?DEBUG("~p, ~p, ~s ~n",[DbConf, ModName, Src]),
              dynamic_module(ModName, Src)
      end,
      DbConfList).

dynamic_module(ModName, Src) ->
    try
        {Mod, Code} = hdynamic_compile:from_string(Src),
        code:load_binary(Mod, ModName ++ ".erl", Code),
        ok
    catch
        Type:Error -> 
            ?ERROR_MSG("Error compiling ~p (~p): ~p stack:~p~n", 
                       [ModName, Type, Error, erlang:get_stacktrace()]),
            error
    end.

db_src(#db_conf{
          mod_name=Db          
         }=DbConf)->    
    Funs = get_funs(DbConf),
    [Db ++ "_interface",
     "-module(" ++ Db ++ ").\t\n"      
     ++ "-compile(export_all).\t\n" 
     ++ Funs ++ "\t\n"
    ].  

get_stat_db_access(Operation) ->
    "    db_mysql_stat:stat_db_access(P1, " ++ Operation ++ "), ".


-define(TRY_BEGIN, "try \t\n begin \t\n").

-define(TRY_END(ModName), "\t\n end \t\n catch _:R -> \t\n logger:error_msg("++ ModName ++ ", ?LINE, \"~p, ~p~n\", [R, erlang:get_stacktrace()]),\n\terror_db \t\n end").

get_parms(Pn) when Pn>=1 ->
    get_parms(Pn-1, "P"++integer_to_list(Pn)).
get_parms(0, Str) ->
    Str;
get_parms(1, Str) ->
    "P"++integer_to_list(1) ++", "++ Str;
get_parms(Pn, Str) when Pn>=2->
    get_parms(Pn-1, "P"++integer_to_list(Pn) ++ ", " ++ Str).

%%重构前
%% select_all(P1, P2, P3, P4) ->
%% try
%%  begin
%%     misc:stat_db_access(P1, select_all),
%%     db_mysql:select_all(db_agent:get_db_srv_pid(db_log_slave), P1, P2, P3, P4)
%%  end
%%  catch _:R ->
%%  logger:error_msg(db_log_slave,?LINE,"db_log_slave error R:~w stack:~p~n",[R, erlang:get_stacktrace()]),
%%         error_db
%%  end.
%%重构后
%% select_all(P1, P2, P3, P4) ->
%% try
%%  begin
%%     misc:stat_db_access(P1, select_all),
%%     db_mysql:select_all(db_log_slave, P1, P2, P3, P4)
%%  end
%%  catch _:R ->
%%  error_logger:error_msg("db_log_slave error R:~w stack:~p~n",[R, erlang:get_stacktrace()]),
%%         error_db
%%  end.

get_funs(#db_conf{
            db_type = DbType,
            poll = Poll
           }) ->
    ConfigFuns = [
                  {"insert", 4},
                  {"mul_insert", 3},
                  {"insert", 3},
                  {"new_insert", 3},
                  {"new_insert", 2},
                  {"insert_or_update", 2},
                  {"insert_or_update", 3},
                  {"replace", 2},
                  {"update", 3},
                  {"update", 5},
                  {"select_one", 3},
                  {"select_one", 5},
                  {"select_row", 3},
                  {"select_row", 5},
                  {"select_count", 2},
                  {"select_all", 3},
                  {"select_all", 5},
                  {"delete", 2},
                  {"delete", 4},
                  {"transaction", 1},
                  {"findAndModify", 3},
                  {"select_one", 2},
                  {"select_one", 4},
                  {"select_row", 2},
                  {"select_row", 4},     
                  {"select_all", 2},
                  {"select_all", 4},
                  {"select_one_from_uniontable", 7},
                  {"select_all_from_multtable", 5},
                  {"select_row_from_multtable", 5},
                  {"column_rename", 4},
                  {"column_remove", 3},
                  {"coin_sum_process", 3},
                  {"gold_sum_process", 3},
                  {"sum", 3}                                                      
                 ],
    Funs = lists:map( 
             fun(Cfg) ->
                     try
                         {F, PN, _End} =
                             case Cfg of
                                 {F0, PN0} ->
                                     {F0, PN0, "."};
                                 {F0, PN0, End0} -> 
                                     {F0, PN0, End0}
                             end,
                         F1 = lists:concat([F]),
                         [Parms1, Parms2] = 
                             case PN =< 0 of
                                 true ->
                                     [" ", " "];
                                 _ ->
                                     TmpParms = get_parms(PN),
                                     [TmpParms, ", " ++ TmpParms]
                             end,
                         F1 ++ "(" ++ Parms1 ++ ") -> \t\n"

                             ++ get_stat_db_access(F) ++ "\t\n"
                             ++ "    " ++ atom_to_list(DbType) ++ ":"
                             ++ F1 ++ "(" ++ atom_to_list(Poll) ++ Parms2 ++ ").\t\n"
                     catch
                         Type:Error -> 
                             ?ERROR_MSG("Type ~p:Error~p, ~p~n", [Type, Error, erlang:get_stacktrace()])
                     end
             end,
             ConfigFuns),      
    lists:concat(Funs).

