%% Author: T400
%% Created: 2011-7-29
%% Description: TODO: Add description to dynamic_db_interface
-module(dynamic_hdb_interface).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([
         start/1
        ]).

%%
%% API Functions
%%
start(TableNameList) ->
    lists:foreach(
      fun(Db) ->
              [ModName, Src] = db_src(Db),
              dynamic_module(ModName, Src, Db)
      end,
      TableNameList).

dynamic_module(ModName, Src, Db) ->
    try
        %% io:format("Src: ~ts~n", [Src]),
        {Mod, Code} = hdynamic_compile:from_string(Src),
        %% io:format("db_2__/~ts/ ~n",[Src]),		
        code:load_binary(Mod, ModName ++ ".erl", Code),
        io:format("~n    dynamic_hdb_interface[~p] OK!", [Db])
    catch
        Type:Error -> 
            io:format("Error compiling ~p (~p): ~p stack:~p~n", 
                      [ModName, Type, Error,erlang:get_stacktrace()])
    end.

db_src(Table)->
    TableName = lists:concat([Table]),
    OtherFun = get_other_fun(TableName),
    ApiFun   = get_api_fun(TableName),
    CallFun  = get_call_fun(TableName),
    CastFun  = get_cast_fun(TableName),
    InfoFun  = get_info_fun(TableName),
    [TableName,
     "-module(" ++ TableName ++ ").\t\n"
     ++ "-behaviour(gen_server).\t\n"   
     ++ "-export([write/1,dirty_write/1,read/1,dirty_read/1,delete/1]).\t\n"
     ++ "-export([start/0]).\t\n"
     ++ "-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).\n"
     ++ ApiFun
     ++ OtherFun
     ++ CallFun
     ++ CastFun
     ++ InfoFun
     ++ "\t\n"
    ].	

get_api_fun(TableName) ->
    "get_server_pid() ->\t\n" ++
        "case whereis("++ TableName ++ ") of\t\n" ++
        "\tundefined -> {ok, Pid} = " ++ TableName ++":start(), Pid;\n" ++
        "\tPid -> Pid \t\nend.\t\n".

get_call_fun(_TableName) ->
    "write(Record) ->\n\t gen_server:call(get_server_pid(), {write, Record}).\t\n" ++ 
        "dirty_write(Record) ->\n\t gen_server:call(get_server_pid(), {dirty_write, Record}).\t\n" ++ 
        "read(Record) ->\n\t gen_server:call(get_server_pid(), {read, Record}).\t\n" ++ 
        "dirty_read(Record) ->\n\t gen_server:call(get_server_pid(), {dirty_read, Record}).\t\n" ++ 
        "delete(Record) ->\n\t gen_server:call(get_server_pid(), {delete, Record}). \t\n" ++ 
        "handle_call({write, Record}, _From, State) ->\n\t Reply = hdb:write(Record), {reply, Reply, State}; \t\n" ++
        "handle_call({dirty_write,  Record}, _From, State) ->\n\t Reply = hdb:dirty_write(Record), {reply, Reply, State}; \t\n" ++
        "handle_call({read,  Record},  _From, State) ->\n\t Reply = hdb:read(Record), {reply, Reply, State}; \t\n" ++
        "handle_call({dirty_read,  Record}, _From, State) ->\n\t Reply = hdb:dirty_read(Record), {reply, Reply, State}; \t\n" ++
        "handle_call({delete,  Record}, _From, State) ->\n\t Reply = hdb:delete(Record), {reply, Reply, State}; \t\n" ++
        "handle_call(_, _From, State) ->\n\t {reply, ok, State}.\t\n".

get_cast_fun(_TableName) ->
    "handle_cast(_, State) ->\n\t {noreply, State}.\t\n".

get_info_fun(_TableName) ->
    "handle_info(_, State) ->\n\t {noreply, State}.\t\n".

get_other_fun(TableName) ->
    "-record(state, {}). \t\n" ++
        "start() ->\n\t gen_server:start({local, "++TableName++"}, "++TableName++", [], []).\t\n" ++
        "init([]) ->\n\t {ok, #state{}}.\t\n" ++
        "terminate(_Reason, _State) ->\n\t ok.\t\n" ++
        "code_change(_OldVsn, State, _Extra) ->\n\t {ok, State}. \t\n".

