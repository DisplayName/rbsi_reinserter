-module(rbsi_reinserter).
-behaviour(gen_server).

-export([
        start_link/0,
        parse/1,
        parse/0
]).

-export([
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3
]).


-include("logging.hrl").


-record(state, {
          server,
          connection,
          db,
          collection,
          file_for_rejected_data,
          failed_records_count,
          reinserted_records_count
}).



%% ===================================================================
%% APIs
%% ===================================================================


start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


parse(LogFileName) ->
   gen_server:cast(?MODULE, {parse, LogFileName}).



%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init([]) ->
   {ok, MongoServer} = application:get_env(rbsi_reinserter, mongo_server),
   {ok, MongoDb} = application:get_env(rbsi_reinserter, mongodb_dbname),
   {ok, MongoCollection} = application:get_env(rbsi_reinserter, collection),
   {ok, FileForRejectedData} = application:get_env(rbsi_reinserter, file_for_rejected_data),

   case mongo:connect(MongoServer) of
      {ok, Connection} ->
      {ok, #state{server = MongoServer, connection = Connection, db = MongoDb,
           collection = MongoCollection, file_for_rejected_data = FileForRejectedData,
           failed_records_count = 0, reinserted_records_count = 0}};
      {error, Reason} ->
         ?log_error("Error Occured. Reason:~p. Connection settings: ~p~n", [Reason, MongoServer])
   end.


handle_cast({parse, LogFileName}, State) ->
   ?log_info("parsing started", []),

   case file:open(LogFileName, [read]) of
      {ok, Device} ->
         init_storage_file(State#state.file_for_rejected_data),
         NewState = parse_logfile_line_by_line(Device, "", State),
         output_summary(NewState),
         file:close(Device),

         {noreply, State};
      {error, Reason} ->
         ?log_error("Can't access file: ~p~n", [LogFileName]),
         {stop, Reason, State}
   end;

handle_cast(Request, State) ->
   {stop, {bad_arg, Request}, State}.
  

handle_info(Info, State) ->
   {stop, {bad_arg, Info}, State}.


handle_call(_Request, _From, State) ->
   {noreply, State}.


terminate(_Reason, _State) ->
   ok.


code_change(_OldSvn, State, _Extra) ->
   {ok, State}.



%% ===================================================================
%% Internal
%% ===================================================================


output_summary(State) ->
  ?log_info("Summary", []),
  ?log_info("  Reinserted messages: ~p. Failed messages: ~p.", 
      [State#state.reinserted_records_count, State#state.failed_records_count]).


%
% extracting messages from log file
%
parse_logfile_line_by_line(Device, Accum, State) ->
   case io:get_line(Device, "") of
      eof ->
         State;
      Line ->
         {Term, UnhandledData} = try_to_get_term(Accum ++ Line, string:chr(Line, $.)),
         NewState = reinsert_message(State, Term),
         parse_logfile_line_by_line(Device, UnhandledData, NewState)
   end.


%
% trying to get Term from raw string
%
try_to_get_term(Text, 0) -> {undefined, Text};

try_to_get_term(Text, _Position) ->
   RefinedString = remove_spaces_and_brakes(Text),
   Position = string:chr(RefinedString, $.),

   UnhandledData = string:substr(RefinedString, Position + 1, length(RefinedString) - Position),

   StringToParse = string:substr(RefinedString, 1, Position),
   {ok, Tokens, _EndLine} = erl_scan:string(StringToParse),
   {ok, Terms} = erl_parse:parse_term(Tokens),
   
   {_RecordName, _RecordData, Binary} = Terms,

   {binary_to_term(Binary), UnhandledData}.


%
% saving data into file.
% when "" passed file should be cleansed
%
init_storage_file(FileName) ->
   file:write_file(FileName, "", []).

save_into_file(FileName, Data) ->
   file:write_file(FileName, io_lib:fwrite("~p.\n", [Data]), [append]).


%
% Replacing spaces and new lines with ""
%
remove_spaces_and_brakes(Text) ->
   lists:filter(fun(C) -> not lists:member(C, "$ $\n") end, Text).


%
% re-insert data in MongoDB or in file (depending it was fired or not)
%
reinsert_message(State, undefined) -> State;

reinsert_message(State = #state{connection = Connection, db = Db, collection = Collection, reinserted_records_count = Cnt},
                _Data = {task, _RecipCnt, Msisdns, TranSid, _Tag, _Error}) ->
   {ok, ok} = mongo:do(safe, master, Connection, Db, fun() ->
      lists:foreach(fun(Msisdn) ->
         ok = mongo:repsert(Collection, {'_id', Msisdn}, { '$push', { v, TranSid } })
      end, Msisdns)
   end),
  State#state{reinserted_records_count = Cnt + 1}   ;

reinsert_message(State = #state{file_for_rejected_data = FileForRejectedData, failed_records_count = Cnt}, Data = {_Message, _UnpackedMessage}) ->
  save_into_file(FileForRejectedData, Data),
  State#state{failed_records_count = Cnt + 1}.