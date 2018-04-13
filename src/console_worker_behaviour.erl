%%%-------------------------------------------------------------------
%%% @author Aiden_Richie
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Dec 2017 12:04 PM
%%%-------------------------------------------------------------------
-module(console_worker_behaviour).
-author("Aiden_Richie").

-record(state,{mysql_pid}).

-type job() ::term().
-type result() :: term().
-type reason() :: term().
-type state() :: #state{}.
-type status() :: success | failure.
-type ref() :: term().


-type reference_job() :: {Reference::ref(),Job::job()}.
-type success_response() :: {Reference::ref(),Result::result()}.
-type failure_response() :: {Reference::ref(),Result::reason()}.

%% API
-export([]).

-callback init(Args::term()) -> {ok,#state{}} | {stop,Reason::term()}.

-callback pre_execute(Request::term(),State::state()) ->
  {success,JobData::job(),NewState::state()} |
  {add_more,JobData::job(),Weight::integer(),NewState::state()} |
  {failure,Reason::reason(),NewState::state()}.

-callback execute(Job::job(),State::state()) ->
  {success,Response::result(),NewState::state()} |
  {failure,Reason::reason(),NewState::state()}.

-callback combine_execute(JobRef::list(reference_job()),State::state()) ->
  {success,ResponseRef::list(success_response()),NewState::state()} |
  {failure,ErrorRef::list(failure_response()),NewState::state()} |
  {both,ResponseRef::list(success_response()),ErrorRef::list(failure_response()),State::state()}.

-callback post_execute(Status::status(), Job::job(),Result::result(),State::state()) ->
  {return,Reply::term(),NewState::state()}.

-callback terminate(State::state()) -> ok.


-export([init/1,pre_execute/2,execute/2,combine_execute/2,post_execute/4,terminate/1]).
%%--------------------------------------------------------------------
%%-spec init(Args::term()) -> {ok,#state{}} | {stop,Reason::term()}.
%%--------------------------------------------------------------------
init(_WorkerArgs) ->
  {ok,Db_Pid} = mysql:start_link([{host, "localhost"}, {user, "root"},{password, "sanders16"}]),
  mysql:query(Db_Pid, "use "++"testingdb"),
  {ok,#state{mysql_pid = Db_Pid}}.

%%--------------------------------------------------------------------
%%-spec pre_execute(Request::term(),State::state()) ->
%%  {success,JobData::job(),NewState::state()} |
%%  {add_more,JobData::job(),Weight::integer(),NewState::state()} |
%%  {failure,Reason::reason(),NewState::state()}.
%%--------------------------------------------------------------------

pre_execute({_ReportWithRef,{report,{DBQuery}}},State = #state{mysql_pid = Pid}) ->
  io:format("I m here in pre execute behavior: ~n",[]),
  case db_query_decoder:convert(DBQuery) of
    {error,Reason} ->
      io:format(" QUERY STOPPED INVALID ~n:~p",[Reason]),
      {failure,Reason,State};
    {Method,Args} ->
      io:format(" QUERY INITIATED ~n:~p ~p",[Method,Args]),
      {success,{Method,Args++[Pid]},State};
    Any ->
      io:format(" QUERY STOPPED IN DEFAULT RUNDB ~n Any : ~p~n",[Any]),
      done
  end;

pre_execute({_ReportWithRef,{call,DBQuery}},State = #state{mysql_pid = Pid}) ->
  io:format("I m here in pre execute behavior: ~n",[]),
  case db_query_decoder:convert(DBQuery) of
    {error,Reason} ->
      io:format(" QUERY STOPPED INVALID ~n:~p",[Reason]),
      {failure,Reason,State};
    {Method,Args} ->
      io:format(" QUERY INITIATED ~n:~p ~p",[Method,Args]),
      {success,{Method,Args++[Pid]},State};
    Any ->
      io:format(" QUERY STOPPED IN DEFAULT RUNDB~n Any : ~p~n",[Any]),
      done
  end;

pre_execute({_ReportWithRef,Request},State) ->
  io:format("Unhandled Request: ~p~n",[Request]),
  {failure,unhandled_request,State}.
%%--------------------------------------------------------------------
%%-spec execute(Job::job(),State::state()) ->
%%  {success,Response::result(),NewState::state()} |
%%  {failure,Reason::reason(),NewState::state()}.
%%--------------------------------------------------------------------
execute({Method,Args} = Resp,State) ->
  io:format("~nI m here in execute behavior: ~p~n",[Resp]),
  Resp1 = apply(db_console_query,Method,Args),
  io:format("~nI m here in execute behavior with RESP: ~p~n",[Resp1]),
  {success,Resp1,State};

execute(_Job,State) ->
  io:format("I m here in execute behavior: ~n",[]),
  {failure,unhandled_request,State}.
%%--------------------------------------------------------------------
%%-spec combine_execute(JobRef::list(reference_job()),State::state()) ->
%%  {success,ResponseRef::list(success_response()),NewState::state()} |
%%  {failure,ErrorRef::list(failure_response()),NewState::state()} |
%%  {both,ResponseRef::list(success_response()),ErrorRef::list(failure_response()),State::state()}.
%%--------------------------------------------------------------------
combine_execute({_Ref,{report,_Request}},State) ->
  {success,[],State};

combine_execute({_Ref,{get_call,_Request}},State) ->
  {success,[],State}.
%%--------------------------------------------------------------------
%%-spec post_execute(Status::status(), Job::job(),Result::result(),State::state()) ->
%%  {return,Reply::term(),NewState::state()}.
%%--------------------------------------------------------------------
post_execute(success,Job,Result,State) ->
  io:format("I m here in post execute behavior Job: ~p Result: ~p State: ~p~n",[Job,Result,State]),
  {return,Result,State}.
%%--------------------------------------------------------------------
%%-spec terminate(State::state()) -> ok.
%%--------------------------------------------------------------------
terminate(_State) -> ok.

