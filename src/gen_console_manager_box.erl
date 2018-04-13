%%%-------------------------------------------------------------------
%%% @author Aiden_Richie
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2017 5:15 PM
%%%-------------------------------------------------------------------
-module(gen_console_manager_box).
-author("Aiden_Richie").

%% API

-export([manager_call/4,manager_inform/3]).
-export([init/5,restore/3,pre_setup/4,timer/4,terminate/4]).
-export([inner_manager_cue/5,child_cue/5,outer_manager_cue/5,outer_box_cue/5]).
-export([inner_manager_query/5,child_query/5,outer_manager_query/5]).
-export([inner_manager_query_response/7,child_query_response/7,outer_manager_query_response/7]).
-export([inner_manager_request/5,child_request/5,outer_manager_request/5]).
-export([enquiry_response/8,outer_box_request/5,locator_box_query_response/7]).

-include_lib("generic_box/include/module_reference.hrl").
-include_lib("gabby_reference/include/common_records.hrl").
-include_lib("gabby_reference/include/credentials.hrl").

-record(console_manager_state,{box_name,token,gen_factory,child_args}).

-define(AccessKey,'kdjhfksdkjfhkf9&7234nlsdfn64$%22864828nl@#4sdf').

manager_call(Type,Priority,Request,Key) ->
  gen_box_connection:call(?ConsoleManagerBox,{Type,Priority,Request},Key).

manager_inform(Priority,Info,Key) ->
  gen_box_connection:inform(?ConsoleManagerBox,{direct_inform,Priority,Info},Key).

%%--------------------------------------------------------------------------------------
%%-spec init(Args::any()) -> {ok,Status::atom(),State::any(),Props::list(term())} | {stop,Reason::any()}.
%%--------------------------------------------------------------------------------------

init(BoxName,Token,_ChildBoxName,GenFactory,ChildArgs) ->
 {ok,status,#console_manager_state{box_name=BoxName,token=Token,gen_factory=GenFactory,child_args=ChildArgs},[],[]}.

%%--------------------------------------------------------------------------------------
%%-spec restore(Status::atom(),State::any(),BoxState::term()) -> {ok,Status::atom(),State::any(),Props::list(term())} | {stop,Reason::any()}.
%%--------------------------------------------------------------------------------------

restore(Status,State,_BoxState) -> {ok,Status,State,[]}.

%%--------------------------------------------------------------------------------------
%%-spec pre_setup(PreSetup::term(),Status::atom(),State::any(),BoxState::term()) -> {ok,Status::atom(),State::any(),Props::list(term())} |
%%  {stop,Reason::any(),State::any()}.
%%--------------------------------------------------------------------------------------

pre_setup([],Status,State = #console_manager_state{box_name = BoxName,token = Token},_BoxProperty) ->
  case gen_box_connection:call(console_management,{add_console_manager,{BoxName,Token}},?AccessKey) of
    {box_added,WorkerBox} ->
      io:format("Connected to worker pool box: ~p~n",[WorkerBox]),
      {ok,Status,State,[{add_following,WorkerBox,[console_worker]}]};
    Error ->
      io:format("Not connected to worker pool box ERROR: ~p~n",[Error]),
      {ok,Status,State,[]}
  end.

%%--------------------------------------------------------------------------------------
%%-spec inner_manager_cue(Request::term(),ManagerID::term(),Status::atom(),ModState::any(),BoxProperty::term()) ->
%%  {ok,NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {tell,Who::term(),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,WhoList::list(term()),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,WhoRequests::list(term()),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,WhoRequestReferences::list(term()),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoList::list(term()),Request2::term(),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoRequests::list(term()),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()}.
%%--------------------------------------------------------------------------------------

inner_manager_cue(Request,ManagerId,Status,ModState,{_,_BoxProps} = _BoxProperty) ->
  io:format("Unhandled Request ~p from ~p~n",[Request,ManagerId]),
  {ok,Status,ModState,[]}.

%%--------------------------------------------------------------------------------------
%%-spec child_cue(Request::term(),ChildID::term(),Status::atom(),ModState::any(),BoxProperty::term()) ->
%%  {ok,NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {tell,Who::term(),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,WhoList::list(term()),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,WhoRequests::list(term()),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,WhoRequestReferences::list(term()),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoList::list(term()),Request2::term(),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoRequests::list(term()),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()}.
%%--------------------------------------------------------------------------------------

child_cue(Request,ChildId,Status,ModState,{_,_BoxProps} = _BoxProperty) ->
  io:format("Unhandled Request ~p from ~p~n",[Request,ChildId]),
  {ok,Status,ModState,[]}.

%%--------------------------------------------------------------------------------------
%%-spec outer_manager_cue(Request::term(),ChildID::term(),Status::atom(),ModState::any(),BoxProperty::term()) ->
%%  {ok,NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {tell,Who::term(),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,WhoList::list(term()),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,WhoRequests::list(term()),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,WhoRequestReferences::list(term()),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoList::list(term()),Request2::term(),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoRequests::list(term()),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()}.
%%--------------------------------------------------------------------------------------

outer_manager_cue(Request,_ManagerID,Status,ModState,{_,_BoxProps} = _BoxProperty) ->
  io:format("Unhandled Request in ProfileManagerBox: ~p~n",[Request]),
  {ok,Status,ModState,[]}.

%%--------------------------------------------------------------------------------------
%%-spec outer_box_cue(Request::term(),ManagerID::term(),Status::atom(),ModState::any(),BoxProperty::term()) ->
%%  {ok,NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {tell,Who::term(),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,WhoList::list(term()),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,WhoRequests::list(term()),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,WhoRequestReferences::list(term()),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoList::list(term()),Request2::term(),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoRequests::list(term()),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()}.
%%--------------------------------------------------------------------------------------

outer_box_cue({direct_inform,Priority,Request},_From,Status,State,{_,_BoxProps} = _BoxProperty) ->
  {tell,console_worker,{'async',Priority,Request},worker_pool_report,Status,State,[]};

outer_box_cue(Request,_From,Status,ModState,{_,_BoxProps} = _BoxProperty) ->
  io:format("Unhandled Request: ~p~n",[Request]),
  {ok,Status,ModState,[]}.

%%--------------------------------------------------------------------------------------
%%-spec inner_manager_query(Request::term(),ManagerID::term(),Status::atom(),ModState::any(),BoxProperty::term()) ->
%%  {convey,Reply::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {tell,Who::term(),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoList::list(term()),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequests::list(term()),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequestReferences::list(term()),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoList::list(term()),Request2::term(),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoRequests::list(term()),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()}.
%%--------------------------------------------------------------------------------------
inner_manager_query(Request,ManagerId,Status,ModState,{_,_BoxProps} = _BoxProperty) ->
  io:format("Unhandled Request ~p from ~p~n",[Request,ManagerId]),
  {ok,Status,ModState,[]}.
%%--------------------------------------------------------------------------------------
%%-spec child_query(Request::term(),ManagerID::term(),Status::atom(),ModState::any(),BoxProperty::term()) ->
%%  {convey,Reply::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {tell,Who::term(),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoList::list(term()),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequests::list(term()),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequestReferences::list(term()),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoList::list(term()),Request2::term(),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoRequests::list(term()),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()}.
%%--------------------------------------------------------------------------------------
child_query(Request,ChildId,Status,ModState,_BoxProps) ->
  io:format("Unhandled Request ~p from ~p~n",[Request,ChildId]),
  {convey,unhandled,Status,ModState,[]}.
%%--------------------------------------------------------------------------------------
%%-spec outer_manager_query(Request::term(),ChildID::term(),Status::atom(),ModState::any(),BoxProperty::term()) ->
%%  {convey,Reply::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {tell,Who::term(),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoList::list(term()),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequests::list(term()),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequestReferences::list(term()),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoList::list(term()),Request2::term(),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoRequests::list(term()),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()}.
%%--------------------------------------------------------------------------------------

outer_manager_query(Request,_ManagerId,Status,ModState,{_,_BoxProps} = _BoxProperty) ->
  io:format("Unhandled Request: ~p~n",[Request]),
  {convey,unhandled,Status,ModState,[]}.

%%--------------------------------------------------------------------------------------
%%-spec inner_manager_query_response(Request::term(),Response::any(),ReplyFlag::boolean(),ManagerID::term(),
%% Status::atom(),ModState::any(),BoxProperty::term()) ->
%%  {ok,NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {tell,Who::term(),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {convey,Reply::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoList::list(term()),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequests::list(term()),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequestReferences::list(term()),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoList::list(term()),Request2::term(),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoRequests::list(term()),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()}.
%%--------------------------------------------------------------------------------------

inner_manager_query_response(Request,Response,ReplyFlag,_ManagerID,Status,ModState,{_,_BoxProps} = _BoxProperty) ->
  io:format("Unhandled Request ~p for the Response ~p ~n",[Request,Response]),
  case ReplyFlag of
    true -> {convey,reply,Status,ModState,[]};
    false -> {ok,Status,ModState,[]}
  end.

%%--------------------------------------------------------------------------------------
%%-spec child_query_response(Request::term(),Response::any(),ReplyFlag::boolean(),ManagerID::term(),
%% Status::atom(),ModState::any(),BoxProperty::term()) ->
%%  {ok,NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {tell,Who::term(),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {convey,Reply::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoList::list(term()),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequests::list(term()),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequestReferences::list(term()),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoList::list(term()),Request2::term(),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoRequests::list(term()),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()}.
%%--------------------------------------------------------------------------------------

child_query_response(Request,Response,ReplyFlag,_ChildID,Status,ModState,{_,_BoxProps} = _BoxProperty) ->
  io:format("Unhandled Request ~p for the Response ~p ~n",[Request,Response]),
  case ReplyFlag of
    true -> {convey,reply,Status,ModState,[]};
    false -> {ok,Status,ModState,[]}
  end.

%%--------------------------------------------------------------------------------------
%%-spec locator_box_query_response(Request::term(),Response::any(),ReplyFlag::boolean(),ChildID::term(),Status::atom(),ModState::any(),BoxProperty::term()) ->
%%  {ok,NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {tell,Who::term(),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {convey,Reply::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoList::list(term()),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequests::list(term()),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequestReferences::list(term()),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoList::list(term()),Request2::term(),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoRequests::list(term()),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()}.
%%--------------------------------------------------------------------------------------

locator_box_query_response(_Request,_Response,_ReplyFlag,_ManagerID,Status,ModState,{_,_BoxProps} = _BoxProperty) ->
  {ok,Status,ModState,[]}.

%%--------------------------------------------------------------------------------------
%%-spec outer_manager_query_response(Request::term(),Response::any(),ReplyFlag::boolean(),ChildID::term(),
%% Status::atom(),ModState::any(),BoxProperty::term()) ->
%%  {ok,NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {tell,Who::term(),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {convey,Reply::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoList::list(term()),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequests::list(term()),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequestReferences::list(term()),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoList::list(term()),Request2::term(),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoRequests::list(term()),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()}.
%%--------------------------------------------------------------------------------------

outer_manager_query_response(Request,Response,ReplyFlag,_ManagerID,Status,ModState,{_,_BoxProps} = _BoxProperty) ->
  io:format("Unhandled Request ~p for the Response ~p ~n",[Request,Response]),
  case ReplyFlag of
    true -> {convey,done,Status,ModState,[]};
    false -> {ok,Status,ModState,[]}
  end.
%%--------------------------------------------------------------------------------------
%%-spec enquiry_response(Request::term(),Response::any(),MissingBoxes::any(),ReplyFlag::boolean(),ManagerID::term(),
%% Status::atom(),ModState::any(),BoxProperty::term()) ->
%%  {ok,NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {tell,Who::term(),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {convey,Reply::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoList::list(term()),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequests::list(term()),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequestReferences::list(term()),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoList::list(term()),Request2::term(),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoRequests::list(term()),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()}.
%%--------------------------------------------------------------------------------------

enquiry_response(Request,Response,_MissingBoxes,_ReplyFlag,_ManagerID,Status,ModState,{_,_BoxProps} = _BoxProperty) ->
  io:format("Unhandled Request ~p for the Response ~p ~n",[Request,Response]),
  {ok,Status,ModState,[]}.

%%--------------------------------------------------------------------------------------
%%-spec inner_manager_request(Request::term(),ManagerID::term(),Status::atom(),ModState::any(),BoxProperty::term()) ->
%%  {ok,NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {tell,Who::term(),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {convey,Reply::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoList::list(term()),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequests::list(term()),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequestReferences::list(term()),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoList::list(term()),Request2::term(),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoRequests::list(term()),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()}.
%%--------------------------------------------------------------------------------------

inner_manager_request(Request,ManagerID,Status,ModState,{_,_BoxProps} = _BoxProperty) ->
  io:format("Unhandled Request ~p from ~p ~n",[Request,ManagerID]),
  {ok,Status,ModState,[]}.

%%--------------------------------------------------------------------------------------
%%-spec child_request(Request::term(),ChildID::term(),Status::atom(),ModState::any(),BoxProperty::term()) ->
%%  {ok,NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {convey,Reply::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoList::list(term()),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequests::list(term()),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequestReferences::list(term()),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoList::list(term()),Request2::term(),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoRequests::list(term()),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()}.
%%--------------------------------------------------------------------------------------

child_request(Request,ChildID,Status,ModState,{_,_BoxProps} = _BoxProperty) ->
  io:format("Unhandled Request ~p from ~p ~n",[Request,ChildID]),
  {ok,Status,ModState,[]}.

%%--------------------------------------------------------------------------------------
%%-spec outer_manager_request(Request::term(),ManagerID::term(),Status::atom(),ModState::any(),BoxProperty::term()) ->
%%  {ok,NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {tell,Who::term(),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {convey,Reply::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoList::list(term()),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequests::list(term()),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {ask,Reply::term(),WhoRequestReferences::list(term()),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoList::list(term()),Request2::term(),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%  {enquiry,WhoRequests::list(term()),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()}.
%%--------------------------------------------------------------------------------------

outer_manager_request(Request,ManagerID,Status,ModState,{_,_BoxProps} = _BoxProperty) ->
  io:format("UnHandled Request: ~p from: ~p~n",[Request,ManagerID]),
  {ok,Status,ModState,[]}.

%%--------------------------------------------------------------------------------------
%% -spec outer_box_request(Request::term(),FromID::term(),Status::atom(),ModState::any(),BoxProperty::term()) ->
%%   {ok,NewStatus::atom(),NewModState::any(),Props::term()} |
%%   {tell,Who::term(),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%   {convey,Reply::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%   {ask,Reply::term(),WhoList::list(term()),Request::term(),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%   {ask,Reply::term(),WhoRequests::list(term()),Reference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%   {ask,Reply::term(),WhoRequestReferences::list(term()),NewStatus::atom(),NewModState::any(),Props::term()} |
%%   {enquiry,WhoList::list(term()),Request2::term(),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()} |
%%   {enquiry,WhoRequests::list(term()),FetchReference::term(),NewStatus::atom(),NewModState::any(),Props::term()}.
%%--------------------------------------------------------------------------------------

outer_box_request({make_report,Priority,Request},_From,Status,State,{_,_BoxProps} = _BoxProperty) ->
  {tell,console_worker,{'async',Priority,Request},worker_pool_report,Status,State,[]};

outer_box_request({make_call,Priority,Request},_From,Status,State,{_,BoxProps} = _BoxProperty) ->
  io:format("Console Manager Modules Received Request: ~p~n",[Request]),
  Reply = gen_manager_box:ask(console_worker,{'async',Priority,Request},BoxProps),
  {convey,Reply,Status,State,[]};

outer_box_request({make_call_with_no_reply,Priority,Request},_From,Status,State,{_,BoxProps} = _BoxProperty) ->
  gen_manager_box:ask(console_worker,{'no_reply',Priority,Request},BoxProps),
  {ok,Status,State,[]};

outer_box_request(Request,FromId,Status,ModState,{_,_BoxProps} = _BoxProperty) ->
  io:format("Unhandled Request ~p from ~p ~n",[Request,FromId]),
  {ok,Status,ModState,[]}.

%%--------------------------------------------------------------------------------------
%%-spec timer(TimeOutFlag::term(),Status::atom(),ModStateL::any(),BoxProperty::term()) ->
%%  {wait,TimeOutFlag::term(),TimeStamp::integer(),Status::atom(),State::any(),Props::list(term())} |
%%  {suspend,TimeOutFlag::term(),TimeStamp::integer(),Status::atom(),State::any(),Props::list(term())} |
%%  {stop,Reason::any(),Status::atom(),State::any(),Props::list(term())}.
%%--------------------------------------------------------------------------------------
timer(_TimeOutFlag,Status,ModState,{_,_BoxProps} = _BoxProperty) ->  {wait,stream_waiting,50000,Status,ModState,[]}.
%%--------------------------------------------------------------------------------------
%%-spec terminate(Reason::term(),Status::atom(),State::any(),BoxState::term()) -> ok.
%%--------------------------------------------------------------------------------------
terminate(_Reason,_Status,_State,_BoxState) -> ok.
%%%=====================================================================================
%%% Internal functions
%%%=====================================================================================
