%%%-------------------------------------------------------------------
%%% @author Aiden_Richie
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2017 4:04 PM
%%%-------------------------------------------------------------------
-module(console_http_handler).
-author("Aiden_Richie").

-include("http_utils.hrl").
-include("console_params.hrl").
-include("console_operations.hrl").

-export([init/3,allowed_methods/2,options/2,content_types_provided/2,content_types_accepted/2]).

-export([handle_req/2]).


init(_Transport,_Req0, _State) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) -> {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

options(Req, State) ->
  Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, POST, OPTIONS">>, Req),
  Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
  Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, <<"Origin, X-Requested-With, Content-Type, Accept">>, Req2),
  {ok, Req3, State}.

content_types_provided(Req, State) -> {[{<<"application/json">>, handle_req}], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>,<<"json">>,[]}, handle_req}], Req, State}.

handle_req(Req, State) ->
  {Method,Req2} = cowboy_req:method(Req),
  handle_method(Method,Req2,State).

handle_method(<<"GET">>,Req,State) -> console_resp_handler:send_resp(?SUCCESS,[<<"ok">>,<<"Welcome to Gabby">>],Req,State);
handle_method(<<"POST">>,Req,State) ->
  {FullPath, Req2} = cowboy_req:path(Req),
  Path = console_utils:strip_path(FullPath),
  handle_action(Path,Req2,State);

handle_method(Method,Req,State) ->
  io:format("Recvd method: ~p~n",[Method]),
  console_resp_handler:send_resp(?FAILURE,[<<"error">>,<<"Unhandled method">>],Req,State).

handle_action(<<>>,Req,State) -> console_resp_handler:send_resp(?SUCCESS,[<<"ok">>,<<"Welcome to Gabby">>],Req,State);
handle_action(<<"test">>,Req,State) -> console_resp_handler:send_resp(?SUCCESS,[<<"ok">>,<<"Testing Gabby">>],Req,State);
handle_action(Path,Req,State) ->
  {_AllHeaders, Req2} = cowboy_req:headers(Req),
  io:format("Received action: ~p~n",[Path]),
  validate_action(console_req_uri:get_operation_id(Path), Req2,State).

validate_action(invalid, Req,State) -> console_resp_handler:send_resp(?FAILURE,[<<"error">>,<<"Invalid request">>],Req,State);
validate_action(OpId, Req,State) -> check_content(OpId,Req,State).

check_content(OpId,Req,State) ->
  {ok,Body,Req2} = cowboy_req:body(Req),
  get_content(json_decoder:scan(Body,[]),OpId,Req2,State).

get_content({complete,Data},OpId,Req,State) ->
  Params = get_params(Data,#params{}),
  Validity = console_req_validator:verify_params(OpId,Params),
  console_operation(Validity,OpId,Params,Req,State);

get_content({incomplete,_},_Id,Req,State) -> console_resp_handler:send_resp(?FAILURE,[<<"error">>,<<"Invalid content1">>],Req,State);
get_content({wrong_data,_},_Id,Req,State) -> console_resp_handler:send_resp(?FAILURE,[<<"error">>,<<"Invalid content2">>],Req,State).

console_operation(undefined,_OpId,_Arg,Req,St) -> console_resp_handler:send_resp(?FAILURE,[<<"error">>,<<"Invalid content3">>],Req,St);
%%console_operation(invalid,_OpId,_Arg,Req,St) -> console_resp_handler:send_resp(?FAILURE,[<<"error">>,<<"Invalid content4">>],Req,St);
console_operation({invalid,Reason},_OpId,_Arg,Req,St) -> console_resp_handler:send_resp(?FAILURE,[<<"error">>,Reason],Req,St);
console_operation({missing, Key},_OpId,_Arg,Req,St) -> console_resp_handler:send_resp(?FAILURE,[<<"error">>,<<Key/binary, " notfound">>],Req,St);

console_operation(valid,OpId,Params,Req,State) ->
  {Status,Data} = console_operations:handle_operation(#console_req{operation_id = OpId,params = Params}),
  io:format("Operation resp: ~p ~p~n",[Status,Data]),
  console_resp_handler:send_resp(Status,Data,Req,State).


get_params([],Acc) -> Acc;

get_params([{?Client_Token,Val}|Rest],Acc) -> get_params(Rest,Acc#params{client_token = Val});
get_params([{?ORG_ID,Val}|Rest],Acc) -> get_params(Rest,Acc#params{org_id = Val});
get_params([{?ORG_NAME,Val}|Rest],Acc) -> get_params(Rest,Acc#params{org_name  = Val});
get_params([{?ORG_OWNER,Val}|Rest],Acc) -> get_params(Rest,Acc#params{org_owner = Val});
get_params([{?CREATED_DATE,Val}|Rest],Acc) -> get_params(Rest,Acc#params{created_date = Val});
get_params([{?DESCRIPTION,Val}|Rest],Acc) -> get_params(Rest,Acc#params{desc = Val});
get_params([{?USER_ID,Val}|Rest],Acc) -> get_params(Rest,Acc#params{user_id = Val});
get_params([{?LOCATION,Val}|Rest],Acc) -> get_params(Rest,Acc#params{location = Val});
get_params([{?WEBSITE,Val}|Rest],Acc) -> get_params(Rest,Acc#params{web_address_id = Val});
get_params([{?PRIVILEGE,Val}|Rest],Acc) -> get_params(Rest,Acc#params{privilege = Val});

get_params([{?USER_NAME,Val}|Rest],Acc) -> get_params(Rest,Acc#params{user_name  = Val});
get_params([{?FIRST_NAME,Val}|Rest],Acc) -> get_params(Rest,Acc#params{first_name =  Val});
get_params([{?LAST_NAME,Val}|Rest],Acc) -> get_params(Rest,Acc#params{last_name =  Val});
get_params([{?PASSWORD,Val}|Rest],Acc) -> get_params(Rest,Acc#params{password =  Val});
get_params([{?EMAIL,Val}|Rest],Acc) -> get_params(Rest,Acc#params{email = Val});
get_params([{?USER_TYPE,Val}|Rest],Acc) -> get_params(Rest,Acc#params{user_type =  Val});
get_params([{?PHONE,Val}|Rest],Acc) -> get_params(Rest,Acc#params{ph = Val});
get_params([{?DOB,Val}|Rest],Acc) -> get_params(Rest,Acc#params{dob = Val});

get_params([{?FEATURE_ID,Val}|Rest],Acc) -> get_params(Rest,Acc#params{feature_id = Val});
get_params([{?FEATURE,Val}|Rest],Acc) -> get_params(Rest,Acc#params{feature = Val});

get_params([{?APP_KEY,Val}|Rest],Acc) -> get_params(Rest,Acc#params{app_key = Val});
get_params([{?APP_NAME,Val}|Rest],Acc) -> get_params(Rest,Acc#params{app_name = Val});
%%  get_params([{?BUDDY_CHAT,Val}|Rest],Acc) -> get_params(Rest,Acc#params{buddy_chat = Val});
%%  get_params([{?GROUP_CHAT,Val}|Rest],Acc) -> get_params(Rest,Acc#params{group_chat  = Val});
%%  get_params([{?MAXIMUM_USERS,Val}|Rest],Acc) -> get_params(Rest,Acc#params{maximum_users = Val});
%%  get_params([{?MEMBER_LIMIT,Val}|Rest],Acc) -> get_params(Rest,Acc#params{member_limit = Val});
%%  get_params([{?MAXIMUM_PUBLIC_CHANNELS,Val}|Rest],Acc) -> get_params(Rest,Acc#params{maximum_public_channels = Val});
%%  get_params([{?MAXIMUM_PRIVATE_CHANNELS,Val}|Rest],Acc) -> get_params(Rest,Acc#params{maximum_private_channels = Val});
%%  get_params([{?MAXIMUM_PRIVATE_CHANNELS,Val}|Rest],Acc) -> get_params(Rest,Acc#params{maximum_private_channels = Val});

get_params([{Other,Val}|Rest],Acc) ->
  io:format("Received other content with key: ~p and Val: ~p~n",[Other,Val]),
  get_params(Rest,Acc).
