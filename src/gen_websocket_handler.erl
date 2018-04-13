%%%-------------------------------------------------------------------
%%% @author kishore
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Nov 2017 4:46 PM
%%%-------------------------------------------------------------------
-module(gen_websocket_handler).
-author("kishore").

%% API
-export([init/3]).
-export([handle_info/3,handle_data/3]).
-include("console_params.hrl").
-include("data_store.hrl").
-include_lib("gabby_reference/include/credentials.hrl").
-include("db_helper.hrl").

-record(gen_ws_state,{org_id}).

init(_TransportName, Req, _Opts) ->
  pre_setup(Req),
  {ok,Req,#gen_ws_state{}}.

pre_setup(Req) ->
  {FullPath, _Req2} = cowboy_req:path(Req),
  OrgName = console_utils:strip_org_path(FullPath),
  case console_operations:get_primary_id(get_org,"org_name",OrgName,["org_id"]) of
    {response,[[OrgId]]} ->
      ets:insert(?ControllerEts,{OrgId,self()}),
      io:format("Websocket connection has been established~n");
    {not_found} ->
      io:format("Websocket connection not established~n"),
      {error,<<"Invalid Organization Name">>}
  end.

handle_info({set_feature, Data},Req,State) ->
  {ok,JsonResponse} = json_encoder:build(Data),
  {reply, <<JsonResponse/binary>> ,Req,State};

handle_info(Data,Req,State) ->
  io:format("Data : ~p~n",[Data]),
  {ok,Req,State}.

handle_data(get_feature,Req,State) ->
  [OrgId] = ets:select(controller,[{{'$1','$2'},[{'==','$2',self()}],['$1']}]),
  {response,AppIdLIst} = console_operations:get_primary_id(get_app_id,"org_id",OrgId,["app_id"]),
  Resp = get_app_feature(AppIdLIst,[]),
  {ok,JsonResponse} = json_encoder:build({feature,Resp}),
  {reply,<<JsonResponse/binary>>,Req,State};

handle_data(Data,Req,State) ->
  io:format("Data : ~p~n",[Data]),
  {reply,<<"hello_from_websocket">>,Req,State}.

%%------------------------------------------------------------------------------------------------
%%Internal Api
%%------------------------------------------------------------------------------------------------

get_app_feature([],Acc) -> Acc;
get_app_feature([[AppId]|Rest],Acc) ->
  Record = db_query:read_record(fetch_features,
    [{[#root.application],[{#application.token,AppId}]}]),
  {success,Resp} = gen_console_manager_box:manager_call(make_call,medium,Record,?ConsoleManagerKey),
  get_app_feature(Rest,Acc ++ [{AppId,Resp}]).













%%------------------------------------------------------------------------------------------------
%%Deprecated Api
%%------------------------------------------------------------------------------------------------

%%get_org_id(OrgName) ->
%%  case console_operations:get_primary_id(get_org,"org_name",OrgName,["org_id"]) of
%%    {response,OrgId} ->  OrgId;
%%    {not_found} -> not_found
%%  end.

%%get_app_ids(What,CompareKey,Value,ExpectedKey) ->
%%  Record = db_query:read_record_only_if(What,[#condition{attribute = CompareKey,compare_by = #compare.equal,value = Value}],ExpectedKey),
%%  case gen_console_manager_box:manager_call(make_call,medium,Record,?ConsoleManagerKey) of
%%    {success,{response,Response}} -> {response,Response};
%%    Any -> io:format("Any : ~p~n",[Any]), {not_found}
%%  end.

%%handle_info(get_feature,Req,State) ->
%%  {response,AppIdLIst} = get_app_ids(get_app_id,"org_id",<<"1503163679156053">>,["app_id"]),
%%  Resp = get_app_feature(AppIdLIst,[]),
%%  {ok,JsonResponse} = json_encoder:build({feature,Resp}),
%%  {reply,<<JsonResponse/binary>>,Req,State}.
