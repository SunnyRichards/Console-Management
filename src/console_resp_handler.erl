%%%-------------------------------------------------------------------
%%% @author Aiden_Richie
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2017 12:27 PM
%%%-------------------------------------------------------------------
-module(console_resp_handler).
-author("Aiden_Richie").

-include("http_utils.hrl").
%% API
-export([send_resp/4]).

%%send_resp(?SUCCESS,Data,Req,State) ->
%%  {ok, Resp} = json_encoder:build(Data),
%%  io:format("Resp: ~p~n",[Resp]),
%%  {ok, Req2} = cowboy_req:reply(200,
%%    [{<<"content-type">>, <<"application/json">>},
%%      {<<"access-control-allow-origin">>, <<"*">>}
%%    ],
%%    Resp,Req),
%%  {halt, Req2, State};

send_resp(?SUCCESS,Data,Req,State) ->
  {ok, Resp} = json_encoder:build(console_utils:form_key_pair(Data)),
  io:format("Resp: ~p~n",[Resp]),
  {ok, Req2} = cowboy_req:reply(200,
    [{<<"content-type">>, <<"application/json">>},
      {<<"access-control-allow-origin">>, <<"*">>}
    ],
    Resp,Req),
  {halt, Req2, State};

send_resp(?FAILURE,Data,Req,State) ->
  {ok, Resp} = json_encoder:build(console_utils:form_key_pair(Data)),
  io:format("Resp: ~p~n",[Resp]),
  {ok, Req2} = cowboy_req:reply(400,
    [{<<"content-type">>, <<"application/json">>}],
    Resp,Req),
  {halt, Req2, State}.
