%%%-------------------------------------------------------------------
%%% @author kishore
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Nov 2017 3:24 PM
%%%-------------------------------------------------------------------
-module(console_ws_handler).
-author("kishore").

%% API
-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(ws_state,{mod = gen_websocket_handler,gen_ws_state,ets_id}).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(TransportName, Req, Opts) ->
  case gen_websocket_handler:init(TransportName, Req, Opts) of
    {ok,Req,NewState} ->
      {ok,Req,#ws_state{gen_ws_state = NewState}};
    {error,_Reason} ->
      {shutdown,Req}
  end.

websocket_handle({text, Msg}, Req, #ws_state{mod = Mod,gen_ws_state = ChildState} = State) ->
  case Mod:handle_data(Msg,Req,ChildState) of
    {reply,Response,Req,NewChildState} ->
      {reply, {text, << "Response ", Response/binary >>}, Req, State#ws_state{gen_ws_state = NewChildState}};
    {error,_Reason,Req,NewChildState} ->
      {ok,Req,State#ws_state{gen_ws_state = NewChildState}}
  end;

websocket_handle({binary,Msg}, Req, State) ->
  {reply, {binary, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data,Req,State) ->
  {ok,Req,State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
  erlang:start_timer(1000, self(), <<"How' you doin'?">>),
  {reply, {text, Msg}, Req, State};
websocket_info(Data, Req, #ws_state{mod = Mod,gen_ws_state = ChildState} = State) ->
  case Mod:handle_info(Data,Req,ChildState) of
    {reply,Response,Req,NewChildState} ->
      {reply, {text, Response}, Req, State#ws_state{gen_ws_state = NewChildState}};
    {error,_Reason} ->
      {ok,Req,State}
  end;
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(Reason, _Req, _State) ->
  io:format("Reason : ~p~n",[Reason]),
  ok.
