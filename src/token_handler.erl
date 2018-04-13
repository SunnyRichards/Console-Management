%%%-------------------------------------------------------------------
%%% @author Aiden_Richie
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2017 5:01 PM
%%%-------------------------------------------------------------------
-module(token_handler).
-author("Aiden_Richie").

%% API
-export([create_orgId/0,create_appId/0,create_userId/0,create_appKey/1,create_appKey/0,create_session_token/0]).

create_orgId()->
  try
    RandNum = integer_to_binary(crypto:rand_uniform(10000,19999)),
    Number = integer_to_binary(calendar:datetime_to_gregorian_seconds(erlang:universaltime())),
    <<RandNum/binary,Number/binary>>
  catch
    Exception:Reason -> {caught,Exception,Reason}
  end.

create_appId()->
  try
    RandNum = integer_to_binary(crypto:rand_uniform(20000,29999)),
    Number = integer_to_binary(calendar:datetime_to_gregorian_seconds(erlang:universaltime())),
    <<RandNum/binary,Number/binary>>
  catch
    Exception:Reason -> {caught,Exception,Reason}
  end.

create_appKey()->
  try
    RandNum = integer_to_binary(crypto:rand_uniform(30000,39999)),
    Number = integer_to_binary(calendar:datetime_to_gregorian_seconds(erlang:universaltime())),
    <<RandNum/binary,Number/binary>>
  catch
    Exception:Reason -> {caught,Exception,Reason}
  end.

create_appKey(AppId)->
  try
      RandKey = integer_to_binary(crypto:rand_uniform(30000,39999)),
      UtcTime = integer_to_binary(calendar:datetime_to_gregorian_seconds(erlang:universaltime())),
      RandNum = crypto:strong_rand_bytes(16),
      Key = <<RandKey/binary,UtcTime/binary>>,
      EData = crypto:block_encrypt(aes_cfb128,Key,AppId,RandNum),
      EData
  catch
    Exception:Reason -> {caught,Exception,Reason}
  end.

create_userId()->
  try
    RandNum = integer_to_binary(crypto:rand_uniform(40000,49999)),
    Number = integer_to_binary(calendar:datetime_to_gregorian_seconds(erlang:universaltime())),
    <<RandNum/binary,Number/binary>>
  catch
    Exception:Reason -> {caught,Exception,Reason}
  end.

create_session_token() ->
  try
    RandNum = integer_to_binary(crypto:rand_uniform(50000,59999)),
    Number = integer_to_binary(calendar:datetime_to_gregorian_seconds(erlang:universaltime())),
    <<RandNum/binary,Number/binary>>
  catch
    Exception:Reason -> {caught,Exception,Reason}
  end.
