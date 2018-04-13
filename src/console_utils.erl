%%%-------------------------------------------------------------------
%%% @author Aiden_Richie
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2017 12:26 PM
%%%-------------------------------------------------------------------
-module(console_utils).
-author("Aiden_Richie").

-include("http_utils.hrl").
-export([get_timestamp/0,strip_path/1,strip_app_path/1,form_key_pair/1,strip_org_path/1]).

get_timestamp() -> calendar:datetime_to_gregorian_seconds(erlang:universaltime()).

strip_path(<<_,Path/binary>>) -> Path.

strip_app_path(<<"/app/",Path/binary>>) -> Path.

strip_org_path(<<"/ws/",Org/binary>>) -> Org.

form_key_pair(L) -> form_key_pair(L,[]).

form_key_pair([],Acc) -> lists:reverse(Acc);
form_key_pair([_Key,undefined|Rest],Acc) -> form_key_pair(Rest,Acc);
form_key_pair([Key,Val|Rest],Acc) -> form_key_pair(Rest, [{Key,Val}|Acc]).

%%  action,org_id,org_name,org_owner,org_desc,created_date,
%%  mobile_office,mobile_home,mobile_others,
%%  land_line_office,land_line_home,land_line_others,
%%  fax_office,fax_home,fax_others,
%%  email_office,email_home,email_others,
%%  web_address_id,participant_url,social_network_id,
%%  token_data,token_valid_from,valid_to,
%%  admin_id,privilege_set,
%%  door_no,street_name,area_name,city,district,state,country,pin_code,
%%
%%  app_id,app_name,app_desc,
%%  user_id,user_name,first_name,last_name,password,email,user_type,ph,dob,location,
%%
%%  privilege,buddy_chat,group_chat,member_limit,maximum_users,
%%  maximum_public_channels,maximum_private_channels

%%form_key_value_rec(#params{action = Action} = Data,Acc) -> form_key_value_rec(Data,[Acc|{}])
