%%%-------------------------------------------------------------------
%%% @author Aiden_Richie
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2017 12:22 PM
%%%-------------------------------------------------------------------
-author("Aiden_Richie").

-define(ACTION,<<"action">>).
-define(SUCCESS,ok).
-define(FAILURE,error).
-define(STATUS,<<"status">>).
-define(CurrentTime,calendar:datetime_to_gregorian_seconds(erlang:universaltime())).

-record(params, {action,org_id,org_name,org_owner, desc,created_date,
                 mobile_office,mobile_home,mobile_others,
                 land_line_office,land_line_home,land_line_others,
                 fax_office,fax_home,fax_others,
                 email_office,email_home,email_others,
                 web_address_id,participant_url,social_network_id,
                 token_data,token_valid_from,valid_to,
                 admin_id,privilege_set,
                 door_no,street_name,area_name,city,district,state,country,pin_code,

                 app_id,app_name,
                 user_id,user_name,first_name,last_name,password,email,user_type,ph,dob,location,

                 privilege,buddy_chat,group_chat,member_limit,maximum_users,
                 maximum_public_channels,maximum_private_channels,

                 feature,feature_id,app_key,client_token

}).


-record(client_token,{token,user_id,session_time}).