%%%-------------------------------------------------------------------
%%% @author Aiden_Richie
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2017 1:30 PM
%%%-------------------------------------------------------------------
-author("Aiden_Richie").

%%  org_id,org_name,org_owner,created_date,
%%  mobile_office,mobile_home,mobile_others,
%%  land_line_office,land_line_home,land_line_others,
%%  fax_office,fax_home,fax_others,
%%  email_office,email_home,email_others,
%%  web_address_id,participant_url,social_network_id,
%%  token_data,token_valid_from,valid_to,
%%  admin_id,privilege_set,
%%  door_no,street_name,area_name,city,district,state,country,pin_code,
%%
%%  app_id,app_name,app_desc

%%%===================================================================
%%% Parameters for organization
%%%===================================================================

-define(ORG_NAME, <<"org_name">>).
-define(ORG_ID,<<"org_id">>).
-define(ORG_OWNER, <<"org_owner">>).
-define(CREATED_DATE, <<"created_date">>).
-define(DESCRIPTION, <<"description">>).
-define(MOBILE_OFFICE, <<"mobile_office">>).
-define(MOBILE_HOME, <<"mobile_home">>).
-define(MOBILE_OTHERS, <<"mobile_others">>).
-define(LL_OFFICE, <<"land_line_office">>).
-define(LL_HOME, <<"land_line_home">>).
-define(LL_OTHERS, <<"land_line_others">>).
-define(WEBSITE, <<"website">>).

%%%===================================================================
%%% Parameters for participant
%%%===================================================================

-define(USER_NAME, <<"user_name">>).
-define(FIRST_NAME, <<"first_name">>).
-define(LAST_NAME, <<"last_name">>).
-define(PASSWORD, <<"password">>).
-define(EMAIL, <<"email">>).
-define(USER_TYPE, <<"user_type">>).
-define(PHONE, <<"ph">>).
-define(DOB, <<"dob">>).
-define(LOCATION, <<"location">>).
-define(USER_ID, <<"user_id">>).
-define(APP_KEY, <<"app_key">>).
-define(APP_NAME, <<"app_name">>).
-define(Client_Token, <<"client_token">>).

-define(PRIVILEGE, <<"privilege">>).
-define(FEATURE_ID, <<"feature_id">>).
-define(FEATURE, <<"feature">>).
-define(FEATURE_LIST, <<"feature_list">>).

%%%===================================================================
%%% Parameters for Privileges (PARTICIPANT)
%%%===================================================================

-define(BUDDY_CHAT, <<"buddy_chat">>).
-define(GROUP_CHAT, <<"group_chat">>).
-define(MEMBER_LIMIT, <<"member_limit">>).
-define(MAXIMUM_USERS, <<"maximum_users">>).
-define(MAXIMUM_PUBLIC_CHANNELS, <<"maximum_public_channels">>).
-define(MAXIMUM_PRIVATE_CHANNELS, <<"maximum_private_channels">>).

%%%===================================================================
%%% Parameters for Device (PARTICIPANT)
%%%===================================================================

-define(ControllerEts,controller).