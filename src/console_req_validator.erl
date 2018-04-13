%%%-------------------------------------------------------------------
%%% @author Aiden_Richie
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2017 1:14 PM
%%%-------------------------------------------------------------------
-module(console_req_validator).
-author("Aiden_Richie").

-export([required_params/2, verify_params/2,verify_key/1,verify_client_token/1]).

-include("http_utils.hrl").
-include("console_operations.hrl").

-define(Minutes,10).

verify_params(#console_operations.sign_in,_Params) -> valid;
verify_params(#console_operations.validate_registration,_Params) -> valid;
verify_params(#console_operations.single_sign_on,_Params) -> valid;
verify_params(#console_operations.sign_out,_Params) -> valid;

verify_params(OperationId,Params) -> verify_key(required_params(OperationId,Params)).

verify_key([]) -> valid;
verify_key([{Key,undefined}|_]) -> {missing,Key};
%%verify_key(invalid) -> invalid;
verify_key({invalid,Reason}) -> {invalid,Reason};
verify_key([{_,_}|Tail]) -> verify_key(Tail).


verify_client_token(ClientToken) ->
  case cache_db:execute(cache_db:read(client_token,ClientToken)) of
    [{client_token,ClientToken,_,OldTime}] ->
      case ?CurrentTime - OldTime < 60 * ?Minutes of
        true ->
          cache_db:execute(cache_db:update(client_token,ClientToken,fun(Old) -> Old#client_token{session_time = ?CurrentTime} end)),
          valid;
        false ->
          cache_db:execute(cache_db:delete(client_token,ClientToken)),
          {invalid,<<"Session_time_out">>}
      end;
    [] -> {invalid,<<"Sorry, Invalid Client token">>}
  end.

%%%===================================================================
%%% API Calls based on Organization
%%%===================================================================

required_params(#console_operations.register_organization,#params{client_token = ClientToken} = Params)  ->
  case verify_client_token(ClientToken) of
    valid ->
      [{#params.org_name,Params#params.org_name},{#params.org_owner,Params#params.org_owner},
      {#params.ph,Params#params.ph},{#params.location,Params#params.location},
      {#params.created_date,Params#params.created_date},{#params.web_address_id,Params#params.web_address_id}];
    Error -> Error
  end;

required_params(#console_operations.set_feature,#params{client_token = ClientToken} = Params)  ->
  case verify_client_token(ClientToken) of
    valid ->
      [{#params.app_key,Params#params.app_key},{#params.feature_id,Params#params.feature_id},
        {#params.feature,Params#params.feature}];
    Error -> Error
  end;

required_params(#console_operations.get_feature,#params{client_token = ClientToken} = Params)  ->
  case verify_client_token(ClientToken) of
    valid -> [{#params.app_key,Params#params.app_key}];
    Error -> Error
  end;

%%  required_params(#console_operations.add_organization_contact,#params{client_token = _ClientToken} = Params)  ->
%%    [{#params.org_id,Params#params.org_id},{#params.mobile_home,Params#params.mobile_home},
%%     {#params.mobile_office,Params#params.mobile_office},{#params.mobile_others,Params#params.mobile_others},
%%     {#params.land_line_home,Params#params.land_line_home},{#params.land_line_office,Params#params.land_line_office},
%%     {#params.land_line_others,Params#params.land_line_others},{#params.fax_home,Params#params.fax_home},
%%     {#params.fax_office,Params#params.fax_office},{#params.fax_others,Params#params.fax_others},
%%     {#params.email_home,Params#params.email_home},{#params.email_office,Params#params.email_office},
%%     {#params.email_others,Params#params.email_others},{#params.web_address_id,Params#params.web_address_id},
%%     {#params.participant_url,Params#params.participant_url},{#params.social_network_id,Params#params.social_network_id}];

required_params(#console_operations.add_organization_contact,#params{client_token = _ClientToken} = Params)  ->
  [{#params.org_id,Params#params.org_id},{#params.mobile_office,Params#params.mobile_office},
   {#params.land_line_office,Params#params.land_line_office},{#params.fax_office,Params#params.fax_office},
   {#params.email_office,Params#params.email_office},{#params.web_address_id,Params#params.web_address_id}];

required_params(#console_operations.add_organization_address,#params{client_token = _ClientToken} = Params)  ->
  [{#params.org_id,Params#params.org_id},{#params.door_no,Params#params.door_no},
    {#params.street_name,Params#params.street_name},{#params.area_name,Params#params.area_name},
    {#params.city,Params#params.city},{#params.district,Params#params.district},
    {#params.state,Params#params.state},{#params.country,Params#params.country},
    {#params.pin_code,Params#params.pin_code}];

required_params(#console_operations.add_organization_admin,#params{client_token = _ClientToken} = Params)  ->
  [{#params.org_id,Params#params.org_id},
   {#params.admin_id,Params#params.admin_id},
   {#params.privilege_set,Params#params.privilege_set}];

required_params(#console_operations.update_organization_admin_privilege,#params{client_token = _ClientToken} = Params)  ->
  [{#params.org_id,Params#params.org_id},
   {#params.admin_id,Params#params.admin_id},
   {#params.privilege_set,Params#params.privilege_set}];

%%  required_params(#console_operations.update_organization_contact,#params{client_token = _ClientToken} = Params)  ->
%%    [{#params.org_id,Params#params.org_id},{#params.mobile_home,Params#params.mobile_home},
%%     {#params.mobile_office,Params#params.mobile_office},{#params.mobile_others,Params#params.mobile_others},
%%     {#params.land_line_home,Params#params.land_line_home},{#params.land_line_office,Params#params.land_line_office},
%%     {#params.land_line_others,Params#params.land_line_others},{#params.fax_home,Params#params.fax_home},
%%     {#params.fax_office,Params#params.fax_office},{#params.fax_others,Params#params.fax_others},
%%     {#params.email_home,Params#params.email_home},{#params.email_office,Params#params.email_office},
%%     {#params.email_others,Params#params.email_others},{#params.web_address_id,Params#params.web_address_id},
%%     {#params.participant_url,Params#params.participant_url},{#params.social_network_id,Params#params.social_network_id}];

required_params(#console_operations.update_organization_contact,#params{client_token = _ClientToken} = Params)  ->
  [{#params.org_id,Params#params.org_id},{#params.mobile_office,Params#params.mobile_office},
    {#params.land_line_office,Params#params.land_line_office},{#params.fax_office,Params#params.fax_office},
    {#params.email_office,Params#params.email_office},{#params.web_address_id,Params#params.web_address_id}];

required_params(#console_operations.update_organization_address,#params{client_token = _ClientToken} = Params)  ->
  [{#params.org_id,Params#params.org_id},{#params.door_no,Params#params.door_no},
   {#params.street_name,Params#params.street_name},{#params.area_name,Params#params.area_name},
   {#params.city,Params#params.city},{#params.district,Params#params.district},
   {#params.state,Params#params.state},{#params.country,Params#params.country},
   {#params.pin_code,Params#params.pin_code}];

required_params(#console_operations.update_organization_name,#params{client_token = _ClientToken} = Params)  ->
  [{#params.org_id,Params#params.org_id},
   {#params.org_name,Params#params.org_name}];

required_params(#console_operations.update_organization_owner,#params{client_token = _ClientToken} = Params)  ->
  [{#params.org_id,Params#params.org_id},
   {#params.org_owner,Params#params.org_owner}];

required_params(#console_operations.remove_organization_admin,#params{client_token = _ClientToken} = Params)  ->
  [{#params.org_id,Params#params.org_id},
   {#params.org_owner,Params#params.org_owner}];

%%%===================================================================
%%% API Calls based Application
%%%===================================================================

required_params(#console_operations.register_application,#params{client_token = ClientToken} = Params)  ->
  case verify_client_token(ClientToken) of
    valid ->
      [{#params.org_name,Params#params.org_name},
        {#params.app_name,Params#params.app_name},{#params.desc,Params#params.desc}];
    Error -> Error
  end;

required_params(#console_operations.get_privileges,#params{client_token = _ClientToken} = Params)  ->
  [{#params.app_key,Params#params.app_key},
   {#params.buddy_chat,Params#params.buddy_chat},{#params.group_chat,Params#params.group_chat},
   {#params.member_limit,Params#params.member_limit},{#params.maximum_users,Params#params.maximum_users},
   {#params.maximum_public_channels,Params#params.maximum_public_channels},
   {#params.maximum_private_channels,Params#params.maximum_private_channels}];

required_params(#console_operations.add_application_admin,#params{client_token = _ClientToken} = Params)  ->
  [{#params.app_key,Params#params.app_key},
   {#params.admin_id,Params#params.admin_id},
   {#params.privilege_set,Params#params.privilege_set}];

required_params(#console_operations.get_applications,#params{client_token = _ClientToken} = Params)  ->
  [{#params.org_id,Params#params.org_id}];

required_params(#console_operations.update_application_admin_privilege,#params{client_token = _ClientToken} = Params)  ->
  [{#params.app_key,Params#params.app_key},
   {#params.admin_id,Params#params.admin_id},
   {#params.privilege_set,Params#params.privilege_set}];

required_params(#console_operations.update_application_name,#params{client_token = _ClientToken} = Params)  ->
  [{#params.app_key,Params#params.app_key},{#params.app_name,Params#params.app_name}];

required_params(#console_operations.update_application_description,#params{client_token = _ClientToken} = Params)  ->
  [{#params.app_key,Params#params.app_key},{#params.desc,Params#params.desc}];

required_params(#console_operations.remove_application_admin,#params{client_token = _ClientToken} = Params)  ->
  [{#params.app_key,Params#params.app_key},{#params.admin_id,Params#params.admin_id}];



%%%===================================================================
%%% API Calls based User
%%%===================================================================

required_params(#console_operations.register_user,#params{client_token = _ClientToken} = Params)  ->
  [{#params.user_name,Params#params.user_name}, {#params.first_name,Params#params.first_name},
    {#params.last_name,Params#params.last_name}, {#params.password,Params#params.password},
    {#params.email,Params#params.email}, {#params.user_type,Params#params.user_type},
    {#params.dob,Params#params.dob},
    {#params.ph,Params#params.ph}, {#params.location,Params#params.location}].