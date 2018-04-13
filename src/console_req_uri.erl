%%%-------------------------------------------------------------------
%%% @author Aiden_Richie
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2017 12:27 PM
%%%-------------------------------------------------------------------
-module(console_req_uri).
-author("Aiden_Richie").

-export([get_operation_id/1,get_console_operation/1]).
-include("console_operations.hrl").

%%%===================================================================
%%% URI for Organizations
%%%===================================================================

get_operation_id(<<"register_organization">>) -> #console_operations.register_organization;
get_operation_id(<<"add_organization_contact">>) -> #console_operations.add_organization_contact;
get_operation_id(<<"add_organization_address">>) -> #console_operations.add_organization_address;
get_operation_id(<<"add_organization_admin">>) -> #console_operations.add_organization_admin;
get_operation_id(<<"update_organization_admin_privilege">>) -> #console_operations.update_organization_admin_privilege;
get_operation_id(<<"update_organization_contact">>) -> #console_operations.update_organization_contact;
get_operation_id(<<"update_organization_address">>) -> #console_operations.update_organization_address;
get_operation_id(<<"update_organization_name">>) -> #console_operations.update_organization_name;
get_operation_id(<<"update_organization_owner">>) -> #console_operations.update_organization_owner;
get_operation_id(<<"remove_organization_admin">>) -> #console_operations.remove_organization_admin;

%%%===================================================================
%%% URI for Applications
%%%===================================================================

get_operation_id(<<"register_application">>) -> #console_operations.register_application;
get_operation_id(<<"get_privileges">>) -> #console_operations.get_privileges;
get_operation_id(<<"add_application_admin">>) -> #console_operations.add_application_admin;
get_operation_id(<<"update_application_admin_privilege">>) -> #console_operations.update_application_admin_privilege;
get_operation_id(<<"update_application_name">>) -> #console_operations.update_application_name;
get_operation_id(<<"update_application_description">>) -> #console_operations.update_application_description;
get_operation_id(<<"remove_application_admin">>) -> #console_operations.remove_application_admin;

%%%===================================================================
%%% URI for User
%%%===================================================================

get_operation_id(<<"single_sign_on">>) -> #console_operations.single_sign_on;
get_operation_id(<<"validate_registration">>) -> #console_operations.validate_registration;
get_operation_id(<<"sign_in">>) -> #console_operations.sign_in;
get_operation_id(<<"sign_out">>) -> #console_operations.sign_out;
get_operation_id(<<"register_user">>) -> #console_operations.register_user;

%%%===================================================================
%%% URI for Settings (privileges)
%%%===================================================================

get_operation_id(<<"set_feature">>) -> #console_operations.set_feature;
get_operation_id(<<"get_feature">>) -> #console_operations.get_feature.

%%%===================================================================
%%% URI for each Operations
%%%===================================================================

get_console_operation(Value) ->
  Tuple = list_to_tuple(record_info(fields,console_operations)),
  atom_to_binary(element(Value-1, Tuple),utf8).

