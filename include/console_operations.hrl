%%%-------------------------------------------------------------------
%%% @author Aiden_Richie
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2017 12:30 PM
%%%-------------------------------------------------------------------
-author("Aiden_Richie").

-record(console_operations,{
  register_organization,add_organization_contact,add_organization_address,
  add_organization_admin,update_organization_admin_privilege,update_organization_contact,
  update_organization_address,update_organization_name,update_organization_owner,remove_organization_admin,

  register_application,add_application_admin,update_application_admin_privilege,
  update_application_name,update_application_description,remove_application_admin,
  get_applications,

  register_user,sign_in,validate_registration,single_sign_on,get_privileges,set_feature,get_feature,sign_out
}).

-record(console_req,{operation_id,params}).