%%%-------------------------------------------------------------------
%%% @author Aiden_Richie
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2017 1:29 PM
%%%-------------------------------------------------------------------
-module(console_operations).
-author("Aiden_Richie").

-include("http_utils.hrl").
-include("console_params.hrl").
-include("console_operations.hrl").
-include_lib("gabby_reference/include/credentials.hrl").

-export([handle_operation/1,get_primary_id/4]).
-include("data_store.hrl").
-include("db_helper.hrl").

%%%===================================================================
%%% API Calls based on Organization
%%%===================================================================
handle_operation(#console_req{operation_id = #console_operations.sign_out,params = #params{client_token = ClientToken}}) ->
  case console_req_validator:verify_client_token(ClientToken) of
    valid ->
      cache_db:execute(cache_db:delete(client_token,ClientToken)),
      handle_db_resp(#console_operations.sign_out,{ok,<<"User Signed Out successfully">>});
    {invalid,Reason} -> handle_db_resp(#console_operations.sign_out,{ok,Reason})
  end;

handle_operation(#console_req{operation_id = #console_operations.register_organization,params = #params{} = Data}) ->
  #params{org_name = OrgName,org_owner = UserName,web_address_id = WebSite,created_date = CreatedDate,ph = Phone,location = Location} = Data,
  OrgId = token_handler:create_orgId(),
  case get_primary_id(sign_in,"user_name",UserName,["user_id"]) of
    {response,[[UserId]]} ->
      OrgRecord = db_query:conform_new_record(new_organization,
        [{[#root.organization],
          [{#organization.id,OrgId},{#organization.name,OrgName},
            {#organization.created_on,CreatedDate},{#organization.owner,UserId},
            {[#organization.address],[{#address.city,Location}]},
            {[#organization.contact],[{#contact.mobile,Phone},{#contact.web_address,WebSite}]}]}]),
      case gen_console_manager_box:manager_call(make_call,medium,OrgRecord,?ConsoleManagerKey) of
        {success,{success}} -> handle_db_resp(#console_operations.register_organization,{ok,<<"Organization Successfully Registered">>});
        Any -> io:format("The received response: ~p~n",[Any]), handle_db_resp(#console_operations.set_feature,{ok,<<"Please give a valid credentials">>})
      end;
    {not_found} -> handle_db_resp(#console_operations.register_organization,{ok,<<"Please give a valid Owner's User name">>})
  end;


%%-todo have to check duplicate application name before register the application
handle_operation(#console_req{operation_id = #console_operations.register_application,params = #params{} = Data}) ->
  #params{org_name = OrgName,app_name = AppName,desc = AppDescription} = Data,
  AppId = token_handler:create_appId(),
  AppKey = token_handler:create_appKey(),
  case get_primary_id(get_org,"org_name",OrgName,["org_id"]) of
    {response,[[OrgId]]} ->
      Record = db_query:conform_new_record(new_application,
        [{[#root.organization],[{#organization.id,OrgId}]},
          {[#root.application],
            [{#application.id,AppId},{#application.name,AppName},
              {#application.description,AppDescription},{#application.token,AppKey}]}]),
      case gen_console_manager_box:manager_call(make_call,medium,Record,?ConsoleManagerKey) of
        {success,{success}} -> handle_db_resp(#console_operations.register_application,{ok,[AppKey]});
        Any -> io:format("The received response: ~p~n",[Any]), handle_db_resp(#console_operations.register_application,{ok,<<"Please give a valid credentials">>})
      end;
    {not_found} ->
      handle_db_resp(#console_operations.register_application,{ok,<<"Please give a valid organization name">>})
  end;

handle_operation(#console_req{operation_id = #console_operations.set_feature,params = #params{} = Data}) ->
  #params{app_key = AppKey,feature_id = FeatureId,feature = FeatureVal,org_id = OrgId} = Data,
  case get_primary_id(get_app_id,"app_key",AppKey,["app_id"]) of
    {response,[[AppId]]} ->
      case get_primary_id(get_feature_id,"feature_id",FeatureId,["feature_id"]) of
        {response,_FeatureId2} ->
          FeatureRecord = db_query:read_record_only_if(new_feature_val,[#condition{attribute = "app_id",compare_by = #compare.equal,value = AppId},
              #condition{attribute = "feature_id",compare_by = #compare.equal,value = FeatureId}],["feature_data"]),
          FeatureRecord2 = case gen_console_manager_box:manager_call(make_call,medium,FeatureRecord,?ConsoleManagerKey) of
                             {success,{response,[]}} ->
                               db_query:conform_new_record(new_feature_val,[{[#root.application],
                                 [{#application.id,AppId},{[#application.feature],[{#feature.id,FeatureId},{#feature.feature_data,FeatureVal}]}]}]);
                             {success,{response,[_FeatureValue]}} ->
                               db_query:update_record_only_if(new_feature_val,[#condition{attribute = "app_id",compare_by = #compare.equal,value = AppId},
                                 #condition{attribute = "feature_id",compare_by = #compare.equal,value = FeatureId}],
                                 [{[#root.application,#application.feature],[{#feature.feature_data,FeatureVal}]}]);
                             Error ->
                               io:format("Error: ~p~n",[Error]),
                               {error,Error}
                           end,
          case make_manager_call(FeatureRecord2) of
            {success,{success}} ->
              send_to_controller(AppId,OrgId),
              handle_db_resp(#console_operations.set_feature,{ok,<<"Successfully feature has been updated">>});
            error -> handle_db_resp(#console_operations.set_feature,{ok,<<"Please give a valid credentials">>})
          end;
        {not_found} -> handle_db_resp(#console_operations.set_feature,{ok,<<"Please give a valid Feature">>})
      end;
    {not_found} -> handle_db_resp(#console_operations.set_feature,{ok,<<"Please give a valid Application Key">>})
  end;

handle_operation(#console_req{operation_id = #console_operations.get_feature,params = #params{} = Data}) ->
  #params{app_key = AppKey} = Data,
  {response,[[AppId]]} = get_primary_id(get_app_id,"app_key",AppKey,["app_id"]),
  Record = db_query:read_record(fetch_features,
    [{[#root.application],[{#application.token,AppId}]}]),
  {success,Resp} = gen_console_manager_box:manager_call(make_call,medium,Record,?ConsoleManagerKey),
  handle_db_resp(#console_operations.get_feature,{ok,Resp});

handle_operation(#console_req{operation_id = #console_operations.register_user,params = Data}) ->
  case validate_credentials(Data) of
    valid ->
      #params{user_name = Username,first_name = FirstName,last_name = LastName,password = Password,dob = Dob,
        email = Mail,ph = Phone,user_type = UserType,location = Location} = Data,
      UserId = token_handler:create_userId(),
      Record = db_query:conform_new_record(new_user,
        [{[#root.user],
          [{#user.id,UserId},{#user.date_of_birth,Dob},
            {[#user.name],[{#name.user_name,Username},{#name.first_name,FirstName},{#name.last_name,LastName}]},
            {#user.password,Password},{#user.user_type,UserType},
            {[#user.contact],[{#contact.email,Mail},{#contact.mobile,Phone},{#contact.address,Location}]}]}]),
      Resp = gen_console_manager_box:manager_call(make_call,medium,Record,?ConsoleManagerKey),
      case Resp of
        {success,{success}} -> handle_db_resp(#console_operations.register_user,{ok,<<"Successfully Registered">>});
        {success,{error,{_,_,Reason}}} -> handle_db_resp(#console_operations.register_user,{ok,Reason})
      end;
    {invalid,Reason} -> handle_db_resp(#console_operations.register_user,{ok,Reason})
  end;

handle_operation(#console_req{operation_id = #console_operations.validate_registration, params = Params}) ->
  {Attribute,Value} = get_condition_params(Params),
  Record = db_query:read_record_only_if(validate_reg,[#condition{attribute = Attribute,compare_by = #compare.equal,value = Value}],[Attribute]),
  case gen_console_manager_box:manager_call(make_call,medium,Record,?ConsoleManagerKey) of
    {success,{response,[]}} -> handle_db_resp(#console_operations.validate_registration,{ok,<<"Valid">>});
    Any ->
      io:format("The receieved response: ~p~n",[Any]),
      handle_db_resp(#console_operations.sign_in,{ok,<<"Already Exist">>})
  end;

handle_operation(#console_req{operation_id = #console_operations.sign_in,
  params = #params{user_name = undefined,email = undefined,ph = undefined,password = undefined} }) ->
    handle_db_resp(#console_operations.sign_in,{ok,<<"Invalid UserName or Password">>});

handle_operation(#console_req{operation_id = #console_operations.sign_in,params = Params })->
  {{Attribute,Value},Password} = get_condition_params(Params),
  Record = db_query:read_record_only_if(sign_in,[#condition{attribute = Attribute,compare_by = #compare.equal,value = Value}],["user_id","password"]),
  case gen_console_manager_box:manager_call(make_call,medium,Record,?ConsoleManagerKey) of
    {success,{response,[[UserId,Password]]}} ->
      ClientToken = token_handler:create_session_token(),
      cache_db:execute(cache_db:write_new(client_token,ClientToken,
        #client_token{token = ClientToken,user_id = UserId,session_time = ?CurrentTime})),
      handle_db_resp(#console_operations.sign_in,{ok,[ClientToken]});
    Any -> io:format("The receieved response: ~p~n",[Any]), handle_db_resp(#console_operations.sign_in,{ok,<<"Invalid UserName or Password">>})
  end;

handle_operation(#console_req{operation_id = #console_operations.single_sign_on,params = Params })->
  case handle_operation(#console_req{operation_id = #console_operations.sign_in,params = Params}) of
    {?SUCCESS,[_,_,_,<<"Successfully Logged in">>]} ->
      {{Attribute,Value},_Password} = get_condition_params(Params),
      ExpectedKeys = ["fname","lname","user_name","dob","user_type","email","phone","location"],
      Record = db_query:read_record_only_if(sign_in,[#condition{attribute = Attribute,compare_by = #compare.equal,value = Value}],ExpectedKeys),
      case gen_console_manager_box:manager_call(make_call,medium,Record,?ConsoleManagerKey) of
        {success,{response,[User_Details]}} ->
          {ok,Response} = json_encoder:build(form_tuple(ExpectedKeys,User_Details,[])),
          io:format("~nJson REsponse : ~p~n",[Response]),
          handle_db_resp(#console_operations.single_sign_on,{ok,Response});
        Any -> io:format("The receieved response: ~p~n",[Any]), handle_db_resp(#console_operations.single_sign_on,{ok,<<"Error in single_sign_on">>})
      end;
    {?SUCCESS,[_,_,_,<<"Invalid UserName or Password">>]} -> handle_db_resp(#console_operations.single_sign_on,{ok,<<"Invalid UserName or Password">>})
  end;

handle_operation(#console_req{operation_id = #console_operations.add_organization_contact,params = #params{} = Data}) ->
  #params{org_id = OrgId,mobile_office = MobOffice,land_line_office = LLOffice,fax_office = FaxOffice,
          email_office = MailOffice,web_address_id = WebAddress} = Data,

  Record = db_query:conform_new_record(add_organization_contact,
    [{[#root.organization],[{#organization.id,OrgId},
     {[#organization.contact],
      [{#contact.mobile,MobOffice},{#contact.land_line,LLOffice},{#contact.fax,FaxOffice},
       {#contact.email,MailOffice},{#contact.web_address,WebAddress}]}]}]),

  Resp = gen_console_manager_box:manager_call(make_call,medium,Record,?ConsoleManagerKey),
  handle_db_resp(#console_operations.add_organization_contact,Resp);

handle_operation(#console_req{operation_id = #console_operations.add_organization_address,params = #params{} = Data}) ->
  #params{org_id = OrgId,door_no = DoorNo,street_name = Street,area_name = Area,city = City,
          district = District,state = State,country = Country,pin_code = PinCode} = Data,

  Record = db_query:conform_new_record(add_organization_address,[{[#root.organization],[{#organization.id,OrgId},
    {[#organization.address],
    [{#address.door_no,DoorNo},{#address.street_name,Street},{#address.area_name,Area},{#address.city,City},
     {#address.district,District},{#address.state,State},{#address.country,Country},{#address.pin_code,PinCode}]}]}]),

  Resp = gen_console_manager_box:manager_call(make_call,medium,Record,?ConsoleManagerKey),
  handle_db_resp(#console_operations.add_organization_address,Resp);

handle_operation(#console_req{operation_id = #console_operations.add_organization_admin,params = #params{} = Data}) ->
  #params{org_id = OrgId,admin_id = AdminId,privilege_set = PrivilegeSet} = Data,

  Record = db_query:conform_new_record(add_organization_admin,[{[#root.organization],[{#organization.id,OrgId},
    {[#organization.admin],[{#admin.id,AdminId},{#admin.privilege_set,PrivilegeSet}]}]}]),

  Resp = gen_console_manager_box:manager_call(make_call,medium,Record,?ConsoleManagerKey),
  handle_db_resp(#console_operations.add_organization_admin,Resp);

handle_operation(#console_req{operation_id = #console_operations.update_organization_admin_privilege,params = #params{} = Data}) ->
  #params{org_id = OrgId,admin_id = AdminId,privilege_set = PrivilegeSet} = Data,

  Record = db_query:update_record(update_org_admin_privilege,OrgId,[{[#root.organization],[{#organization.id,OrgId},
    {[#organization.admin],[{#admin.id,AdminId},{#admin.privilege_set,PrivilegeSet}]}]}]),

  Resp = gen_console_manager_box:manager_call(make_report,medium,Record,?ConsoleManagerKey),
  handle_db_resp(#console_operations.update_organization_admin_privilege,Resp);

handle_operation(#console_req{operation_id = #console_operations.update_organization_contact,params = #params{} = Data}) ->
  #params{org_id = OrgId,mobile_office = MobOffice,land_line_office = LLOffice,fax_office = FaxOffice,
          email_office = MailOffice,web_address_id = WebAddress} = Data,

  Record = db_query:update_record(update_organization_contact,OrgId,[{[#root.organization],[{#organization.id,OrgId},
    {[#organization.contact],
      [{#contact.mobile,MobOffice},{#contact.land_line,LLOffice},{#contact.fax,FaxOffice},
       {#contact.email,MailOffice},{#contact.web_address,WebAddress}]}]}]),

  Resp = gen_console_manager_box:manager_call(make_report,medium,Record,?ConsoleManagerKey),
  handle_db_resp(#console_operations.update_organization_contact,Resp);

handle_operation(#console_req{operation_id = #console_operations.update_organization_address,params = #params{} = Data}) ->
  #params{org_id = OrgId,door_no = DoorNo,street_name = Street,area_name = Area,city = City,
          district = District,state = State,country = Country,pin_code = PinCode} = Data,

  Record = db_query:update_record(update_organization_address,OrgId,[{[#root.organization],[{#organization.id,OrgId},
    {[#organization.address],
     [{#address.door_no,DoorNo},{#address.street_name,Street},{#address.area_name,Area},{#address.city,City},
      {#address.district,District},{#address.state,State},{#address.country,Country},{#address.pin_code,PinCode}]}]}]),

  Resp = gen_console_manager_box:manager_call(make_report,medium,Record,?ConsoleManagerKey),
  handle_db_resp(#console_operations.update_organization_address,Resp);

handle_operation(#console_req{operation_id = #console_operations.update_organization_name,params = #params{} = Data}) ->
  #params{org_id = OrgId,org_name = OrgName} = Data,

  Record = db_query:update_record(update_organization_name,OrgId,
    [{[#root.organization],
      [{#organization.id,OrgId},{#organization.name,OrgName}]}]),

  Resp = gen_console_manager_box:manager_call(make_report,medium,Record,?ConsoleManagerKey),
  handle_db_resp(#console_operations.update_organization_name,Resp);

handle_operation(#console_req{operation_id = #console_operations.update_organization_owner,params = #params{} = Data}) ->
  #params{org_id = OrgId,org_owner = OrgOwner} = Data,

  Record = db_query:conform_new_record(new_organization,
    [{[#root.organization],
      [{#organization.id,OrgId},{#organization.owner,OrgOwner}]}]),

  Resp = gen_console_manager_box:manager_call(make_report,medium,Record,?ConsoleManagerKey),
  handle_db_resp(#console_operations.update_organization_owner,Resp);

handle_operation(#console_req{operation_id = #console_operations.remove_organization_admin,params = #params{} = Data}) ->
  #params{org_id = OrgId,admin_id = AdminId} = Data,

  Record = db_query:update_record(remove_organization_admin,OrgId,[{[#root.organization],[{#organization.id,OrgId},
    {[#organization.admin],[{#admin.id,AdminId}]}]}]),

  Resp = gen_console_manager_box:manager_call(make_call,medium,Record,?ConsoleManagerKey),
  handle_db_resp(#console_operations.remove_organization_admin,Resp);

%%%===================================================================
%%% API Calls based on Application
%%%===================================================================


handle_operation(#console_req{operation_id = #console_operations.add_application_admin,params = #params{} = Data}) ->
  #params{app_key = AppKey,admin_id = AdminId,privilege_set = PrivilegeSet} = Data,

  Record = db_query:update_record(add_application_admin,AppKey,
    [{[#root.application],[{#application.token,AppKey},
    {[#application.admin],[{#admin.id,AdminId},{#admin.privilege_set,PrivilegeSet}]}]}]),

  Resp = gen_console_manager_box:manager_call(make_call,medium,Record,?ConsoleManagerKey),
  handle_db_resp(#console_operations.add_application_admin,Resp);


handle_operation(#console_req{operation_id = #console_operations.get_applications,params = #params{} = Data}) ->
  #params{org_id = OrgId} = Data,

  Record = db_query:read_record(get_applications,OrgId),

  Resp = gen_console_manager_box:manager_call(make_call,medium,Record,?ConsoleManagerKey),
  handle_db_resp(#console_operations.remove_organization_admin,Resp);

handle_operation(#console_req{operation_id = #console_operations.update_application_admin_privilege,params = #params{} = Data}) ->
  #params{app_key = AppKey,admin_id = AdminId,privilege_set = PrivilegeSet} = Data,

  Record = db_query:update_record(update_app_admin_privilege,AppKey,
    [{[#root.application],[{#application.token,AppKey},
    {[#application.admin],[{#admin.id,AdminId},{#admin.privilege_set,PrivilegeSet}]}]}]),

  Resp = gen_console_manager_box:manager_call(make_report,medium,Record,?ConsoleManagerKey),
  handle_db_resp(#console_operations.add_application_admin,Resp);

handle_operation(#console_req{operation_id = #console_operations.update_application_name,params = #params{} = Data}) ->
  #params{app_key = AppKey,app_name = AppName} = Data,

  Record = db_query:update_record(update_application_name,AppKey,
    [{[#root.application],
     [{#application.token,AppKey},{#application.name,AppName}]}]),

  Resp = gen_console_manager_box:manager_call(make_report,medium,Record,?ConsoleManagerKey),
  handle_db_resp(#console_operations.update_application_name,Resp);

handle_operation(#console_req{operation_id = #console_operations.update_application_description,params = #params{} = Data}) ->
  #params{app_key = AppKey, desc = Description} = Data,

  Record = db_query:update_record(update_application_description,AppKey,
    [{[#root.application],
      [{#application.token,AppKey},{#application.description,Description}]}]),

  Resp = gen_console_manager_box:manager_call(make_report,medium,Record,?ConsoleManagerKey),
  handle_db_resp(#console_operations.update_application_description,Resp);

handle_operation(#console_req{operation_id = #console_operations.remove_application_admin,params = #params{} = Data}) ->
  #params{app_key = AppKey,admin_id = AdminId} = Data,

  Record = db_query:update_record(remove_application_admin,AppKey,[{[#root.application],[{#application.token,AppKey},
    {[#application.admin],[{#admin.id,AdminId}]}]}]),

  Resp = gen_console_manager_box:manager_call(make_call,medium,Record,?ConsoleManagerKey),
  handle_db_resp(#console_operations.remove_application_admin,Resp).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

validate_credentials(#params{user_name = Username, email = Email, ph = Phone}) ->
  UserRec = db_query:read_record_only_if(validate_reg,[#condition{attribute = "user_name",compare_by = #compare.equal,value = Username}],["user_name"]),
  case gen_console_manager_box:manager_call(make_call,medium,UserRec,?ConsoleManagerKey) of
    {success,{response,[]}} ->
      EmailRec = db_query:read_record_only_if(validate_reg,[#condition{attribute = "email",compare_by = #compare.equal,value = Email}],["email"]),
      case gen_console_manager_box:manager_call(make_call,medium,EmailRec,?ConsoleManagerKey) of
        {success,{response,[]}} ->
          PhoneRec = db_query:read_record_only_if(validate_reg,[#condition{attribute = "phone",compare_by = #compare.equal,value = Phone}],["phone"]),
          case gen_console_manager_box:manager_call(make_call,medium,PhoneRec,?ConsoleManagerKey) of
            {success,{response,[]}} -> valid;
            {success,{response,[_Any]}} -> {invalid,<<"Mobile number already exist">>}
          end;
        {success,{response,[_Any]}} -> {invalid,<<"Email already exist">>}
      end;
    {success,{response,[_Any]}} -> {invalid,<<"User_Name already exist">>}
  end.

get_primary_id(What,CompareKey,Value,ExpectedKey) ->
  Record = db_query:read_record_only_if(What,[#condition{attribute = CompareKey,compare_by = #compare.equal,value = Value}],ExpectedKey),
  case gen_console_manager_box:manager_call(make_call,medium,Record,?ConsoleManagerKey) of
    {success,{response,[]}} -> {not_found};
    {success,{response,Response}} -> {response,Response};
    Any -> io:format("Any : ~p~n",[Any]), {not_found}
  end.


get_condition_params(#params{app_name = AppName})
  when AppName =/= undefined -> {"app_name",AppName} ;
get_condition_params(#params{user_name = Username,email = undefined,ph = undefined,password = Pwd})
  when Username =/= undefined -> {{"user_name",Username},Pwd} ;
get_condition_params(#params{email = Email,user_name = undefined,ph = undefined,password = Pwd})
  when Email =/= undefined -> {{"email",Email},Pwd};
get_condition_params(#params{ph = PhoneNumber, user_name =  undefined, email = undefined, password = Pwd})
  when PhoneNumber =/= undefined -> {{"phone",PhoneNumber},Pwd}.

form_tuple([],[],Acc) -> Acc;
form_tuple([],_,_Acc) -> error;
form_tuple(_,[],_Acc) -> error;
form_tuple([Key|RestKey],[Value|RestValues],Acc) -> form_tuple(RestKey,RestValues,Acc++[{Key,Value}]).

make_manager_call(FeatureRecord2 = #db_query{}) ->
 gen_console_manager_box:manager_call(make_call,medium,FeatureRecord2,?ConsoleManagerKey);

make_manager_call(_) -> error.

handle_db_resp(OpId,Resp) ->
  Action = console_req_uri:get_console_operation(OpId),
  case Resp of
    {ok, Message} when is_binary(Message) -> {?SUCCESS,[?ACTION,Action,?STATUS,Message]};
    {ok, Data} -> handle_operation_resp(Action,OpId,Data);
    {error, Reason} -> {?FAILURE,[?ACTION,Action,?STATUS,Reason]}
  end.

handle_operation_resp(Action,#console_operations.sign_in,[ClientToken]) ->
  {?SUCCESS,[?ACTION,Action,?Client_Token,ClientToken]};

handle_operation_resp(Action,#console_operations.get_applications,[ApplicationLists]) ->
  {?SUCCESS,[?ACTION,Action,ApplicationLists]};

handle_operation_resp(Action,#console_operations.single_sign_on,Response) ->
  {?SUCCESS,[?ACTION,Action,Response]};

handle_operation_resp(Action,#console_operations.register_application,[Data]) ->
  {?SUCCESS,[?ACTION,Action,?APP_KEY,Data]};

handle_operation_resp(Action,#console_operations.get_feature,Response) ->
  {?SUCCESS,[?ACTION,Action,?FEATURE_LIST,Response]}.


send_to_controller(AppId,OrgId) ->
  Record = db_query:read_record(fetch_features,
    [{[#root.application],[{#application.token,AppId}]}]),
  {success,Resp} = gen_console_manager_box:manager_call(make_call,medium,Record,?ConsoleManagerKey),
  case ets:lookup(?ControllerEts,OrgId) of
    [{OrgId,Controller}] ->
      Controller ! {set_feature, Resp};
    [] -> io:format("Controller is not alive~n")
  end.



%%get_condition_params(#params{user_name = Username,email = undefined,ph = undefined,password = Pwd})
%%  when Username =/= undefined ; Pwd =/= undefined -> {{"user_name",Username},Pwd} ;
%%get_condition_params(#params{email = Email,user_name = undefined,ph = undefined,password = Pwd})
%%  when Email =/= undefined ; Pwd =/= undefined -> {{"email",Email},Pwd};
%%get_condition_params(#params{ph = PhoneNumber, user_name =  undefined, email = undefined, password = Pwd})
%%  when PhoneNumber =/= undefined ; Pwd =/= undefined -> {{"phone",PhoneNumber},Pwd}.

%%  form_list([],ReferRoot,ReferRecord,Acc) -> Acc;
%%
%%  form_list([{}],ReferRoot,{KeyRec,ValueRec},_Acc) ->
%%
%%  [{[ReferRoot],[{#property.key,buddy_chat},{#property.value,BuddyChat}]},
%%  {[#settings.property],[{#property.key,group_chat},{#property.value,GroupChat}]},
%%  {[#settings.property],[{#property.key,maximum_users},{#property.value,MaxUsers}]},
%%  {[#settings.property],[{#property.key,member_limit},{#property.value,MemberLimit}]},
%%  {[#settings.property],[{#property.key,maximum_public_channels},{#property.value,MaxPublicChannels}]},
%%  {[#settings.property],[{#property.key,maximum_private_channels},{#property.value,MaxPrivateChannels}]}].