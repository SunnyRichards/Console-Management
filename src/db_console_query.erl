
%%%-------------------------------------------------------------------
%%% @author Aiden_Richie
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Oct 2017 2:57 PM
%%%-------------------------------------------------------------------
-module(db_console_query).
-author("Aiden_Richie").

%% API
-export([init/1,write_new/3,read_by_condition/4,update/4,read_specific/3]).

%%-export([update_specific/4,delete_specific/3,read_specific/3]).

-include("data_store.hrl").
-include("db_helper.hrl").
-include("box_reference.hrl").


%%-include("db_params.hrl").

-record(state, {}).

init(_Args) ->
  {ok,#state{}}.

%%======================================================================================================================
%% ORGANIZATION - conform and write functions
%%======================================================================================================================
%%org_location varchar(30),website varchar(15), created_date date,

write_new(new_organization,Data,Pid) ->
  Key = [{[#root.organization],
    [#organization.id,#organization.name,#organization.owner,
      {[#organization.contact],[#contact.mobile,#contact.web_address]},
      {[#organization.address],[#address.city]},
      #organization.created_on]}],
  AttributeList = finder:exec(Data,Key),
  io:format("Value List: ~p~n",[AttributeList]),
  mysql_query:write(organization,AttributeList,Pid);

write_new(add_organization_contact,Data,Pid) ->
  Key = [{[#root.organization],[#organization.id,
    {[#organization.contact],[#contact.mobile,#contact.land_line,#contact.fax,#contact.email,#contact.web_address]}]}],
  AttributeList = finder:exec(Data,Key),
  mysql_query:write(contact,AttributeList,Pid);

write_new(add_organization_address,Data,Pid) ->
  Key = [{[#root.organization],[#organization.id,
    {[#organization.address],
      [#address.door_no,#address.street_name,#address.area_name,#address.city,
        #address.state,#address.country,#address.pin_code]}]}],
  AttributeList = finder:exec(Data,Key),
  mysql_query:write(address,AttributeList,Pid);

write_new(add_organization_admin,Data,Pid) ->
  Key = [{[#root.organization],[#organization.id,{[#organization.admin],[#admin.id,#admin.privilege_set]}]}],
  [PrimaryKey,AdminId,PrivilegeSet] = finder:exec(Data,Key),
  mysql_query:write(admin,[AdminId,PrimaryKey,organization,PrivilegeSet],Pid);

%%======================================================================================================================
%% USER - conform and write functions
%%======================================================================================================================

write_new(new_user,Data,Pid) ->
  Key =
    [{[#root.user], [#user.id,
      {[#user.name],[#name.first_name,#name.last_name,#name.user_name]},
      #user.date_of_birth,#user.password,#user.user_type,
      {[#user.contact],[#contact.email,#contact.mobile,#contact.address]}]}],
  AttributeList = finder:exec(Data,Key),
  io:format("Value List: ~p~n",[AttributeList]),
  mysql_query:write(user,AttributeList,Pid);


%%======================================================================================================================
%% APPLICATION - conform and write functions
%%======================================================================================================================

write_new(new_application,Data,Pid) ->
  Key1 = [{[#root.application],[#application.id,#application.name,#application.description,#application.token]}],
  Key2 = [{[#root.organization],[#organization.id]}],
  AttributeList1 = finder:exec(Data,Key1),
  [OrgId] = finder:exec(Data,Key2),
  io:format("Value List: ~p~n",[AttributeList1]),
  io:format("OrgId: ~p~n",[OrgId]),
  mysql_query:write(application,AttributeList1 ++ [OrgId],Pid);

write_new(add_application_admin,Data,Pid) ->
  Key = [{[#root.application],[#application.id,{[#application.admin],[#admin.id,#admin.privilege_set]}]}],
  [PrimaryKey,AdminId,PrivilegeSet] = finder:exec(Data,Key),
  mysql_query:write(admin,[AdminId,PrimaryKey,application,PrivilegeSet],Pid);

write_new(new_feature_val,Data,Pid) ->
  Key = [{[#root.application],[#application.id,{[#application.feature],[#feature.id,#feature.feature_data]}]}],
  Attribute = finder:exec(Data,Key),
  mysql_query:write(app_feature,Attribute,Pid).

%%read_specific(fetch_features,[{[4],[{7,<<"2642363677859496">>}]}],<0.1341.0>)

%%======================================================================================================================
%% USER - updating certain fields in a record
%%======================================================================================================================
read_specific(fetch_features,Data,Pid) ->
  Key = [{[#root.application],[#application.token]}],
  [AppId] = finder:exec(Data,Key),
  io:format("Received Data: ~p~n",[AppId]),
  case mysql_query:read_specific(app_feature,AppId,["feature_id","feature_data"],Pid) of
    {?response,List} ->
      io:format("Received APP FEATURE LIST: ~p~n",[List]),
      ProfileFeatures = {?PROFILE_BOX,get_box_rules(?PROFILE_BOX,List,Pid)},
      ChannelFeatures = {?CHANNEL_BOX,get_box_rules(?CHANNEL_BOX,List,Pid)},
      ContentFeatures = {?CONTENT_BOX,get_box_rules(?CONTENT_BOX,List,Pid)},
      SessionFeatures = {?SESSION_BOX,get_box_rules(?SESSION_BOX,List,Pid)},
      [ProfileFeatures,ChannelFeatures,ContentFeatures,SessionFeatures];

%%      ContentFeatures = get_box_rules(?CONTENT_BOX,List,Pid,[?BOX_NAME,?CONTENT_BOX]),
%%      SessionFeatures = get_box_rules(?SESSION_BOX,List,Pid,ContentFeatures ++ [?BOX_NAME,?SESSION_BOX]),
%%      ProfileFeatures = get_box_rules(?PROFILE_BOX,List,Pid,SessionFeatures ++ [?BOX_NAME,?PROFILE_BOX]),
%%      ChannelFeatures = get_box_rules(?CHANNEL_BOX,List,Pid,ProfileFeatures ++ [?BOX_NAME,?CHANNEL_BOX]),
%%      ChannelFeatures;
    Error ->
      io:format("Error: ~p~n",[Error])
  end.

read_by_condition(sign_in,Condition,ExpectedKey,Pid) ->
  mysql_query:read_by_multi_condition(user,Condition,ExpectedKey,Pid);

read_by_condition(validate_reg,Condition,ExpectedKey,Pid) ->
  mysql_query:read_by_multi_condition(user,Condition,ExpectedKey,Pid);

read_by_condition(new_feature_val,Condition,ExpectedKey,Pid) ->
  mysql_query:read_by_multi_condition(app_feature,Condition,ExpectedKey,Pid);

read_by_condition(get_app_id,Condition,ExpectedKey,Pid) ->
  mysql_query:read_by_multi_condition(application,Condition,ExpectedKey,Pid);

read_by_condition(get_feature_id,Condition,ExpectedKey,Pid) ->
  mysql_query:read_by_multi_condition(feature,Condition,ExpectedKey,Pid);

read_by_condition(get_org,Condition,ExpectedKey,Pid) ->
  mysql_query:read_by_multi_condition(organization,Condition,ExpectedKey,Pid).

%%======================================================================================================================
%% ORGANIZATION - updating certain fields in a record
%%======================================================================================================================

%%-todo have to start from here for update
update(new_feature_val,Condition,Data,Pid) ->
  Key = [{[#root.application,#application.feature],[#feature.feature_data]}],
  [Feature] = finder:exec(Data,Key),
  mysql_query:update_by_multi_condition(app_feature,Condition,[{"feature_data",Feature}],Pid).


%%  update_specific(update_org_admin_privilege,AdminId,Data,Pid) ->
%%    Key = [{[#root.organization],[#organization.id,{[#organization.admin],[#admin.id,#admin.privilege_set]}]}],
%%    [_OrgId,AdminId,PrivilegeSet] = finder:exec(Data,Key),
%%    mnesia_query:update_specific(admin,AdminId,[{,PrivilegeSet}],Pid);
%%
%%  update_specific(update_organization_contact,PrimaryKey,Data,Pid) ->
%%    Key = [{[#root.organization],[#organization.id,
%%      {[#organization.contact],[#contact.mobile,#contact.land_line,#contact.fax,#contact.email,#contact.web_address]}]}],
%%    [PrimaryKey,MobileNo,PhoneNo,Fax,Mail,WebAddress] = finder:exec(Data,Key),
%%    ContactInfo = form_key_pair([?MOB_NO,MobileNo,?PHONE_NO,PhoneNo,?FAX,Fax,?MAIL,Mail,?WEB_ADDRESS,WebAddress]),
%%    mnesia_query:update_specific(contact,PrimaryKey,ContactInfo,Pid);
%%
%%  update_specific(update_organization_address,PrimaryKey,Data,Pid) ->
%%    Key = [{[#root.organization],[#organization.id,
%%      {[#organization.address],
%%       [#address.door_no,#address.street_name,#address.area_name,#address.city,
%%        #address.state,#address.country,#address.pin_code]}]}],
%%    [PrimaryKey,DoorNo,Street,Village,City,StateVal,Country,PinCode] = finder:exec(Data,Key),
%%    AddressInfo = form_key_pair([?DOOR_NO,DoorNo,?STREET,Street,?VILLAGE,Village,?CITY,City,
%%                                 ?STATE,StateVal,?COUNTRY,Country,?PIN_CODE,PinCode]),
%%    mnesia_query:update_specific(address,PrimaryKey,AddressInfo,Pid);
%%
%%  update_specific(remove_organization_admin,PrimaryKey,Data,Pid) ->
%%    Key = [{[#root.organization],[#organization.id,{[#organization.admin],[#admin.id]}]}],
%%    [PrimaryKey,AdminId] = finder:exec(Data,Key),
%%    mnesia_query:update_specific({organization,remove},PrimaryKey,AdminId,Pid),
%%    mnesia_query:delete_specific({admin,organization},PrimaryKey,Pid);
%%
%%  update_specific(update_organization_name,PrimaryKey,Data,Pid) ->
%%    Key = [{[#root.organization],[#organization.id,#organization.name]}],
%%    AttributeList = finder:exec(Data,Key),
%%    mnesia_query:update_specific({organization,name},PrimaryKey,AttributeList,Pid);
%%
%%  update_specific(update_organization_owner,PrimaryKey,Data,Pid) ->
%%    Key = [{[#root.organization],[#organization.id,#organization.owner]}],
%%    AttributeList = finder:exec(Data,Key),
%%    mnesia_query:update_specific({organization,owner},PrimaryKey,AttributeList,Pid);
%%
%%  %%======================================================================================================================
%%  %% APPLICATION - updating certain fields in a record
%%  %%======================================================================================================================
%%
%%
%%  update_specific(update_app_admin_privilege,PrimaryKey,Data,Pid) ->
%%    Key = [{[#root.application],[#application.id,{[#application.admin],[#admin.id,#admin.privilege_set]}]}],
%%    [PrimaryKey,AdminId,PrivilegeSet] = finder:exec(Data,Key),
%%    mnesia_query:update_specific({admin,application},AdminId,[PrimaryKey,PrivilegeSet],Pid);
%%
%%  update_specific(remove_application_admin,PrimaryKey,Data,Pid) ->
%%    Key = [{[#root.application],[#application.id,{[#application.admin],[#admin.id]}]}],
%%    [PrimaryKey,AdminId] = finder:exec(Data,Key),
%%    mnesia_query:update_specific({application,remove},PrimaryKey,AdminId,Pid),
%%    mnesia_query:delete_specific({admin,application},PrimaryKey,Pid);
%%
%%  update_specific(update_application_name,PrimaryKey,Data,Pid) ->
%%    Key = [{[#root.application],[#application.id,#application.name]}],
%%    AttributeList = finder:exec(Data,Key),
%%    mnesia_query:update_specific({application,name},PrimaryKey,AttributeList,Pid);
%%
%%  update_specific(update_application_description,PrimaryKey,Data,Pid) ->
%%    Key = [{[#root.application],[#application.id,#application.description]}],
%%    AttributeList = finder:exec(Data,Key),
%%    mnesia_query:update_specific({application,description},PrimaryKey,AttributeList,Pid).
%%
%%  %%======================================================================================================================
%%  %% ORGANIZATION - delete certain fields in a record
%%  %%======================================================================================================================
%%
%%  delete_specific(delete_organization,PrimaryKey,Pid) ->
%%    mnesia_query:delete_specific(organization,PrimaryKey,Pid);
%%
%%  %%======================================================================================================================
%%  %% APPLICATION - delete certain fields in a record
%%  %%======================================================================================================================
%%
%%  delete_specific(delete_application,PrimaryKey,Pid) ->
%%    mnesia_query:delete_n_remove(application,PrimaryKey,Pid).
%%
%%  %%  delete_specific(delete_application,PrimaryKey,Pid) ->
%%  %%    mnesia_query:delete_specific(application,PrimaryKey,Pid),
%%  %%    mnesia_query:update_specific({organization,remove_app},PrimaryKey,AdminId,Pid).
%%
%%  %%======================================================================================================================
%%  %% APPLICATION - read certain fields in a record
%%  %%======================================================================================================================
%%
%%
%%  read_specific(get_applications,PrimaryKey,Pid) ->
%%    mnesia_query:read_specific(application,PrimaryKey,Pid).

%%======================================================================================================================
%% Internal Functions
%%======================================================================================================================

%%  form_key_pair(L) -> form_key_pair(L,[]).
%%
%%  form_key_pair([],Acc) -> lists:reverse(Acc);
%%  form_key_pair([_Key,undefined|Rest],Acc) -> form_key_pair(Rest,Acc);
%%  form_key_pair([Key,Val|Rest],Acc) -> form_key_pair(Rest, [{Key,Val}|Acc]).

get_box_rules(BoxName,AppFeatureList,Pid) ->
  case mysql_query:read_by_condition(box,#condition{attribute = "box_name",compare_by = #compare.equal,value = BoxName},["box_id"],Pid) of
    {?response,[[BoxId]]} ->
      io:format("Received BOXID: ~p~n",[BoxId]),
      case mysql_query:read_by_condition(box_feature,#condition{attribute = "box_id",compare_by = #compare.equal,value = integer_to_binary(BoxId)},["feature_id"],Pid) of
        {?response,FeatureIdList} ->
          io:format("Received FEATURELIST: ~p~n",[FeatureIdList]),
          io:format("Received LIST: ~p~n",[AppFeatureList]),
          sort_feature_opt(FeatureIdList,AppFeatureList,Pid,[],[]);
        Error -> io:format("Error1: ~p~n",[Error])
      end;
    Error -> io:format("Error2: ~p~n",[Error])
  end.

sort_feature_opt([_|Rest],[],Pid,BoxFeatures,OtherBoxFeatures) ->
  sort_feature_opt(Rest,OtherBoxFeatures,Pid,BoxFeatures,[]);
sort_feature_opt([],_,_Pid,Acc,_Acc2) -> Acc;
sort_feature_opt([[FeatureId]|Rest1],[[FeatureId,Feature]|RestFeatures],Pid,BoxFeatures,OtherBoxFeatures) ->
  case mysql_query:read_specific(feature,integer_to_binary(FeatureId),["feature_name"],Pid) of
    {?response,[[FeatureName]]} -> sort_feature_opt(Rest1,OtherBoxFeatures++RestFeatures,Pid,BoxFeatures ++ [{FeatureName,Feature}],[]);
    Error -> io:format("Error3: ~p~n",[Error])
  end;
sort_feature_opt(FeatureIdList,[Feature|Rest],Pid,BoxFeatures,OtherBoxFeatures) ->
  sort_feature_opt(FeatureIdList,Rest,Pid,BoxFeatures,OtherBoxFeatures++[Feature]).

%%sort_feature_opt([],_,_,_Pid,Acc) ->
%%  io:format("Received Features: ~p~n",[Acc]),
%%  Acc;
%%
%%sort_feature_opt([[FeatureId]|Rest1],[[FeatureId,Feature]|_Rest2],FeatureList,Pid,Acc) ->
%%    io:format("Inside SORT MODULE: ~p~n",[FeatureId]),
%%  case mysql_query:read_specific(feature,integer_to_binary(FeatureId),["feature_name"],Pid) of
%%    {?response,[[FeatureName]]} ->
%%      sort_feature_opt(Rest1,FeatureList -- [FeatureId,Feature],FeatureList,Pid,Acc ++ [{FeatureName,Feature}]);
%%    Error ->
%%      io:format("Error3: ~p~n",[Error])
%%  end;
%%
%%sort_feature_opt([[FeatureId]|Rest1],[[_OtherFeatureId,_Feature]|Rest2],FeatureList,Pid,Acc) ->
%%  sort_feature_opt([[FeatureId]|Rest1],Rest2,FeatureList,Pid,Acc).

%%  sort_feature([],_,_,_Pid,Acc) ->
%%    Acc;
%%
%%  sort_feature([[_OtherFeatureId,_Feature]|Rest1],[],FeatureList,Pid,Acc) ->
%%    sort_feature(Rest1,FeatureList,FeatureList,Pid,Acc);
%%
%%  sort_feature([[FeatureId,Feature]|Rest1],[[FeatureId]|Rest2],FeatureList,Pid,Acc) ->
%%    case mysql_query:read_specific(feature,FeatureId,["feature_name"],Pid) of
%%      {?response,[[FeatureName]]} ->
%%        sort_feature([[FeatureId,Feature]|Rest1],Rest2,FeatureList,Pid,Acc ++[{FeatureName,Feature}]);
%%      Error ->
%%        io:format("Error: ~p~n",[Error])
%%    end;
%%
%%  sort_feature([[FeatureId,Feature]|Rest1],[[_RemainingFeatureId]|Rest2],FeatureList,Pid,Acc) ->
%%    sort_feature([[FeatureId,Feature]|Rest1],Rest2,FeatureList,Pid,Acc).