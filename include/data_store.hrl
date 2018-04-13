%%%-------------------------------------------------------------------
%%% @author Aiden_Richie
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2017 4:43 PM
%%%-------------------------------------------------------------------
-author("Aiden_Richie").

%% ======================================================
%%                      Root node
%% ======================================================

-record(root,{user,organization,application,channel,message}).

%% ======================================================
%%                      Middle Leaf nodes
%% ======================================================

-record(user,{id,name,contact,profile,date_of_birth,created_time,org_referred,gender,password,user_type}).
-record(organization,{id,name,owner,description,admin,application,contact,address,created_on,token,settings}).
-record(application,{id,name,description,admin,created_on,token,settings,feature}).

-record(contact,{mobile,land_line,fax,email,address,web_address}).
-record(settings,{id,name,property}).
-record(channel,{id,profile,owner,created_on,member,message}).
-record(message,{id,sender,data,delivery_time,read_time,created_time}).

%% ======================================================
%%                      End Leaf nodes
%% ======================================================

-record(name,{user_name,first_name,middle_name,last_name,nick_name,full_name}).
-record(address,{id,door_no,street_name,area_name,city,district,state,country,pin_code}).
-record(web_address,{id,participant_url,social_network_id}).
-record(mobile,{office,home,other}).
-record(fax,{office,home,other}).
-record(land_line,{office,home,other}).
-record(email,{personal,official,other}).

-record(property,{key,value_type,value}).
-record(token,{data,valid_from,valid_to}).
-record(profile,{name, description,logo}).
-record(member,{id,role, joined_date,access_type}).
-record(admin,{id,privilege_set}).
-record(feature,{id,feature_data}).