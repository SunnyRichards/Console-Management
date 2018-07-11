%%%-------------------------------------------------------------------
%%% @author Aiden_Richie
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Oct 2017 3:43 PM
%%%-------------------------------------------------------------------
-module(console_management_app).
-author("Aiden_Richie").

-behaviour(application).

%% Application callbacks

-export([start/2,stop/1]).
-export([do_it_first/0]).

-export([create_database/0,initial_data/0]).

-include_lib("generic_box/include/module_reference.hrl").
-include_lib("gabby_reference/include/credentials.hrl").
-include("http_utils.hrl").

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {fail, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).

start(_StartType, _StartArgs) ->
  ets:new(controller,[bag,public,named_table]),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/ws/[...]",console_ws_handler,[]},
      {"/[...]", console_http_handler, []}
    ]}
  ]),
  Res = cowboy:start_http(http, 10, [{port, 8585}],
    [{env, [{dispatch, Dispatch}]}]
  ),
  case Res of
    {ok,_Pid} ->
      io:format("Console_Management_Module Started ~n",[]),
      create_session_management(),
      create_database(),
      gen_box_factory:start_factory(?ConsoleBox,gen_console_handler,?ConsoleManagerKey,[]);
    {error,Reason} ->
      io:format("inside of error : ~p~n",[Reason]),
      {error,Reason};
    Error ->
      io:format("inside of error2 : ~p~n",[Error]),
      {error,Error}
  end.


create_session_management() ->
  mnesia:create_schema([node()]),
  mnesia:start().
%%  do_it_first().

do_it_first() ->
  mnesia:create_table(client_token,[{disc_copies, [node()]}, {index, [user_id]}, {attributes, record_info(fields, client_token)}]).

create_database() ->
  {ok,Pid} = mysql:start_link([{host, "localhost"}, {user, "root"},{password, "your_password"}]),

  mysql:query(Pid, "create database if not exists "++"testingdb"),

  mysql:query(Pid, "use "++"testingdb"),

  mysql:query(Pid, "create table if not exists user
  (user_id varchar(40),fname varchar(30),lname varchar(30),user_name varchar(30),dob date,password varchar(30),user_type varchar(10),
  email varchar(30),phone bigint,location varchar(20), primary key(user_id))"),


  mysql:query(Pid, "create table if not exists organization
  (org_id varchar(40), org_name varchar(40), user_id varchar(40),org_phone varchar(15),org_location varchar(30),website varchar(15), created_date date,
  primary key (org_id), constraint fk_org_user foreign key(user_id) references user(user_id))"),

  mysql:query(Pid, "create table if not exists application(app_id varchar(40),app_name varchar(20),app_description varchar(50),app_key varchar(40),org_id varchar(40),
  primary key(app_id), constraint fk_app_org foreign key(org_id) references organization(org_id))"),

  mysql:query(Pid, "create unique index idx_user on user(user_name,email,phone)"),

  mysql:query(Pid, "create unique index idx_org on organization(org_name)"),

  mysql:query(Pid, "create table if not exists box(box_id int NOT NULL AUTO_INCREMENT, box_name varchar(30), primary key(box_id))"),

  mysql:query(Pid, "create table if not exists feature(feature_id int NOT NULL AUTO_INCREMENT, feature_name varchar(30), primary key(feature_id))"),

  mysql:query(Pid, "create table if not exists box_feature(box_id int, feature_id int)"),

  mysql:query(Pid, "create table if not exists app_feature(app_id varchar(40), feature_id int, feature_data varchar(9999))"),


  io:format("~nAll Tables are created successfully~n").


initial_data() ->
  {ok,Pid} = mysql:start_link([{host, "localhost"}, {user, "root"},{password, "sanders16"}]),

  mysql:query(Pid, "use "++"testingdb"),

  mysql:query(Pid, "insert into box (box_id,box_name) values (1001,'profile_box')"),
  mysql:query(Pid, "insert into box (box_name) values ('channel_box')"),
  mysql:query(Pid, "insert into box (box_name) values ('content_box')"),
  mysql:query(Pid, "insert into box (box_name) values ('session_box')"),

  mysql:query(Pid, "insert into feature (feature_id,feature_name) values (2001,'buddy_chat')"),
  mysql:query(Pid, "insert into feature (feature_name) values ('group_chat')"),
  mysql:query(Pid, "insert into feature (feature_name) values ('secret_chat')"),
  mysql:query(Pid, "insert into feature (feature_name) values ('data_format_manager')"),
  mysql:query(Pid, "insert into feature (feature_name) values ('scalable_public_channel')"),
  mysql:query(Pid, "insert into feature (feature_name) values ('scalable_private_channel')"),
  mysql:query(Pid, "insert into feature (feature_name) values ('scalable_content_box')"),
  mysql:query(Pid, "insert into feature (feature_name) values ('scalable_buddy')"),
  mysql:query(Pid, "insert into feature (feature_name) values ('scalable_channel_members')"),
  mysql:query(Pid, "insert into feature (feature_name) values ('device_limit')"),

  mysql:query(Pid, "insert into box_feature (box_id,feature_id) values (1001,2008)"),
  mysql:query(Pid, "insert into box_feature (box_id,feature_id) values (1002,2001)"),
  mysql:query(Pid, "insert into box_feature (box_id,feature_id) values (1002,2002)"),
  mysql:query(Pid, "insert into box_feature (box_id,feature_id) values (1002,2003)"),
  mysql:query(Pid, "insert into box_feature (box_id,feature_id) values (1002,2005)"),
  mysql:query(Pid, "insert into box_feature (box_id,feature_id) values (1002,2006)"),
  mysql:query(Pid, "insert into box_feature (box_id,feature_id) values (1002,2009)"),
  mysql:query(Pid, "insert into box_feature (box_id,feature_id) values (1003,2007)"),
  mysql:query(Pid, "insert into box_feature (box_id,feature_id) values (1004,2004)"),
  mysql:query(Pid, "insert into box_feature (box_id,feature_id) values (1004,2010)"),

  io:format("~nInitial Data successfully created~n",[]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.
