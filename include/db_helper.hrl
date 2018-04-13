%%%-------------------------------------------------------------------
%%% @author Aiden_Richie
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Oct 2017 2:00 PM
%%%-------------------------------------------------------------------
-author("Aiden_Richie").

-record(entity,{purpose,single,multi}).

-record(db_query,{what,operation,primary_item,attribute,condition,present_by,output_keys,output_count}).

-record(operation,{read,update_return,write,enqueue,dequeue,push,pop,write_new,update,delete,flush}).

-record(condition,{attribute,compare_by,value}).

-record(compare,{equal,not_equal,maximum,minimum,with_in,greater,lesser,greater_from,lesser_from,regexp,like}).

-record(present_by,{ascending,descending}).

-record(priority,{low,medium,high}).
