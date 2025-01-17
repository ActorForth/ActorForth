-module(thread).

-export_type([thread/0]).


-type type() :: atom().
-type stack_item() :: {type(), term() | function()}.

-type thread() :: function().

