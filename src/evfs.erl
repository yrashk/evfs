-module(evfs).
-export([register/2, register/3, unregister/1, unregister/2, 
        original_file_server/0, original_file_server/1]).

-include("internal.hrl").

-spec register(module(), term()) -> ok.
-spec register(pid() | atom(), module(), term()) -> ok.
                       
register(Handler, Args) ->
    register(?FILE_SERVER, Handler, Args).

register(Server, Handler, Args) ->
    {module, Handler} = code:ensure_loaded(Handler),
    gen_server:call(Server, {register_handler, Handler, Args}).


-spec unregister(module()) -> ok.
-spec unregister(pid() | atom(), module()) -> ok.

unregister(Handler) ->
    unregister(?FILE_SERVER, Handler).

unregister(Server, Handler) ->
    gen_server:call(Server, {unregister_handler, Handler}).

-spec original_file_server() -> pid().
-spec original_file_server(pid()) -> pid().

original_file_server() ->
    original_file_server(?FILE_SERVER).

original_file_server(Server) ->
    gen_server:call(Server, original_file_server).
                                  
