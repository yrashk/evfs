-module(evfs_httpc_handler_tests).
-include_lib("eunit/include/eunit.hrl").

t_read_file() ->
    ?assertMatch({ok, Binary} when is_binary(Binary), file:read_file("http://google.com")).

t_read_file_404() ->
    ?assertEqual({error, enoent}, file:read_file("http://google.com/willnotfind")).

httpc_test_() ->
    {foreach,
     fun () ->
             application:start(inets), 
             application:start(crypto), 
             application:start(public_key),  
             application:start(ssl),
             ok = application:start(evfs),
             evfs:register(evfs_httpc_handler, default)
     end,
     fun (_) ->
             ok = application:stop(evfs)
     end,
     [
      {"read_file", ?_test(t_read_file())},
      {"read_file from 404", ?_test(t_read_file_404())}
     ]
    }.
