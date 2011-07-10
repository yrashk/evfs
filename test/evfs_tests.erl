-module(evfs_tests).
-include_lib("eunit/include/eunit.hrl").

t_unregister() ->
    evfs:register(evfs_test_fs, []),
    evfs:unregister(evfs_test_fs),
    ?assertEqual({error, enoent}, file:read_file("test://any")).

t_file_scheme() ->
    application:stop(evfs),
    ?assertEqual({error, enoent}, file:read_file("file:///")),
    application:start(evfs),
    ?assertEqual({error, eisdir}, file:read_file("file:///")).

t_read_file() ->
    evfs:register(evfs_test_fs, []),
    ?assertEqual({error, eisdir}, file:read_file("/")),
    ?assertEqual({ok, <<"test://any">>}, file:read_file("test://any")).

t_pread() ->
    evfs:register(evfs_test_fs, []),
    {ok, File} = file:open("test://anyfile",[read]),
    ?assertEqual({ok, ["test://any"]}, file:pread(File, [{0,10}])).

t_filename() ->    
    ?assertEqual("/", filename:dirname("/")),
    ?assertEqual("/", filename:dirname("file:///")),
    evfs:register(evfs_test_fs, []),
    ?assertEqual("test://", filename:dirname("test://any")).

evfs_test_() ->
    {foreach,
     fun () ->
             ok = application:start(evfs)
     end,
     fun (_) ->
             ok = application:stop(evfs)
     end,
     [
      {"unregister", ?_test(t_unregister())},
      {"file:// scheme should route to the default file server", ?_test(t_file_scheme())},
      {"test_fs read_file", ?_test(t_read_file())},
      {"test_fs pread", ?_test(t_pread())},
      {"test_fs & default fs filename", ?_test(t_filename())}
     ]
    }.



