-module(evfs_default_handler).
-behaviour(evfs_handler).
-export([init/1, supports/2, terminate/2]).
-export([open/3, 
         read_file/2,
         write_file/3,
         set_cwd/2,
         delete/2,
         rename/3,
         make_dir/2,
         del_dir/2,
         list_dir/2,
         get_cwd/1,
         get_cwd/2,
         read_file_info/2,
         altname/2,
         write_file_info/3,
         read_link_info/2,
         read_link/2,
         make_link/3,
         make_symlink/3,
         copy/6
        ]).

init(FileServer) ->
    {ok, FileServer}.

supports("file://" ++ _, State) ->
    {true, State};

supports(_, State) ->
    {true, State}.

%% Filesystem API

open(Filename, Mode, FileServer) ->
    {ok, gen_server:call(FileServer, {open, filename(Filename), Mode}), FileServer}.

read_file(Filename, FileServer) ->
    {ok, gen_server:call(FileServer, {read_file, filename(Filename)}), FileServer}.

write_file(Filename, Bin, FileServer) ->
    {ok, gen_server:call(FileServer, {write_file, filename(Filename), Bin}), FileServer}.

set_cwd(Filename, FileServer) ->
    {ok, gen_server:call(FileServer, {set_cwd, filename(Filename)}), FileServer}.

delete(Filename, FileServer) ->
    {ok, gen_server:call(FileServer, {delete, filename(Filename)}), FileServer}.

rename(Fr, To, FileServer) ->
    {ok, gen_server:call(FileServer, {rename, filename(Fr), filename(To)}), FileServer}.

make_dir(Filename, FileServer) ->
    {ok, gen_server:call(FileServer, {make_dir, filename(Filename)}), FileServer}.

del_dir(Filename, FileServer) ->
    {ok, gen_server:call(FileServer, {del_dir, filename(Filename)}), FileServer}.

list_dir(Filename, FileServer) ->
    {ok, gen_server:call(FileServer, {list_dir, filename(Filename)}), FileServer}.

get_cwd(FileServer) ->
    {ok, gen_server:call(FileServer, {get_cwd}), FileServer}.

get_cwd(Filename, FileServer) ->
    {ok, gen_server:call(FileServer, {get_cwd, filename(Filename)}), FileServer}.

read_file_info(Filename, FileServer) ->
    {ok, gen_server:call(FileServer, {read_file_info, filename(Filename)}), FileServer}.

altname(Filename, FileServer) ->
    {ok, gen_server:call(FileServer, {altname, filename(Filename)}), FileServer}.

write_file_info(Filename, Info, FileServer) ->
    {ok, gen_server:call(FileServer, {write_file_info, filename(Filename), Info}), FileServer}.

read_link_info(Filename, FileServer) ->
    {ok, gen_server:call(FileServer, {read_link_info, filename(Filename)}), FileServer}.

read_link(Filename, FileServer) ->
    {ok, gen_server:call(FileServer, {read_link, filename(Filename)}), FileServer}.

make_link(Old, New, FileServer) ->
    {ok, gen_server:call(FileServer, {make_link, filename(Old), filename(New)}), FileServer}.

make_symlink(Old, New, FileServer) ->
    {ok, gen_server:call(FileServer, {make_symlink, filename(Old), filename(New)}), FileServer}.

copy(SourceName, SourceOpts, DestName, DestOpts, Length, FileServer) ->
    {ok, gen_server:call(FileServer, {copy, filename(SourceName), SourceOpts, filename(DestName), DestOpts, Length}), FileServer}.

terminate(_Reason, _State) ->
    ok.

%% Internal
filename("file://" ++ Filename) ->
    Filename;
filename(Filename) ->
    Filename.
