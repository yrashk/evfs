-module(evfs_default_handler).
-behaviour(evfs_handler).
-export([init/1, supports/2, terminate/2]).
-export([open/4, 
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
-export([filename_absname/2,
         filename_absname/3,
         filename_absname_join/3,
         filename_basename/2,
         filename_basename/3,
         filename_dirname/2,
         filename_extension/2,
         filename_join/2,
         filename_join/3,
         filename_pathtype/2,
         filename_rootname/2,
         filename_rootname/3,
         filename_split/2,
         filename_nativename/2,
         filename_flatten/2,
         filename_append/3]).

init(FileServer) ->
    {ok, FileServer}.

supports("file://" ++ _, State) ->
    {true, State};

supports(_, State) ->
    {true, State}.

%% Filesystem API

-define(FILE_IO_SERVER_TABLE, file_io_servers).
-define(FILE_IO_SERVER, file_io_server).  % Module

open(Pid, Filename, Mode, FileServer) ->
    %% Had to copy this from file_server.erl to avoid direct linking
    %% to the process spawned in evfs_file_server
    Child = ?FILE_IO_SERVER:start_link(Pid, Filename, Mode),
    case Child of
        {ok, P} when is_pid(P) ->
            ets:insert(?FILE_IO_SERVER_TABLE, {P, Filename});
        _ ->
            ok
    end,
    {ok, Child, FileServer}.

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

%% filename
filename_absname(File, State) ->
    {ok, filename_1:absname(filename(File)), State}.

filename_absname(File, Dir, State) ->
    {ok, filename_1:absname(filename(File), Dir), State}.

filename_absname_join(Dir, File, State) ->
    {ok, filename_1:absname_join(filename(Dir), File), State}.

filename_basename(File, State) ->
    {ok, filename_1:basename(filename(File)), State}.

filename_basename(File, Ext, State) ->
    {ok, filename_1:basename(filename(File), Ext), State}.

filename_dirname(File, State) ->
    {ok, filename_1:dirname(filename(File)), State}.

filename_extension(File, State) ->
    {ok, filename_1:extension(filename(File)), State}.

filename_join(Components, State) ->
    {ok, filename_1:join([filename(hd(Components))|tl(Components)]), State}.

filename_join(File1, File2, State) ->
    {ok, filename_1:join(filename(File1), File2), State}.

filename_pathtype(File, State) ->
    {ok, filename_1:pathtype(filename(File)), State}.

filename_rootname(File, State) ->
    {ok, filename_1:rootname(filename(File)), State}.

filename_rootname(File, Ext, State) ->
    {ok, filename_1:rootname(filename(File), Ext), State}.

filename_split(File, State) ->
    {ok, filename_1:split(filename(File)), State}.

filename_nativename(File, State) ->
    {ok, filename_1:nativename(filename(File)), State}.

filename_flatten(File, State) ->
    {ok, filename_1:flatten(filename(File)), State}.

filename_append(Dir, File, State) ->
    {ok, filename_1:append(filename(Dir), File), State}.

%% Internal
filename("file://" ++ Filename) ->
    Filename;
filename(Filename) ->
    Filename.
