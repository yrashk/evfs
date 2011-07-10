-module(evfs_test_fs).
-behaviour(evfs_handler).
-behaviour(evfs_io_server).
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
-export([io_list/1]).
-export([filename_dirname/2]).

init(Args) ->
    {ok, Args}.

supports("test://" ++ _Rest, State) ->
    {true, State};
supports(_, State) ->
    {false, State}.

%% Filesystem API

open(Filename, _Mode, State) ->
    Child = evfs_io_server:start_link(?MODULE, Filename),
    {ok, Child, State}.

read_file(Filename, State) ->
    {ok, {ok, list_to_binary(Filename)}, State}.

write_file(_Filename, _Bin, State) ->
    {ok, {error, enotsup}, State}.

set_cwd(_Filename, State) ->
    {ok, {error, enotsup}, State}.

delete(_Filename, State) ->
    {ok, {error, enotsup}, State}.

rename(_Fr, _To, State) ->
    {ok, {error, enotsup}, State}.

make_dir(_Filename, State) ->
    {ok, {error, enotsup}, State}.

del_dir(_Filename, State) ->
    {ok, {error, enotsup}, State}.

list_dir(_Filename, State) ->
    {ok, {error, enotsup}, State}.

get_cwd(State) ->
    {ok, {error, enotsup}, State}.

get_cwd(_Filename, State) ->
    {ok, {error, enotsup}, State}.

read_file_info(_Filename, State) ->
    {ok, {error, enotsup}, State}.

altname(_Filename, State) ->
    {ok, {error, enotsup}, State}.

write_file_info(_Filename, _Info, State) ->
    {ok, {error, enotsup}, State}.

read_link_info(_Filename, State) ->
    {ok, {error, enotsup}, State}.

read_link(_Filename, State) ->
    {ok, {error, enotsup}, State}.

make_link(_Old, _New, State) ->
    {ok, {error, enotsup}, State}.

make_symlink(_Old, _New, State) ->
    {ok, {error, enotsup}, State}.

copy(_SourceName, _SourceOpts, _DestName, _DestOpts, _Length, State) ->
    {ok, {error, enotsup}, State}.

terminate(_Reason, _State) ->
    ok.

%% IO server
io_list(Handle) ->
    Handle.

%% Filename
filename_dirname("test://" ++ _, State) ->
    {ok, "test://", State}.
