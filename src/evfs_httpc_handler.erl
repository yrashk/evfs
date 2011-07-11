-module(evfs_httpc_handler).
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
-export([
         %% filename_absname/2,
         %% filename_absname/3,
         %% filename_absname_join/3,
         %% filename_basename/2,
         %% filename_basename/3,
         %% filename_dirname/2,
         %% filename_extension/2,
         %% filename_join/2,
         %% filename_join/3,
         %% filename_pathtype/2,
         %% filename_rootname/2,
         %% filename_rootname/3,
         %% filename_split/2,
         %% filename_nativename/2,
         %% filename_flatten/2,
         %% filename_append/3
]).

-export([io_list/1]).

init(Profile) ->
    {ok, Profile}.

supports("http://" ++ _, State) ->
    {true, State};

supports("https://" ++ _, State) ->
    {true, State};

supports(_, State) ->
    {false, State}.

%% Filesystem API

open(Filename, _Mode, Profile) ->
    Child = evfs_io_server:start_link(?MODULE, {Filename, Profile}),
    {ok, Child, Profile}.

read_file(Filename, Profile) ->
    case io_list({Filename, Profile}) of
        {error, Reason} ->
            {ok, {error, Reason}, Profile};
        Bin ->
            {ok, {ok, iolist_to_binary(Bin)}, Profile}
    end.

write_file(_Filename, _Bin, Profile) ->
    {ok, {error, enotsup}, Profile}.

set_cwd(_Filename, Profile) ->
    {ok, {error, enotsup}, Profile}.

delete(_Filename, Profile) ->
    {ok, {error, enotsup}, Profile}.

rename(_Fr, _To, Profile) ->
    {ok, {error, enotsup}, Profile}.

make_dir(_Filename, Profile) ->
    {ok, {error, enotsup}, Profile}.

del_dir(_Filename, Profile) ->
    {ok, {error, enotsup}, Profile}.

list_dir(_Filename, Profile) ->
    {ok, {error, enotsup}, Profile}.

get_cwd(Profile) ->
    {ok, {error, enotsup}, Profile}.

get_cwd(_Filename, Profile) ->
    {ok, {error, enotsup}, Profile}.

read_file_info(_Filename, Profile) ->
    {ok, {error, enotsup}, Profile}.

altname(_Filename, Profile) ->
    {ok, {error, enotsup}, Profile}.

write_file_info(_Filename, _Info, Profile) ->
    {ok, {error, enotsup}, Profile}.

read_link_info(_Filename, Profile) ->
    {ok, {error, enotsup}, Profile}.

read_link(_Filename, Profile) ->
    {ok, {error, enotsup}, Profile}.

make_link(_Old, _New, Profile) ->
    {ok, {error, enotsup}, Profile}.

make_symlink(_Old, _New, Profile) ->
    {ok, {error, enotsup}, Profile}.

copy(_SourceName, _SourceOpts, _DestName, _DestOpts, _Length, Profile) ->
    {ok, {error, enotsup}, Profile}.

terminate(_Reason, _State) ->
    ok.

%% filename

%%
%% IO server

io_list({Handle, Profile}) ->
    {ok, {{_Http, Code, _Reply}, _Headers, Body}} = httpc:request(Handle, Profile),
    case Code of
        404 ->
            {error, enoent};
        _ ->
            Body
    end.
