-module(evfs_app).

-behaviour(application).

-include("internal.hrl").

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = evfs_sup:start_link(),
    {ok, OriginalFilename} = evfs_filename:load(),
    {evfs_file_server, FileServer, _Type, _Modules} = 
        lists:keyfind(evfs_file_server, 1, supervisor:which_children(Pid)),
    {ok, Pid, {FileServer, evfs:original_file_server(FileServer), OriginalFilename}}.

prep_stop({_FileServer, _OriginalFileServer, Filename} = State) ->
    ok = evfs_filename:unload(Filename),
    State.

stop({_FileServer, OriginalFileServer, _Filename}) ->
    ok = register(?FILE_SERVER, OriginalFileServer),
    ok.
