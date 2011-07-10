-module(evfs_app).

-behaviour(application).

-include("internal.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Pid} = evfs_sup:start_link(),
    {evfs_file_server, FileServer, _Type, _Modules} = 
        lists:keyfind(evfs_file_server, 1, supervisor:which_children(Pid)),
    {ok, Pid, {FileServer, evfs:original_file_server(FileServer)}}.

stop({_FileServer, OriginalFileServer}) ->
    register(?FILE_SERVER, OriginalFileServer),
    ok.
