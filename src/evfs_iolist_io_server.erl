-module(evfs_iolist_io_server).
-behaviour(evfs_io_server).
%% File requests
-export([
         advise/4,
         pread/3,
         pwrite/3,
         datasync/1,
         sync/1,
         close/1,
         position/1,
         truncate/1
        ]).
%% IO requests
-export([
         put_chars/2,
         put_chars/3,
         put_chars/4,
         put_chars/5,
         get_until/4,
         get_until/5,
         get_until/6,
         get_chars/3,
         get_chars/4,
         get_line/2,
         get_line/3,
         setopts/2,
         getopts/1
        ]).

%% File requests
advise(_Offset, _Length, _Advise, Handle) ->
    {reply, ok, Handle}.

pread(At, Sz, Handle) ->
    Binary = iolist_to_binary(Handle),
    {reply, {ok, binary_to_list(binary:part(Binary, {At, Sz}))}, Handle}.

pwrite(_At, _Data, Handle) ->
    %% TODO
    {reply, ok, Handle}.

datasync(Handle) ->    
    {reply, ok, Handle}.

sync(Handle) ->
    {reply, ok, Handle}.

close(Handle) ->
    {stop, normal, ok, Handle}.

position(Handle) ->
    {reply, 0, Handle}.

truncate(Handle) ->
    {reply, ok, Handle}.

%% IO requests
put_chars(Chars, Handle) ->
    put_chars(latin1, Chars, Handle).

put_chars(_Enc, _Mod, _Func, _Args, Handle) ->
    {reply, {error, notsup}, Handle}.

put_chars(Mod, Func, Args, Handle) ->
    put_chars(latin1, Mod, Func, Args, Handle).


put_chars(_Enc, _Chars, Handle) ->
    {reply, {error, notsup}, Handle}.


get_until(_Enc, _Prompt, _Mod, _Func, _XtraArgs, Handle) ->
    {reply, {error, notsup}, Handle}.

get_until(_Enc, _Prompt, _N, Handle) ->
    {reply, {error, notsup}, Handle}.

get_until(Prompt, Mod, Func, XtraArgs, Handle) ->
    get_until(latin1, Prompt, Mod, Func, XtraArgs, Handle).

get_chars(_Enc, _Prompt, _N, Handle) ->
    {reply, {error, notsup}, Handle}.

get_chars(Prompt, N, Handle) ->
    get_chars(latin1, Prompt, N, Handle).

get_line(_Enc, _Prompt, Handle) ->
    {reply, {error, notsup}, Handle}.

get_line(Prompt, Handle) ->
    get_line(latin1, Prompt, Handle).
    
setopts(_Opts, Handle) ->
    {reply, {error, notsup}, Handle}.

getopts(Handle) ->
    {reply, {error, notsup}, Handle}.

