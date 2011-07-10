-module(evfs_io_server).
-behaviour(gen_server).
-export([behaviour_info/1]).

-export([start_link/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

behaviour_info(callbacks) ->
    [];
behaviour_info(_) ->
    undefined.

-record(state,
        {
          module :: module(),
          handle :: term()
        }).

-type state() :: #state{}.

-spec start_link(module(), term()) -> {ok, pid()} | {error, term()}.
                        
start_link(Module, Args) ->
    gen_server:start_link(?MODULE, {Module, Args}, []).

-spec init(module()) -> {ok, state()} | {stop, term()}.

init({Module, Args}) ->
    {ok, #state{ module = Module, handle = Args }}.

-spec handle_call(term(), term(), state()) ->
                         {noreply, state()} |
                         {stop, normal, stopped, state()}.
handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) ->
        {noreply, state()} | {stop, term(), state()}.

handle_info({file_request, From, ReplyAs, Request},
            #state{ module = Module, handle = Handle } = State) when is_pid(From) ->
    case file_request(Request, Module, Handle) of
        {reply, Reply, Handle1} ->
            From ! {file_reply, ReplyAs, Reply},
            {noreply, State#state{ handle = Handle1} };
        {error, Reply, Handle1} ->
            From ! {file_reply, ReplyAs, Reply},
            {noreply, State#state{ handle = Handle1} };
        {stop, Reason, Reply, Handle1} ->
            From ! {file_reply, ReplyAs, Reply},
            {stop, Reason, State#state{ handle = Handle1 }}
    end;

handle_info({io_request, From, ReplyAs, Request},
            #state{ module = Module, handle = Handle} = State) when is_pid(From) ->
    case io_request(Request, Module, Handle) of
        {reply, Reply, Handle1} ->
            From ! {io_reply, ReplyAs, Reply},
            {noreply, State#state{ handle = Handle1} };
        {error, Reply, Handle1} ->
            From ! {io_reply, ReplyAs, Reply},
            {noreply, State#state{ handle = Handle1} };
        {stop, Reason, Reply, Handle1} ->
            From ! {io_reply, ReplyAs, Reply},
            {stop, Reason, State#state{ handle = Handle1 }}
    end.


-spec terminate(term(), state()) -> ok.

terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private
file_request(Request, Module, Handle) when is_atom(Request) ->
    file_request({Request}, Module, Handle);
file_request(Request, Module, Handle) ->
    Command = element(1, Request),
    Args = tl(tuple_to_list(Request)),
    case (catch apply(Module, Command, Args ++ [Handle])) of
        {'EXIT', {undef, _}} ->
            file_request(Request, evfs_iolist_io_server, Module:io_list(Handle));
         Result ->
            Result
    end.

io_request({requests, Requests}, Module, Handle) ->
    io_request({requests, Requests, undefined}, Module, Handle);
io_request({requests, [], Result}, _Module, _Handle) ->
    Result;
io_request({requests, [Request|Rest], _Result}, Module, Handle) ->
    Result = io_request(Request, Module, Handle),
    io_request({requests, Rest, Result}, Module, Handle);
io_request(Request, Module, Handle) ->
    Command = element(1, Request),
    Args = tl(tuple_to_list(Request)),
    case (catch apply(Module, Command, Args ++ [Handle])) of
        {'EXIT', {undef, _}} ->
            file_request(Request, evfs_iolist_io_server, Module:io_list(Handle));
         Result ->
            Result
    end.
