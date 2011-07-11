-module(evfs_file_server).
-behaviour(gen_server).

-include("internal.hrl").

-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(EVFS_HANDLERS_TABLE, evfs_handlers).

-type handler() :: {module(), term()}.

-record(state,
        {
          handlers = [] :: [handler()],
          file_server :: pid()
        }).

-type state() :: #state{}.

-spec start_link(pid()) -> {ok, state()} | {error, term()}.

start_link(FileServer) ->
    gen_server:start({local, ?FILE_SERVER}, ?MODULE, FileServer, []).
                        
-spec init(pid()) -> {ok, state()} | {stop, term()}.

init(FileServer) ->
    case ?DEFAULT_HANDLER:init(FileServer) of
        {ok, HState} ->
            {ok, #state{ handlers = [{?DEFAULT_HANDLER, HState}], file_server = FileServer }};
        {stop, _Reason} = Error ->
            Error
    end.

-type filename_command() :: absname | absname_join | basename |
                            dirname | extension | join | pathtype |
                            rootname | split | nativename |
                            find_src | flatten.

-type file_server_command() :: 
        original_file_server |
        {unregister_handler, module()} |
        {register_handler, module(), term()} | 
        %% Filename API
        {filename, filename_command(), [term()]} |
        %% Filesystem API
        {open, file:filename(), [file:mode()]} |
        {read_file, file:filename()} |
        {write_file, file:filename(), binary()} |
        {set_cwd, file:filename()} |
        {delete, file:filename()} |
        {rename, file:filename(), file:filename()} |
        {make_dir, file:filename()} |
        {del_dir, file:filename()} |
        {list_dir, file:filename()} |
        get_cwd |
        {get_cwd} |
        {get_cwd, file:filename()} |
        {read_file_info, file:filename()} |
        {altname, file:filename()} |
        {write_file_info, file:filename(), file:file_info()} |
        {read_link_info, file:filename()} |
        {read_link, file:filename()} |
        {make_link, file:filename(), file:filename()} |
        {make_symlink, file:filename(), file:filename()} |
        {copy, file:filename(), [file:mode()],
         file:filename(), [file:mode()], non_neg_integer()}.


-spec handle_call(file_server_command(), term(), state()) ->
                         {noreply, state()} |
                         {reply, eof | ok | 
                          {error, term()} |
                          {ok, term()}, state()} |
                         {stop, normal, stopped, state()}.

handle_call({register_handler, Handler, Args}, _From,
            #state{ handlers = Handlers } = State) ->
    case Handler:init(Args) of
        {ok, HState} ->
            {reply, ok, State#state{ handlers = [{Handler, HState}|Handlers] } };
        {stop, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({unregister_handler, Handler}, _From,
            #state{ handlers = Handlers } = State) ->
    {reply, ok, State#state{
                  handlers = Handlers -- [{Handler,
                                           proplists:get_value(Handler, Handlers)}] } };

handle_call(original_file_server, _From,
            #state{ file_server = FileServer } = State) ->
    {reply, FileServer, State };

handle_call({filename, absname, [Filename]}, From, State) ->
    safe_call_handler(From, Filename, filename_absname, [Filename], State);

handle_call({filename, absname, [Filename, Dir]}, From, State) ->
    safe_call_handler(From, Filename, filename_absname, [Filename, Dir], State);

handle_call({filename, absname_join, [Dir, Filename]}, From, State) ->
    safe_call_handler(From, Dir, filename_absname_join, [Dir, Filename], State);

handle_call({filename, basename, [Filename]}, From, State) ->
    safe_call_handler(From, Filename, filename_basename, [Filename], State);

handle_call({filename, basename, [Filename, Ext]}, From, State) ->
    safe_call_handler(From, Filename, filename_basename, [Filename, Ext], State);

handle_call({filename, dirname, [Filename]}, From, State) ->
    safe_call_handler(From, Filename, filename_dirname, [Filename], State);

handle_call({filename, extension, [Filename]}, From, State) ->
    safe_call_handler(From, Filename, filename_extension, [Filename], State);

handle_call({filename, join, [Components]}, From, State) ->
    safe_call_handler(From, hd(Components), filename_join, [Components], State);

handle_call({filename, join, [Name1, Name2]}, From, State) ->
    safe_call_handler(From, Name1, filename_join, [Name1, Name2], State);

handle_call({filename, append, [Dir, Name]}, From, State) ->
    safe_call_handler(From, Dir, filename_append, [Dir, Name], State);

handle_call({filename, pathtype, [Path]}, From, State) ->
    safe_call_handler(From, Path, filename_pathtype, [Path], State);

handle_call({filename, rootname, [Filename]}, From, State) ->
    safe_call_handler(From, Filename, filename_rootname, [Filename], State);

handle_call({filename, rootname, [Filename, Ext]}, From, State) ->
    safe_call_handler(From, Filename, filename_rootname, [Filename, Ext], State);

handle_call({filename, split, [Filename]}, From, State) ->
    safe_call_handler(From, Filename, filename_split, [Filename], State);

handle_call({filename, nativename, [Filename]}, From, State) ->
    safe_call_handler(From, Filename, filename_nativename, [Filename], State);

handle_call({filename, find_src, Args}, _From, State) ->
    {reply, apply(filename_1,find_src, Args), State};

handle_call({filename, flatten, [Filename]}, From, State) ->
    safe_call_handler(From, Filename, filename_flatten, [Filename], State);

%%

handle_call({open, Filename, ModeList}, From, State) ->
    call_handler(From, Filename, open, [Filename, ModeList], State);

handle_call({read_file, Filename}, From, State) ->
    call_handler(From, Filename, read_file, [Filename], State);

handle_call({write_file, Filename, Bin}, From, State) ->
    call_handler(From, Filename, write_file, [Filename, Bin], State);

handle_call({set_cwd, Filename}, From, State) ->
    call_handler(From, Filename, set_cwd, [Filename], State);

handle_call({delete, Filename}, From, State) ->
    call_handler(From, Filename, delete, [Filename], State);

handle_call({rename, Fr, To}, From, State) ->
    call_handler(From, Fr, rename, [Fr, To], State);

handle_call({make_dir, Filename}, From, State) ->
    call_handler(From, Filename, make_dir, [Filename], State);

handle_call({del_dir, Filename}, From, State) ->
    call_handler(From, Filename, del_dir, [Filename], State);

handle_call({list_dir, Filename}, From, State) ->
    call_handler(From, Filename, list_dir, [Filename], State);

handle_call(get_cwd, From, State) ->
    call_handler(From, "/", get_cwd, [], State);

handle_call({get_cwd}, From, State) ->
    call_handler(From, "/", get_cwd, [], State);

handle_call({get_cwd, Filename}, From, State) ->
    call_handler(From, Filename, get_cwd, [Filename], State);

handle_call({read_file_info, Filename}, From, State) ->
    call_handler(From, Filename, read_file_info, [Filename], State);

handle_call({altname, Filename}, From, State) ->
    call_handler(From, Filename, altname, [Filename], State);

handle_call({write_file_info, Filename, Info}, From, State) ->
    call_handler(From, Filename, write_file_info, [Filename, Info], State);

handle_call({read_link_info, Filename}, From, State) ->
    call_handler(From, Filename, read_link_info, [Filename], State);

handle_call({read_link, Filename}, From, State) ->
    call_handler(From, Filename, read_link, [Filename], State);

handle_call({make_link, Old, New}, From, State) ->
    call_handler(From, Old, make_link, [Old, New], State);

handle_call({make_symlink, Old, New}, From, State) ->
    call_handler(From, Old, make_symlink, [Old, New], State);

handle_call({copy, SourceName, SourceOpts, DestName, DestOpts, Length},
            From, State) ->
    call_handler(From, SourceName, copy, [SourceName, SourceOpts, DestName, DestOpts, Length], State);

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(Request, From, State) ->
    error_logger:error_msg("handle_call(~p, ~p, _)", [Request, From]),
    {noreply, State}.


-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast(Msg, State) ->
    error_logger:error_msg("handle_cast(~p, _)", [Msg]),
    {noreply, State}.

-spec handle_info(term(), state()) ->
        {noreply, state()} | {stop, normal, state()}.
handle_info(_Info, State) ->
    {noreply, State}.


-spec terminate(term(), state()) -> ok.

terminate(Reason, #state{ handlers = Handlers } = _State) ->
    [ Handler:terminate(Reason, HState) || {Handler, HState} <- Handlers ],
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
-spec find_handler([handler()], file:filename()) -> {module(), term(), [handler()]} | {false, [handler()]}.

find_handler(Handlers, Filename) ->
    find_handler_1(Handlers, Filename, []).

find_handler_1([], _Filename, NewHandlers) ->
    {false, lists:reverse(NewHandlers)};
find_handler_1([{Handler, HState}|Rest], Filename, NewHandlers) ->
    {Result, HState1} = Handler:supports(Filename, HState),
    NewHandlers1 = [{Handler, HState1}|NewHandlers],
    find_handler_2(Result, Rest, Filename, NewHandlers1).

find_handler_2(true, Rest, _Filename, [{Handler, HState}|_] = NewHandlers) ->
    {Handler, HState, lists:reverse(NewHandlers) ++ Rest};
find_handler_2(false, Rest, Filename, NewHandlers) ->
    find_handler_1(Rest, Filename, NewHandlers).

-spec update_state(module(), term(), [handler()]) -> [handler()].

update_state(Handler, HState, [{Handler, _}|Rest]) ->
    [{Handler, HState}|Rest];
update_state(Handler, HState, [OtherHandler|Rest]) ->
    [OtherHandler|update_state(Handler, HState, Rest)].

                          
-spec call_handler(file:filename(), atom(), [term()], state()) -> {reply, term(), state()}.
                          
call_handler(Filename, Function, Arguments, #state{ handlers = Handlers } = State) ->
    case find_handler(Handlers, Filename) of
        {Handler, HState, Handlers1} ->
            case apply(Handler, Function, Arguments ++ [HState]) of
                {ok, Result, HState1} ->
                    {reply, Result, 
                     State#state{ 
                       handlers = update_state(Handler,
                                               HState1,
                                               Handlers1) }};
                {error, Reason, HState1} ->
                    {reply, {error, Reason}, 
                     State#state{ 
                       handlers = update_state(Handler,
                                               HState1,
                                               Handlers1) }}
            end;
        {false, Handlers1} ->
            {reply, {error, notsup}, State#state{ handlers = Handlers1 }}
    end.

-spec call_handler(term(), file:filename(), atom(), [term()], state()) -> {reply, term(), state()}.

call_handler(From, Filename, Function, Arguments, State) ->
    spawn_link(fun () ->
                       {reply, Result, State1} = call_handler(Filename, Function, Arguments, State),
                       gen_server:reply(From, Result)
               end),
    {noreply, State}.
    
-spec safe_call_handler(file:filename(), atom(), [term()], state()) -> {reply, term(), state()}.

safe_call_handler(Filename, Function, Arguments, 
                  #state{ handlers = Handlers } = State) ->
    case (catch call_handler(Filename, Function, Arguments, State)) of
        {'EXIT',{undef, _}} ->
            call_handler(Filename, Function, Arguments, 
                         State#state{ 
                           handlers = tl(Handlers)
                          });
        Result ->
            Result
    end.

-spec safe_call_handler(term(), file:filename(), atom(), [term()], state()) -> {reply, term(), state()}.

safe_call_handler(From, Filename, Function, Arguments, State) ->
    spawn_link(fun () ->
                       {reply, Result, State1} = safe_call_handler(Filename, Function, Arguments, State),
                       gen_server:reply(From, Result)
               end),
    {noreply, State}.
