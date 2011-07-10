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

-type file_server_command() :: 
        original_file_server |
        {unregister_handler, module()} |
        {register_handler, module(), term()} | 
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

handle_call({open, Filename, ModeList}, _From, State) ->
    call_handler(Filename, open, [Filename, ModeList], State);

handle_call({read_file, Filename}, _From, State) ->
    call_handler(Filename, read_file, [Filename], State);

handle_call({write_file, Filename, Bin}, _From, State) ->
    call_handler(Filename, write_file, [Filename, Bin], State);

handle_call({set_cwd, Filename}, _From, State) ->
    call_handler(Filename, set_cwd, [Filename], State);

handle_call({delete, Filename}, _From, State) ->
    call_handler(Filename, delete, [Filename], State);

handle_call({rename, Fr, To}, _From, State) ->
    call_handler(Fr, rename, [Fr, To], State);

handle_call({make_dir, Filename}, _From, State) ->
    call_handler(Filename, make_dir, [Filename], State);

handle_call({del_dir, Filename}, _From, State) ->
    call_handler(Filename, del_dir, [Filename], State);

handle_call({list_dir, Filename}, _From, State) ->
    call_handler(Filename, list_dir, [Filename], State);

handle_call(get_cwd, _From, State) ->
    call_handler("/", get_cwd, [], State);

handle_call({get_cwd}, _From, State) ->
    call_handler("/", get_cwd, [], State);

handle_call({get_cwd, Filename}, _From, State) ->
    call_handler(Filename, get_cwd, [Filename], State);

handle_call({read_file_info, Filename}, _From, State) ->
    call_handler(Filename, read_file_info, [Filename], State);

handle_call({altname, Filename}, _From, State) ->
    call_handler(Filename, altname, [Filename], State);

handle_call({write_file_info, Filename, Info}, _From, State) ->
    call_handler(Filename, write_file_info, [Filename, Info], State);

handle_call({read_link_info, Filename}, _From, State) ->
    call_handler(Filename, read_link_info, [Filename], State);

handle_call({read_link, Filename}, _From, State) ->
    call_handler(Filename, read_link, [Filename], State);

handle_call({make_link, Old, New}, _From, State) ->
    call_handler(Old, make_link, [Old, New], State);

handle_call({make_symlink, Old, New}, _From, State) ->
    call_handler(Old, make_symlink, [Old, New], State);

handle_call({copy, SourceName, SourceOpts, DestName, DestOpts, Length},
            _From, State) ->
    call_handler(SourceName, copy, [SourceName, SourceOpts, DestName, DestOpts, Length], State);

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
