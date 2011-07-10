-module(evfs_filename).
-export([load/0,unload/1]).

-include("internal.hrl").

load() ->
    {ok, filename, Bin} = module(),
    Original = code:which(filename),
    Dir = filename:dirname(Original),

    %% Process original filename module
    {ok,{filename,[{abstract_code,{raw_abstract_v1, Abs0}}]}} =
        beam_lib:chunks(Original, [abstract_code]),
    Abs = lists:keyreplace(module, 3, Abs0, {attribute, 1, module, filename_1}),
    {ok, filename, Bin0} = compile:forms(Abs0),
    {ok, filename_1, Bin1} = compile:forms(Abs),
    %%

    ok = code:unstick_dir(Dir),
    {module, filename} = code:load_binary(filename, code:which(?MODULE), Bin),
    {module, filename_1} = code:load_binary(filename_1, Original, Bin1),
    ok = code:stick_dir(Dir),
    {ok, {Bin0, Original, Dir}}.

unload({Original, Filename, Dir}) ->
    ok = code:unstick_dir(Dir),

    {module, filename} = code:load_binary(filename, Filename, Original),

    code:delete(filename_1),
    code:purge(filename_1),

    ok = code:stick_dir(Dir),
    ok.
    

module() ->
    compile:forms(forms()).

forms() ->
    Exports0 = filename:module_info(exports),
    Exports = Exports0 -- [{module_info, 0}, {module_info, 1}],
    Attributes = 
        [{attribute, 1, file, {code:which(?MODULE), 1}},
         {attribute, 1, module, filename},
         {attribute, 1, export, Exports}],
    Functions = [ export(Export) || Export <- Exports ],
    Attributes ++ Functions.

export({Name, Arity}) when is_atom(Name) ->
    Args = [ list_to_atom("Arg__" ++ integer_to_list(ArgNo)) || ArgNo <- lists:seq(1, Arity) ],
    ArgVars = [ {var, 1, Arg} || Arg <- Args ],
    {function,1,Name,Arity,[
                            {clause, 1, ArgVars,
                             [], %% guard
                             [ %% body
                               {call, 1, {remote, 1, 
                                          {atom, 1, gen_server}, 
                                          {atom, 1, call}},
                                [{atom, 1, ?FILE_SERVER},
                                 {tuple, 1, [
                                             {atom, 1, filename}, 
                                             {atom, 1, Name},
                                             cons(ArgVars)
                                             ]}
                                 
                                ]}
                             ]}
                            ]}.
                          
cons([]) ->
    {nil, 1};
cons([H|T]) ->
    {cons, 1, H, cons(T)}.





