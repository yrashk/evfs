-module(evfs_handler).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{init, 1},
     {supports, 2},
     {terminate, 2},
     %% File system API
     {open,3}, 
     {read_file,2},
     {write_file,3},
     {set_cwd,2},
     {delete,2},
     {rename,3},
     {make_dir,2},
     {del_dir,2},
     {list_dir,2},
     {get_cwd,1},
     {get_cwd,2},
     {read_file_info,2},
     {altname,2},
     {write_file_info,3},
     {read_link_info,2},
     {read_link,2},
     {make_link,3},
     {make_symlink,3},
     {copy,6}
    ];
behaviour_info(_) ->
    undefined.

