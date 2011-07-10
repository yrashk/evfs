evfs: Erlang Virtual Filesystem
===============================

This application allows to create virtual filesystems on top of standard
`file` API. Imagine if you can do this:

    > {ok, Bin} = file:read("http://www.google.com/").

Or this:

    > {ok, Bin} = file:read("config://host").

Looks interesting, right? Now, basically, because of the way evfs operates,
you can feed unsuspecting Erlang code file names that are served off your VFSes. 
That's where the fun begins.

You can register any number of virtual FS handlers using `evfs:register/2` function.
Also, by default, you get the handler that redirects file:// and plain filename requests
to the original file server, but you can unregiser it with `evfs:unregister/1' if you need.

Please bear in mind that this is a very early prototype. I still haven't implemented
a behaviour for file I/O (`file_request` and `io_request). Not that it blocks you
from creating your VFS. It just will make it simpler. Right now you have to look into
file_io_server.erl from OTP in order to figure out how to do this.

For an example on how to build a VFS, look into `tests/evfs_test_fs.erl`. Hopefully
it will help.