# Streams

A _stream_ is an object through which the model (client) communicates with the server.
It can be viewed as a channel through which we send commands and data, which are received
and processed by a single PE on the server.
There is a (configurable) maximum number of streams that can be open concurrently, but there
is no limit to the total number of streams opened during one run of a program.
[See the public stream API](#model_stream_module::model_stream).

Since a stream is mapped to a single server PE, it is well suited to write a specific file.
All operations on a certain file (open, write/modify, close) should be done through a single stream,
so as to avoid multiple processes attempting to modify the same file.
With this convention, a single stream is able to manage multiple files, concurrently or not.

To maximize concurrency on the server, we should create several streams that will process different
data. If there are more streams than available server PEs, multiple streams may be mapped to the
same server PE.
The number of _stream processor_ PEs can be chosen when launching the server.

## Implementation details

Multiple server PEs receive model data and commands, and they are not the same as the stream processors.
This means that every PE on the server need to be able to access each server-side stream.
The shared part of a stream is managed by a [shared server stream](#server_stream_module::shared_server_stream)
instance. Each PE can only access this shared part through their own
[local server stream](#server_stream_module::local_server_stream).

In addition to all the shared stream instances, there is a shared memory heap that is common to
everyone on the server. Each local stream instance has a handle to that heap so that they can
allocate and access the space to assemble a global grid.

Each stream has both a _stream\_rank_ and a _stream\_id_. Each stream that is ever open has a unique stream_id.
However, since there is a fixed maximum number of streams that can be open concurrently, the _rank_ refers to
the position of an open stream within the list of currently open streams.
