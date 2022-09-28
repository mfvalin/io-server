# Communication

This page describes how messages are sent from Model processes to the server. These messages can be either a command or grid data to process and assemble on the server.

## Message structure

Every message consists of three parts
1. A message header
2. The message content (optional)
3. An end cap

### Message header
The _message header_ contains basic info about the message:
- **Length of the data**.
- **Command code**. This is either "data" or some other command, like "open" or "close" stream.
- **Stream ID**. Most messages relate to a specific stream manipulated by the model, so we need to identify it.
- **Tag**. Almost all messages are collective calls; the server might receive them in any order, so it needs to be able to match all messages from a single call. 1 call = 1 tag
- **Sender rank**. On the communicator that includes _all_ IO-server processes

### Message content
The content is interpreted according to what the header indicates. If the header contains an _open_ command, the message content will be the name of the file to open. If the header indicates the message contains _grid data_, the content will have a more complex structure (detailed below).

### End cap
The end cap is simply an integer that repeats the length of the message. It is used to make (somewhat) sure the entire message has been received.

## Types of messages

### Commands
### {#Commands}
The possible commands are, so far
- **Data**. Grid data, to assemble on the server. More detail below.
- **dummy**. Used for debugging. Usually just an empty message, but could in theory contain any amount of data.
- **Open stream**. Open a communication [stream](Streams.md) with the server.
- **Close stream**. Close a previously open [stream](Streams.md)
- **Model stop**. Indicate that this model process will no longer send anything (end of execution, basically)
- **Relay stop**. Indicate that all model PEs to which the current relay listens are done.
- **Server command**. Indicate that the message contains a command that will be passed on to `process_command`.
- **Model stats**. The message contains data accumulated during the run (mostly usage and timing), to be compiled on the server.
- **User command**. The content of the message will be passed as-is to a user-provided function, set at initialization.
- **Acknowledge**. Is this still used? Kind of like the dummy...

### Data
Since the server needs a bit more information to assemble the grid, the content of a _data_ message is more complex. It consists of
- **Record**.
  - Global grid dimensions and positions
  - Subgrid dimensions and position (this PE's section of the global grid)
  - A pointer to the data within this process' heap, and associated metadata (size and type info)
  - The size of any additional data.
- **A command to execute** (optional). Information about what to do with the global grid once it is assembled. Will be passed as-is to the `process_data` function.
- **The data itself** (for relay-server communication only). This data is not directly included in model-relay communications, since the relay can simply read it directly from the node's shared memory. This avoids a bit of copy and MPI transmission.

## Relay process (server-bound data)
Each relay process mostly goes through the [CBs](#circular_buffer) from all model processes it manages and passes the content along. There are some additional steps in some cases.
- For all commands, the message header, content and cap is just stored in a larger buffer to pack a bunch of messages together before sending them to the server (we want larger, less-frequent MPI communications).
- For data messages, the message header is put into the larger buffer, then the data is retrieved from the corresponding shared memory heap and copied into the buffer.
  - Gotta also put metadata, before the data (not implemented yet)
- During any of these steps, if the larger buffer becomes full, it is sent to the server (through the [DCB](#distributed_circular_buffer))
  and emptied before moving on.

## Server process (server-bound data)
Each server process loops through the CB instances it manages within the DCB and processes any message found in them.
- It processes messages individually, not as an entire batch from a relay
- For _command_ messages, it does the corresponding action (see [Commands](#Commands))
- For _data_ messages, it puts the given data into the corresponding data assembly line, within the file associated with the message. If there is no corresponding assembly line yet, it creates it.
- At every loop iteration, checks whether the files this process is responsible for contain a completed assembly line. If so, write that line to disk. Only writes lines according to their tag order (it shouldn't be possible to complete a line with a higher tag before one with a lower tag in any case...)
