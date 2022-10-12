# Usage

The IO server library exposes 3 main elements to be used by the model to interact with it:

* **[Streams](Streams.md)**: The model can open them, use them to send data to be interpolated, written, or otherwise processed on the server, and close them.
One stream will be processed by a single process on the server node. To distribute the work among multiple PEs on the server, we have to use multiple streams.
* **Shared memory heap**: Can be used to place data directly accessible to other processes on the node so that they can transmit it without the model having to copy it.
* **Serializer**: Can be used to transmit data without knowing anything about how that data is structured.

Using the IO server requires three actions from the model:
1. Library initialization/finalization. Done only once (each)
2. Stream opening/closing. Can be done as frequently as desired. Might be done only once in an entire run.
3. Sending data/commands

## Initialization

We might want (**or not**) to have a few global variables for certain structures.

```.f90
use ioserver_context_module
use jar_module
type(ioserver_context)      :: context
type(model_stream), pointer :: stream1, stream2
type(jar)                   :: metadata_jar
```

So, at first, we must initialize the [context](#ioserver_context_module::ioserver_context).
This creates all communicators used internally by the IO server library, as well as the data structures
used to communicate between PEs on a single node. _Note that the relay PEs run in the same executable and
must thus be managed by the client program_.
```.f90
use run_model_node_module           ! To access relay functions
type(ioserver_input_parameters) :: io_params
logical :: success
integer :: ierr

! First adjust any desired parameters, like size of buffers between model and relay PEs, debug level, etc.
io_params % debug_level        = 2    ! Level 1+ enables some additional synchronization
io_params % num_relay_per_node = 2    ! 1 server-bound and 1 model-bound
success = context % init(io_params)

if (.not. success) print *, 'Could not initialize the io-server library!!!'

! We must now manage the relay PEs. [num_relay_per_node] PEs were selected during initialization
! to be the relays on each compute node. These have a different execution path than the rest of
! PEs on these nodes.
if (context % is_relay()) then
  ! Run the appropriate relay function
  if (context % is_server_bound()) then)
    success = default_server_bound_relay()
  else if (context % is_model_bound()) then
    success = default_model_bound_relay()
  end if

  if (.not. success) print *, 'Could not run relay function!!'

  ! Finalize
  call io_context % finalize()
  call MPI_Finalize(ierr)
  stop
end if

! Model PEs (all PEs that reach this point) can now go do what they want and use the IO server
!
```

## Stream opening/closing

[Streams](Streams.md) can be opened and closed as many times as you want. This can be done every time step, or only once in the entire run, or for every needed output. Each time a stream is (re)opened, it
_may_ be mapped to a different PE on the server. See [model_stream](#model_stream_module::model_stream).

To open:
```.f90
call context % open_stream_model(stream1)
call context % open_stream_model(stream2)

if (.not. associated(stream1) .or. .not. associated(stream2)) print *, 'Unable to open a stream!!!!'
```

To close:
```.f90
success = stream1 % close()
if (.not. success) print *, 'Unable to close stream!!!'
```

## Sending commands and data
Once a stream is open, you can use it to send stuff to the server.
To send a command, you must first put it into a jar. The content of the jar will be interpreted by
the process_command() function. You need to create some sort of protocol so that process_command() will
understand what the model is sending.

```.f90
! A module/include file containing constants
integer, parameter :: CMD_OPEN_FILE  = 1
integer, parameter :: CMD_CLOSE_FILE = 2
integer, parameter :: CMD_WRITE_FILE = 3
```

### Commands
For example, to send a command that will open a file on the server:
```.f90
! Somewhere in the model code
logical   :: success
type(jar) :: cmd_jar
character(len=:), allocatable :: filename

filename = 'my_file.out'

! First put the content of the command into a jar
success = cmd_jar % new(50)
success = JAR_PUT_ITEM(cmd_jar, CMD_OPEN_FILE)  .and. success
success = JAR_PUT_ITEM(cmd_jar, len(filename))  .and. success
success = JAR_PUT_STRING(cmd_jar, filename)     .and. success

if (.not. success) print *, 'ERROR while creating command jar'

! Now just send it!
success = stream1 % send_command(cmd_jar)
```

### Data
```.f90
subroutine send_tile_to_server()
  use shmem_heap_module
  use jar_module
  implicit none
  type(jar) :: metadata_jar
  logical :: success
  type(grid_bounds_t) :: local_grid, global_grid
  type(shmem_heap)    :: model_heap

  #include <serializer.hf>

  ! i_min, j_min, k_min, i_max, j_max, k_max, etc. are given
  ! computed is the result of computation by the model
  integer, dimension(i_size, j_size, k_size) :: computed_data
  integer, dimension(:,:,:), pointer, contiguous :: h_data
  type(block_meta) :: h_info

  ! Access the heap for this PE
  model_heap = context % get_local_heap()

  ! Put data somewhere on the heap
  h_info = model_heap % allocate(h_data, min_bound = [i_min, j_min, k_min], max_bound = [i_max, j_max, k_max])
  h_data(:, :, :) = computed_data(:, :, :)

  ! Setup metadata
  local_grid % set_min(i_min, j_min, k_min)
  local_grid % set_max(i_max, j_max, k_max)
  global_grid % set_min(i_min_g, j_min_g, k_min_g)
  global_grid % set_max(i_max_g, j_max_g, k_max_g)

  success = metadata_jar % new(500)
  success = JAR_PUT_ITEM(metadata_jar, CMD_WRITE_FILE) .and. success
  ! Put any other useful info into the jar
  ! success = JAR_PUT_ITEM(metadata_jar, ...) .and. success

  if (.not. success) print *, 'Could not create the metadata jar!!'

  ! Do the call. This will return right away
  success = stream1 % send_data(h_info, local_grid, global_grid, metadata_jar)

  if (.not. success) print *, 'Unable to send data!!!'

  ! Now move on and do other computations
end subroutine send_tile_to_server()
```
This pattern can be repeated for every batch of data that needs to be sent to the server.
**Note**: The array pointer `h_data` does _not_ need to be deallocated; it will be done 
automatically by the io-server library once that data is no longer needed.


Overall, the simulation could look like
```.f90

call initialize_io_server(...)

call open_streams(...)

do i = 1, num_steps
  call do_some_computations(...)
  call send_tile_to_server(...)

  call do_some_more_computations(...)
  call send_tile_to_server(...)

  ...
end do

call close_streams(...)

call finalize_server(...)
```


## The model (user) interface to the IO server package
### model -> IO server 
```.f90
 use ioserver_context
 type(ioserver_context) :: context
 type(model_stream)     :: f

 ! Must first initialize the IO-server library
 ! return .true. for success, .false. for failure
 status = context % init(num_relays_per_node)

 f = context % open_stream('file_name_pattern')               ! Open a stream 

 status = f % set_value(symbol, value)                              ! Set the value of a symbol
 status = f % write(mydata , area ,grid_in ,grid_out , cprs, meta)  ! write data
   type(block_meta), intent(IN) :: mydata               ! array descriptor from h % allocate
   type(subgrid), intent(IN) :: area                    ! area in global space
   type(grid), intent(IN) :: grid_in                    ! input grid code
   type(grid), intent(IN) :: grid_out                   ! output grid code
   type(cmeta), intent(IN), optional :: cprs            ! compression related metadata
   type(jar), intent(IN), optional :: meta              ! metadata associated with data
                                                        ! (carried blindly in serializing container)

 status = f % close()                                   ! Close output file
 call context % finalize()                              ! Terminate server library (will close any open file)
```

## The data serializer (supersedes pickling)

Creating a serializing container
```
 #include <serializer.hf>
 use data_serialize
 JAR_DECLARE(my_jar)               ! declaration of a serializing container
 integer :: ok
 . . . .
 ok = JAR_CREATE(my_jar, 4096)     ! this container can hold of to 4096 32 bit items
```

Adding data to a container
```
 type(some_type) :: scalar
 type(some_other_type), dimension(nnn) :: array
 integer :: n
 . . . .
 n = JAR_PUT_ITEM(my_jar, scalar)             ! put scalar into container
 n = JAR_PUT_ITEMS(my_jar, array(start:end))  ! put array(start:end) into container
```
Retrieving data from a container
```
 type(some_type) :: scalar2
 type(some_other_type), dimension(nnn) :: array2
 n = JAR_GET_ITEM(my_jar, scalar2)              ! populate scalar2 from container
 n = JAR_GET_ITEMS(my_jar,array2(start2:end2))  ! populate array2(start2:end2) from container
 ! end2 - start2 must be equal to end - start
```

## Configuring the IO server processes (work in progress)
```
 export IO_SERVER_CONFIG='CTRLMEM = n0,  REMOTEMEM = n2, SERVERMEM = n3'
 export IO_RELAY_CONFIG='RELAYMEM = n1, MHEAP = n4, MCIO = n5,n6, RHEAP = n7, RCIO = n8,n9'
 n0, n1, n2, n3, ... in MegaBytes
```
