# The API (proposed user view)

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
used to communicate between PEs on a single node.
```.f90
type(ioserver_input_parameters) :: io_params
logical :: success

! First adjust any desired parameters, like size of buffers between model and relay PEs, debug level, etc.
io_params % debug_level = 2
success = ioserver
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

## The Shared Memory user interface
declaring a heap

```
use shmem_heap         ! acquire type definitions and associated procedures
type(heap) :: h        ! a (shared) memory heap (by default the "process default heap")`
```

To use the heap created during IO server context initialization:
```
use ioserver_context
type(ioserver_context) :: context  ! Initialization described above
type(heap)             :: h

h = context % get_local_heap()  ! Access the heap created for the current process
```


Creating a heap from my own memory (do we want to allow that?)
```
 type(heap) :: h                                        ! a heap
 integer(C_INT), dimension(whatever), target :: myheap  ! array to transform into a heap
 type(C_PTR) :: p                                       ! will point to the heap address in memory
 p = h%create(C_LOC(myheap), whatever)                  ! create heap
```

(De)allocating Fortran arrays on a heap
```
 use shmem_heap         ! acquire type definitions and associated procedures
 type(heap) :: h        ! a (shared) memory heap (by default the "process default heap")
 integer(kind=4), dimension(:,:),  pointer :: iarray2d
 real(kind=8), dimension(:,:,:,:), pointer :: darray4d
 integer :: ni, nj, nk, nt
 type(block_meta_f08) :: ibmi2d, dbmi4d            ! descriptors associated with the arrays
 
 ibmi2d = h % allocate(iarray2d, [ni, nj])         ! allocate a 2D integer Fortran array
 iarray2d = 0                                      ! do something with iarray2d
 h % free(ibmi2d)                                  ! free using metadata
 dbmi4d = h % allocate(darray4d, [ni, nj, nk, nt]) ! allocate a 4D real*8 Fortran array
 darray4d = 0.0_8                                  ! do something with darray4d
 h % free(dbmi4d)                                  ! free using metadata
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
