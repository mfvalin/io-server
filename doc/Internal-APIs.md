# Internal APIs

## Heap manager 
* Declaring a heap
```.f90
use shmem_heap         ! acquire type definitions and associated procedures
type(heap) :: h        ! a (shared) memory heap
!
```
* Creating a heap
```.f90
type(C_PTR) :: p, addr
p = h % create(addr, heap_size)  ! addr must point to a valid memory address
!
```
* Acquiring an already created heap  (normally done only by the io server package)
```.f90
type(C_PTR) :: p, addr
p = h % clone(addr)        ! addr must be the address of a valid heap
!
```
* Allocating memory blocks on a heap
```.f90
type(C_PTR) :: block
integer(C_SIZE_T) :: nbytes
block = h % alloc(nbytes)
!
```

## Local circular buffer (CB)
* Declaring a [CB](#circular_buffer)
```.f90
use circular_buffer_module
type(circular_buffer) :: cb
!
```

* Creating a CB
```.f90
success = cb % create(num_bytes)                         ! From nothing
success = cb % create(memory_ptr, num_bytes)             ! From supplied memory
success = cb % create(memory_ptr_that_already_has_a_cb)  ! Initialize from existing CB (in shared memory, for example)
success = cb % delete()                                  ! Delete the buffer when done
!
```

* Read/write data to the CB
```.f90
success = cb % atomic_put(data_to_send, num_elements, element_type, .true.)   ! Put data in the buffer
success = cb % atomic_get(extracted_data, num_elements, element_type, .true.) ! Extract data from the buffer
success = cb % peek(read_data, num_elements, element_type)                    ! Look at the data without changing anything
success = cb % atomic_put(data_to_put, num_elements, element_type, .false.)   ! Put data in the buffer *without* making it accessible for reading
success = cb % atomic_get(read_data, num_elements, element_type, .false.)     ! Read data from the buffer *without* releasing the space
!
```

* Query the buffer
```.f90
max_num_elements = cb % get_capacity(element_type)     ! How much it can contain
num_elements     = cb % get_num_elements(element_type) ! How much it currently contains
num_spaces       = cb % get_num_spaces(element_type)   ! How much available space there is
num_elements     = cb % get_capacity(element_type)     ! How many elements can fit in total
is_valid         = cb % is_valid()         ! Check whether the buffer is usable
!
```

* Debugging help
```.f90
call cb % print_stats(id, .true./.false.)  ! Print usage and performance stats
call cb % print_header()                   ! Print buffer metadata
!
```

* C interface

Similar to Fortran, but all sizes are in bytes

```.c
cb = CB_create_bytes(num_bytes) 

result = CB_put(cb, src_data,  num_bytes, operation)    ! Can commit the data now to make it available immediately, or can wait
result = CB_get(cb, dest_data, num_bytes, operation)    ! Can simply peek, get the data without emptying the buffer, or completely extract it

num_bytes = CB_get_available_space_bytes(cb)            ! How many bytes can still fit
num_bytes = CB_get_available_data_bytes(cb)             ! How many bytes are stored
```

## Remote circular buffer (DCB)

[DCB](#distributed_circular_buffer)
