# Shared memory heap user interface

[Shared memory heap API](#shmem_heap_module::shmem_heap)

Declaring a heap
```.f90
use shmem_heap_module   ! acquire type definitions and associated procedures
type(shmem_heap) :: h   ! a (shared) memory heap (by default the "process default heap")
!
```

To use the heap created during IO server context initialization:
```.f90
use ioserver_context_module
type(ioserver_context) :: context  ! Initialization described above
type(shmem_heap)       :: h

h = context % get_local_heap()  ! Access the heap created for the current process
!
```

Creating a heap from my own memory (do we even want to allow that?)
```.f90
use shmem_heap_module
type(shmem_heap) :: h                                       ! A shmem heap instance
integer(C_INT), dimension(heap_size), target :: heap_space  ! Array to use as heap data area
logical :: success                                          ! Will indicate whether heap creation was successful
success = h % create(C_LOC(heap_space), heap_size)          ! Create the heap object
!
```

Allocating and deallocating Fortran arrays on a heap
```.f90
use shmem_heap_module           ! acquire type definitions and associated procedures
type(shmem_heap) :: h           ! a (shared) memory heap
integer(kind=4), dimension(:,:),     pointer :: iarray2d
real(kind=8),    dimension(:,:,:,:), pointer :: darray4d
integer :: ni, nj, nk, nt, i0, j0
type(block_meta) :: ibmi2d, dbmi4d                ! descriptors associated with the arrays

ibmi2d = h % allocate(iarray2d, [ni, nj])         ! allocate a 2D integer Fortran array
iarray2d(:, :) = 0                                ! do something with iarray2d
h % free(ibmi2d)                                  ! free using metadata

dbmi4d = h % allocate(darray4d, [ni, nj, nk, nt]) ! allocate a 4D real*8 Fortran array
darray4d(:, :, :, :) = 0.0_8                      ! do something with darray4d
h % free(dbmi4d)                                  ! free using metadata

! Can also allocate using array bounds
ibmi2d = h % allocate(iarray2d, [i0, j0], [i0 + ni - 1, j0 + nj - 1])  ! allocate a 2D integer Fortran array
iarray2d(:, :) = 0                                ! do something with iarray2d
h % free(ibmi2d)                                  ! free using metadata
!
```
