This pages describes some issues we have encountered during the development process and explains how we worked around them.

### The use of `MPI_MODE_NOCHECK`

This issue occurred when using RMA functions after locking an MPI window with [MPI_Win_lock](https://www.mpich.org/static/docs/v3.2/www3/MPI_Win_lock.html):

`int MPI_Win_lock(int lock_type, int rank, int assert, MPI_Win win)`

When locking an MPI window for passively synchronized RMA, two types of locks are available: exclusive or shared.
In the distributed circular buffer shared memory (on the server), every _remote_ process is assigned a specific section, so that only one process ever attempts to access each section. This means that even if we lock the same window, even through the same target process, we can use a shared lock.

The third parameter is a hint that MPI implementations can use to optimize the call. The [documentation](https://www.mpich.org/static/docs/v3.2/www3/MPI_Win_lock.html) indicates that we can use `MPI_MODE_NOCHECK` to signal that no one will try to acquire a conflicting lock during the RMA epoch (which is the case in our program). *However*, we noticed that when using OpenMPI, a memory transfer using a call to `MPI_Put` or `MPI_Accumulate` will be limited to a size of approximately 8 kB. Beyond that, the call to `MPI_Put` or `MPI_Accumulate` simply never returns. This does not seem to occur with MPICH based implementations.

#### Solution
We have thus chosen not to use `MPI_MODE_NOCHECK` as an option to `MPI_Win_lock` when filling the distributed circular buffer from remote processes. We have not evaluated the performance impact of that option when using MPICH, so we don't know how useful it actually is.

### Allocating shared memory

For a while, we used the MPI-supplied function `MPI_Win_allocate_shared` to allocate shared memory while creating a window. We used `MPI_Win_shared_query` to get a pointer to that memory from other processes on the server, then have all server processes create a common window (with the remote processes) from pointers to that same memory. 
However, on some of our systems, remotely accessing that window through any process other than process 0 results in a bus error. This error occurs even if a process other than 0 actually did the memory allocation.

#### Solution
We just allocate the memory directly with calls to `shmget` and `shmat`, which is less portable but has the benefit of actually working correctly on linux systems. We send the ID of the allocated shared memory to other server processes by broadcasting it with MPI, and can successfully use that as the window data. The vulnerability window is quite narrow, as a call to mark the block for deletion is performed immediately after the allocating process attached the memory segment (this works on Linux systems).

### Using the Fortran transfer intrinsic

When using something like
```.f90
b = transfer( A , b )
```
it can be unhealthy not to have a variable in A.
```.f90
b = transfer( fn(x,y,...) , b )
```
can lead to silently generated incorrect code (if unlucky) or a compiler crash (if lucky)

N.B. the same behavior has been observed when `transfer(a,b)` is used as a function/subroutine argument (same workaround)

#### Workaround

```.f90
temp = fn(x,y,...)
b = transfer( temp, b )
```

### Using the Fortran C_LOC intrinsic

Some compilers do not behave in a sane manner when `C_LOC(whatever)` is used as a procedure argument

#### Workaround

```.f90
type(C_PTR) :: temp
temp = C_LOC(whatever)
call procedure(temp)
```

### Default initialization of nested derived types (Intel compiler)

The following code illustrates an issue with the Intel Fortran compiler (tested with 19.x and 21.4), where the compiler crashes when a function tries to instantiate a default-initialized derived type "user" that contains a derived type "grid" (from another module), which itself contains an array of another derived type "grid_inner". The solution in this particular case is to pass the result of the default initialization function of "grid_inner" to the constructor of "grid" when providing a default value for the member of "user" (-> `type(grid) :: g = grid(grid_inner())`).

```.f90
module grid_assembly_module
  implicit none

  type :: grid_inner
    integer :: tag = -1
  end type

  type :: grid
    type(grid_inner), dimension(1) :: inner = grid_inner()
  contains
    procedure, pass, public :: get_num
  end type

contains

  function get_num(this) result(num)
    implicit none
    class(grid), intent(in) :: this
    integer :: num
    num = 0
  end function get_num

end module grid_assembly_module

!--------------------------
! User module
module grid_user
  use grid_assembly_module
  implicit none

  type :: user
      type(grid) :: g = grid(grid_inner()) ! OK for both compilers
      !type(grid) :: g = grid()            ! crashes ifort, OK for gfortran
  contains
    procedure, pass :: generate_crash
  end type user

contains

  subroutine generate_crash(this)
    implicit none
    class(user), intent(in) :: this
    integer :: dummy
    dummy = this % g % get_num()
  end subroutine generate_crash

end module grid_user
!--------------------------
!
```

### Using (printing) a not-yet-allocated allocatable character variable

```.f90
character(len=:), allocatable :: name
print *, name                            ! Fine with Intel and GNU, but crashes at runtime with AOCC
!
```

Printing an unallocated character variable works with Intel and GNU compilers, as if the variable is simply an empty string. However, when compiled with AOCC, the program crashes with a failed deallocation error.
