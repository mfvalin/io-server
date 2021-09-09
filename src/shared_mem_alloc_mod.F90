module shared_mem_alloc_module
  implicit none
contains

  ! Allocate one area of shared memory and distribute a (process-local) pointer to it to every process in
  ! the given communicator. This is a collective call on that communicator, so every process that belong
  ! to it must do the call. Only the process with rank 0 will actually do the allocation, so only the size
  ! given to that process is taken into account.
  subroutine RPN_allocate_shared(wsize, comm, baseptr)
    use ISO_C_BINDING
    use memory_arena_mod
    use mpi_f08
    implicit none
    INTEGER(KIND=MPI_ADDRESS_KIND), INTENT(IN) :: wsize
    type(MPI_Comm),    INTENT(IN)  :: comm              ! Communicator whose process will access the shared memory
    type(C_PTR),       INTENT(OUT) :: baseptr           ! Process-local pointer to the allocated shared memory

    integer :: myrank, shmid, thesize, i, hostid
    integer(C_INT64_T) :: siz
    type(C_PTR) :: p
    integer, dimension(:), allocatable :: hostids

    interface
        function c_get_hostid() result(id) bind(C,name='gethostid')
        import :: C_LONG
        implicit none
        integer(C_LONG) :: id
        end function c_get_hostid
    end interface

    baseptr = C_NULL_PTR
    p = C_NULL_PTR
    baseptr = transfer(p, baseptr) ! ??

    call MPI_Comm_rank(comm, myrank)
    call MPI_Comm_size(comm, thesize)
    allocate(hostids(thesize))
    hostid = INT(c_get_hostid(), 4)
    call MPI_Allgather(hostid, 1, MPI_INTEGER, hostids, 1, MPI_INTEGER, comm)
    do i = 1, thesize
        if(hostids(i) .ne. hostid) return   ! ERROR, hostid MUST be the same everywhere
    enddo

    if(myrank == 0) then
        siz = wsize
        p = memory_allocate_shared(shmid, siz)
    endif
    call MPI_Bcast(shmid, 1, MPI_INTEGER, 0, comm)
    if(myrank .ne. 0) then
        p = memory_address_from_id(shmid)
    endif
    baseptr = transfer(p, baseptr)
  end subroutine RPN_allocate_shared

end module shared_mem_alloc_module
