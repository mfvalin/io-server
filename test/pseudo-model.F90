module helpers
  use ISO_C_BINDING
  use ioserver_functions
!   include 'io-server/ioserver.inc'

 contains

subroutine print_created(temp, name)
  implicit none
  type(C_PTR), intent(IN), value :: temp
  character(len=*), intent(IN) :: name
  if(C_ASSOCIATED(temp)) then
    write(6,*)"block "//name//" created"
  else
    write(6,*)"block "//name//" creation failed"
  endif
end subroutine print_created

subroutine print_comm(is_null, name)
  implicit none
  logical, intent(IN), value :: is_null
  character(len=*), intent(IN) :: name
  character(len=8) :: n8
  n8 = name
  if(is_null) then
    write(6,*)n8//' communicator is NULL'
  else
    write(6,*)n8//' communicator is DEFINED'
  endif
end subroutine print_comm

subroutine print_comms(model, modelio, allio, nodeio, serverio, nodecom)
  implicit none
  integer, intent(IN) :: model, allio, nodeio, serverio, modelio, nodecom
!   include 'io-server/ioserver.inc'
  call print_comm(IOSERVER_Commisnull(model),    'model')
  call print_comm(IOSERVER_Commisnull(modelio),  'modelio')
  call print_comm(IOSERVER_Commisnull(allio),    'allio')
  call print_comm(IOSERVER_Commisnull(nodeio),   'nodeio')
  call print_comm(IOSERVER_Commisnull(serverio), 'serverio')
  call print_comm(IOSERVER_Commisnull(nodecom),  'nodecom')
end subroutine print_comms

end module helpers

program pseudomodelandserver
  use ISO_C_BINDING
  use helpers
  use ioserver_functions
  use memory_arena_mod
  implicit none
  external io_relay_fn
  integer :: status
  integer :: model, allio, nodeio, serverio, nio_node, modelio, nodecom, me
  integer :: comm, rank, size, nserv, ierr, noops, noderank, nodesize
  logical :: error
  character(len=128) :: arg
  type(C_PTR) :: p_base, p_relay, p_server, temp
  integer(C_INTPTR_T) :: sz_base, sz_relay, sz_server
  integer :: sz32
  type(memory_arena) :: ma
  character(len=8) :: blockname
  type(comm_rank_size) :: local_crs

  call mpi_init(status)
  local_crs = COMM_RANK_SIZE_NULL

  call IOSERVER_debug(1)            ! activate debug mode

  arg = '0'
  if(COMMAND_ARGUMENT_COUNT() >= 3) call GET_COMMAND_ARGUMENT(3, arg)
  read(arg,*)noops
  arg = '3'
  if(COMMAND_ARGUMENT_COUNT() >= 1) call GET_COMMAND_ARGUMENT(1, arg)
  read(arg,*) nserv
  nserv = nserv + noops
  arg = '2'
  if(COMMAND_ARGUMENT_COUNT() >= 2) call GET_COMMAND_ARGUMENT(2, arg)

  call get_local_world(comm, rank, size)
  me = ma%setid(rank)
  error = ioserver_set_winsizes(2*MBYTE, GBYTE/4, GBYTE/2)   !  base, relay, server
  if(error) then
    write(6,*)'ERROR: bad window sizes'
    goto 777
  endif

  if(rank >= nserv) then                    ! compute or IO relay Processes
    read(arg,*) nio_node                    ! number of relay processes per node
    call set_IOSERVER_relay(io_relay_fn)
    !  no return from ioserver_int_init in the case of IO relay processes
    !  compute processes will return from call
    status = ioserver_int_init(model, modelio, allio, nodeio, serverio, nodecom, nio_node, 'M')
    !  from this point on, this is a model compute process
    call print_comms(model, modelio, allio, nodeio, serverio, nodecom)

    call MPI_Comm_rank(model, rank, ierr)
    call MPI_Comm_size(model, size, ierr)
    call MPI_Comm_rank(nodecom, noderank, ierr)
    call MPI_Comm_size(nodecom, nodesize, ierr)
    write(6,*)'in pseudo model, PE',rank+1,' of',size
    write(6,*)'            node PE',noderank+1,' of', nodesize

    call IOSERVER_get_winmem(p_base, p_relay, p_server)
    call IOSERVER_get_winsizes(sz_base, sz_relay, sz_server)  ! of interest is sz_relay
    sz_relay = sz_relay / 4       ! convert to 32 bit units
    sz_relay = sz_relay / (size + nio_node)

    call MPI_Barrier(modelio, ierr)
    temp = ma%clone(p_relay)
    write(blockname,'(A5,I3.3)') "MHEAP",rank
    sz32 = sz_relay * 3 / 4       ! use 3/4 of relay memory
    temp = ma%newblock(sz32, blockname)
    call print_created(temp, blockname)
    write(blockname,'(A4,I4.4)') "MCIO",rank
    sz32 = sz_relay / 10
    temp = ma%newblock(sz32, blockname)
    call print_created(temp, blockname)
    write(blockname,'(A4,I4.4)') "MCIO",rank+1000
    temp = ma%newblock(sz32, blockname)
    call print_created(temp, blockname)
    call MPI_Barrier(modelio, ierr)
    call flush(6)
    call ma%dump()
    write(6,*)'END: pseudo compute, PE',noderank

  else            ! ranks 0, 1,..., nserv-1 : server and no-op processes

    nio_node = -1
    if(rank < noops) then          ! ranks below noops are NO-OP processes
      ! no return from ioserver_int_init in the case of NO-OP processes
      status = ioserver_int_init(model, modelio, allio, nodeio, serverio, nodecom, nio_node, 'Z')
    else                           ! server processes (normally on another node)
      status = ioserver_int_init(model, modelio, allio, nodeio, serverio, nodecom, nio_node, 'O')
      ! io_server_out will return from call
      call io_server_out(model, modelio, allio, nodeio, serverio, nodecom)
    endif

  endif
777 continue
  call ioserver_set_time_to_quit()
  write(6,*)'FINAL:, PE',noderank
  call mpi_finalize(status)
end program

subroutine io_relay_fn(model, modelio, allio, nodeio, serverio, nodecom)
  use ISO_C_BINDING
  use helpers
  use memory_arena_mod
  implicit none
  integer, intent(IN) :: model, allio, nodeio, serverio, nodecom, modelio
!   include 'io-server/ioserver.inc'
  integer :: rank, size, ierr, noderank, nodesize
  type(C_PTR) :: p_base, p_relay, p_server, temp
  type(memory_arena) :: ma
  integer(C_INTPTR_T) :: sz_base, sz_relay, sz_server
  integer(C_INT64_T) :: shmsz64

  call MPI_Comm_rank(nodeio, rank, ierr)
  call MPI_Comm_size(nodeio, size, ierr)
  call MPI_Comm_rank(nodecom, noderank, ierr)
  call MPI_Comm_size(nodecom, nodesize, ierr)
  write(6,*)'in pseudo io-relay, PE',rank+1,' of',size
  write(6,*)'               node PE',noderank+1,' of', nodesize

  call IOSERVER_get_winmem(p_base, p_relay, p_server)
  call IOSERVER_get_winsizes(sz_base, sz_relay, sz_server)

  call print_comms(model, modelio, allio, nodeio, serverio, nodecom)

  if(noderank == 0) then
    shmsz64 = sz_relay - 1024
!     temp = ma%create(p_relay, 128, shmsz64)
    temp = ma%clone(p_relay)
!     temp = ma%newblock(1024*1024, "RHEAP000")
!     temp = ma%newblock(1024*1024, "RCIOB000")
!     temp = ma%newblock(1024*1024, "RCIOB001")
    call flush(6)
    call ma%dump()
  else
    temp = ma%clone(p_relay)
  endif
  write(6,*)'relay before barrier'
  call MPI_Barrier(modelio, ierr)
  call MPI_Barrier(modelio, ierr)
  write(6,*)'relay after barrier'
  call flush(6)
  call ma%dump()
  write(6,*)'END: pseudo relay, PE',noderank
end subroutine io_relay_fn

subroutine io_server_out(model, modelio, allio, nodeio, serverio, nodecom)
  use ISO_C_BINDING
  use helpers
  use memory_arena_mod
  implicit none
  integer, intent(IN) :: model, allio, nodeio, serverio, modelio, nodecom
!   include 'io-server/ioserver.inc'

  type(C_PTR) :: p_base, p_relay, p_server
  integer(C_INTPTR_T) :: sz_base, sz_relay, sz_server
  integer :: rank, size, ierr, noderank, nodesize

  call MPI_Comm_rank(serverio, rank, ierr)
  call MPI_Comm_size(serverio, size, ierr)
  call MPI_Comm_rank(nodecom, noderank, ierr)
  call MPI_Comm_size(nodecom, nodesize, ierr)

  write(6,*)'in pseudo IO server, PE',rank+1,' of',size
  write(6,*)'         server node PE',noderank+1,' of', nodesize
  call print_comms(model, modelio, allio, nodeio, serverio, nodecom)

  call IOSERVER_get_winmem(p_base, p_relay, p_server)
  call IOSERVER_get_winsizes(sz_base, sz_relay, sz_server)
  write(6,*)'END: pseudo IO server, PE'

end subroutine io_server_out

subroutine get_local_world(comm, rank, size)
  use ISO_C_BINDING
  implicit none
  include 'mpif.h'
  integer, intent(OUT) :: comm, rank, size
  integer :: ierr
  comm = MPI_COMM_WORLD
  call MPI_Comm_rank(comm, rank, ierr)
  call MPI_Comm_size(comm, size, ierr)
end subroutine get_local_world
