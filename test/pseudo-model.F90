program pseudomodelandserver
  use ISO_C_BINDING
  use memory_arena_mod
  implicit none
  external io_relay
  include 'io-server/ioserver.inc'
!   include 'io-server/memory_arena.inc'
  integer :: status
  integer :: model, allio, nodeio, serverio, nio_node, modelio, me
  integer :: comm, rank, size, nserv, ierr, noops, node_rank, node_size
  logical :: error
  character(len=128) :: arg
  type(C_PTR) :: p_base, p_relay, p_server, temp
  type(memory_arena) :: ma

  call mpi_init(status)

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
  error = ioserver_set_winsizes(2*MBYTE, GBYTE/2, GBYTE)   !  base, relay, server
  if(error) then
    write(6,*)'ERROR: bad window sizes'
    goto 777
  endif

  if(rank >= nserv) then
    read(arg,*) nio_node ! relay processes per node
    call set_IOSERVER_relay(io_relay)
    status = ioserver_init(model, modelio, allio, nodeio, serverio, nio_node, 'M')
    call MPI_Comm_rank(model, rank, ierr)
    call MPI_Comm_size(model, size, ierr)
    write(6,*)'in pseudo model, PE',rank+1,' of',size
    call IOSERVER_get_winmem(p_base, p_relay, p_server)
    temp = ma%clone(p_relay)
    write(6,*)'model before barrier'
    call MPI_Barrier(modelio, ierr)
    call flush(6)
    call ma%dump()
  else  ! ranks 0, 1, nserv-1 : server
    nio_node = -1
    if(rank < noops) then ! ranks below noops are NO-OP processes
      status = ioserver_init(model, modelio, allio, nodeio, serverio, nio_node, 'Z')
    else
      status = ioserver_init(model, modelio, allio, nodeio, serverio, nio_node, 'O')
      call IOSERVER_get_winmem(p_base, p_relay, p_server)
    endif
    call MPI_Comm_rank(serverio, rank, ierr)
    call MPI_Comm_size(serverio, size, ierr)
    write(6,*)'in pseudo IO server, PE',rank+1,' of',size
  endif
  if(IOSERVER_Commisnull(model))    write(6,*)'model    communicator is NULL'
  if(.not. IOSERVER_Commisnull(model))    write(6,*)'model    communicator is DEFINED'
  if(IOSERVER_Commisnull(modelio))  write(6,*)'modelio  communicator is NULL'
  if(.not. IOSERVER_Commisnull(modelio))  write(6,*)'modelio  communicator is DEFINED'
  if(IOSERVER_Commisnull(allio))    write(6,*)'allio    communicator is NULL'
  if(.not. IOSERVER_Commisnull(allio))    write(6,*)'allio    communicator is DEFINED'
  if(IOSERVER_Commisnull(nodeio))   write(6,*)'nodeio   communicator is NULL'
  if(.not. IOSERVER_Commisnull(nodeio))   write(6,*)'nodeio   communicator is DEFINED'
  if(IOSERVER_Commisnull(serverio)) write(6,*)'serverio communicator is NULL'
  if(.not. IOSERVER_Commisnull(serverio)) write(6,*)'serverio communicator is DEFINED'
777 continue
  call IOSERVER_time_to_quit()
  call mpi_finalize(status)
end program

subroutine io_relay(model, modelio, allio, nodeio, serverio, nodecom)
  use ISO_C_BINDING
  use memory_arena_mod
  implicit none
  integer, intent(IN) :: model, allio, nodeio, serverio, nodecom, modelio
  include 'io-server/ioserver.inc'
  integer :: rank, size, ierr, noderank
  type(C_PTR) :: p_base, p_relay, p_server, temp
  type(memory_arena) :: ma
  integer(C_INTPTR_T) :: sz_base, sz_relay, sz_server
  integer(C_INT64_T) :: shmsz64

  call MPI_Comm_rank(nodeio, rank, ierr)
  call MPI_Comm_size(nodeio, size, ierr)
  write(6,*)'in pseudo io-relay, PE',rank+1,' of',size

  call IOSERVER_get_winmem(p_base, p_relay, p_server)
  call IOSERVER_get_winsizes(sz_base, sz_relay, sz_server)

  if(IOSERVER_Commisnull(model))    write(6,*)'model    communicator is NULL'
  if(.not. IOSERVER_Commisnull(model))    write(6,*)'model    communicator is DEFINED'
  if(IOSERVER_Commisnull(modelio))  write(6,*)'modelio  communicator is NULL'
  if(.not. IOSERVER_Commisnull(modelio))  write(6,*)'modelio  communicator is DEFINED'
  if(IOSERVER_Commisnull(allio))    write(6,*)'allio    communicator is NULL'
  if(.not. IOSERVER_Commisnull(allio))    write(6,*)'allio    communicator is DEFINED'
  if(IOSERVER_Commisnull(nodeio))   write(6,*)'nodeio   communicator is NULL'
  if(.not. IOSERVER_Commisnull(nodeio))   write(6,*)'nodeio   communicator is DEFINED'
  if(IOSERVER_Commisnull(serverio)) write(6,*)'serverio communicator is NULL'
  if(.not. IOSERVER_Commisnull(serverio)) write(6,*)'serverio communicator is DEFINED'
  call MPI_Comm_rank(nodecom, noderank, ierr)
  if(noderank == 0) then
    shmsz64 = sz_relay - 1024
    temp = ma%create(p_relay, 128, shmsz64)
    temp = ma%newblock(1024*1024, "HEAP000")
    temp = ma%newblock(1024*1024, "CIOB000")
    temp = ma%newblock(1024*1024, "CIOB001")
    call flush(6)
    call ma%dump()
  endif
  write(6,*)'relay before barrier'
  call MPI_Barrier(modelio, ierr)
end subroutine io_relay

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
