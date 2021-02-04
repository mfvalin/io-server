program pseudomodelandserver
  use ISO_C_BINDING
  implicit none
  external io_relay
  include 'io-server/ioserver.inc'
  integer :: status
  integer :: model, allio, nodeio, serverio, nio_node
  integer :: comm, rank, size, nserv, ierr, noops
  logical :: error
  character(len=128) :: arg

  call mpi_init(status)

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
  error = ioserver_winsizes(2*MBYTE, GBYTE/2, GBYTE/2)
  if(error) then
    print *,'ERROR: bad window sizes'
    goto 777
  endif

  if(rank >= nserv) then
    read(arg,*) nio_node ! relay processes per node
    status = ioserver_init(model, allio, nodeio, serverio, nio_node, 'M', C_FUNLOC(io_relay))
    call MPI_Comm_rank(model, rank, ierr)
    call MPI_Comm_size(model, size, ierr)
    print *,'in pseudo model, PE',rank+1,' of',size
  else  ! ranks 0, 1, nserv-1 : server
    nio_node = -1
    if(rank < noops) then ! ranks below noops are NO-OP processes
      status = ioserver_init(model, allio, nodeio, serverio, nio_node, 'Z', C_NULL_FUNPTR)
    else
      status = ioserver_init(model, allio, nodeio, serverio, nio_node, 'S', C_NULL_FUNPTR)
    endif
    call MPI_Comm_rank(serverio, rank, ierr)
    call MPI_Comm_size(serverio, size, ierr)
    print *,'in pseudo IO server, PE',rank+1,' of',size
  endif
  if(IOSERVER_Commisnull(model))    print *,'model    communicator is NULL'
  if(.not. IOSERVER_Commisnull(model))    print *,'model    communicator is DEFINED'
  if(IOSERVER_Commisnull(allio))    print *,'allio    communicator is NULL'
  if(IOSERVER_Commisnull(nodeio))   print *,'nodeio   communicator is NULL'
  if(IOSERVER_Commisnull(serverio)) print *,'serverio communicator is NULL'
  if(.not. IOSERVER_Commisnull(serverio)) print *,'serverio communicator is DEFINED'
777 continue
  call IOSERVER_time_to_quit()
  call mpi_finalize(status)
end program

subroutine io_relay(model, allio, nodeio, serverio)
  use ISO_C_BINDING
  implicit none
  integer, intent(IN) :: model, allio, nodeio, serverio
  include 'io-server/ioserver.inc'
  integer :: rank, size, ierr
  call MPI_Comm_rank(nodeio, rank, ierr)
  call MPI_Comm_size(nodeio, size, ierr)
  print *,'in pseudo io-relay, PE',rank+1,' of',size
  if(IOSERVER_Commisnull(model))    print *,'model    communicator is NULL'
  if(IOSERVER_Commisnull(allio))    print *,'allio    communicator is NULL'
  if(IOSERVER_Commisnull(nodeio))   print *,'nodeio   communicator is NULL'
  if(.not. IOSERVER_Commisnull(nodeio))   print *,'nodeio   communicator is DEFINED'
  if(IOSERVER_Commisnull(serverio)) print *,'serverio communicator is NULL'
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
