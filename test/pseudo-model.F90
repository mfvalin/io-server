program pseudomodelandserver
  use ISO_C_BINDING
  implicit none
  external io_relay
  include 'io-server/ioserver.inc'
  integer :: status
  integer :: model, allio, nodeio, serverio, nio_node
  integer :: comm, rank, size, nserv, ierr
  character(len=128) :: arg

  call mpi_init(status)

  arg = '3'
  if(COMMAND_ARGUMENT_COUNT() >= 1) call GET_COMMAND_ARGUMENT(1, arg)
  read(arg,*) nserv
  arg = '2'
  if(COMMAND_ARGUMENT_COUNT() >= 2) call GET_COMMAND_ARGUMENT(2, arg)

  call get_local_world(comm, rank, size)
  if(rank >= nserv) then
    read(arg,*) nio_node ! relay processes per node
    status = ioserver_init(model, allio, nodeio, serverio, nio_node, 'M', C_FUNLOC(io_relay))
    call MPI_Comm_rank(model, rank, ierr)
    call MPI_Comm_size(model, size, ierr)
    print *,'in pseudo model, PE',rank+1,' of',size
  else  ! ranks 0, 1, 2 : server
    nio_node = -1
    status = ioserver_init(model, allio, nodeio, serverio, nio_node, 'S', C_NULL_FUNPTR)
    call MPI_Comm_rank(serverio, rank, ierr)
    call MPI_Comm_size(serverio, size, ierr)
    print *,'in pseudo IO server, PE',rank+1,' of',size
  endif
  if(RPNMPI_Commisnull(model))    print *,'model    communicator is NULL'
  if(.not. RPNMPI_Commisnull(model))    print *,'model    communicator is DEFINED'
  if(RPNMPI_Commisnull(allio))    print *,'allio    communicator is NULL'
  if(RPNMPI_Commisnull(nodeio))   print *,'nodeio   communicator is NULL'
  if(RPNMPI_Commisnull(serverio)) print *,'serverio communicator is NULL'
  if(.not. RPNMPI_Commisnull(serverio)) print *,'serverio communicator is DEFINED'
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
  if(RPNMPI_Commisnull(model))    print *,'model    communicator is NULL'
  if(RPNMPI_Commisnull(allio))    print *,'allio    communicator is NULL'
  if(RPNMPI_Commisnull(nodeio))   print *,'nodeio   communicator is NULL'
  if(.not. RPNMPI_Commisnull(nodeio))   print *,'nodeio   communicator is DEFINED'
  if(RPNMPI_Commisnull(serverio)) print *,'serverio communicator is NULL'
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
