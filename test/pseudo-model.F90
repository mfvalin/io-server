program pseudomodel
  use ISO_C_BINDING
  implicit none
  external io_relay
  include 'include/ioserver.inc'
  integer :: status
  integer :: model, allio, nodeio, serverio, nio_node

  call mpi_init(status)
  nio_node = 2   ! 2 relay processes per node
  status = ioserver_init(model, allio, nodeio, serverio, nio_node, 'M', C_FUNLOC(io_relay))
  print *,'in pseudo model'
  if(RPNMPI_Commisnull(model))    print *,'model    communicator is NULL'
  if(RPNMPI_Commisnull(allio))    print *,'allio    communicator is NULL'
  if(RPNMPI_Commisnull(nodeio))   print *,'nodeio   communicator is NULL'
  if(RPNMPI_Commisnull(serverio)) print *,'serverio communicator is NULL'
  call mpi_finalize(status)
end program

subroutine io_relay(i)
  implicit none
  integer, intent(IN) :: i
  print *,'in pseudo io-relay',i
end subroutine io_relay
