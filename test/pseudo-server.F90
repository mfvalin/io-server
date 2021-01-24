program pseudoserver
  use ISO_C_BINDING
  implicit none
  include 'include/ioserver.inc'
  integer :: status
  integer :: model, allio, nodeio, serverio, nio_node

  call mpi_init(status)
  nio_node = -1
  status = ioserver_init(model, allio, nodeio, serverio, nio_node, 'S', C_NULL_FUNPTR)
  print *,'in pseudo IO server'
  if(RPNMPI_Commisnull(model))    print *,'model    communicator is NULL'
  if(RPNMPI_Commisnull(allio))    print *,'allio    communicator is NULL'
  if(RPNMPI_Commisnull(nodeio))   print *,'nodeio   communicator is NULL'
  if(RPNMPI_Commisnull(serverio)) print *,'serverio communicator is NULL'
  call mpi_finalize(status)
end program
