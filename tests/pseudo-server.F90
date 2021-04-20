program pseudoserver
  use ISO_C_BINDING
  implicit none
  interface
    function IOserver_Commisnull(comm) result(status)   !  is this communicator the NULL communicator ?
    implicit none
    integer, intent(IN) :: comm
    logical :: status
    end function IOserver_Commisnull
  end interface
  integer :: status
  integer :: model, allio, nodeio, serverio, nio_node

  call mpi_init(status)
  nio_node = -1
  status = ioserver_init(model, allio, nodeio, serverio, nio_node, 'S', C_NULL_FUNPTR)
  print *,'in pseudo IO server'
  if(IOSERVER_Commisnull(model))    print *,'model    communicator is NULL'
  if(IOSERVER_Commisnull(allio))    print *,'allio    communicator is NULL'
  if(IOSERVER_Commisnull(nodeio))   print *,'nodeio   communicator is NULL'
  if(IOSERVER_Commisnull(serverio)) print *,'serverio communicator is NULL'
  call mpi_finalize(status)
end program
