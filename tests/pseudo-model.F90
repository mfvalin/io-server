module helpers
  use ISO_C_BINDING
  use ioserver_functions

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

subroutine print_comms(model, modelio, allio, relay, server, nodecom)
  implicit none
  integer, intent(IN) :: model, allio, relay, server, modelio, nodecom

  call print_comm(IOSERVER_Commisnull(model),    'model')
  call print_comm(IOSERVER_Commisnull(modelio),  'modelio')
  call print_comm(IOSERVER_Commisnull(allio),    'allio')
  call print_comm(IOSERVER_Commisnull(relay),    'relay')
  call print_comm(IOSERVER_Commisnull(server),   'server')
  call print_comm(IOSERVER_Commisnull(nodecom),  'nodecom')
end subroutine print_comms

subroutine check_comm(comm, crs, name)
  implicit none
  include 'mpif.h'
  integer, intent(IN) :: comm
  type(comm_rank_size), intent(IN) :: crs
  character(len=*), intent(IN) :: name
  character(len=16) :: extra
  extra = ' (defined)'
  if(crs % comm == MPI_COMM_NULL) extra = ' (null)'
  if(comm == crs % comm) then
    print *,'INFO : '//trim(name)//' .eq. '//trim(name)//'_crs % comm'
  else
    print *,'ERROR: '//trim(name)//' .ne. crs % comm'//trim(extra)
  endif
end subroutine check_comm

end module helpers

program pseudomodelandserver
  use ISO_C_BINDING
  use helpers
  use ioserver_functions
  use memory_arena_mod
  implicit none
  external io_relay_fn
  integer :: status
  integer :: model, allio, relay, server, nio_node, modelio, nodecom, me
  integer :: comm, rank, size, nserv, ierr, noops, noderank, nodesize
  logical :: error
  character(len=128) :: arg
  type(C_PTR) :: p_base, p_relay, p_server, temp
  integer(C_INTPTR_T) :: sz_base, sz_relay, sz_server
  integer :: sz32
  type(memory_arena) :: ma
  character(len=8) :: blockname
  type(comm_rank_size) :: local_crs, model_crs, relay_crs, fullnode_crs
  type(comm_rank_size) :: modelio_crs, allio_crs, server_crs, nodecom_crs

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
  me = ma % setid(rank)
  error = ioserver_set_winsizes(2*MBYTE, GBYTE/4, GBYTE/2)   !  base, relay, server
  if(error) then
    write(6,*)'ERROR: bad window sizes'
    goto 777
  endif

  if(rank >= nserv) then
    ! =============================================================================================
    !                                 compute or IO relay Processes
    ! =============================================================================================
    read(arg,*) nio_node                    ! number of relay processes per node
    call set_IOSERVER_relay(io_relay_fn)
    !  no return from ioserver_int_init in the case of IO relay processes when io_relay_fn is defined
    !  compute processes will return from call
    status = ioserver_init(nio_node, 'M')
    ! =============================================================================================
    !                                 compute Processes
    ! =============================================================================================
    !  from this point on, this is a model compute process

    model_crs    = IOserver_get_crs(MODEL_COLOR)
    modelio_crs  = IOserver_get_crs(MODEL_COLOR + RELAY_COLOR)
    allio_crs    = IOserver_get_crs(RELAY_COLOR + SERVER_COLOR)
    relay_crs    = IOserver_get_crs(RELAY_COLOR)
    server_crs   = IOserver_get_crs(SERVER_COLOR)
    nodecom_crs  = IOserver_get_crs(MODEL_COLOR + RELAY_COLOR + NODE_COLOR)
    fullnode_crs = IOserver_get_crs(NODE_COLOR)

    model   = model_crs % comm
    modelio = modelio_crs % comm
    allio   = allio_crs % comm
    relay   = relay_crs % comm
    server  = server_crs % comm
    nodecom = nodecom_crs % comm
    call print_comms(model, modelio, allio, relay, server, nodecom)

    rank = model_crs % rank
    size = model_crs % size
    noderank = nodecom_crs % rank
    nodesize = nodecom_crs % size
    write(6,*)'START: compute PE',rank+1,' of',size
    write(6,*)' model+io node PE',noderank+1,' of', nodesize

    call IOSERVER_get_winmem(p_base, p_relay, p_server)
    call IOSERVER_get_winsizes(sz_base, sz_relay, sz_server)  ! of interest is sz_relay

    temp = ma%clone(p_relay)

    call MPI_Barrier(modelio, ierr)                      ! barrier 1 compute/relay
    ! compute -> relay traffic
    call MPI_Barrier(modelio, ierr)                      ! barrier 2 compute/relay
    call flush(6)
    call ma%dump()
    write(6,*)'END: compute, PE',rank+1,' of',size

  else            ! ranks 0, 1,..., nserv-1 : 
    ! =============================================================================================
    !                                server and no-op processes
    ! =============================================================================================
    nio_node = -1
    if(rank < noops) then          ! ranks below noops are NO-OP processes
      ! =============================================================================================
      !                                no-op processes
      ! =============================================================================================
      ! no return from ioserver_int_init in the case of NO-OP processes
      status = ioserver_init(nio_node, 'Z')
    else
      ! =============================================================================================
      !                                server processes (usually on another node)
      ! =============================================================================================
      call set_IOserver_server(io_server_out)
      status = ioserver_init(nio_node, 'O')   ! this function should not return as set_IOserver_server is set
      ! io_server_out may or may not return from call
      call io_server_out()
      noderank = server_crs % rank
      nodesize = server_crs % size
    endif

  endif
777 continue
  call ioserver_set_time_to_quit()
  write(6,*)'FINAL:      node PE',noderank+1,' of',nodesize
  write(6,*)'FINAL: full node PE',fullnode_crs % rank+1,' of',fullnode_crs % size
  call mpi_finalize(status)
end program

! =============================================================================================
!                      RELAY     ( skeleton code, interim API )
! =============================================================================================
subroutine io_relay_fn()
  use ISO_C_BINDING
  use helpers
  use io_relay_mod
  implicit none
  integer :: model, allio, relay, server, nodecom, modelio, old, new
  integer :: rank, size, ierr, noderank, nodesize
  type(C_PTR) :: temp

  call io_relay_mod_init()

  model    = model_crs % comm
  modelio  = modelio_crs % comm
  allio    = allio_crs % comm
  relay    = relay_crs % comm
  server   = server_crs % comm
  nodecom  = nodecom_crs % comm
  
  rank = relay_crs % rank
  new = 0
  if(rank == 0) new = 1        ! debug mode on rank 0 in 
  old = io_relay_debug(new)
  size = relay_crs % size
  write(6,*)'in pseudo io-relay, PE',rank+1,' of',size,' debug mode (old,new) =',old, new
  if(relay_debug) write(6,*)'      model+io node PE',nodecom_crs % rank + 1, ' of', nodecom_crs % size
  if(relay_debug) write(6,*)'          full node PE',fullnode_crs % rank + 1,' of', fullnode_crs % size
  if(relay_debug) call print_comms(model, modelio, allio, relay, server, nodecom)

  call MPI_Barrier(modelio, ierr)        ! barrier 1 compute/relay
  ! compute -> relay traffic

  ! actual relay code goes here

  call MPI_Barrier(modelio, ierr)        ! barrier 2 compute/relay

  if(relay_debug) then
    call flush(6)
    call ma%dump()
  endif
  write(6,*)'END: pseudo relay PE',relay_crs % rank+1,' of',relay_crs % size
  call IOserver_set_time_to_quit()              ! activate quit signal for NO-OP PEs
  write(6,*)'FINAL: full node PE',fullnode_crs % rank+1,' of',fullnode_crs % size
  call MPI_Finalize(ierr)                   ! DO NOT return to caller, call finalize, then stop
  stop
end subroutine io_relay_fn

! =============================================================================================
!                      SERVER  ( skeleton code, interim API )
! =============================================================================================

subroutine io_server_out()
  use ISO_C_BINDING
  use helpers
  use io_server_mod
  implicit none
  integer :: model, allio, relay, server, modelio, nodecom

  integer :: rank, size, ierr, noderank, nodesize

  call io_server_mod_init()

  model    = model_crs % comm
  modelio  = modelio_crs % comm
  allio    = allio_crs % comm
  relay    = relay_crs % comm
  server   = server_crs % comm
  nodecom  = nodecom_crs % comm

  write(6,*)'in pseudo IO server, PE',server_crs % rank + 1  ,' of', server_crs % size
  write(6,*)'         server node PE',nodecom_crs % rank + 1 ,' of', nodecom_crs % size
  write(6,*)'           full node PE',fullnode_crs % rank + 1,' of', fullnode_crs % size
  call print_comms(model, modelio, allio, relay, server, nodecom)

  write(6,*)'END: pseudo IO server, PE',server_crs % rank + 1  ,' of', server_crs % size
  call IOserver_set_time_to_quit()              ! activate quit signal for NO-OP PEs
  write(6,*)'FINAL:       full node PE',fullnode_crs % rank + 1,' of', fullnode_crs % size

  call mpi_finalize(ierr)
  stop
end subroutine io_server_out


! extra code , to avoid including mpif.h when not wanted

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
