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
  external io_relay_fn, io_server_out, compute_fn
  integer :: status
  integer :: comm, rank, size, nserv, noops, me, nio_node
  integer :: noderank, nodesize
  logical :: error
  character(len=128) :: arg
  type(memory_arena) :: ma
  type(comm_rank_size) :: server_crs, fullnode_crs, model_crs

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
    !  mettre ce qui suit dans un sous-programme pseudo-modele
    call compute_fn()
    model_crs = IOserver_get_crs(MODEL_COLOR)
    noderank = model_crs % rank
    nodesize = model_crs % size

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
      status = ioserver_init(nio_node, 'O')   ! this function should not return as set_IOserver_server is used
      ! io_server_out may or may not return from call
      call io_server_out()
      server_crs   = IOserver_get_crs(SERVER_COLOR)
      noderank = server_crs % rank
      nodesize = server_crs % size
    endif

  endif
777 continue
  fullnode_crs = IOserver_get_crs(NODE_COLOR)
  call ioserver_set_time_to_quit()
  write(6,*)'FINAL:      node PE',noderank+1,' of',nodesize
  write(6,*)'FINAL: full node PE',fullnode_crs % rank+1,' of',fullnode_crs % size
  call mpi_finalize(status)
end program

! subroutine verify_translations_old()  ! old version, migrated to ioserver_init.F90
!   use ioserver_functions
!   implicit none
!   type(shared_memory), pointer :: mem
!   type(C_PTR) :: p_base, p_relay, temp
!   integer(C_INTPTR_T), dimension(0:1024) :: iora1, iora2
!   integer(C_INTPTR_T) :: iora0
!   type(comm_rank_size) :: fullnode_crs
!   integer :: i, errors
!   
!   fullnode_crs = IOserver_get_crs(NODE_COLOR)   ! smp node information
!   p_relay = IOserver_get_win_ptr(IO_RELAY)      ! memory arena address
!   iora0 = transfer(p_relay, iora0)
!   p_base  = IOserver_get_win_ptr(IO_CONTROL)    ! control memory adddress
!   call C_F_POINTER(p_base, mem)                 ! Fortran control memory description
! 
!   do i = 0, fullnode_crs % size -1              ! p_relay for all PEs for which it makes sense
!     iora1(i) = transfer(mem % pe(i) % io_ra , iora1(i))
!   enddo
! !   write(6,'(A,/(5Z18.16))') 'IO-RA :', iora1(0:fullnode_crs % size -1)
!   do i = 0, fullnode_crs % size -1              ! translate local address into other PE address
!     iora2(i) = transfer(ptr_translate_to(p_relay, NODE_COLOR, i), iora2(i))
!   enddo
! !   write(6,'(A,/(5Z18.16))') '      :', iora2(0:fullnode_crs % size -1)
!   errors = 0
!   do i = 0, fullnode_crs % size -1
!     if(iora1(i) .ne. iora2(i) ) then
!       errors = errors + 1
!       write(6,'(A,/(5Z18.16))') 'ERROR: bad address translation, expected , found',iora1(i), iora2(i)
!     endif
!   enddo
!   if(errors == 0) then
!     write(6,*) 'INFO: address translations_to are coherent'
!   else
!     write(6,*) 'INFO: number of errors in address translations_to =',errors
!   endif
!   do i = 0, fullnode_crs % size -1     ! translate other PE adddress into local address
!     iora2(i) = transfer(ptr_translate_from(mem % pe(i) % io_ra, NODE_COLOR, i), iora2(i))
!   enddo
!   errors = 0
!   do i = 0, fullnode_crs % size -1
!     if(iora0 .ne. iora2(i) ) then
!       errors = errors + 1
!       write(6,'(A,/(5Z18.16))') 'ERROR: bad address translation, expected , found',iora0, iora2(i)
!     endif
!   enddo
!   if(errors == 0) then
!     write(6,*) 'INFO: address translations_from are coherent'
!   else
!     write(6,*) 'INFO: number of errors in address translations_from =',errors
!   endif
! 
! end subroutine verify_translations_old
! =============================================================================================
!                      COMPUTE     ( skeleton demo code, interim API )
! =============================================================================================
subroutine compute_fn()
  use ISO_C_BINDING
  use helpers
  use ioserver_functions
  use memory_arena_mod
  implicit none
  integer :: model, allio, relay, server, nio_node, modelio, nodecom, me
  integer :: comm, rank, size, ierr, noderank, nodesize, navail
  type(memory_arena) :: ma
  type(comm_rank_size) :: local_crs, model_crs, relay_crs
  type(comm_rank_size) :: modelio_crs, allio_crs, server_crs, nodecom_crs
  type(C_PTR) :: p_relay, temp
  type(circular_buffer) :: cio_in
  integer, dimension(1) :: tag

  call verify_translations()

  model_crs    = IOserver_get_crs(MODEL_COLOR)
  modelio_crs  = IOserver_get_crs(MODEL_COLOR + RELAY_COLOR)
  allio_crs    = IOserver_get_crs(RELAY_COLOR + SERVER_COLOR)
  relay_crs    = IOserver_get_crs(RELAY_COLOR)
  server_crs   = IOserver_get_crs(SERVER_COLOR)
  nodecom_crs  = IOserver_get_crs(MODEL_COLOR + RELAY_COLOR + NODE_COLOR)

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

  p_relay = IOserver_get_win_ptr(IO_RELAY)
  temp = ma%clone(p_relay)         ! get memory arena address

!   call MPI_Barrier(modelio, ierr)                      ! barrier 1 compute/relay

  call ioserver_start()                                ! inbound/outbound traffic priming

  ! compute -> relay traffic

  ! relay -> compute traffic

!   call MPI_Barrier(modelio, ierr)                      ! barrier 2 compute/relay
  call flush(6)
  call ma%dump()
  call print_io_colors()
  write(6,*)'END: compute, PE',rank+1,' of',size

end subroutine compute_fn
! =============================================================================================
!                      RELAY     ( skeleton code, interim API )
! =============================================================================================
subroutine io_relay_fn()
  use ISO_C_BINDING
  use helpers
  use io_relay_mod
  use ioserver_memory_mod
  implicit none
  integer :: model, allio, relay, server, nodecom, modelio, old, new
  integer :: rank, size, ierr, noderank, nodesize, ncompute, i, bsize, bflags, nleft, n, navail
  integer, dimension(1) :: tag
  type(C_PTR) :: temp, arena
  integer(C_INTPTR_T) :: itemp, iarena
  character(len=8) :: cio_name
  logical :: ok

  call io_relay_mod_init()
relay_debug = .true.
  if(relay_debug) then   ! mem % pe() % io_ra
    call verify_translations()
  endif

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

!   call MPI_Barrier(modelio, ierr)        ! barrier 1 compute/relay

  ncompute = modelio_crs % size - relay_crs % size
  arena = ma % addr()
  write(6,*) 'INFO: number of compute processes found on node =',ncompute

  if(rank == 0) then              ! outbound relay PR
    ! compute -> relay traffic
    ! get compute -> relay cio addresses, get and check priming tag
    allocate(c_cio_out(0:ncompute-1))
    do i = 0, ncompute ! now locate the circular buffers
      write(cio_name,'(A4,I4.4)') "MCIO",i + 1000     ! target outbound buffer
      temp = ma % getblock(bsize, bflags, cio_name)   ! address of outbound circular buffer of compute PE i
      if( C_ASSOCIATED(temp) ) then
        itemp = transfer(temp, itemp)
  !       iarena = ptr_diff(arena, temp)   ! find offset in arena
        iarena = Pointer_offset(arena, temp, 1)  ! find offset in arena (in bytes)
        write(6,1) ' NOTE: block '//cio_name//' found, size =', bsize, ', flags =', bflags,' address =', itemp,', arena offset =', iarena
        ok = c_cio_out(i) % create(temp)

        if(ok) then
          tag = -1
          n     = c_cio_out(i) % atomic_get(tag, 1, .true.)   ! get priming tag
          nleft = c_cio_out(i) % get_num_elements()
          write(6,2) 'INFO: compute oubound buffer PE, size, free, avail, tag, expected',i,c_cio_out(i) % get_capacity(), &
                      c_cio_out(i) % get_num_spaces(),nleft,tag,10000+i
        else
          write(6,*) 'ERROR: failed to connect to outbound buffer of compute PE',i
        endif
      else
        write(6,1) ' ERROR: block '//cio_name//' NOT FOUND'
      endif
    enddo

    ! will need to add priming for relay -> server traffic
    ! will send my rank in server+relay communicator

    ! actual outgoing relay code goes here

  else              ! inbound relay PR

    ! relay -> compute traffic
    ! get relay -> compute cio addresses, put priming tags
    allocate(c_cio_in(0:ncompute-1))
    do i = 0, ncompute ! now locate the circular buffers
      write(cio_name,'(A4,I4.4)') "MCIO",i + 0000     ! target inbound buffer
      temp = ma % getblock(bsize, bflags, cio_name)   ! address of outbound circular buffer of compute PE i
      if( C_ASSOCIATED(temp) ) then
        itemp = transfer(temp, itemp)
        iarena = Pointer_offset(arena, temp, 1)  ! find offset in arena (in bytes)
        write(6,1) ' NOTE: block '//cio_name//' found, size =', bsize, ', flags =', bflags,' address =', itemp,', arena offset =', iarena
        ok = c_cio_in(i) % create(temp)

        if(ok) then
          tag = i + 20000
          ! prime the pump by injecting tag
          n  = c_cio_in(i) % atomic_put(tag, 1, .true.)   ! put priming tag
          navail = c_cio_in(i) % get_num_elements()
          if(ok) write(6,2) 'INFO: compute inbound buffer PE, size, free, avail, tag',i,c_cio_in(i) % get_capacity(), &
                            c_cio_in(i) % get_num_spaces(),navail, tag
        else
          write(6,*) 'ERROR: failed to connect to inbound buffer of compute PE',i
        endif
      else
        write(6,1) ' ERROR: block '//cio_name//' NOT FOUND'
      endif
    enddo

    ! will need to check that server -> relay traffic is primed
    ! will receive my rank in server+relay communicator

    ! actual incoming relay code goes here

  endif

!   call MPI_Barrier(modelio, ierr)        ! barrier 2 compute/relay

  if(relay_debug) then
    call flush(6)
    call ma%dump()
  endif
  write(6,*)'END: pseudo relay PE',relay_crs % rank+1,' of',relay_crs % size
  call IOserver_set_time_to_quit()              ! activate quit signal for NO-OP PEs
  write(6,'(A,(15I5))')' DEBUG: colors =',mem % pe(0:max_smp_pe) % color
  write(6,*)'FINAL: full node PE',fullnode_crs % rank+1,' of',fullnode_crs % size
  call MPI_Finalize(ierr)                   ! DO NOT return to caller, call finalize, then stop
  stop
1 format(A,I10,A,Z10.8,A,Z18.16,A,I10)
2 format(1X,A,10I8)
end subroutine io_relay_fn

! =============================================================================================
!                      SERVER  ( skeleton code, interim API )
! =============================================================================================

subroutine io_server_out()
  use ISO_C_BINDING
  use helpers
  use io_server_mod
  use ioserver_memory_mod
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
  write(6,'(A,(15I5))')' DEBUG: colors =',mem % pe(0:max_smp_pe) % color
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
