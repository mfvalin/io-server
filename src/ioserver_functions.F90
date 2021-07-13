!  functions for C and FORTRAN programming
!  Copyright (C) 2021  Recherche en Prevision Numerique
! 
!  This software is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation,
!  version 2.1 of the License.
! 
!  This software is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
module ioserver_functions
  use ISO_C_BINDING
  use shmem_heap
  use circular_buffer_module, only : circular_buffer
  use ioserver_constants
  implicit none
#if ! defined(VERSION)
#define VERSION 10000
#endif
  include 'io-server/ioserver.inc'
  private :: ioserver_int_init

  save

  public :: cmeta, MAXPACK
  integer, parameter :: MAXPACK = 16
  type :: cmeta                        ! information passed to compression software
    real(kind=8)    :: errabs_r = 0.0
    integer(kind=8) :: errabs_i = 0
    real(kind=4)    :: errrel_r = 0.0
    integer         :: errrel_i = 0
    integer         :: nbits    = 0
    integer, dimension(MAXPACK) :: pack_info
  end type

  public :: server_file
  type :: server_file
    private
    integer    :: version = VERSION    ! version marker, used to check version coherence
    integer    :: fd = -1              ! file number for internal use
    character(len=1), dimension(:), pointer :: name => NULL()
    type(heap) :: h
    logical    :: debug = .false.      ! debug mode at the file level

    contains

    procedure   :: open      => ioserver_open
    procedure   :: read      => ioserver_read
    procedure   :: write     => ioserver_write
    procedure   :: close     => ioserver_close
    procedure   :: is_open   => file_is_open
    procedure   :: is_valid  => file_version_is_valid
    procedure   :: set_debug => set_file_debug

  end type

  type, public :: grid
    integer :: id            ! grid id
    integer :: size_i, size_j      ! horizontal dimensions of full grid
  end type

  type, public :: subgrid
    integer :: i0            ! starting point of this subgrid in the x direction
    integer :: ni            ! number of points of this subgrid in the x direction
    integer :: j0            ! starting point of this subgrid in the y direction
    integer :: nj            ! number of points of this subgrid in the y direction
    integer :: nk            ! number of vertical levels
    integer :: nv            ! number of variables
  end type

  type, private :: data_header        ! record : data_header , metadata(nm integers) , subgrid(ni * nj * nk * nv elements)
    integer :: nw            ! number of elements in record
    integer :: nbits         ! number of bits per subgrid element in record
    integer :: tag           ! unique sequence tag
    integer :: weight        ! number of pieces in parcel (normally 1)
    integer :: np            ! number of pieces to reassemble
    integer :: grid          ! grid id
    type(subgrid) :: s       ! subgrid description
    integer :: nm            ! length of metadata "jar" (32 bit units)
  end type

  ! Type used as a header when writing data (from a model process)
  type, public :: model_record
    integer(C_INT) :: record_length    ! global record length = size of model_record + csize + msize
    integer(C_INT) :: tag, stream
    integer(C_INT) :: ni, nj
    integer(C_INT) :: grid_size_i, grid_size_j, gnignj
    integer(C_INT) :: output_grid
    integer(C_INT) :: i0, j0
    integer(C_INT) :: nk, nvar
    integer(C_INT) :: type_kind_rank
    type(C_PTR)    :: data
    integer(C_INT) :: csize, msize
  end type

  private  :: file_tag_seq, file_open_seq, local_heap, initialized    
  private  :: cio_in, cio_out
!   private gdt, MAXGRIDS

  logical                :: initialized = .false.
  integer                :: file_tag_seq = 0
  integer                :: file_open_seq = 0
  type(heap)             :: local_heap   ! type is self initializing
  type(circular_buffer)  :: cio_in       ! type is self initializing
  type(circular_buffer)  :: cio_out      ! type is self initializing
  integer, parameter     :: ioserver_version = VERSION
  logical                :: debug = .false.

!   private :: model, modelio, allio, nodeio, serverio, nodecom
!   integer :: model        ! communicator for model compute PEs         (may be MPI_COMM_NULL)
!   integer :: modelio      ! communicator for compute and relay PEs     (may be MPI_COMM_NULL)
!   integer :: allio        ! communicator for relay and server IO PEs   (may be MPI_COMM_NULL)
!   integer :: nodeio       ! communicator for relay PEs on model nodes  (may be MPI_COMM_NULL)
!   integer :: serverio     ! communicator for io server PEs             (may be MPI_COMM_NULL)
!   integer :: nodecom      ! communicator for io server PEs on a node   (may be MPI_COMM_NULL)

!   integer, parameter :: MAXGRIDS = 1024
!   type(grid), dimension(MAXGRIDS) :: gdt   ! grid description table

  private :: bump_ioserver_tag

 contains
  
  function set_io_debug(dbg) result(status)
    implicit none
    logical, intent(IN) :: dbg
    logical :: status

    status = debug
    debug = dbg

  end function set_io_debug

  function set_file_debug(this, dbg) result(status)
    implicit none
    class(server_file), intent(INOUT) :: this
    logical, intent(IN) :: dbg
    logical :: status

    status = this % debug
    this % debug = dbg

  end function set_file_debug

  function file_version_is_valid(this) result(status)
    implicit none
    class(server_file), intent(IN) :: this
    logical :: status

    status = this % version == VERSION

  end function file_version_is_valid

  function file_is_open(this) result(status)
    implicit none
    class(server_file), intent(IN) :: this
    logical :: status

    status = file_version_is_valid(this)
    status = status .and. (this % fd > 0) 
    status = status .and. associated(this % name)

  end function file_is_open

  subroutine bump_ioserver_tag(this, new_file)
    implicit none
    class(server_file), intent(IN) :: this
    logical, intent(in), optional :: new_file
    integer :: ierr
    type(comm_rank_size), save :: model_crs !  = COMM_RANK_SIZE_NULL  (initialization is redundant)

    if(debug .or. this % debug) then
      if(model_crs % size == 0) model_crs = IOserver_get_crs(MODEL_COLOR)
      call MPI_Barrier(model_crs % comm, ierr)    ! in debug mode, enforce collective mode
    endif
    file_tag_seq = file_tag_seq + 1

    if (present(new_file)) then
      if (new_file) file_open_seq = file_open_seq + 1
    end if
  end subroutine bump_ioserver_tag

  function ioserver_heap(n) result(h)
    implicit none
    integer, intent(IN) :: n
    type(heap) :: h
    type(heap) :: nh
    if(n == 0) then
      if(.not. C_ASSOCIATED(local_heap % ptr()) ) then
        print *,'initializing io relay heap'
        local_heap = IOserver_get_heap()
      endif
      h = local_heap
    else
      h = nh                  ! null heap
    endif
  end function ioserver_heap

  subroutine ioserver_start()
    implicit none
    integer :: navail, nfree
    type(comm_rank_size), save :: model_crs !  = COMM_RANK_SIZE_NULL  (initialization is redundant)
    integer, dimension(1) :: tag

    if(model_crs % size == 0) model_crs = IOserver_get_crs(MODEL_COLOR)

    ! prime outbound cio
    tag = model_crs%rank + 10000
    nfree = cio_out % atomic_put( tag, 1, .true.)
    write(6,*) 'INFO: token written into outbound buffer =',tag

    ! check that inboind cio is primed
    tag = -1
    navail = cio_in%atomic_get(tag, 1, .true.)
    write(6,2) 'INFO: inbound buffer PE, size, free, avail, tag, expected', &
               model_crs%rank, cio_in%get_capacity(), cio_in%get_num_spaces(), &
               navail, tag, model_crs%rank+20000

2   format(1X,A,10I8)
  end subroutine ioserver_start

  function ioserver_init(nio_node, app_class) result(status)
    implicit none
    integer, intent(IN)  :: nio_node     ! number of relay processes per compute node
    character(len=*), intent(IN) :: app_class
    integer :: status
    type(comm_rank_size)  :: crs 

!     status = ioserver_int_init(model, modelio, allio, nodeio, serverio, nodecom, nio_node, app_class)
    status = ioserver_int_init(nio_node, app_class)
    if(.not. initialized) then
      local_heap  = IOserver_get_heap()
      cio_in      = IOserver_get_cio_in()
      cio_out     = IOserver_get_cio_out()
      initialized = .true.
      print *,'initializing io heap and circular buffers'
    endif
    crs = IOserver_get_crs(MODEL_COLOR)
!     if(model .ne. crs % comm) print *,'ERROR : model .ne. crs % comm'
!     if(model .eq. crs % comm) print *,'INFO : model .eq. crs % comm'
  end function ioserver_init

  function ioserver_terminate() result(status)
    integer :: status
    status = ioserver_int_finalize()
  end function ioserver_terminate

  function ioserver_open(this, name) result(status)
    implicit none
    class(server_file), intent(INOUT) :: this
    character(len=*), intent(IN) :: name
    integer :: status
    integer :: lname
    type(C_PTR) :: p

    status = -1
    if(.not. file_version_is_valid(this) ) return ! wrong version
    if(file_is_open(this) )        return ! already open

    call bump_ioserver_tag(this, .true.)
    lname = len(trim(name))
    allocate(this % name(lname))
    this % name(1:lname) = transfer(trim(name), this % name)
    print *,"'",this % name(1:lname),"'"

    if(.not. C_ASSOCIATED(local_heap % ptr()) ) then
      print *,'locating local heap'
    endif
    this % h = local_heap                 ! associate heap to file
    p = this % h % ptr()
    if(C_ASSOCIATED(p)) then 
      print *,' p is defined'
    else
      print *,' p is NULL'
    endif

    status = 0
  end function ioserver_open

  function ioserver_close(this) result(status)
    implicit none
    class(server_file), intent(INOUT) :: this
    integer :: status

    status = -1
    if(this % fd <= 0)                return    ! not open
    if(.not. associated(this % name)) return    ! not open

    call bump_ioserver_tag(this)
    this % fd = -1
    deallocate(this % name)
    status = 0
  end function ioserver_close

  ! cprs and meta only need to be supplied by one of the writing PEs
  function ioserver_write(this, mydata, area, grid_in, grid_out, cprs, meta) result(status)
    use data_serialize
    implicit none
    class(server_file), intent(INOUT) :: this
    type(block_meta),   intent(IN)    :: mydata           ! array descriptor from h % allocate
    type(subgrid),      intent(IN)    :: area             ! area in global space
    type(grid),         intent(IN)    :: grid_in          ! input grid
    type(grid),         intent(IN)    :: grid_out         ! output grid

    type(cmeta), intent(IN), optional :: cprs             ! compression related metadata (carried serialized)
    type(jar),   intent(IN), optional :: meta             ! metadata associated with data (carried serialized and blindly)

    integer :: status, n
    type(model_record) :: rec
    integer(C_INT), dimension(:), pointer :: metadata
    integer(C_INT) :: low, high
    type(C_PTR) :: p
    integer(C_INT), dimension(MAX_ARRAY_RANK) :: d
    integer(C_INT) :: tkr
    integer(C_SIZE_T) :: o
    logical :: dimok

    status = -1
    if(this % fd <= 0) return

    ! transmission layout for circular buffer (compute PE -> relay PE) :
    ! length                 (32 bits)
    ! tag number             (32 bits)
    ! file (stream) number   (32 bits)
    ! grid segment dimensions(2 x32 bits)
    ! input grid size (i,j)  (2 x 32 bits)
    ! global grid size       (32 bits)
    ! grid_out (code)        (32 bits)
    ! area lower left corner (2 x 32 bits)
    ! nk, nvar               (2 x 32 bits)
    ! array type/kind/rank   (32 bits)
    ! data pointer           (64 bits)     (will be translated to its own memory space by relay PE)
    ! cprs size , may be 0   (32 bits)
    ! meta size, may be 0    (32 bits)
    ! cprs                   (cprs size x 32 bits)
    ! meta                   (meta size x 32 bits)
    ! length                 (32 bits)
!
    call block_meta_internals(mydata, p, d, tkr, o)        ! grep block_meta private contents
    ! check that dimensions in area are consistent with metadata
    dimok = d(1) == area % ni .and. d(2) == area % nj .and. d(3) == area % nk .and. d(4) == area % nv

    if(.not. dimok) return

    call bump_ioserver_tag(this)
    rec % csize = 0
    rec % msize = 0
    if(present(cprs)) then
      rec % csize = storage_size(cprs) / storage_size(n)
    endif
    if(present(meta)) then
      low = meta % low()
      high = meta % high()
      rec % msize =  high - low       ! useful number of elements
      metadata    => meta % array()
    endif
    rec % record_length = storage_size(rec) / storage_size(n)
    rec % record_length = rec % record_length + rec % csize
    rec % record_length = rec % record_length + rec % msize
    rec % tag           = file_tag_seq
    rec % stream        = this % fd
    rec % ni            = area % ni
    rec % nj            = area % nj
    rec % grid_size_i   = grid_in % size_i
    rec % grid_size_j   = grid_in % size_j
    rec % gnignj        = grid_in % size_i * grid_in % size_j
    rec % output_grid   = grid_out % id
    rec % i0            = area % i0
    rec % j0            = area % j0
    rec % nk            = area % nk
    rec % nvar          = area % nv
    rec % type_kind_rank = tkr
    rec % data          = p
!
    n = cio_out % atomic_put( rec, storage_size(rec) / storage_size(n), .false.)
    if(present(cprs)) n = cio_out % atomic_put( cprs, rec % csize, .false.)
    ! only send useful part of metadata jar (if supplied)
    if(present(meta)) n = cio_out % atomic_put( metadata(low + 1 : high), rec % msize, .false.)
    n = cio_out % atomic_put( rec % record_length, 1, .true.)

    if (n >= 0) status = 0
  end function ioserver_write

  function ioserver_read(this) result(status)
    implicit none
    class(server_file), intent(INOUT) :: this
    integer :: status

    status = -1
    if(this % fd <= 0) return
    call bump_ioserver_tag(this)

    status = 0
  end function ioserver_read

end module ioserver_functions

subroutine ioserver_functions_demo
  use ioserver_functions
  implicit none
  type(server_file) :: f
  integer :: status
  type(heap) :: h

#if defined(WITH_ERRORS)
  print *, file_tag_seq         ! this line must not compile successfully (private reference)
  print *, file_open_seq        ! this line must not compile successfully (private reference)
  print *, f % fd               ! this line must not compile successfully (private reference)
  call bump_ioserver_tag(f)
#endif
  status = ioserver_init(0, 'M')
  h = ioserver_heap(0)
  status = f % open('my/file/path')
  status = f % close()
  status = ioserver_terminate()
end subroutine ioserver_functions_demo
!
! =============================================================================================
!                                               IO COMMON
! =============================================================================================
!                                 ( data common to server and relay PEs )
! =============================================================================================
! general information useful to io relay processes and server processes
module io_common_mod
  use ISO_C_BINDING
  use memory_arena_mod
  use ioserver_functions
  implicit none

  save
  type(comm_rank_size) :: model_crs        ! compute PEs
  type(comm_rank_size) :: modelio_crs      ! compute and relay PEs
  type(comm_rank_size) :: allio_crs        ! relay and server PEs
  type(comm_rank_size) :: nodecom_crs      ! compute and relay PEs on THIS SMP NODE
  type(comm_rank_size) :: relay_crs        ! relay PEs
  type(comm_rank_size) :: server_crs       ! server PEs
  type(comm_rank_size) :: fullnode_crs     ! all PEs on this SMP node

  type(C_PTR) :: p_base   = C_NULL_PTR     ! base shared memory address for this PE, used for control tables
  integer(C_INTPTR_T) :: sz_base = 0       ! size in bytes of above 

  type(C_PTR) :: p_relay  = C_NULL_PTR     ! memory shared between relay and compute PEs
  integer(C_INTPTR_T) :: sz_relay = 0      ! size in bytes of above 

  type(C_PTR) :: p_server = C_NULL_PTR     ! memory shared between server PEs
  integer(C_INTPTR_T) :: sz_server = 0     ! size in bytes of above 

  type(memory_arena) :: ma                 ! local memory arena, contains heaps and circular buffers (relay+compute)

end module io_common_mod
!
! =============================================================================================
!                                               IO RELAY
! =============================================================================================
!                                 ( local data and initialization module )
! =============================================================================================
! general information useful to io relay processes
module io_relay_mod
  use io_common_mod
  use circular_buffer_module
  implicit none

  logical :: initialized = .false.
  logical :: relay_debug = .false.

  type(circular_buffer), dimension(:), pointer :: c_cio_out  ! compute PEs outbound circular buffers
  type(circular_buffer), dimension(:), pointer :: c_cio_in   ! compute PEs inbound circular buffers
  type(heap),            dimension(:), pointer :: c_heaps    ! compute PEs heaps

  contains

  function io_relay_debug(new) result(old) BIND(C,name='IoRelayDebug')
    implicit none
    integer(C_INT), intent(IN), value :: new
    integer(C_INT) :: old

    old = 0
    if(relay_debug) old = 0
    relay_debug = (new .ne. 0)
  end function io_relay_debug

  subroutine io_relay_mod_init()
    use ioserver_memory_mod
    implicit none
    type(C_PTR) :: temp

    if(initialized) return

    model_crs    = comm_rank_size(MPI_COMM_NULL, -1, 0)          ! only useful on compute nodes
    server_crs   = comm_rank_size(MPI_COMM_NULL, -1, 0)          ! only useful on server nodes
    modelio_crs  = IOserver_get_crs(MODEL_COLOR + RELAY_COLOR)   ! compute and relay PEs
    allio_crs    = IOserver_get_crs(RELAY_COLOR + SERVER_COLOR)  ! relay and server PEs
    relay_crs    = IOserver_get_crs(RELAY_COLOR)                 ! relay PEs only
    nodecom_crs  = IOserver_get_crs(MODEL_COLOR + RELAY_COLOR + NODE_COLOR)   ! compute and relay PEs on THIS SMP NODE
    fullnode_crs = IOserver_get_crs(NODE_COLOR)                  ! all PEs on this SMP node

    call IOSERVER_get_winmem(p_base, p_relay, p_server)
    call IOSERVER_get_winsizes(sz_base, sz_relay, sz_server)

    temp = ma%clone(p_relay)

    c_cio_out => circ_buffer_out
    c_cio_in  => circ_buffer_in
    c_heaps   => node_heaps

    initialized = .true.
  end subroutine io_relay_mod_init
end module io_relay_mod
!
! =============================================================================================
!                                               IO SERVER
! =============================================================================================
!                                 ( local data and initialization module )
! =============================================================================================
! general information useful to io server processes
module io_server_mod
  use io_common_mod
  implicit none

  logical :: initialized = .false.
  logical :: server_debug = .false.

  contains

  function io_server_debug(new) result(old) BIND(C,name='IoServerDebug')
    implicit none
    integer(C_INT), intent(IN), value :: new
    integer(C_INT) :: old

    old = 0
    if(server_debug) old = 0
    server_debug = (new .ne. 0)
  end function io_server_debug

  subroutine io_server_mod_init()
    implicit none
    type(C_PTR) :: temp

    if(initialized) return

    model_crs    = comm_rank_size(MPI_COMM_NULL, -1, 0)          ! only useful on compute PEs
    modelio_crs  = comm_rank_size(MPI_COMM_NULL, -1, 0)          ! only useful on compute and relay PEs
    allio_crs    = IOserver_get_crs(RELAY_COLOR + SERVER_COLOR)  ! relay and server PEs
    relay_crs    = comm_rank_size(MPI_COMM_NULL, -1, 0)          ! only useful on compute nodes
    server_crs   = IOserver_get_crs(SERVER_COLOR)                ! server PEs
    nodecom_crs  = IOserver_get_crs(SERVER_COLOR + NODE_COLOR)   ! server PEs on THIS SMP NODE
    fullnode_crs = IOserver_get_crs(NODE_COLOR)

    call IOSERVER_get_winmem(p_base, p_relay, p_server)
    call IOSERVER_get_winsizes(sz_base, sz_relay, sz_server)

    temp = ma%clone(p_relay)

    initialized = .true.
  end subroutine io_server_mod_init
end module io_server_mod
