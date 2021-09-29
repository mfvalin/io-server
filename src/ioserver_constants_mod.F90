
module ioserver_constants
  use ISO_C_BINDING
  use mpi_f08
  implicit none

  integer(C_SIZE_T), parameter :: KBYTE = 1024
  integer(C_SIZE_T), parameter :: MBYTE = 1024 * 1024
  integer(C_SIZE_T), parameter :: GBYTE = 1024 * 1024 * 1024

  integer, parameter :: NO_COLOR      = 0
  integer, parameter :: MODEL_COLOR   = 1
  integer, parameter :: RELAY_COLOR   = 2
  integer, parameter :: SERVER_COLOR  = 4
  integer, parameter :: OUTPUT_COLOR  = 8
  integer, parameter :: INPUT_COLOR   = 16
  integer, parameter :: INOUT_COLOR   = INPUT_COLOR + OUTPUT_COLOR
  integer, parameter :: CHANNEL_COLOR = 32
  integer, parameter :: NODE_COLOR    = 4096
  integer, parameter :: NO_OP_COLOR   = 8192   ! MUST BE THE HIGHEST VALUE

  integer, parameter :: IO_CONTROL   = 1000
  integer, parameter :: IO_BASE      = 1001
  integer, parameter :: IO_RELAY     = 1002
  integer, parameter :: IO_SERVER    = 1003

  type :: comm_rank_size
    type(MPI_Comm) :: comm = MPI_COMM_NULL
    integer :: rank = -1
    integer :: size = 0
  contains
    procedure, pass :: is_null => IOserver_is_CRS_null
  end type
  type(comm_rank_size), parameter :: COMM_RANK_SIZE_NULL = comm_rank_size(MPI_COMM_NULL, -1, 0)

  type, bind(C) :: qualified_address
    type(C_PTR)    :: p                   ! address (C pointer)
    integer(C_INT) :: color               ! PE color (MODEL_COLOR | RELAY_COLOR | NODE_COLOR)
    integer(C_INT) :: rank                ! pe rank in above color
  end type

contains

function IOserver_is_CRS_null(crs) result(status)   !  is this a NULL communicator combo ?
  implicit none
  class(comm_rank_size), intent(inout) :: crs
  logical :: status
  status = (crs % rank < 0 .or. crs % size <= 0)
  if (status) crs % comm = MPI_COMM_NULL
end function IOserver_is_CRS_null

end module ioserver_constants
