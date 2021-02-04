module memory_arena_mod
  use ISO_C_BINDING
  implicit none
  include 'io-server/memory_arena.inc'

  type :: memory_arena
    private
    type(C_PTR) :: p
  contains
    procedure :: init
    procedure :: clone
    procedure :: addr                                          ! get memory arena address
    procedure :: dump                                          ! dump arena metadata
    procedure, nopass :: setid                                 ! set local owner id for arena(s)

    procedure :: create_from_address
    procedure :: create_shared
    GENERIC   :: create => create_shared, create_from_address  ! create arena

    procedure :: adrblock
    procedure :: adrblockw
    procedure :: getblock
    procedure :: getblockw
    GENERIC   :: find => adrblock, getblock, getblockw         ! find a block

    procedure :: newblock                                      ! create block in arena
    procedure :: markblock                                     ! mark block as initialized
  end type

  contains

  function init(this, nsym, size) result(id)              !< initial setup of memory arena
    implicit none
    class(memory_arena), intent(INOUT)    :: this
    integer(C_INT), intent(IN), value     :: nsym         !< size of symbol table to allocate (max number of blocks expected)
    integer(C_INT64_T), intent(IN), value :: size         !< size of memory area in bytes (max 32GBytes)
    integer(C_INT) :: id                                  !< id of current owner process (not necessarily me)
    id = memory_arena_init(this%p, nsym, size)
  end function init

  function clone(this, memaddr) result(p)                 !< build memory arena object using address of existing arena
    implicit none
    class(memory_arena), intent(INOUT)    :: this
    type(C_PTR), intent(IN), value        :: memaddr      !< user memory address
    type(C_PTR) :: p                                      !< address of memory arena, NULL if error
    this%p = memaddr
    p      = this%p
  end function clone

  function create_from_address(this, memaddr, nsym, size) result(p)   !< use supplied address, initialize arena
    implicit none
    class(memory_arena), intent(INOUT)    :: this
    type(C_PTR), intent(IN), value        :: memaddr      !< user memory address
    integer(C_INT), intent(IN), value     :: nsym         !< size of symbol table to allocate (max number of blocks expected)
    integer(C_INT64_T), intent(IN), value :: size         !< size of arena in bytes
    type(C_PTR) :: p
    this%p = memory_arena_create_from_address(memaddr, nsym, size)
    p      = this%p                                       !< address of memory arena, NULL if error
  end function create_from_address

  function create_shared(this, shmid, nsym, size) result(p)  !< allocate shared memory, initialize arena, return id of shared memory segment
    implicit none
    class(memory_arena), intent(INOUT)    :: this
    integer(C_INT), intent(OUT)           :: shmid        !< shared memory id of segment (see shmget)
    integer(C_INT), intent(IN), value     :: nsym         !< size of symbol table to allocate (max number of blocks expected)
    integer(C_INT64_T), intent(IN), value :: size         !< size of arena in bytes
    type(C_PTR) :: p                                      !< address of memory arena, NULL if error
    this%p = memory_arena_create_shared(shmid, nsym, size)
    p      = this%p
  end function create_shared

  function getblockw(this, size, flags, name, timeout) result(p)   !< get address, size, flags of a block (with timeout)
    implicit none
    class(memory_arena), intent(IN)       :: this
    integer(C_INT), intent(OUT)           :: size            !< size of memory block in 32 bit units (0 if not found)
    integer(C_INT), intent(OUT)           :: flags           !< block flags (0 if not found)
    character(len=*), intent(IN) :: name                     !< name of block to find (characters beyond the 8th will be ignored)
    integer(C_INT), intent(IN), value     :: timeout         !< timeout in milliseconds, -1 means practically forever
    type(C_PTR) :: p                                         !< address of memory block, NULL if error
    p = memory_block_find_wait(this%p, size, flags, trim(name)//achar(0), timeout)
  end function getblockw

  function adrblockw(this, name, timeout) result(p)         !< get address of a block (with timeout)
    implicit none
    class(memory_arena), intent(IN)       :: this
    character(len=*), intent(IN) :: name                     !< name of block to find (characters beyond the 8th will be ignored)
    integer(C_INT), intent(IN), value     :: timeout         !< timeout in milliseconds, -1 means practically forever
    type(C_PTR) :: p                                         !< address of memory block, NULL if error
    integer(C_INT)           :: size                         ! size of memory block in 32 bit units (0 if not found)
    integer(C_INT)           :: flags                        ! block flags (0 if not found)
    p = memory_block_find_wait(this%p, size, flags, trim(name)//achar(0), timeout)
  end function adrblockw

  function getblock(this, size, flags, name) result(p)       !< get address, size, flags of a block
    implicit none
    class(memory_arena), intent(IN)       :: this
    integer(C_INT), intent(OUT)           :: size            !< size of memory block in 32 bit units (0 if not found)
    integer(C_INT), intent(OUT)           :: flags           !< block flags (0 if not found)
    character(len=*), intent(IN) :: name                     !< name of block to find (characters beyond the 8th will be ignored)
    type(C_PTR) :: p                                         !< address of memory block, NULL if error
    p = memory_block_find(this%p, size, flags, trim(name)//achar(0))
  end function getblock

  function adrblock(this, name) result(p)              !< get address of a block
    implicit none
    class(memory_arena), intent(IN)       :: this
    character(len=*), intent(IN) :: name                     !< name of block to find (characters beyond the 8th will be ignored)
    type(C_PTR) :: p                                         !< address of memory block, NULL if error
    integer(C_INT)      :: size                              ! size of memory block in 32 bit units (0 if not found)
    integer(C_INT)      :: flags                             ! block flags (0 if not found)
    p = memory_block_find(this%p, size, flags, trim(name)//achar(0))
  end function adrblock

  function newblock(this, size, name) result(p)
    implicit none
    class(memory_arena), intent(IN)       :: this
    integer(C_INT), intent(IN), value     :: size            !< desired size of block in 32 bit units
    character(len=*), intent(IN) :: name                     !< name of block to create (characters beyond the 8th will be ignored)
    type(C_PTR) :: p                                         !< address of memory block, NULL if error
    p = memory_block_create(this%p, size, trim(name)//achar(0))
  end function newblock

  function markblock(this, name) result(p)
    implicit none
    class(memory_arena), intent(IN)       :: this
    character(len=*), intent(IN) :: name                     !< name of block to mark as initialized (see newblock)
    type(C_PTR) :: p                                         !< address of memory block, NULL if error
    p = memory_block_mark_init(this%p, trim(name)//achar(0))
  end function markblock

  function setid(id) result(me)
    implicit none
    integer(C_INT), intent(IN), value :: id                   !< owner's id (usually MPI rank) 
    integer(C_INT)                    :: me                   !< -1 upon error, value > 0 otherwise
    me = memory_arena_set_id(id)
  end function setid

  subroutine dump(this)
    implicit none
    class(memory_arena), intent(IN)       :: this
    call memory_arena_print_status(this%p)
  end subroutine dump

  function addr(this) result(p)
    implicit none
    class(memory_arena), intent(IN)       :: this
    type(C_PTR)                           :: p
    p = this%p                                                !< address of memory arena, NULL if error
  end function addr

end module
