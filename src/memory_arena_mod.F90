module memory_arena_mod
  use ISO_C_BINDING
  implicit none
  include 'io-server/memory_arena.inc'

  type :: memory_arena
    private
    type(C_PTR) :: p
  contains
    procedure :: init
    procedure :: create_from_address
    procedure :: create_shared
    GENERIC   :: create => create_shared, create_from_address  ! create arena
    procedure :: getblock_no_wait
    procedure :: getblock_wait
    GENERIC   :: getblock => getblock_wait, getblock_no_wait   ! find block in arena
    procedure :: newblock                                      ! create block in arena
    procedure :: dump                                          ! dump arena metadata
    procedure :: addr                                          ! get arena address
    procedure, nopass :: markblock                             ! mark block as initialized
    procedure, nopass :: setid                                 ! set local owner id for arena(s)
  end type

  contains

  function init(this, nsym, size) result(id)
    implicit none
    class(memory_arena), intent(INOUT)    :: this
    integer(C_INT), intent(IN), value     :: nsym         !< size of symbol table to allocate (max number of blocks expected)
    integer(C_INT64_T), intent(IN), value :: size         !< size of memory area in bytes (max 32GBytes)
    integer(C_INT) :: id                                  !< id of current owner process (not necessarily me)
    id = memory_arena_init(this%p, nsym, size)
  end function init

  function create_from_address(this, memaddr, nsym, size) result(p)
    implicit none
    class(memory_arena), intent(INOUT)    :: this
    type(C_PTR), intent(IN), value        :: memaddr      !< user memory address
    integer(C_INT), intent(IN), value     :: nsym         !< size of symbol table to allocate (max number of blocks expected)
    integer(C_INT64_T), intent(IN), value :: size         !< size of arena in bytes
    type(C_PTR) :: p
    this%p = memory_arena_create_from_address(memaddr, nsym, size)
    p      = this%p
  end function create_from_address

  function create_shared(this, shmid, nsym, size) result(p)
    implicit none
    class(memory_arena), intent(INOUT)    :: this
    integer(C_INT), intent(OUT)           :: shmid        !< shared memory id of segment (see shmget)
    integer(C_INT), intent(IN), value     :: nsym         !< size of symbol table to allocate (max number of blocks expected)
    integer(C_INT64_T), intent(IN), value :: size         !< size of arena in bytes
    type(C_PTR) :: p
    this%p = memory_arena_create_shared(shmid, nsym, size)
    p      = this%p
  end function create_shared

  function getblock_wait(this, size, flags, name, timeout) result(p)
    implicit none
    class(memory_arena), intent(IN)       :: this
    integer(C_INT), intent(OUT)           :: size            !< size of memory block in 32 bit units (0 if not found)
    integer(C_INT), intent(OUT)           :: flags           !< block flags (0 if not found)
    character(C_CHAR), dimension(*), intent(IN) :: name      !< name of block to find (characters beyond the 8th will be ignored)
    integer(C_INT), intent(IN), value     :: timeout         !< timeout in milliseconds, -1 means practically forever
    type(C_PTR) :: p
    p = memory_block_find_wait(this%p, size, flags, name, timeout)
  end function getblock_wait

  function getblock_no_wait(this, size, flags, name) result(p)
    implicit none
    class(memory_arena), intent(IN)       :: this
    integer(C_INT), intent(OUT)           :: size            !< size of memory block in 32 bit units (0 if not found)
    integer(C_INT), intent(OUT)           :: flags           !< block flags (0 if not found)
    character(C_CHAR), dimension(*), intent(IN) :: name      !< name of block to find (characters beyond the 8th will be ignored)
    type(C_PTR) :: p
    p = memory_block_find(this%p, size, flags, name)
  end function getblock_no_wait

  function newblock(this, size, name) result(p)
    implicit none
    class(memory_arena), intent(IN)       :: this
    integer(C_INT), intent(IN), value     :: size            !< desired size of block in 32 bit units
    character(C_CHAR), dimension(*), intent(IN) :: name      !< name of block to create (characters beyond the 8th will be ignored)
    type(C_PTR) :: p
    p = memory_block_create(this%p, size, name)
  end function newblock

  function markblock(mem, name) result(p)
    implicit none
    type(C_PTR), intent(IN), value :: mem                    !< pointer to the managed 'memory arena' (see  memory_arena_init)
    character(C_CHAR), dimension(*), intent(IN) :: name      !< name of block to create (characters beyond the 8th will be ignored)
    type(C_PTR) :: p
    p = memory_block_mark_init(mem, name)
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
    p = this%p
  end function addr

end module
