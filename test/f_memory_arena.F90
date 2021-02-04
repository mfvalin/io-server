program test_memory_arena
  use ISO_C_BINDING
  use memory_arena_mod
  implicit none

#define DBLK 20
#define STR(a) a//achar(0)
#define NBLKS 399
#define NSYM (NBLKS+32)
#define XTRA 10

  include 'mpif.h'
!   include 'io-server/memory_arena.inc'
  interface
    function shmat(shmid, ptr, opt) result(p) BIND(C,name='shmat')
      import :: C_PTR, C_INT
      implicit none
      integer(C_INT), intent(IN), value :: shmid
      type(C_PTR), intent(IN), value :: ptr
      integer(C_INT), intent(IN), value :: opt
      type(C_PTR) :: p
    end function shmat
  end interface

  integer(C_INT) :: ierr, rank, size, win, id, id2, shmid, bsize, bflags, timeout, i, j, jerr
  integer(KIND=MPI_ADDRESS_KIND) :: winsize, shmaddr, disp_unit
  integer(C_INT64_T) :: shmsz64
  type(C_PTR) :: memadr, p
  integer, dimension(:), pointer :: fp
  type(memory_arena) :: m
  character(len=8) :: bname
  type(C_PTR), dimension(0:NBLKS*2) :: pp

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, size, ierr)
  id = m % setid(rank)
!   id = memory_arena_set_id(rank);

  shmsz64 = 1024 * 1024 * 96       ! 96 MBytes
  shmid   = -1
  timeout = 1000                   ! 1 second

#if defined(MPI_SHARED)
  winsize = 0
  if(rank == 0) winsize = shmsz64  ! allocate size non zero only on PE 0
  call MPI_Win_allocate_shared (winsize, 1, MPI_INFO_NULL, MPI_COMM_WORLD, shmaddr, win, ierr)
  call MPI_Win_shared_query (win, 0, winsize, disp_unit, shmaddr, ierr) ! get address of segment for rank 0
  memadr = transfer(shmaddr, C_NULL_PTR)
!   call MPI_Win_shared_query (win, 0, winsize, disp_unit, memadr, ierr)  ! get address of segment for rank 0
  if(rank == 0) then            ! allocation and initialization of arena done by PE 0 only
    print *,"creating/initializing memory arena from address\n"
!     memadr = memory_arena_create_from_address(memadr, NSYM, shmsz64)   ! initialize arena
    memadr = m % create(memadr, NSYM, shmsz64)
  else
    print *,"cloning memory arena from address\n"
    memadr = m % clone(memadr)    ! create local object for memory arena using shared memory segment
  endif

#else

  memadr = C_NULL_PTR
  if(rank == 0) then            ! allocation of shared segment done by PEE 0 only
!     memadr = memory_allocate_shared(shmid, shmsz64)             ! allocate only
!     id2 = memory_arena_init(memadr, NSYM, shmsz64)              ! initialize
    memadr = m % create(shmid, NSYM, shmsz64)
!     memadr = memory_arena_create_shared(shmid, NSYM, shmsz64)   ! allocate and initialize arena
    print *,'shmid =',shmid
  endif
  call MPI_Bcast(shmid, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
  if(rank > 0) then
    memadr = shmat(shmid, C_NULL_PTR, 0)
    shmaddr = transfer(memadr,shmaddr)
    print 1,"attached segment: shmid, addr =",rank, shmid, shmaddr
    memadr = m % clone(memadr)    ! create local object for memory arena using shared memory segment
  endif
#endif

  if(rank == 0) then
!     id2 = memory_arena_init(memadr, NSYM, shmsz64)    ! deliberate extra initalization
    id2 = m % init(NSYM, shmsz64)
    call m % dump()
    print 1,"id2, rank, address =", id2, rank,transfer(memadr,shmaddr)
    p = m%newblock(DBLK*1, "BLOCK000") ! p = memory_block_create(memadr, DBLK*1, STR("BLOCK000"))
    p = m%markblock("BLOCK000")        ! p = memory_block_mark_init(memadr, STR("BLOCK000"))
    print 1,"BLOCK000 created at address",0, 0,transfer(p,shmaddr)
    p = m%newblock(DBLK*2, "BLOCK001") ! p = memory_block_create(memadr, DBLK*2, STR("BLOCK001"))
    p = m%markblock("BLOCK001")        ! p = memory_block_mark_init(memadr, STR("BLOCK001"))
    print 1,"BLOCK001 created at address",0, 0,transfer(p,shmaddr)
    p = m%newblock(DBLK*3, "BLOCK002") ! p = memory_block_create(memadr, DBLK*3, STR("BLOCK002"))
    p = m%markblock("BLOCK002")        ! p = memory_block_mark_init(memadr, STR("BLOCK002"))
    print 1,"BLOCK002 created at address",0, 0,transfer(p,shmaddr)
    p = m%newblock(DBLK*4, "BLOCK003") ! p = memory_block_create(memadr, DBLK*4, STR("BLOCK003"))
    p = m%markblock("BLOCK003")        ! p = memory_block_mark_init(memadr, STR("BLOCK003"))
    print 1,"BLOCK003 created at address",0, 0,transfer(p,shmaddr)
    p = m%newblock(DBLK*5, "BLOCK004") ! p = memory_block_create(memadr, DBLK*5, STR("BLOCK004"))
!     p = m%markblock("BLOCK004")        ! p = memory_block_mark_init(memadr, STR("BLOCK004"))
    print 1,"BLOCK004 created at address",0, 0,transfer(p,shmaddr)
  endif
  call MPI_Barrier(MPI_COMM_WORLD, ierr)

!   shmaddr = transfer(memadr,shmaddr)
  print *,"I am process",rank+1," of",size
  call m % dump()

  call MPI_Barrier(MPI_COMM_WORLD, ierr)

  p = m%find(bsize, bflags, "BLOCK000", timeout) ! p = memory_block_find_wait(memadr, bsize, bflags, STR("BLOCK000"), timeout)
  print 1,"BLOCK000 found at address",bsize, bflags,transfer(p,shmaddr)
  p = m%find(bsize, bflags, "BLOCK001", timeout) ! p = memory_block_find_wait(memadr, bsize, bflags, STR("BLOCK001"), timeout)
  print 1,"BLOCK001 found at address",bsize, bflags,transfer(p,shmaddr)
  p = m%find(bsize, bflags, "BLOCK002", timeout) ! p = memory_block_find_wait(memadr, bsize, bflags, STR("BLOCK002"), timeout)
  print 1,"BLOCK002 found at address",bsize, bflags,transfer(p,shmaddr)
  p = m%find(bsize, bflags, "BLOCK003"         ) ! p = memory_block_find(     memadr, bsize, bflags, STR("BLOCK003"))
  print 1,"BLOCK003 found at address",bsize, bflags,transfer(p,shmaddr)
  p = m%find("BLOCK004"                        ) ! p = memory_block_find(     memadr, bsize, bflags, STR("BLOCK004"))
  print 1,"BLOCK004 found at address",-1, -1,transfer(p,shmaddr)

  ierr = 0
  do i = rank, NBLKS, size                          ! create blocks
    write(bname,'(A,I4.4)') 'BLCK', i            ! create block name BLCKnnnn
    pp(i) = m%newblock(XTRA+i, bname)             ! create the block
    if( .not. C_ASSOCIATED(pp(i)) ) ierr = ierr + 1
  enddo
  if(ierr .ne. 0) then
    print *,ierr," errors in block creation"
  endif
  do i = rank, NBLKS, size                          ! fill blocks
    call C_F_POINTER(pp(i), fp, [XTRA+i])
    do j = 1, XTRA+i
      fp(j) = XTRA+i - j
    enddo
  enddo

  call MPI_Barrier(MPI_COMM_WORLD, ierr)

!   call m % dump()
  ierr = 0
  do i = 0, NBLKS
    write(bname,'(A,I4.4)') 'BLCK', i
    pp(i) = m%find(bname)
    if( C_ASSOCIATED(pp(i)) ) ierr = ierr + 1
  enddo
!   print *,'blocks found =',ierr,', expected =',NBLKS+1
  if(ierr .ne. NBLKS+1) then
    print *,'blocks found =',ierr,', expected =',NBLKS+1
    goto 888
  endif

  jerr = 0
  do i = 0, NBLKS
    call C_F_POINTER(pp(i), fp, [XTRA+i])
    ierr = 0
    do j = 1, XTRA+i
      if( fp(j) .ne. XTRA+i - j ) then
        ierr = ierr + 1
        jerr = jerr + 1
      endif
    enddo
    if(ierr > 0) then
      print *,'errors in block',i,' =',ierr
    endif
  enddo
  print *,'total errors =',jerr

777 continue
  call MPI_Finalize(ierr)
  stop
888 continue
  print *,'ERROR IN TEST'
  goto 777
1   format(A,2I10,2X,Z16.16)
2 format(100I4)
end program
