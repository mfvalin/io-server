! Author: Christopher Subich, 2021
! 
! Program to test the limits of MPI_Put on some machines. It crashes on our XC50, on an interactive node of 40 cores, with aprun -n 160 ./put_error,
! when compiled with ftn put_error.F90 -o put_error
!
program put_error
    use iso_c_binding
    use mpi
    implicit none
 
    integer :: gsize, grank ! number of processors and rank on comm world
    integer :: lsize, lrank ! size and rank on local (split) communicator
    ! integer :: colour ! used to split world into two
    integer :: ierr ! mpi error var
    !type(mpi_comm) :: lcomm
    integer :: lcomm
 
    integer, parameter :: numel = 1000000 ! number of elements to send to each other process
 
    !type(mpi_win) :: mpi_window ! handle for MPI RMA window
    integer :: mpi_window
    integer, pointer, dimension(:) :: rma_buffer ! to be allocated by mpi_win_allocate
    integer, allocatable, dimension(:) :: local_buffer ! local send buffer
    integer(kind=mpi_address_kind) :: winsize
    integer :: disp_unit
    !type(mpi_info) :: info_obj
    integer :: info_obj
    type(c_ptr) :: winptr ! C-pointer to allocated window
 
    call mpi_init(ierr)
 
    ! Get global size/rank
    call mpi_comm_size(mpi_comm_world, gsize,ierr)
    call mpi_comm_rank(mpi_comm_world, grank, ierr)
 
 !   if (grank >= gsize/2) then
 !      colour = 1
 !   else
 !      colour = 0
 !   endif
 !
    ! Split in half, into a yin/yang facsimile
 !   call mpi_comm_split(mpi_comm_world, colour, grank, lcomm, ierr)
 !   call mpi_comm_size(lcomm, lsize, ierr)
 !   call mpi_comm_rank(lcomm, lrank, ierr)
    lcomm = MPI_COMM_WORLD
    lsize = gsize
    lrank = grank
 
    ! Allocate RMA window
    disp_unit = 4 ! size of MPI_INTEGER
    winsize = disp_unit*numel*lsize ! numel*lsize total elements, disp_unit bytes per element
    info_obj = MPI_INFO_NULL
    !print *, winsize, info_obj
 
    call mpi_win_allocate(winsize, disp_unit, info_obj, lcomm, winptr, mpi_window, ierr)
    !call mpi_alloc_mem(winsize, info_obj, winptr, ierr)
    call c_f_pointer(winptr, rma_buffer, [numel*lsize])
    ! Allocate local buffer with matching size
    allocate(local_buffer(numel*lsize))
 
    local_buffer = lrank ! Initialize with source rank for later validation
 
    block
       integer(kind=mpi_address_kind) :: tdisp
    !    integer :: target_rank = 1
    !    integer(kind=mpi_address_kind) :: target_disp = 1
    !    integer, dimension(1) :: origin_addr
       integer ii
 
       integer loopct
       integer :: receive_okay = 1
       integer :: all_okay = 1
       real(kind=C_DOUBLE) :: now, later
 
       ! Synchronize before beginning timing
       call MPI_Barrier(MPI_COMM_WORLD,ierr)
 
       now = MPI_Wtime() ! Start timing
 
       ! Loop to get a performance measurement.  If this case triggers the error,
       ! we expect failure on the first loop.
 
       do loopct=1,10
          ! Clear remote buffer
          rma_buffer = 0
 
          ! Fence to begin RMA window
          call mpi_win_fence(0, mpi_window, ierr)
          ! Initial fence
          do ii=0,lsize-1 ! loop over destination MPI ranks, 0..size-1
             tdisp = numel*lrank
             ! Place into our target section of the destination array
             call mpi_put(local_buffer(1+numel*(ii)), numel, MPI_INTEGER, &
                          ii, tdisp, numel, MPI_INTEGER, mpi_window, ierr)
          end do
          call mpi_win_fence(0, mpi_window, ierr) ! End RMA window
 
          ! Verify received buffer
          receive_okay = 1
          do ii=1,lsize
             if (any(rma_buffer((ii-1)*numel+1 : (ii*numel)) /= ii-1)) then
                receive_okay = 0
             endif
          enddo
          call mpi_allreduce(receive_okay, all_okay, 1, MPI_INTEGER, MPI_MIN, MPI_COMM_WORLD, ierr)
          if (grank == 0 .and. loopct==1) then
             if (all_okay < 1) then
                print * , ' Failure to validate '
             endif
          endif
       end do
 
       call MPI_Barrier(MPI_COMM_WORLD,ierr)
       later = MPI_Wtime()
       if (grank == 0) then
          print('("Completed ",I0," loops of size ",I0," in ",G0.2," seconds.")'), loopct, numel*lsize, later-now
       end if
    end block
 
    call mpi_finalize(ierr)
 end program
 