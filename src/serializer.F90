!  serializer for FORTRAN programming
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
! _the Cray Fortran compiler treats loc() as a type(C_PTR), other compilers as integer(C_INTPTR_T)
#if defined(_CRAYFTN)
#define WHAT_TYPE type(C_PTR)
#else
#define WHAT_TYPE integer(C_INTPTR_T)
#endif

module data_serialize
  use ISO_C_BINDING
  implicit none

  public

  private :: c_malloc    ! DO NOT EXPORT
  interface
    function c_malloc(sz) result(p) BIND(C,name='malloc')
      import :: C_SIZE_T, C_PTR
      implicit none
      integer(C_SIZE_T), intent(IN), value :: sz
      type(C_PTR) :: p
    end function c_malloc
    subroutine c_free(p) BIND(C, name='free')
      import :: C_PTR
      implicit none
      type(C_PTR), intent(IN), value :: p
    end subroutine c_free
  end interface

  logical, save, private :: debug_mode = .false.

  integer, parameter :: JAR_ELEMENT = C_INT
  type, BIND(C) :: c_jar                         ! C interoperable version of jar
    private
    integer(JAR_ELEMENT) :: size = 0             ! capacity of jar
    integer(JAR_ELEMENT) :: top  = 0             ! last posision "written" (cannot write beyond size)
    integer(JAR_ELEMENT) :: bot  = 0             ! last position "read" (cannot read beyond top)
    type(C_PTR)          :: p = C_NULL_PTR       ! address of actual data
  end type

  type :: jar                                    ! same as c_jar, but with type bound procedures
    private
    integer(JAR_ELEMENT) :: size = 0             ! capacity of jar
    integer(JAR_ELEMENT) :: top  = 0             ! last posision "written" (cannot write beyond size)
    integer(JAR_ELEMENT) :: bot  = 0             ! last position "read" (cannot read beyond top)
    type(C_PTR)          :: p = C_NULL_PTR       ! address of actual data
  contains
    procedure, PASS :: new     => new_jar        ! create a new data jar, allocate data storage
    procedure, PASS :: valid   => valid_jar      ! is jar valid (is there is a valid data pointer ?)
    procedure, PASS :: free    => free_jar       ! free jar data
    procedure, PASS :: rewind  => rewind_jar     ! rewind jar for extraction
    procedure, PASS :: reset   => reset_jar      ! make jar empty, keep allocated space
    procedure, PASS :: data    => jar_pointer    ! get C pointer to actual jar data
    procedure, PASS :: content => jar_contents   ! get Fortran pointer to actual jar data
    procedure, PASS :: usable  => jar_size       ! maximum capacity of data jar
    procedure, PASS :: stored  => jar_top        ! current number of elements inserted
    procedure, PASS :: avail   => jar_avail      ! current number of elements available for extraction
    procedure, PASS :: put     => add_to_jar     ! add data to jar at top of jar (or at a specific position)
    procedure, PASS :: get     => get_from_jar   ! get data from jar at current (or specific position)
    procedure, PASS :: print   => print_jar
    procedure, NOPASS :: debug => debug_jars
    final :: final_jar
  end type

  contains

  function debug_jars(mode) result(old)      ! set debug mode, get previous setting
    implicit none
    logical, intent(IN), value :: mode       ! .true. , set debug mode, .false. cancel debug mode
    logical :: old                           ! return previous setting

    if(debug_mode .or. mode) print *,'DEBUG: setting debug mode to',mode,' was',debug_mode
    old        = debug_mode
    debug_mode = mode
  end function debug_jars

  function new_jar(j, data_size) result(ok)  ! allocate a jar of arbitrary size
    implicit none
    class(jar), intent(INOUT) :: j           ! the data jar
    integer, intent(IN), value :: data_size  ! number of elements in jar
    integer :: ok                            ! 0 if O.K., -1 if error

    integer(C_SIZE_T) :: dsz

    ok = -1
    if(C_ASSOCIATED(j % p)) return          ! error, there is already an allocated data container

    dsz = data_size
    j % p = c_malloc( dsz * 4)              ! size in bytes
    if(.not. C_ASSOCIATED(j % p)) return    ! malloc failed
    ok = 0

    j % top  = 0                            ! data jar is empty (no data written)
    j % bot  = 0                            ! data jar is empty (no data to read)
    j % size = data_size                    ! data jar capacity

  end function new_jar

  function shape_as_jar(j, array, arraysize) result(ok)  ! transform an integer array into a jar
    implicit none
    class(jar), intent(INOUT) :: j          ! the data jar
    integer(C_INT), intent(IN), value :: arraysize  ! number of elements in arrray
    integer, dimension(arraysize), intent(IN) :: array    ! DO NOT LIE
    integer :: ok                            ! 0 if O.K., -1 if error

    integer(C_INTPTR_T) :: temp

    ok = -1
    if(C_ASSOCIATED(j % p)) return          ! error, there is already an allocated data container

    temp = LOC(array)
    j % p = transfer(temp, j % p)
    ok = 0

    j % top  = 0                            ! data jar is empty (no data written)
    j % bot  = 0                            ! data jar is empty (no data to read)
    j % size = arraysize                    ! data jar capacity

  end function shape_as_jar

  function valid_jar(j) result(ok)          ! do we have a valid data pointer
    implicit none
    class(jar), intent(INOUT) :: j          ! the data jar
    logical :: ok                           ! .true. if valid, .false. if not

    ok = C_ASSOCIATED(j%p)
  end function valid_jar

  subroutine reset_jar(j)                   ! re initialize a jar (leave data conttainer alone)
    implicit none
    class(jar), intent(INOUT) :: j          ! the data jar

    j % top  = 0                            ! data jar is empty (no data written)
    j % bot  = 0                            ! data jar is empty (no data read)
  end subroutine reset_jar

  subroutine rewind_jar(j)                  ! reset extraction pointer of a jar to the beginning
    implicit none
    class(jar), intent(INOUT) :: j          ! the data jar

    j % bot  = 0                            ! no data read yet
  end subroutine rewind_jar

  subroutine print_jar(j, max_elem)         ! print jar info
    implicit none
    class(jar), intent(IN) :: j             ! the data jar
    integer, intent(IN), value :: max_elem  ! max data elements to print

    integer, dimension(:), pointer :: data

    call C_F_POINTER(j%p, data, [j%size])
    print 1, j%size, j%bot, j%top, data(1:min(max_elem,j%top))
1   format(3I6,(20Z9.8))
  end subroutine print_jar

  function jar_avail(j) result(sz)          ! get amount of available data in jar (32 bit units)
    implicit none
    class(jar), intent(IN) :: j             ! the data jar
    integer :: sz                           ! available data in jar (32 bit units)

    sz = -1
    if(.not. C_ASSOCIATED(j % p)) return
    sz = j % top - j % bot

  end function jar_avail

  function jar_top(j) result(sz)            ! get amount of data in jar (32 bit units)
    implicit none
    class(jar), intent(IN) :: j             ! the data jar
    integer :: sz                           ! amount of data inserted in jar (32 bit units)

    sz = -1
    if(.not. C_ASSOCIATED(j % p)) return
    sz = j % top

  end function jar_top

  function jar_size(j) result(sz)           ! get data jar capacity (32 bit units)
    implicit none
    class(jar), intent(IN) :: j             ! the data jar
    integer :: sz                           ! data jar capacity (32 bit units)

    sz = -1
    if(.not. C_ASSOCIATED(j % p)) return
    sz = j % size

  end function jar_size

  function jar_pointer(j) result(p)         ! get C pointer to jar data (1 D array of JAR_ELEMENTs)
    implicit none
    class(jar), intent(IN) :: j             ! the data jar
    type(C_PTR) :: p                        ! C pointer to jar data 

    p = C_NULL_PTR
    if(.not. C_ASSOCIATED(j % p)) return
    p = j % p

  end function jar_pointer

  function jar_contents(j) result(fp)       ! get Fortran pointer to jar data (1 D array of JAR_ELEMENTs)
    implicit none
    class(jar), intent(IN) :: j             ! the data jar
    integer(JAR_ELEMENT), dimension(:),  pointer :: fp  ! Fortran pointer to jar data

    nullify(fp)
    if(.not. C_ASSOCIATED(j % p)) return    ! no data pointer, return NULL pointer
    if(j % top == 0) return                 ! empty jar, return NULL pointer

    call C_F_POINTER(j % p, fp, [j % top])  ! Fortran pointer to array of j%size JAR_ELEMENTs

  end function jar_contents

  subroutine final_jar(j)                   ! deallocate a jar's data if not already done at finalize
    implicit none
    type(jar), intent(INOUT) :: j           ! the data jar

    if(C_ASSOCIATED(j % p)) then
      if(debug_mode) print *,'DEBUG(jar finalize): freing jar, size =', j%size
      call c_free(j % p)  ! release storage associated with jar
      j % p = C_NULL_PTR
    else
      if(debug_mode) print *,'DEBUG(jar finalize): nothing to free in jar'
    endif
    j % top  = 0                             ! data jar is now empty (no data written)
    j % bot  = 0                             ! data jar is now empty (no data to read)
    j % size  = 0                            ! data jar cannot store data
  end subroutine final_jar

  function free_jar(j) result(status)        ! deallocate a jar's data space
    implicit none
    class(jar), intent(INOUT) :: j           ! the data jar
    integer :: status                        ! 0 if O.K., -1 if nothing to free

    j % top  = 0                             ! data jar is now empty (no data written)
    j % bot  = 0                             ! data jar is now empty (no data to read)
    j % size  = 0                            ! data jar cannot store data
    if(C_ASSOCIATED(j % p)) then
      if(debug_mode) print *,'DEBUG(jar free): freing jar'
      call c_free(j % p)                     ! release storage associated with jar
      j % p = C_NULL_PTR                     ! nullify data pointer to avoid accidents
      status = 0
    else
      if(debug_mode) print *,'DEBUG(jar free): nothing to free in jar'
      status = -1                            ! already freed
    endif
  end function free_jar

  function add_to_jar(j, what, size, where) result(sz)      ! insert data into data jar
    implicit none
    class(jar), intent(INOUT) :: j                          ! the data jar
    WHAT_TYPE, intent(IN), value :: what                    ! must match type of loc(xxx)
    integer, intent(IN), value :: size                      ! size to insert = storage_size(item) * nb_of_items
    integer, intent(IN), optional, value :: where           ! optional argument to force insertion point (1 = start of jar)
    integer :: sz                                           ! position of last inserted element (-1 if error)

    integer :: intsize, pos
    type(C_PTR) :: temp
    integer(JAR_ELEMENT), dimension(:), pointer :: je, content

    sz = -1
    if(.not. C_ASSOCIATED(j % p)) return

    call C_F_POINTER(j % p, content, [j % size])            ! pointer to jar data
    if( present(where) ) then
      pos = where - 1                                       ! insert starting at position "where"
    else
      pos = j%top                                           ! insert after data currently in jar
    endif
    if(pos < 0) return                                      ! invalid insertion position
    if(pos > j%top) content(j%top+1:pos) = 0                ! zero fill skipped portion

    intsize = size / storage_size(content(1))
    if(pos + intsize > j%size) return                       ! jar would overflow

    temp    = transfer(what,temp)                           ! address of data to insert
    call C_F_POINTER(temp, je, [intsize])                   ! make what into an integer array
    content(pos + 1 : pos + intsize) = je(1 : intsize)      ! insert into data portion of jar
    j % top = pos + intsize                                 ! update top of jar position
    sz = j % top                                            ! number of elements in jar

  end function add_to_jar

  function get_from_jar(j, what, size, where) result(sz)    ! get data grom data jar
    implicit none
    class(jar), intent(INOUT) :: j                          ! the data jar
    WHAT_TYPE, intent(IN), value :: what                    ! must match type of loc(xxx)
    integer, intent(IN), value :: size                      ! size to insert = storage_size(item) * nb_of_items
    integer, intent(IN), optional, value :: where           ! optional argument to force insertion point (1 = start of jar)
    integer :: sz                                           ! position of last extracted element (-1 if error)

    integer :: intsize, pos
    type(C_PTR) :: temp
    integer(JAR_ELEMENT), dimension(:), pointer :: je, content

    sz = -1
    if(.not. C_ASSOCIATED(j % p)) return

    call C_F_POINTER(j % p, content, [j % size])            ! pointer to jar data
    if( present(where) ) then
      pos = where - 1                                       ! insert at position "where"
    else
      pos = j%bot                                           ! insert after data currently in jar
    endif
    if(pos < 0) return                                      ! invalid insertion position

    intsize = size / storage_size(content(1))
    if(pos + intsize > j%top) return                        ! insufficient data in jar

    temp    = transfer(what,temp)                           ! address of data to insert
    call C_F_POINTER(temp, je, [intsize])                   ! make what into an integer array
    je(1 : intsize) = content(pos + 1 : pos + intsize)      ! insert into data portion of jar
    j % bot = pos + intsize                                 ! update top of jar position
    sz = j % bot                                            ! position of last extracted element

  end function get_from_jar

end module

#if defined(SELF_TEST)
#include <serializer.hf>
program test_jar
  implicit none
  call test_free
  call test_pickling
end program

subroutine test_pickling
  use data_serialize
  implicit none
  logical :: old_debug
  JAR_DECLARE(my_jar)
  integer :: ok, ne, i
  type :: machin1
    integer, dimension(3) :: ip
    integer               :: date
    character(len=4)      :: nom
    character(len=2)      :: typ
    character(len=2)      :: tmp
  end type
  type(machin1) :: a1, x1

  type :: machin2
    integer               :: l1
    integer               :: l2
    character(len=2)      :: unit
  end type
  type(machin2), dimension(4) :: a2, x2

  print *,'==========================================================='

  ne = JARSPACE_ITEM(a1)
  print *,'each machin1 scalar item will use',ne,' cells in jar'
  ne = JARSPACE_ITEM(a2)
  print *,'a machin2 array / array section will use',ne,' cells per array element in jar'

  old_debug = my_jar%debug(.true.)
  if(.not. JAR_VALID(my_jar)) print *,'SUCCESS: jar not valid before creation'
  if(JAR_VALID(my_jar))       print *,'ERROR:   jar is valid before creation'
  ok = JAR_CREATE(my_jar, 4096)
  if(JAR_VALID(my_jar)) print *,'jar created successfully'
  print 1,'(test_pickling) my_jar : ok, size, avail =',ok, my_jar%usable(), my_jar%avail()

  a1 = machin1([0,1,2],-123456,'xxxx','yy','zz')
  ne = JAR_PUT_SINGLE(my_jar, a1)
  print 1,'(test_pickling) my_jar : ne, size, avail =',ne, my_jar%usable(), my_jar%avail()

  do i = 1, size(a2)
    a2(i) = machin2(500+i, 600+i, 'X_')
  enddo
  ne = JAR_PUT_MULTI(my_jar, a2(2:3))
  print 1,'(test_pickling) my_jar : ne, size, avail =',ne, my_jar%usable(), my_jar%avail()

  x1 = machin1([-1,-1,-1],999999,'    ','  ','  ')
  ne = JAR_GET_SINGLE(my_jar, x1)
  print *,a1
  print *,x1
  print 1,'(test_pickling) my_jar : ne, size, avail =',ne, my_jar%usable(), my_jar%avail()
  x2 = machin2(-1, -1, '**')
  ne = JAR_GET_MULTI(my_jar, x2(1:2))
  print *,a2(2)
  print *,x2(1)
  print *,a2(3)
  print *,x2(2)
  print 1,'(test_pickling) my_jar : ne, size, avail =',ne, my_jar%usable(), my_jar%avail()
  call my_jar%print(15)

  JAR_REWIND(my_jar)

  print *,''
  JAR_RESET(my_jar)
  call my_jar%print(20)
  print 1,'(test_pickling) my_jar reset : size, avail =',my_jar%usable(), my_jar%avail()
  ne = JAR_PUT_SINGLE_AT(my_jar, a1, 2)                        ! skip one position, start injectiong at 2 rather than 1
  print 1,'(test_pickling) my_jar : ne, size, avail =',ne, my_jar%usable(), my_jar%avail()
  call my_jar%print(20)
  ne = JAR_PUT_MULTI_AT(my_jar, a2(2:4), my_jar%stored()+2)    ! skip one position, start at top + 2
  print 1,'(test_pickling) my_jar : ne, size, avail =',ne, my_jar%usable(), my_jar%avail()
  call my_jar%print(20)

  x1 = machin1([-1,-1,-1],999999,'    ','  ','  ')
  ne = JAR_GET_SINGLE_AT(my_jar, x1, 2)                        ! skip one position, start injectiong at 2 rather than 1
  print *,a1
  print *,x1
  x2 = machin2(-1, -1, '**')
  ne = JAR_GET_MULTI_AT(my_jar, x2(1:2), ne+2)                 ! skip one position, start at bot + 2 rather than bot +1
  print *,a2(2)
  print *,x2(1)
  print *,a2(3)
  print *,x2(2)
  ne = JAR_GET_MULTI(my_jar, x2(3:3))
  print *,a2(4)
  print *,x2(3)

  ok = JAR_FREE(my_jar)
  if(.not. JAR_VALID(my_jar)) print *,'SUCCESS: jar is not valid after free'
  if(JAR_VALID(my_jar))       print *,'ERROR:   jar is valid after free'

1 format(A,10I8)
end subroutine test_pickling

subroutine test_free
  use data_serialize
  implicit none
  type(jar) :: myjar
  logical :: old_debug, scrap
  integer :: ok

  old_debug = myjar%debug(.false.)
  print *,'old debug mode =',old_debug
  old_debug = myjar%debug(.true.)
  print *,'old debug mode =',old_debug
  scrap = myjar%debug(.true.)
  print *,'old debug mode =',scrap
  print *,''

  ok = myjar%new(640)
  print *,'(test_free) myjar%new() =',ok, myjar%usable()
  ok = myjar%free()
  if(ok .eq. 0) print *,'INFO: jar datafreed'
  ok = myjar%free()
  if(ok .ne. 0) print *,'WARNING: jar data already freed'
  print *,''

  call test_jar_1
  print *,''
  old_debug = myjar%debug(.false.)
  print *,''
  print *,'test_free: old debug mode =',old_debug
  old_debug = myjar%debug(.false.)
  print *,'test_free: old debug mode =',old_debug
  
end subroutine test_free

subroutine test_jar_1
  use data_serialize
  implicit none
  integer :: ok
  type(jar) :: myjar

  ok = myjar%new(1280)
  call test_jar_finalize
  print *,''

  block
    type(jar) :: myjar          ! overrrides myjar in main scope
    ok = myjar%new(128)
    print *,'myjar%new() =',ok, myjar%usable()
!     ok = myjar%free()
    print *,'jar going out of scope in block'
  end block
  print *,''

!   print *,'freeing jar at end of test_jar_1, capacity =',myjar%usable()
!   ok = myjar%free()
!   if(ok .ne. 0) print *,'WARNING: jar data already freed'
  print *,'myjar%new() test_jar_1 =',ok, myjar%usable()
  print *,'jar going out of scope in test_jar_1'
end subroutine test_jar_1

subroutine test_jar_finalize
  use data_serialize
  implicit none
  type(jar) :: myjar
  integer :: ok

  ok = myjar%new(1024)
  print *,'myjar%new(1024) =',ok, myjar%usable()
  if(myjar%usable() .ne. 1024) print *,'unexpected size'
  print *,'jar going out of scope in subroutine test_jar_finalize'
end subroutine test_jar_finalize
#endif
