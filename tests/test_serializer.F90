!  serializer test
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

! test extended macros
#define JAR_EXTENDED_MACROS 1
! include serializer macros
#include <serializer.hf>

program test_jar
  implicit none
  call test_free
  call test_pickling
end program

module machins
  type :: machin1
    integer, dimension(3) :: ip
    integer               :: date
    character(len=4)      :: nom
    character(len=2)      :: typ
    character(len=2)      :: tmp
  end type
  type :: machin2
    integer               :: l1
    integer               :: l2
    character(len=2)      :: unit
  end type

  interface same_machin
    module procedure same_machin1
    module procedure same_machin2
  end interface

 contains
  function same_machin1(a, b) result(ok)
    implicit none
    type(machin1), intent(IN) :: a,b
    logical :: ok
    ok = all(a%ip == b%ip) .and. a%date == b%date .and. a%nom == b%nom .and. a%typ == b%typ .and. a%tmp == b%tmp
    if(ok) then
      print *,'SUCCESS 1'
    else
      print *,'ERROR 1'
    endif
  end function same_machin1
  function same_machin2(a, b) result(ok)
    implicit none
    type(machin2), intent(IN) :: a,b
    logical :: ok
    ok = a%l1 == b%l1 .and. a%l2 == b%l2 .and. a%unit == b%unit
    if(ok) then
      print *,'SUCCESS 2'
    else
      print *,'ERROR 2'
    endif
  end function same_machin2
end module

subroutine test_pickling
  use data_serialize
  use machins
  implicit none
  logical :: old_debug
  JAR_DECLARE(my_jar)
  integer :: ok, ne, i
  type(machin1) :: a1, x1
  type(machin2), dimension(4) :: a2, x2
  integer, dimension(:), pointer :: blind_array
  ! type(C_PTR) :: c_blind_array
  logical :: success

  print *,'==========================================================='

  ne = JAR_ELEM_COUNT(a1)
  print *,'each machin1 scalar item will use',ne,' cells in jar'
  ne = JAR_ELEM_COUNT(a2)
  print *,'a machin2 array / array section will use',ne,' cells per array element in jar'

  old_debug = my_jar%debug(.true.)
  if(.not. JAR_VALID(my_jar)) print *,'SUCCESS: jar not valid before creation'
  if(JAR_VALID(my_jar))       print *,'ERROR:   jar is valid before creation'
  ok = JAR_CREATE(my_jar, 4096)
  if(JAR_VALID(my_jar)) print *,'jar created successfully'
  print 1,'(test_pickling) my_jar : ok, size, avail =',ok, my_jar%usable(), my_jar%avail()

  a1 = machin1([0,1,2],-123456,'xxxx','yy','zz')
  ne = JAR_PUT_ITEM(my_jar, a1)
  print 1,'(test_pickling) my_jar : ne, size, avail =',ne, my_jar%usable(), my_jar%avail()

  do i = 1, size(a2)
    a2(i) = machin2(500+i, 600+i, 'X_')
  enddo
  ne = JAR_PUT_ITEMS(my_jar, a2(2:3))
  print 1,'(test_pickling) my_jar : ne, size, avail =',ne, my_jar%usable(), my_jar%avail()

  JAR_DATA(my_jar, blind_array)
  print 2,'before get        ',blind_array(my_jar%low()+1:my_jar%high()),-1
  x1 = machin1([-1,-1,-1],999999,'    ','  ','  ')
  ne = JAR_GET_ITEM(my_jar, x1)
  print *,'       ',a1
  print *,'x1    =',x1
  success = same_machin(a1,x1)
  print 1,'(test_pickling) my_jar : ne, size, avail =',ne, my_jar%usable(), my_jar%avail()
  print 2,'after get #1        ',blind_array(my_jar%low()+1:my_jar%high()),-1
  x2 = machin2(-1, -1, '**')
  ne = JAR_GET_ITEMS(my_jar, x2(1:2))
  print *,'       ',a2(2)
  print *,'x2(1) =',x2(1)
  print *,'       ',a2(3)
  print *,'x2(2) =',x2(2)
  success = success .and. same_machin(a2(2),x2(1)) .and. same_machin(a2(3),x2(2))
  print 1,'(test_pickling) my_jar : ne, size, avail =',ne, my_jar%usable(), my_jar%avail()
  print 2,'after get #2        ',blind_array(my_jar%low()+1:my_jar%high()),-1
  call my_jar%print(15)

  JAR_REWIND(my_jar)

  ! disguise jar as integer array, test that it can be regenerated after pass-through
  ! here -> pass_through -> level2
  ! check that finalize does not release jas data space
!   blind_array => my_jar%array()     ! get Fortran pointer to jar contents
  JAR_DATA(my_jar, blind_array)
  print *,'DIAG: before pass_through, blind_array size is',size(blind_array)
  call pass_through(blind_array, size(blind_array))  ! send integer array and its dimension
  print *,'DIAG: after pass_through'

  print *,''
  JAR_RESET(my_jar)
  call my_jar%print(20)
  print 1,'(test_pickling) my_jar reset : size, avail =',my_jar%usable(), my_jar%avail()
  ne = JAR_PUT_ITEM_AT(my_jar, a1, 2)                        ! skip one position, start injectiong at 2 rather than 1
!   ne = my_jar%put( a1, storage_size(a1), where=2 )
  print 1,'(test_pickling) my_jar : ne, size, avail =',ne, my_jar%usable(), my_jar%avail()
  call my_jar%print(20)
  ne = JAR_PUT_ITEMS_AT(my_jar, a2(2:4), my_jar%high()+2)    ! skip one position, start at top + 2
!   ne = my_jar%put(a2(2:4), storage_size(a2(2:4))*size(a2(2:4)), where=my_jar%high()+2 )
  print 1,'(test_pickling) my_jar : ne, size, avail =',ne, my_jar%usable(), my_jar%avail()
  call my_jar%print(20)

  print 2,'before get        ',blind_array(my_jar%low()+1:my_jar%high()),-1
  x1 = machin1([-1,-1,-1],999999,'    ','  ','  ')
  ne = JAR_GET_ITEM_AT(my_jar, x1, 2)                        ! skip one position, start injectiong at 2 rather than 1
  success = success .and. same_machin(a1,x1)
!   ne = my_jar%get( x1, storage_size(x1), where=2 )
  print *,'         ',a1
  print *,'x1      =',x1
  print 2,'after get #1        ',blind_array(my_jar%low()+1:my_jar%high()),-1
  x2 = machin2(-1, -1, '**')
  ne = JAR_GET_ITEMS_AT(my_jar, x2(1:2), ne+2)                 ! skip one position, start at bot + 2 rather than bot +1
  success = success .and. same_machin(a2(2),x2(1)) .and. same_machin(a2(3),x2(2))
!   ne = my_jar%get(x2(1:2), storage_size(x2(1:2))*size(x2(1:2)), where=ne+2 )
  print *,'       ',a2(2)
  print *,'x2(1) =',x2(1)
  print *,'       ',a2(3)
  print *,'x2(2) =',x2(2)
  print 2,'after get #2        ',blind_array(my_jar%low()+1:my_jar%high()),-1
  ne = JAR_GET_ITEM(my_jar, x2(3:3))
  success = success .and. same_machin(a2(4),x2(3))
  print *,'       ',a2(4)
  print *,'x2(3) =',x2(3)
  print 2,'after get #3        ',blind_array(my_jar%low()+1:my_jar%high()),-1

  ok = JAR_FREE(my_jar)
  if(.not. JAR_VALID(my_jar)) print *,'SUCCESS: jar is not valid after free'
  if(JAR_VALID(my_jar))       print *,'ERROR:   jar is valid after free'

  if(success) then
    print *,'============= TEST is a SUCCESS ============='
    return
  else
    print *,'============= ERRORS detected ============='
    error stop 1
  endif

1 format(A,10I8)
2 format(A15,30Z9.8)

  stop
end subroutine test_pickling

subroutine pass_through(blind_array, n)    !  integer array inbound, jar outbound
  use data_serialize
  implicit none
  interface
    subroutine level2(my_jar)
    import :: jar
    type(jar), intent(INOUT) :: my_jar
    end subroutine level2
  end interface
  integer, intent(IN) :: n
  integer, dimension(n), intent(IN) :: blind_array
  type(jar) :: my_jar
  integer :: ok
  print *,'DIAG(pass_through) :blind_array size is',size(blind_array)
  ok = my_jar%shape(blind_array,size(blind_array))      ! jar data pointing to incoming integer array
  call level2(my_jar)                                   ! pass jar down
end subroutine pass_through

subroutine level2(my_jar)    ! receives jar, recreated from integer array by pass_through
  use data_serialize
  use machins
  implicit none
  type(jar), intent(INOUT) :: my_jar
  type(machin1) :: x1
  type(machin2), dimension(4) :: x2
  integer :: ne

  print *,'DIAG(level2) :'
  call my_jar%print(20)
  ne = JAR_GET_ITEM(my_jar, x1)
  print *,'x1    =',x1
  ne = JAR_GET_ITEMS(my_jar, x2(1:2))
  print *,'x2(1) =',x2(1)
  print *,'x2(2) =',x2(2)
  print *,'DIAG(level2) exiting'

end subroutine level2

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
