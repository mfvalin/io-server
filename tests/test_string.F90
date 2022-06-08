module test_string_mod
  implicit none
  contains

  subroutine do_stuff_with_string(string)
    implicit none
    character(len=*), intent(in) :: string
    character(len=:), allocatable :: copy
    copy = string
    print *, 'storage size of copy: ', storage_size(copy), storage_size(string)
  end subroutine do_stuff_with_string
end module test_string_mod

program test_string
  use test_string_mod
  implicit none
  character(len=16) :: c
  integer :: i
  c = 'abcdefghi'
  print *, storage_size(c)
  print *, storage_size(c(1:15))
  print *, storage_size(c(1:16))

  i = 5
  print *, storage_size(c(2:i))

  do i = 1, 16
    print '(A, A, 1X, I4)', c(1:i), ', storage size = ', storage_size(c(1:i))
    call do_stuff_with_string(c(1:i))
  end do
end program test_string

