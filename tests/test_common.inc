interface
   subroutine c_exit(code) bind(C,name='exit')
      import :: C_INT
      integer(C_INT), intent(IN), value :: code
   end subroutine c_exit
end interface
