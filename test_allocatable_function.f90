Module test_module
  implicit none
contains
  function test_alloc_fun( n )
    implicit none
    integer, intent(in) :: n
    real, allocatable :: test_alloc_fun(:)
    integer :: i
    
    If ( allocated( test_alloc_fun ) ) then
      write( *,'(1x,a)' ) 'Array has already been allocated!'
    else
      allocate( test_alloc_fun(n) )
    End if
    
    Do i = 1, n
      test_alloc_fun(i) = i
    End do
    
  end function test_alloc_fun
  
End module test_module
  
  
Program main
  use test_module
  implicit none
  integer :: n = 5
  write( *,'(1x,a,20F4.1)' ) 'Allocatable Function is:', test_alloc_fun(n)
End program main