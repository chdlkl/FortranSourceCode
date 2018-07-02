Program main
  implicit none
  integer :: i
  
  do i = 1, 10
    call sub()
  end do
  
End program main
  
Subroutine sub()
  implicit none
  integer, allocatable, save :: a(:)
  
  if ( .not. allocated(a) ) then
    allocate( a(2) )
    a = 1
  end if
  print*, a
  a = a + 1

End subroutine sub