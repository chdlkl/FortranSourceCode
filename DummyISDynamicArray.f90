!// 形参为动态数组，需要写接口interface或用模块module
Module mod
  Implicit none 
Contains 
Subroutine sub ( b )
  Implicit none 
  Integer, allocatable :: b(:)
  Integer :: i
  
  Allocate( b(5) )
  Do i = 1, size(b)
    b(i) = i 
  End do 
  Print*, b
  Deallocate( b )
  
End subroutine sub
End Module mod
  
  
Program main
  Use mod
  Implicit none 
  Integer, allocatable :: a(:)
  call sub ( a )
End program main