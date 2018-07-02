Module m
  implicit none
contains
  Elemental real function fun1( a1, b1 ) result( res )  !.. 逐元函数
    implicit none
    real, intent(in) :: a1  !.. 此处不管a，b是标量还是数组，只写传递变量的名称
    real, intent(in) :: b1
    res = a1 + b1
  End function fun1
  
  Elemental subroutine sub1( a1, b1, c1 )  !.. 逐元子程序
    implicit none
    real, intent(in) :: a1  
    real, intent(in) :: b1
    real, intent(inout) :: c1
    c1 = a1 - b1
  End subroutine sub1
  
  Pure function fun2( a2, b2 ) result( res )  !.. 纯函数
    implicit none
    real, intent(in) :: a2(:,:)  !.. 纯函数中要声明数组的形状
    real, intent(in) :: b2(:,:)
    real :: res
    res = sum(a2) - sum(b2)  
  End function fun2
  
  Pure subroutine sub2( a2, b2, c2 )  !.. 纯子程序
    implicit none
    real, intent(in) :: a2(:,:)    !.. 不管是纯函数还是纯子程序都要声明数组的形状
    real, intent(in) :: b2(:,:)
    real, intent(inout) :: c2(:,:)
    c2 = a2 * b2
  End subroutine sub2
    
End module m
  
  
Program test_elemental
  use m
  implicit none
  real :: a(2,2) = [ 1., 2., 3., 4. ]  !.. 此处a，b可以是标量，也可以是数组
  real :: b(2,2) = [ 2., 3., 4., 5. ]
  real :: c(2,2), d
  
  c = fun1( a, b )
  print*, c
  call sub1( a, b, c )
  print*, c
  d = fun2( a, b )  !.. 实验表明，纯函数好像只能返回一个数值
  print*, d
  call sub2( a, b, c )
  print*, c
End program test_elemental