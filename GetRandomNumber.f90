Program RandomNumber
  Implicit None
  real :: x(3) = 0.0 , y(2) = 0.0 , a = 0.0
  integer :: i
  call random_seed() !// 只调用一次，读者可尝试去掉此行并多次运行
  Do i = 1 , size(x)
    call random_number( x(i) ) !// 可以循环调用
  End Do
  call random_number(y) !// 也可以一次生成一个数组
  call random_number(a) !// 单变量也可以
  write(*,*) a
  write(*,*) x
  write(*,*) y
End Program RandomNumber