Program TestSgetrf
  use lapack95
  Implicit none
  Integer, parameter :: n = 3
  Integer :: ipiv(n)
  Integer :: i, j, info
  Real(kind=8) :: work(n), a(n,n), aa(n,n), tmp(n,n)
  
  tmp = reshape([3.,-1.,-1.,4.,-2.,-1.,-3.,2.,1.],[n,n] )
  a = transpose(tmp)
  aa = a
  
  write(*,'(1x,a)') "a = "
  Do i = 1, n
    write(*,'(*(f12.6,3x))') a(i,:)
  End do
  
  !// 使用lapack求解逆矩阵
  call getrf( a, ipiv, info )
  call getri( a, ipiv, info )
  
  write(*,'(1x,a)') 'inv(a) = '
  Do i = 1, n
    write(*,'(*(f12.6,3x))') a(i,:)
  End do
  
  write(*,'(1x,a)') "checking..."
  aa = matmul(aa,a)
  Do i = 1, n
    write(*,'(*(f12.6,3x))') aa(i,:)
  End do

End program TestSgetrf