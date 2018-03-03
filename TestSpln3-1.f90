!// 三次样条插值测试代码
Program main
  Implicit none
  Integer :: i
  Integer, parameter :: n1 = 51, n2 = 101
  Real(kind=8) :: x1(n1), y1(n1), x2(n2), y2(n2)
  Real, parameter :: t0 = 0.d0, t1 = 1.d0
  
  Open ( 101, file = 'origin.dat' )
  Do i = 1, n1
    x1(i) = ( i-1 ) * ( t1-t0 ) / ( n1-1 )
    y1(i) = exp( x1(i) )
    Write( 101,* ) x1(i), y1(i)
  End do
  Close ( 101 )
  
  Do i = 1, n2
    x2(i) = ( i-1 ) * ( t1-t0 ) / ( n2-1 )
  End do
  
  Open ( 101, file = 'Interpolation.dat' )
  Do i = 1, n2
    call spln( x1, y1, n1, x2(i), y2(i), 1, n1-1, n1-2 )
    Write( 101,* ) x2(i), y2(i)
  End do
  Close ( 101 )
End program main
  
Subroutine spln( x, y, n, t, f, m, n1, n2 )
  Implicit none
  Integer :: i, j, k, m, n, n1, n2
  Real(kind=8) :: x(n), y(n), t(m), f(m), f1(m)
  Real(kind=8) :: s2(n), h(n1), dy(n1), s(n1), e(n2)
  Real(kind=8) :: z, h1, h2, h3, h4
  
  Do i = 1, n1
    h(i) = x(i+1) - x(i)
    dy(i) = ( y(i+1) - y(i) ) / h(i)
  End do
  s2(1) = 0.d0; s2(n) = 0.d0
  Do i = 2, n1
    s2(i) = 6.d0 * ( dy(i) - dy(i-1) )
  End do
  z = 0.5d0 / ( h(1) + h(2) )
  s(1) = -h(2) * z
  e(1) = s2(2) * z
  Do i = 2, n2
    k = i - 1
    j = i + 1
    z = 1.d0 / ( 2.d0*( h(i)+h(j) ) + h(i)*s(k) )
    s(i) = -h(j) * z
    e(i) = ( s2(j)-h(i)*e(k) ) * z
  End do
  s2(n1) = e(n2)
  Do i = n2, 2, -1
    k = i - 1
    s2(i) = s(k)*s2(i+1) + e(k)
  End do
  Do i = 1, n1
    s(i) = ( s2(i+1) - s2(i) ) / h(i)
  End do
  i = 2
  k = 1
  Do j = 1, m
    do 
      If ( t(j) > x(i) ) then
        k = i
        i = i + 1
      else
        exit
      End if
    End do
    h1 = t(j) - x(k)
    h2 = t(j) - x(i)
    h3 = h1 * h2
    h4 = s2(k) + h1*s(k)
    z = ( s2(i) + s2(k) + h4 ) / 6.d0
    f(j) = y(k) + h1*dy(k) + h3*Z
    f1(j) = dy(k) + z*( h1+h2 ) + h3 * s(k) / 6.d0
  End do
End subroutine spln