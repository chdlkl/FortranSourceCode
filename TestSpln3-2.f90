!// 三次样条插值测试代码
Program main
  Implicit none
  Integer :: i
  Integer, parameter :: n1 = 51, n2 = 101
  Real(kind=8) :: dy1 = 0.d0, dyn = 0.d0, t
  Real(kind=8) :: x1(n1), y1(n1), h(n1), x2(n2), y2(n2)
  Real(kind=8) :: dy(n1), ddy(n1), ds(n2), dds(n2)
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
  call spln( x1, y1, n1, dy1, dyn, x2, y2, n2, dy, ddy, ds, dds, t, h )
  
  Open ( 101, file = 'Interpolation.dat' )
  Do i = 1, n2
    Write( 101,* ), x2(i), y2(i)
  End do
  Close ( 101 )
End program main
  
  
Subroutine spln(x, y, n, dy1, dyn, xx, s, m, dy, ddy, ds, dds, t, h)
  Integer :: n, m
  Real(kind=8) :: x(n), y(n), xx(m), dy(n), ddy(n)
  Real(kind=8) :: s(m), ds(m), dds(m), h(n)
  Real(kind=8) :: dy1, dyn, t, h0, h1, beta, alpha
  
  dy(1) = 0d0
  h(1) = dy1
  h0 = x(2) - x(1)
  
  Do j = 2, n - 1
    h1 = x(j+1) - x(j)
    alpha = h0 / (h0+h1)
    beta = (1d0-alpha) * ( y(j)-y(j-1) ) / h0
    beta = 3d0 * ( beta + alpha*( y(j+1)-y(j) ) / h1 )
    dy(j) = -alpha / ( 2d0 + ( 1d0-alpha ) * dy(j-1) )
    h(j) = ( beta - (1d0-alpha) * h(j-1) )
    h(j) = h(j) / ( 2d0 + (1d0-alpha) * dy(j-1) )
    h0 = h1
  End Do
  
  dy(n) = dyn
  Do j = n - 1, 1, -1
    dy(j) = dy(j)*dy(j+1) + h(j)
  End Do
  
  Do j = 1, n - 1
    h(j) = x(j+1) - x(j)
  End Do
  
  Do j = 1, n - 1
    h1 = h(j)*h(j)
    ddy(j) = 6d0*( y(j+1)-y(j) ) / h1 - 2d0*( 2d0*dy(j) + dy(j+1) ) / h(j)
  End Do
  
  h1 = h(n-1)*h(n-1)
  ddy(n) = 6d0*( y(n-1)-y(n) ) / h1 + 2d0*( 2d0*dy(n) + dy(n-1) ) / h(n-1)
  t = 0d0
  Do i = 1, n - 1
    h1 = 0.5*h(i)*( y(i)+ y(i+1) )
    h1 = h1 - h(i)*h(i)*h(i)*( ddy(i) + ddy(i+1) ) / 24d0
    t = t + h1
  End Do
  
  outdo: Do j = 1, m
    outif: If ( xx(j)>=x(n) ) Then
      i = n - 1
    Else
      i = 1
      indo: Do
        inif: If ( xx(j)>x(i+1) ) Then
          i = i + 1
        Else
          Exit
        End If inif
      End Do indo
    End If outif 
    h1 = ( x(i+1) - xx(j) ) / h(i)
    s(j) = ( 3d0*h1*h1 - 2d0*h1*h1*h1 ) * y(i)
    s(j) = s(j) + h(i) * ( h1*h1-h1*h1*h1 ) * dy(i)
    ds(j) = 6d0*( h1*h1-h1 ) * y(i) / h(i)
    ds(j) = ds(j) + ( 3d0*h1*h1 - 2d0*h1 ) * dy(i)
    dds(j) = ( 6d0-12d0*h1 ) * y(i) / ( h(i)*h(i) )
    dds(j) = dds(j) + ( 2d0-6d0*h1 ) * dy(i) / h(i)
    h1 = ( xx(j)-x(i) ) / h(i)
    s(j) = s(j) + ( 3d0*h1*h1-2d0*h1*h1*h1 ) * y(i+1)
    s(j) = s(j) - h(i) * ( h1*h1 - h1*h1*h1 ) * dy(i+1)
    ds(j) = ds(j) - 6d0 * ( h1*h1-h1 ) * y(i+1) / h(i)
    ds(j) = ds(j) + ( 3d0*h1*h1 - 2d0*h1 ) * dy(i+1)
    dds(j) = dds(j) + ( 6d0 - 12d0*h1 ) * y(i+1) / ( h(i)*h(i) )
    dds(j) = dds(j) - ( 2d0 - 6d0*h1 ) * dy(i+1) / h(i)
  End Do outdo
End Subroutine spln
