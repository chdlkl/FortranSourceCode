Subroutine spln3(x, y, n, xx, s, m)
  Integer :: n, m
  Real(kind=8) :: x(n), y(n), xx(m), dy(n), ddy(n)
  Real(kind=8) :: s(m), ds(m), dds(m), h(n)
  Real(kind=8) :: dy1, dyn, t, h0, h1, beta, alpha
  
  dy1 = 0.d0; dyn = 0.d0
  
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
End Subroutine spln3