Program main
  Implicit none
  Integer, parameter :: n = 2048
  real(kind=8), parameter :: f1 = 100.0, f2 =300.0, pi = acos(-1.d0)
  real(kind=8), parameter :: t1 = 0.d0, t2 = 0.2d0
  real(kind=8) :: t, s
  Integer :: i, fileid
  
  open( newunit = fileid, file = 'shuju.dat' )
  Do i = 1, n
    t = t1 + dble(i-1) * ( t2 - t1 ) / dble( n - 1 )
    s = sin(2.d0*pi*f1*t) + sin(2.d0*pi*f2*t)
    write( fileid, * ) t, s
  End do
  close( fileid )
  
End program main