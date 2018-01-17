Program main 
  Include 'link_fnl_shared.h'
  Use CSIEZ_INT
  Implicit none 
  Integer, parameter :: NDATA = 10, N = 20 
  Real :: XDATA(NDATA), FDATA(NDATA) 
  Real :: XVEC(N), VALUE(N) 
  Real, parameter :: xmin = -5.0, xmax = 5.0 
  Real :: xinc, xp 
  Integer :: i 
  
  xinc = (xmax-xmin) / (NDATA-1) 
  xp = xmin 
  Do i = 1, NDATA 
    XDATA(I) = xp 
    FDATA(I) = sin( XDATA(I) ) 
    xp = xp + xinc 
  End do 
  xinc = (xmax-xmin) / (N-1) 
  xp = xmin 
  Do i = 1, N 
    XVEC(I) = xp 
    xp = xp + xinc 
  End do 
  ! 做插值 
  call CSIEZ (XDATA, FDATA, XVEC, VALUE) 
  ! 输出插值结果 
  Do i=1, N 
    Write(*,*) XVEC(i), VALUE(i), sin( XVEC(i) ) 
  End do 
  Read*
End program