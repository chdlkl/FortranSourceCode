!// 派生类型数据扩展
Program TypeExpansion
  Implicit none
  Type :: point 
    Real :: x
    Real :: y
  End type point
  
  Type, extends(point) :: point3d
    Real :: z
  End type point3d
  Type( point3d ) :: p
  
  p%x = 1.
  p%y = 2.
  p%z = 3.
  Write(*,*) p
End program TypeExpansion