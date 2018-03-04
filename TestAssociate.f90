!// 派生数据类型关联测试
Program TestAssociate
  Implicit none
  Type :: trackfile
    Real :: target_x
    Real :: target_y
  End type trackfile
  Real :: z
  
  Type( trackfile ) :: dist
  dist = trackfile( 3., 4. )
  !// associate结构并不是必不可少的，但是它对于简化和突出算法很有帮助
  Associate ( x => dist%target_x, &
              y => dist%target_y)
    z = sqrt( x**2 + y**2 )  !// 与x,y相关的执行语句要放在associate与end associate语句之间，并且x,y不用定义
  End associate
  Write(*,*) z
End program TestAssociate