!// �����������͹�������
Program TestAssociate
  Implicit none
  Type :: trackfile
    Real :: target_x
    Real :: target_y
  End type trackfile
  Real :: z
  
  Type( trackfile ) :: dist
  dist = trackfile( 3., 4. )
  !// associate�ṹ�����Ǳز����ٵģ����������ڼ򻯺�ͻ���㷨���а���
  Associate ( x => dist%target_x, &
              y => dist%target_y)
    z = sqrt( x**2 + y**2 )  !// ��x,y��ص�ִ�����Ҫ����associate��end associate���֮�䣬����x,y���ö���
  End associate
  Write(*,*) z
End program TestAssociate