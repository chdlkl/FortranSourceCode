Module test
  Contains
  !//������ת��ֱ������
  Function Spheral2xyz( lon,lat,r ) Result(res)
    Implicit none
    Real :: lon, lat, r
    Real :: x, y, z, h, res(3)  !//ע�⣬�˴��ķ���ֵ���Զ���Ϊ����
    h = r * cosd(lat)
    x = h * cosd(lon)
    y = h * sind(lon)
    z = r * sind(lat)
    res = [x,y,z]
  End Function Spheral2xyz
End Module test
  
Program main
  Use test
  Real :: xyz(3)
  xyz = Spheral2xyz( 30.0,90.0,1.0 )  !//��ͬ����״��������շ���ֵ
  !//����ֵ�ͽ��շ���ֵ��������״����һ�£����Ͳ�һ��һ��
  Write(*,*) xyz
End Program main