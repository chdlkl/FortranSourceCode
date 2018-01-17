Module test
  Contains
  !//球坐标转换直角坐标
  Function Spheral2xyz( lon,lat,r ) Result(res)
    Implicit none
    Real :: lon, lat, r
    Real :: x, y, z, h, res(3)  !//注意，此处的返回值可以定义为数组
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
  xyz = Spheral2xyz( 30.0,90.0,1.0 )  !//用同样形状的数组接收返回值
  !//返回值和接收返回值的数组形状必须一致，类型不一定一致
  Write(*,*) xyz
End Program main