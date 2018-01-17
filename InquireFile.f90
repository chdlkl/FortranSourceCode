!//查询文件状态
Program Main
  Implicit None
  Character(len=20) :: Filename="test.txt"
  Logical :: alive
  
  Inquire(File=Filename,Exist=alive)  !//Exist检查文件是否存在，会返回一个布尔变量给后面的逻辑变量，返回真值表示文件存在，返回假值表示文件不存在
  If(alive) Then
    Write(*,*) Trim(Filename)," ","Exist."
  Else 
    Write(*,*) Trim(Filename)," ","doesn't exist."
  End If
  
End Program main