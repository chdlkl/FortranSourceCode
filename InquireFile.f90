!//��ѯ�ļ�״̬
Program Main
  Implicit None
  Character(len=20) :: Filename="test.txt"
  Logical :: alive
  
  Inquire(File=Filename,Exist=alive)  !//Exist����ļ��Ƿ���ڣ��᷵��һ������������������߼�������������ֵ��ʾ�ļ����ڣ����ؼ�ֵ��ʾ�ļ�������
  If(alive) Then
    Write(*,*) Trim(Filename)," ","Exist."
  Else 
    Write(*,*) Trim(Filename)," ","doesn't exist."
  End If
  
End Program main