Program main
  Implicit None
  Integer :: info, i = 0
  Integer, Allocatable :: a(:)
  Logical :: loc
  !-----------------test allocate-------------------!
  Allocate( a(10),stat = info )  !//�˴�Ϊstat,infoΪ���ͱ���
  If( info == 0 ) Then
    write(*,*) 'Allocate successfully!'
  Else
    write(*,*) 'Allocate faile!'
  End If
  !-----------------test inquire--------------------!
  Inquire( file = '19��.pdf',Exist = loc )  !//�˴�ΪExist,locΪ�߼�����
  If( loc ) write(*,*) '19��.pdf���ڣ�'
  !-----------------test iostat---------------------!
  Open( 101, file = 'test.dat' )
  Do 
    Read(101,*,iostat = info)  !//iostat������read�����,������open���
    If( info/=0 ) Exit   
    i = i + 1
  End Do
  write(*,*) i
  Close(101)
End Program main


