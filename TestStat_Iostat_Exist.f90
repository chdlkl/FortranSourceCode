Program main
  Implicit None
  Integer :: info, i = 0
  Integer, Allocatable :: a(:)
  Logical :: loc
  !-----------------test allocate-------------------!
  Allocate( a(10),stat = info )  !//此处为stat,info为整型变量
  If( info == 0 ) Then
    write(*,*) 'Allocate successfully!'
  Else
    write(*,*) 'Allocate faile!'
  End If
  !-----------------test inquire--------------------!
  Inquire( file = '19班.pdf',Exist = loc )  !//此处为Exist,loc为逻辑变量
  If( loc ) write(*,*) '19班.pdf存在！'
  !-----------------test iostat---------------------!
  Open( 101, file = 'test.dat' )
  Do 
    Read(101,*,iostat = info)  !//iostat出现在read语句中,而不是open语句
    If( info/=0 ) Exit   
    i = i + 1
  End Do
  write(*,*) i
  Close(101)
End Program main


