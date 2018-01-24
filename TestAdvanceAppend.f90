Program main
  Implicit None
  Integer i
  
  Open(10,file='shujv.txt',status='unknown')
  Do i=1,10
    !Write(10,'(I3)',advance='no') i  !//本应该按列输出的数据输出成为一行
    Write( 10, '(I3\)' ) i  !// 这两句等效
  End Do
  Write(10,'(/,\)')
  Write(10,*) "luk"
  Write(10,*) "luk"
  Close(10)
  
  Open(10,file='shujv.txt',status='unknown',position='append')
  Write(10,*) 'lkl'
  Close(10)
End Program main