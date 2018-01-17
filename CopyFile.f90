Program CopyFile
  Implicit none 
  Integer :: i
  Integer :: startfile, stopfile
  Character(len=80) :: str = '', filename
  
  Write ( *,'(1x,a)' ) "请输入拷贝文件号:"
  Read ( *,* ) startfile
  Write ( *,'(1x,a)' ) "请输入截止文件号:"
  Read ( *,* ) stopfile
  Write ( str,'(g0)' ) startfile
  filename = trim(str)//'.txt'
  
  Do i = startfile + 1, stopfile
    Write ( str,'(g0)' ) i
    call system ( 'copy '//filename//' '//trim(str)//'.txt' )
  End do

End program CopyFile