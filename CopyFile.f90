Program CopyFile
  Implicit none 
  Integer :: i
  Integer :: startfile, stopfile
  Character(len=80) :: str = '', filename
  
  Write ( *,'(1x,a)' ) "�����뿽���ļ���:"
  Read ( *,* ) startfile
  Write ( *,'(1x,a)' ) "�������ֹ�ļ���:"
  Read ( *,* ) stopfile
  Write ( str,'(g0)' ) startfile
  filename = trim(str)//'.txt'
  
  Do i = startfile + 1, stopfile
    Write ( str,'(g0)' ) i
    call system ( 'copy '//filename//' '//trim(str)//'.txt' )
  End do

End program CopyFile