!// 批量修改文件名
Program main
  Implicit none
  Integer :: n1 = 23, n2 = 56
  Integer :: i 
  Character(len=20) :: str1, str2
  
  Do i = n1, n2
    Write ( str1,'(g0)' ) i
    str1 = trim(str1)//'.txt'
    Write ( str2,'(g0)' ) i - 1
    str2 = trim(str2)//'.txt'
    call system( 'move '//(str1)//' '//(str2) )
  End do
  
  Write (*,*) "Please input Enter!!!"
  Read (*,*)

End program main