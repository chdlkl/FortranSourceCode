!// 此代码的目的是对文件数为偶数时，进行反向替换
Program main
  Implicit none
  Integer, parameter :: x1 = 1, n = 10  !// n为文件数的一半
  Integer :: n1 = 23, n2 = 42, node(n), i  !// n1和n2为文件起始号
  Character(len=20) :: str1, str2
  
  !// 获取改变前一半文件需要的差值
  Do i = 1, n
    node(i) = -2*(i-1) + ( 2*n-1 )
  End do
  
  !// 对前一半文件进行改名处理
  Do i = n1, (n1+n2) / 2
    Write ( str1,'(g0)' ) i
    str1 = trim(str1)//'.txt'
    Write ( str2,'(g0)' ) i + node(i-22)
    str2 = trim(str2)//'-1'//'.txt'
    call system( 'copy '//str1//' '//str2 )
  End do 
  
  !// 获取改变后一半文件需要的差值
  Do i = 1, n
    node(i) = 2*(i-1) + 1
  End do
  
  !// 对后一半文件进行改名处理
  Do i = (n1+n2) / 2 + 1, n2
    Write ( str1,'(g0)' ) i
    str1 = trim(str1)//'.txt'
    Write ( str2,'(g0)' ) i - node(i-32)
    str2 = trim(str2)//'-1'//'.txt'
    call system( 'copy '//str1//' '//str2 )
  End do 
  
  !// 删除原始文件
  Do i = n1, n2
    Write ( str1,'(g0)' ) i
    str1 = trim(str1)//'.txt'
    call system( 'del '//str1 )
  End do
  
  !// 对改名后文件重新命名
  Do i = n1, n2
    Write ( str1,'(g0)' ) i
    str2 = str1
    str1 = trim(str1)//'-1'//'.txt'
    str2 = trim(str2)//'.txt'
    call system( 'copy '//str1//' '//str2 )
  End do
  
  !// 删除文件
  Do i = n1, n2
    Write ( str1,'(g0)' ) i
    str1 = trim(str1)//'-1'//'.txt'
    call system( 'del '//str1 )
  End do
  Print*, "Please input Enter and stop!!!"
  Read(*,*)
End program main