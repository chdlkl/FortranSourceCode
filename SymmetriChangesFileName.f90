!// �˴����Ŀ���Ƕ��ļ���Ϊż��ʱ�����з����滻
Program main
  Implicit none
  Integer, parameter :: x1 = 1, n = 10  !// nΪ�ļ�����һ��
  Integer :: n1 = 23, n2 = 42, node(n), i  !// n1��n2Ϊ�ļ���ʼ��
  Character(len=20) :: str1, str2
  
  !// ��ȡ�ı�ǰһ���ļ���Ҫ�Ĳ�ֵ
  Do i = 1, n
    node(i) = -2*(i-1) + ( 2*n-1 )
  End do
  
  !// ��ǰһ���ļ����и�������
  Do i = n1, (n1+n2) / 2
    Write ( str1,'(g0)' ) i
    str1 = trim(str1)//'.txt'
    Write ( str2,'(g0)' ) i + node(i-22)
    str2 = trim(str2)//'-1'//'.txt'
    call system( 'copy '//str1//' '//str2 )
  End do 
  
  !// ��ȡ�ı��һ���ļ���Ҫ�Ĳ�ֵ
  Do i = 1, n
    node(i) = 2*(i-1) + 1
  End do
  
  !// �Ժ�һ���ļ����и�������
  Do i = (n1+n2) / 2 + 1, n2
    Write ( str1,'(g0)' ) i
    str1 = trim(str1)//'.txt'
    Write ( str2,'(g0)' ) i - node(i-32)
    str2 = trim(str2)//'-1'//'.txt'
    call system( 'copy '//str1//' '//str2 )
  End do 
  
  !// ɾ��ԭʼ�ļ�
  Do i = n1, n2
    Write ( str1,'(g0)' ) i
    str1 = trim(str1)//'.txt'
    call system( 'del '//str1 )
  End do
  
  !// �Ը������ļ���������
  Do i = n1, n2
    Write ( str1,'(g0)' ) i
    str2 = str1
    str1 = trim(str1)//'-1'//'.txt'
    str2 = trim(str2)//'.txt'
    call system( 'copy '//str1//' '//str2 )
  End do
  
  !// ɾ���ļ�
  Do i = n1, n2
    Write ( str1,'(g0)' ) i
    str1 = trim(str1)//'-1'//'.txt'
    call system( 'del '//str1 )
  End do
  Print*, "Please input Enter and stop!!!"
  Read(*,*)
End program main