Program RandomNumber
  Implicit None
  real :: x(3) = 0.0 , y(2) = 0.0 , a = 0.0
  integer :: i
  call random_seed() !// ֻ����һ�Σ����߿ɳ���ȥ�����в��������
  Do i = 1 , size(x)
    call random_number( x(i) ) !// ����ѭ������
  End Do
  call random_number(y) !// Ҳ����һ������һ������
  call random_number(a) !// ������Ҳ����
  write(*,*) a
  write(*,*) x
  write(*,*) y
End Program RandomNumber