Program Main
  Implicit None
  Integer :: n
  Integer,External :: fact
  Write(*,*) 'N='
  Read(*,*) n
  Write(*,"(I2,'!=',I8)") n,fact(n)
End Program Main
  
Recursive Integer Function fact(n) result(ans)  !//result���������ڳ��������ʹ������һ�����������ú����Ĵ���ֵ��������ans����ԭ����fact�����ҿ��Բ����ڳ����ж���
  Implicit None
  Integer,Intent(in) :: n
  
  If(n<0) Then  !//�����������
    ans=-1    !//�������һ����ֵ
    Return  !//n������ֱ��return
  Else If(n<=1) Then
    ans=1
    Return  !//���������µݹ飬return
  End If
!//��ִ�е��������n>1������n*(n-1)������n!
  ans=n*fact(n-1)  !//�����*����ɡ�+�����ǽ׳˼��㣬�������ۼ�����
  Return
End function fact