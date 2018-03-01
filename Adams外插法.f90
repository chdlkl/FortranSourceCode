!---------------------------------------------------------
!             �˳����õ���Adams��巨���
!                    y=exp(-5*x)
!���԰������ľ�����Ľ�Euler��Euler���ľ��Ƚ��бȽ�һ��
!---------------------------------------------------------  
Program main
  Implicit None
  Integer, Parameter :: n = 3, T1 = 1,T0 = 0
  Real, Parameter :: h = 0.05
  Integer, Parameter :: num = Nint( (T1-T0)/h )
  Real :: a(0:n), b(0:n), sum, t, sum1, b1, jL, u(0:n), u1(0:num)
  Integer :: i, j, L
  
  Write(*,*) "----------���Adams��幫ʽϵ��ֵ-----------"
  a(0) = 1.
  Do i = 1, n
    sum = 0.; t = 2.
    Do j = i, 1, -1
      sum = sum + a(j-1) / (t)
      t = t+1
    End Do
    a(i) = 1. - sum
  End Do
  Write(*,*) a
  
  Write(*,*) "----------��֤Adams��幫ʽϵ��ֵ-----------"
  Print*, "......"
  !//��֤Adams��幫ʽϵ��ֵ�ֽ��Ƿ���ȷ
  sum1 = 0.
  Do i = 0, n
    sum1 = sum1 + a(n-i) / (1+i)
  End Do
  If( abs(sum1-1.0) < 1e-6 ) Then   !//sum1=1.0,˵��Adams��幫ʽϵ���ֽ���ȷ
    Write(*,*) "----------Adams��幫ʽϵ���ֽ���ȷ---------"  
  End If
  
  Print*
  Print*
  Print*, '--------------------------------------------'
  Write(*,*) "---------------���ϵ��Bklֵ----------------"
  Print*, '--------------------------------------------'
  Write(*,*) "......"
  
  Do L = 0, n
    b1 = 0.0
    Do j = L, n
      Call calculate_jL(j,L,jL)  !//���(s(s-1)...(s-j+1)/j!)
      b1 = b1 + (-1)**L * a(j) * jL
    End do
    b(L) = b1
  End Do
  Write(*,*) b*24
  
  !//����ǰ�ĸ���ȷ��
  Do i = 0, n
    u(i) = Exp(-5*h*i)
    u1(i) = u(i)
  End Do
  
  !//��Adams��巨���м���������
  Do i = n+1, num
    u1(i) = u1(i-1) + h* ( b(0)*(-5.*u1(i-1)) + b(1)*(-5.*u1(i-2)) + b(2)*(-5.*u1(i-3))+b(3)*(-5.*u1(i-4)) )
  End Do
  
  Open(10,file='waicha.txt',status='unknown')
  Do i = 0, num
    Write(10,*) u1(i), exp(-5*h*i), abs((u1(i)-exp(-5.*h*i)) / exp(-5.*h*i)) * 100
  End Do
  Close(10)
End Program main
  
Subroutine calculate_jL(j,L,jL)
  Implicit None
  Integer :: i, j, L
  Real :: jL, Product1, Product2
 
  If( L == 0 ) Then
    jL = 1.
    Return
  Else
    Product1 = 1.; Product2 = 1.
    Do i = 1, L
      Product1 = Product1 * i  !//������s(s-1)...(s-j+1)
    End Do
    Do i = j, j-L+1, -1
      Product2 = Product2 * i  !//���j!
    End Do
    jL = Product2 / Product1
  End If
End Subroutine calculate_jL