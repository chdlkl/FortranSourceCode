!---------------------------------------------------------
!             �˳����õ���Adams�ڲ巨���
!                     y=exp(-5*x)
!���԰������ľ�����Ľ�Euler��Euler���ľ��Ƚ��бȽ�һ��
!---------------------------------------------------------  
Program main
  Implicit None
  Integer, Parameter :: n = 3,T1 = 1,T0 = 0 !//nΪ������T1��T0Ϊ��������
  Real, Parameter :: h = 0.05  !//hΪ����
  Integer, Parameter :: num = Nint((T1-T0)/h)  !//numΪ�ܵ���
  Real :: a(0:n), b(0:n), sum, t, sum1, b1, jL, u1(0:num), t11, t22, u11(0:num)
  !//����a������ɺ������ĵ���ϵ��������b������ձ��ʽǰ��ϵ��
  !//sum,sum1,b1��Ϊ��ʼ�������������ѭ��ֵ��t11,t22Ϊ��ʱ���������е�������
  !//u1���������ֵ�⣬u11�������巨�������ֵ��
  Integer :: i, j, L
  
  Write(*,*) "----------���Adams�ڲ幫ʽϵ��-------------"
  a(0) = 1.
  Do i = 1, n!n+1
    sum = 0.; t = 2.
    Do j = i, 1, -1
      sum = sum + a(j-1) / (t)
      t = t + 1
    End Do
    a(i) = 0. - sum  !//�˴�����巨��һ����a(i)=0.-sum;����巨Ϊa(i)=1.-sum
  End Do
  Write(*,*) a
  
  Print*,"----------��֤Adams�ڲ幫ʽϵ��-------------"
  Print*, "......"
  sum1 = 0.  !//��֤Adams�ڲ幫ʽϵ��ֵ�ֽ��Ƿ���ȷ
  Do i = 0, n
    sum1 = sum1 + a(n-i) / (1+i)
  End Do
  If( abs(sum1-0.0) < 1e-6 ) Then   !//sum1=0.0,˵��Adams�ڲ幫ʽϵ���ֽ���ȷ
    Write(*,*) "----------Adams�ڲ幫ʽϵ���ֽ���ȷ---------"  
  End If
  
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
  Write(*,*) b*24  !//24��Ϊ�˽�ϵ����ʾ��������û�������ر���  

  Open(10,file='waicha.txt',status='old')  !//��ȡAdams��巨�������ֵ
  Do i = 0, num
    Read(10,*) u11(i)
    If( i<n ) u1(i) = u11(i)   !//ǰ3����ֵ������㷨����
  End Do
  Close(10)
  
  !//��Adams�ڲ巨���м���������
  Do i = n, num  !���ڱ������õ���3�ף������ڲ巨��ǰ3����ֵҪ����巨�������ڲ巨Ҫ����ǰ4����ֵ��
    t11 = u11(i)
    Do
      t22 = u1(i-1) + h * ( b(0)*(-5.*t11) + b(1)*(-5.*u1(i-1)) + b(2)*(-5.*u1(i-2)) + b(3)*(-5.*u1(i-3)) )
      If( abs(t22-t11) < 1e-5 ) Then  
        Exit  !//������������Ļ�������ѭ������ֵ��u1(i)
      Else
        t11 = t22  !//���������Ҫ�󣬼�������
      End If
    End Do
    u1(i) = t22
  End Do
  
  Open(10,file='neicha.txt',status='unknown')
  Do i = 0, num
    Write(10,*) u1(i), exp(-5*h*i), abs( (u1(i)-exp(-5.*h*i))/exp(-5.*h*i) )*100
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
      Product2 = Product2 * i  !//���j�Ľ׳�
    End Do
    jL = Product2 / Product1
  End If
End Subroutine calculate_jL