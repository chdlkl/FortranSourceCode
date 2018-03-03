Program main
  Implicit None
  Real(kind=8),Parameter :: h=0.001,T=1  
  Integer,Parameter :: N=Nint(T/h)
  Integer :: i,k
  Real(kind=8) :: U(0:N),U1(0:100),U2(0:N)
  
  !//一般Euler方法求解
  U(0)=1.0
  Do i=1,N
    U(i)=(1-5*h)*U(i-1)
  End Do
  
  !//改进的Euler法求解
  U2(0)=U(0)
  Do i=1,N
    k=0
    U1(k)=U(i-1)
    Do While(1)
      U1(k+1)=(1-5.0/2.0*h)*U(i-1)-(5.0/2.0*h)*U1(k)
      If(Abs(U1(k+1)-U1(k))<1e-6) Then
        Exit
      Else
        k=k+1
      End If
    End Do
    U2(i)=U1(k+1)
  End Do

  !//求解析解和相对误差，原函数为Exp(-5*x)
  Open(10,file='data.txt',status='unknown')
  Do i=0,N
    Write(10,100) i,U(i),U2(i),Exp(-5*i*h),Abs(U2(i)-Exp(-5*i*h))/Exp(-5*i*h)*100,Abs(U(i)-Exp(-5*i*h))/Exp(-5*i*h)*100
  End Do
  Close(10)
100 Format(I4.4,'  ',F17.14,'  ',F17.14,'  ',F17.14,'  ',F18.14,'  ',F18.14/)
End Program