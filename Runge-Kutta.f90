!//---------------------------------------
!//本程序以四阶Runge-Kutta法为原理进行编写
!//以课本的第三十七页例4.1为题
!//du/dt = 4*t*sqrt(u), 0=<t<=2, u(0)=1 精确解为：u(t) = (1+t^2)^2
!//---------------------------------------
Module Runge_Kutta
  Implicit None
  Real,Parameter :: t0=0.,t1=1.,h=0.02,Num=Nint((t1-t0)/h),u0=1.
  Real :: k1,k2,k3,k4
  Real(kind=8) :: t(0:Num),u(0:Num)
  Integer :: i
Contains
Subroutine solve()
  Implicit None
  
  u(0)=u0;t(0)=t0
  Do i=1,Num
    k1=2.*t(i-1)*u(i-1)
    k2=2.*(t(i-1)+0.5*h)*(u(i-1)+0.5*h*k1)
    k3=2.*(t(i-1)+0.5*h)*(u(i-1)+0.5*h*k2)
    k4=2.*(t(i-1)+h)*(u(i-1)+h*k3)
    t(i)=t0+i*h
    u(i)=u(i-1)+h/6.*(k1+2.*k2+2.*k3+k4)
  End Do

  Open(10,file='shujv.txt',status='unknown')
  Do i=0,Num
    Write(10,"(4f14.8)") t(i),Exp(t(i)*t(i)),u(i),Abs(u(i)-Exp(t(i)*t(i)))/Exp(t(i)*t(i))*100
  End Do
  Close(10)
End Subroutine solve
End Module Runge_kutta
  
Program main
  Use Runge_Kutta  
  Implicit None
  Call solve()
End Program main