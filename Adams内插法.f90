!---------------------------------------------------------
!             此程序用的是Adams内插法求解
!                     y=exp(-5*x)
!可以把这个解的精度与改进Euler和Euler法的精度进行比较一下
!---------------------------------------------------------  
Program main
  Implicit None
  Integer, Parameter :: n = 3,T1 = 1,T0 = 0 !//n为阶数，T1，T0为计算区间
  Real, Parameter :: h = 0.05  !//h为步长
  Integer, Parameter :: num = Nint((T1-T0)/h)  !//num为总点数
  Real :: a(0:n), b(0:n), sum, t, sum1, b1, jL, u1(0:num), t11, t22, u11(0:num)
  !//数组a存放生成函数法的导出系数，数组b存放最终表达式前的系数
  !//sum,sum1,b1都为初始变量，用来存放循环值，t11,t22为临时变量，进行迭代收敛
  !//u1存放最终数值解，u11存放用外插法计算的数值解
  Integer :: i, j, L
  
  Write(*,*) "----------求解Adams内插公式系数-------------"
  a(0) = 1.
  Do i = 1, n!n+1
    sum = 0.; t = 2.
    Do j = i, 1, -1
      sum = sum + a(j-1) / (t)
      t = t + 1
    End Do
    a(i) = 0. - sum  !//此处与外插法不一样，a(i)=0.-sum;而外插法为a(i)=1.-sum
  End Do
  Write(*,*) a
  
  Print*,"----------验证Adams内插公式系数-------------"
  Print*, "......"
  sum1 = 0.  !//验证Adams内插公式系数值分解是否正确
  Do i = 0, n
    sum1 = sum1 + a(n-i) / (1+i)
  End Do
  If( abs(sum1-0.0) < 1e-6 ) Then   !//sum1=0.0,说明Adams内插公式系数分解正确
    Write(*,*) "----------Adams内插公式系数分解正确---------"  
  End If
  
  Print*, '--------------------------------------------'
  Write(*,*) "---------------求解系数Bkl值----------------"
  Print*, '--------------------------------------------'
  Write(*,*) "......"
  
  Do L = 0, n
    b1 = 0.0
    Do j = L, n
      Call calculate_jL(j,L,jL)  !//求解(s(s-1)...(s-j+1)/j!)
      b1 = b1 + (-1)**L * a(j) * jL
    End do
    b(L) = b1
  End Do
  Write(*,*) b*24  !//24是为了将系数显示成整数，没有其他特别含义  

  Open(10,file='waicha.txt',status='old')  !//读取Adams外插法计算的数值
  Do i = 0, num
    Read(10,*) u11(i)
    If( i<n ) u1(i) = u11(i)   !//前3个数值由外插算法给出
  End Do
  Close(10)
  
  !//用Adams内插法进行计算其它项
  Do i = n, num  !由于本程序用的是3阶，所以内插法的前3个数值要用外插法给出（内插法要给出前4个数值）
    t11 = u11(i)
    Do
      t22 = u1(i-1) + h * ( b(0)*(-5.*t11) + b(1)*(-5.*u1(i-1)) + b(2)*(-5.*u1(i-2)) + b(3)*(-5.*u1(i-3)) )
      If( abs(t22-t11) < 1e-5 ) Then  
        Exit  !//如果迭代收敛的话，跳出循环，赋值给u1(i)
      Else
        t11 = t22  !//不满足迭代要求，继续迭代
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
      Product1 = Product1 * i  !//求解分子s(s-1)...(s-j+1)
    End Do
    Do i = j, j-L+1, -1
      Product2 = Product2 * i  !//求解j的阶乘
    End Do
    jL = Product2 / Product1
  End If
End Subroutine calculate_jL