Program Main
  Implicit None
  Integer :: n
  Integer,External :: fact
  Write(*,*) 'N='
  Read(*,*) n
  Write(*,"(I2,'!=',I8)") n,fact(n)
End Program Main
  
Recursive Integer Function fact(n) result(ans)  !//result可以用来在程序代码中使用另外一个名字来设置函数的传回值，这里用ans代替原来的fact；并且可以不用在程序中定义
  Implicit None
  Integer,Intent(in) :: n
  
  If(n<0) Then  !//不合理的输入
    ans=-1    !//随便设置一个数值
    Return  !//n不合理，直接return
  Else If(n<=1) Then
    ans=1
    Return  !//不用再向下递归，return
  End If
!//会执行到这里，代表n>1，利用n*(n-1)来计算n!
  ans=n*fact(n-1)  !//如果“*”变成“+”，是阶乘计算，否则是累加运算
  Return
End function fact