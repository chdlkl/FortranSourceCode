Program test_gesv
  use lapack95
  Implicit None
  Real :: a(3,3),aa(3,3),b(3)
  
  aa = Reshape([2.,1.,3.,6.,2.,-4.,4.,3.,-6.],[3,3])
  a = aa
  b = [998.,999.,1000.]
  Write(*,*) 'a=',a
  Write(*,*) 'b=',b
  Call gesv(a,b)
  Write(*,*) 'solve=',b
  Write(*,*) Matmul(aa,Reshape(b,[3,1]))
  
End Program test_gesv