Module SolInv
  Implicit none
Contains
Subroutine Inv ( aa, b, n )
  Implicit none
  Integer :: n, i, j, k
  Real(kind=8) :: aa(n,n), b(n,n), a(n,n)
  
  Write ( *,'(1x,A)' ) "Matrix A is "
  Do i = 1, n
    Write ( *,'(*(f9.4))' ) aa(i,:)
  End do
  
  a = aa
  b = 0.d0
  Do i = 1, n
    b(i,i) = 1.d0
  End do
  
  Do i = 1, n
    b(i,:) = b(i,:) / a(i,i)
    a(i,i:n) = a(i,i:n) / a(i,i)
    Do j = i + 1, n
      Do k = 1, n
        b(j,k) = b(j,k) - b(i,k)*a(j,i)
      End do
      a(j,i:n) = a(j,i:n) - a(i,i:n)*a(j,i)
    End do
  End do
  
  Do i = n, 1, -1
    Do j = i - 1, 1, -1
      Do k = 1, n
        b(j,k) = b(j,k) - b(i,k)*a(j,i)
      End do
    End do
  End do
  
  Write ( *,'(1x,A)' ) "Matrix InvA is "
  Do i = 1, n
    Write ( *,'(*(f9.4))' ) b(i,:)
  End do
  
  write(*,'(1x,a)') '检验求逆是否正确'
  a = matmul( aa,b )
  Do i = 1, size(aa,dim=1)
    write ( *,'(*(f9.4))' ) a(i,:)
  End do
  
End subroutine Inv

End module SolInv

  
Program main
  Use SolInv
  Implicit none
  Integer, parameter :: n = 3
  Real(kind=8) :: a(n,n) = reshape( [ 1, -1, 2, -1, 0, 2, 3, -2, 4 ], [3,3] ), Inv_a(n,n) = 0.d0
  a = a * 1.d0
  call Inv ( a, Inv_a, n )
End program main