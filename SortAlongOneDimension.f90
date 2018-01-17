Program main
  Implicit none
  Integer, parameter :: m = 3, n = 3
  Integer :: a(m,n) = Reshape( [ 5, 4, 7, 2, 5, 8, 6, 4, 5 ], [ m,n ] )
  Integer :: temp(m,n) = 0
  Integer :: i, j, k, L
  
  L = size(a,dim=1)
  k = size(a,dim=2)
  
  print*, k
  Write (*,'(3x,g0)') '排序之前:'
  Do i = 1, L
    Write (*,*) ( a(i,j), j = 1, k )
  End Do
  
  Do j = 1, L - 1
    Do i = 1, L - j
      If ( a(i,k) > a(i+1,k) ) Then
        temp(i,:) = a(i,:)
        a(i,:) = a(i+1,:)
        a(i+1,:) = temp(i,:)
      End If
    End Do
  End Do
  
  Write (*,'(3x,g0)') '排序之后:'
  Do i = 1, L
    Write (*,*) ( a(i,j), j = 1, k )
  End Do
  
End Program main