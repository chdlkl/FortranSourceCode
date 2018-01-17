Program main
  Implicit None
  Integer :: i,ii,k
  Integer,Parameter :: N=4
  Real :: x(N)=[1,1,0,0],y(N)=[1,2,3,4],sum,r( 2*N-1 )
  
  Do i = 1,2*N-1
    ii = i-N
    sum = 0.
    Do k = 1,N
      If( (ii+k)>=1 .and. (ii+k)<=N ) Then
        sum = sum + x(ii+k)*y(k)
      End If
    End Do
    r(i) = sum
  End Do
    
  Print*, r
End Program main