Module test
  Implicit None
Contains
  Subroutine WriteMatrix( imat )
    Implicit None
    Integer, Intent(In) :: imat(:,:)
    Integer :: i
    Do i = 1 , size( iMat , dim = 2 )
      write( *,* ) iMat( :,i )
    End Do
  End Subroutine WriteMatrix
End Module test

Program AssumedShapeArrays
  Use test
  Implicit None
  integer :: x(3,3) = reshape( &
    [1,2,3, &
     4,5,6, &
     7,8,9],[3,3])
     
  call WriteMatrix( x )
  
End Program AssumedShapeArrays