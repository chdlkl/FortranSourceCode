!// ÀûÓÃÁ´±í´æ´¢Ï¡Êè¾ØÕó
Program CSR
  Integer, parameter :: m = 5, n = 5  !// depend on your data
  Integer :: i, j, mm, tmp, nn, fileid, first, num
  Real(kind=4) :: matrix(m,n)
  Real(kind=8), allocatable :: a(:)
  Integer, allocatable :: ia(:), ja(:)
  
  Type :: node
    Real(kind=4) :: s
    Integer :: k1, k2
    Type (node), pointer :: next
  End type node 
  Type (node), pointer :: head, tail, p, q
  
  !// input data
  Open ( newunit = fileid, file = 'm.txt' )  !// The sparse matrix data needs to be given by itself
  Do i = 1, m
    Do j = 1, n
      Read ( fileid,* ) matrix(i,j)
    End do
  End do
  Close ( fileid )
  
  nn = 0
  Do i = 1, m
    Do j = i, n
      If ( matrix(i,j) /= 0.0 ) then
        nn = nn + 1
        exit
      End if
    End do
  End do

  !// store data by CSR format
  first = 1
  If ( .not.associated(p) ) then
    Allocate( p )
    q => p
    Nullify( p%next )
    q%k2 = first
    tmp = q%k2
  End if
  
  num = 0  !// calculate the number of no-zero
  Do i =  1, m
    mm = 0  !// calculate the position of no-zero in every row
    Do j = i, n
      If ( matrix(i,j) /= 0.0 ) then
        If ( .not.associated(head) ) then
          Allocate( head )
          tail => head
          Nullify( tail%next )
          tail%s = matrix(i,j)
          tail%k1 = j
          num = num + 1
          mm = mm + 1
        Else
          Allocate( tail%next )
          tail => tail%next
          tail%s = matrix(i,j)
          tail%k1 = j
          num = num + 1  
          mm = mm + 1  
          Nullify( tail%next )
        End if
      End if
    End do
    If ( associated(p) ) then
      Allocate( q%next )
      q => q%next
      q%k2 = tmp + mm
      tmp = q%k2
      Nullify( q%next )
    End if
  End do
  
  Allocate( a(num), ja(num), ia(nn+1) )  !// store the no-zero element
  !// output a and ja
  tail => head
  j = 1
  Do 
    If ( .not.associated(tail) ) exit
    a(j) = tail%s
    ja(j) = tail%k1
    j = j + 1
    tail => tail%next
  End do
  
  !// output ia
  q => p
  j = 1
  Do 
    If ( .not.associated(q) ) exit
    ia(j) = q%k2
    j = j + 1
    q => q%next
  End do
  
  Write ( *,'(a)' ) 'Original data'
  Write ( *,'(5f7.2)' ) matrix
  Write ( *,'(a)' ) 'CSR data'
  Write ( *,'(*(f7.2))' ) a
  Write ( *,'(a)' ) 'Colnum of CSR data'
  Write ( *,'(*(i4))' ) ja
  Write ( *,'(a)' ) 'The position of CSR data'
  Write ( *,'(*(i4))' ) ia

  Deallocate( a, ja, ia )
  
End program CSR