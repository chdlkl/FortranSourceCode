Program CSR
  !// author: luk
  !// qq: 735343320
  Integer, parameter :: m = 5, n = 5  !// depend on your equation
  Integer :: i, j, mm, tmp, nn, fileid, first, num
  Real(kind=8) :: matrix(m,n), b(m), x(m)
  Real(kind=8), allocatable :: aa(:)
  Integer, allocatable :: ia(:), ja(:)
  
  !// =====================the variables of pardiso====================
  Integer(kind=8) :: pt(64)   !// kind=8:x64; kind=4:win32
  Integer :: maxfct, mnum, mtype, phase, nrhs, error, msglvl
  Integer, allocatable :: perm(:)
  Integer :: iparm(64)
  Integer, external :: mkl_get_max_threads
  !// =====================the variables of pardiso====================
  
  Type :: node
    Real(kind=8) :: s
    Integer :: k1, k2
    Type (node), pointer :: next
  End type node 
  Type (node), pointer :: head, tail, p, q
  
  allocate( perm( n ) )
  
  !// input data
  open ( newunit = fileid, file = 'm.txt' )  !// The sparse matrix data needs to be given by itself
  Do i = 1, m
    do j = 1, n
      read ( fileid,* ) matrix(i,j)
    end do
  End do
  read( fileid,* ) b
  close ( fileid )
  
  nn = 0
  Do i = 1, m
    do j = i, n
      If ( matrix(i,j) /= 0.0 ) then
        nn = nn + 1
        exit
      End if
    end do
  End do

  !// =========================store data by CSR format=======================
  first = 1
  If ( .not.associated(p) ) then
    allocate( p )
    q => p
    nullify( p%next )
    q%k2 = first
    tmp = q%k2
  End if
  
  num = 0  !// calculate the number of no-zero
  Do i =  1, m
    mm = 0  !// calculate the position of no-zero in every row
    do j = i, n
      If ( matrix(i,j) /= 0.0 ) then
        if ( .not.associated(head) ) then
          allocate( head )
          tail => head
          nullify( tail%next )
          tail%s = matrix(i,j)
          tail%k1 = j
          num = num + 1
          mm = mm + 1
        else
          allocate( tail%next )
          tail => tail%next
          tail%s = matrix(i,j)
          tail%k1 = j
          num = num + 1  
          mm = mm + 1  
          nullify( tail%next )
        end if
      End if
    end do
    if ( associated(p) ) then
      allocate( q%next )
      q => q%next
      q%k2 = tmp + mm
      tmp = q%k2
      nullify( q%next )
    end if
  End do
  
  allocate( aa(num), ja(num), ia(nn+1) )  !// store the no-zero element
  !// output a and ja
  tail => head
  j = 1
  Do 
    if ( .not.associated(tail) ) exit
    aa(j) = tail%s
    ja(j) = tail%k1
    j = j + 1
    tail => tail%next
  End do
  
  !// output ia
  q => p
  j = 1
  Do 
    if ( .not.associated(q) ) exit
    ia(j) = q%k2
    j = j + 1
    q => q%next
  End do
  !// =========================store data by CSR format=======================
  write ( *,'(a)' ) '< Original data'
  write ( *,'(5f7.2)' ) matrix
  write ( *,'(a)' ) '< CSR data'
  write ( *,'(*(f7.2))' ) aa
  write ( *,'(a)' ) '< B data'
  write ( *,'(*(f7.2))' ) b
  write ( *,'(a)' ) '< Colnum of CSR data'
  write ( *,'(*(i4))' ) ja
  write ( *,'(a)' ) '< The position of CSR data'
  write ( *,'(*(i4))' ) ia
  
  !// ===========================solve equations=============================
  pt = 0  !// pointer initialization
  maxfct = 1; mnum = 1; mtype = -2  !// mtype = -2: symmetric nonpositive definite matrix, mtype = 2: symmetric positive definite matrix
  perm = 0; nrhs = 1
  x = 0.d0
  iparm(1) = 0  !// iparm use default values
  iparm(3) = mkl_get_max_threads()  !// iparm(3): parallel threads are obtained by MKL. In general, the number of cpus is the same as the number of threads
  error = 0  
  msglvl = 1
  
  phase = 12  !// LU decompose
  call pardiso( pt, maxfct, mnum, mtype, phase, n, aa, ia, ja, perm, nrhs, iparm, msglvl, b, x, error )
  phase = 33  !// solve equations
  call pardiso( pt, maxfct, mnum, mtype, phase, n, aa, ia, ja, perm, nrhs, iparm, msglvl, b, x, error )
  write ( *,'(a)' ) '< The solution as follows ...... '
  do i = 1, n
    write( *,'(f7.4)' ) x(i)
  end do
  
  phase = -1
  call pardiso (pt, maxfct, mnum, mtype, phase, n, aa, ia, ja, perm, nrhs, iparm, msglvl, b, x, error)
  
  deallocate( aa, ja, ia, perm )
  !// ===========================stop solving equations=============================
  
End program CSR
  

!// remark:
!// windows: set mkl libraries or use command line with /Qmkl  [ifort /Qmkl pardiso.f90]
!// linux: use command line with -mkl   [ifort -mkl pardiso.f90]
!// phase = 12  !// LU decompose
!// call pardiso( pt, maxfct, mnum, mtype, phase, n, aa, ia, ja, perm, nrhs, iparm, msglvl, b, x, error )
!// phase = 33  !// solve equations 
!// call pardiso( pt, maxfct, mnum, mtype, phase, n, aa, ia, ja, perm, nrhs, iparm, msglvl, b, x, error )
!// The top four rows can be replaced by the bottom two rows
!// phase = 13  !// solve equations 
!// call pardiso( pt, maxfct, mnum, mtype, phase, n, aa, ia, ja, perm, nrhs, iparm, msglvl, b, x, error )