Module Gauss_Legendre !//��˹�����õ»��ָ�˹�㼰Ȩ�ص����ģ��
  Implicit none
  Integer, parameter :: n  = 11                          !// ��������˹��ĸ���
  Integer, parameter :: DP = selected_real_kind( p=13 )  !// ����kind����ֵ
  Real(kind=DP), parameter :: eps = 1.0e-15_DP           !// ��������
Contains
  Real(Kind=DP) function N_Legendre(x) !// ����n�����õ¶���ʽ
    Implicit none
    Integer :: i
    Real(Kind=DP) :: a(n), x
    a(1) = x !// 1�����õ¶���ʽ
    a(2) = 1.5_DP*x*x - 0.5_DP !// 2�����õ¶���ʽ
    Do i = 3, n
      a(i) = ( dble(i+i-1)*x*a(i-1) - dble(i-1)*a(i-2) ) / dble(i) !// ���õ��ƹ�ϵ����n�����õ¶���ʽ
    End do
    N_Legendre=a(n) !//���ɵ�n�����õ¶���ʽ
  End function N_Legendre

  Real(Kind=DP) Function N1_Legendre(x)  !// ����n-1�����õ¶���ʽ 
    Implicit none
    Integer :: i
    Real (Kind=DP) :: a(n), x
    a(1) = x
    a(2) = 1.5_DP*x**2 - 0.5_DP
    Do i = 3, n - 1
      a(i) = (2*i-1)*x*a(i-1)/i - (i-1)*a(i-2)/i
    End Do
    N1_Legendre = a(n-1)     
  End function N1_Legendre
  
  Real(Kind=DP) function DN_Legendre(x)  !// ����n�����õ¶���ʽ�ĵ������ʽ
    Implicit none
    Integer :: i
    Real(Kind=DP) :: a(n), x
    a(1) = x  !// 1�����õ¶���ʽ
    a(2) = 1.5_DP*x*x - 0.5_DP !// 2�����õ¶���ʽ
    Do i = 3, n
      a(i) = ( dble(i+i-1)*x*a(i-1) - dble(i-1)*a(i-2) ) / dble(i) !// ���õ��ƹ�ϵ����n�����õ¶���ʽ
    End Do
    DN_Legendre = ( a(n-1) - x*a(n) )*dble(n) / (1.0_DP - x*x ) 
  End function DN_Legendre

  Real(Kind=DP) function NewtonIteration(a, b) !// ţ�ٷ���⺯���Ľ�
    Implicit none
    Integer :: i
    Real(Kind=DP) :: a, b, x, xtmp
    Integer, parameter :: nloop = 2000
    !// a,b�Ǵ��ݽ����Ļ��ֺõ���һ������ڵ�����
    x = ( a + b ) / 2.d0  !// ��ʼ����ֵ
    i = 0
    Do 
      xtmp = x - N_Legendre(x) / DN_Legendre(x)   !// X(i+1) = Xi - f(Xi) / f'(Xi)  i = 1,2,...N
      i = i + 1
      If ( abs( xtmp-x ) < eps .and. i > nloop ) exit
      x = xtmp
    End do 
    NewtonIteration = x
  End function NewtonIteration

  Subroutine root_coeff ( f_root, f_coeff )  !// ����N�����õ¶���ʽ�ĸ���ȥ��Ȩ��ϵ��
    Implicit none
    Real(Kind=DP) :: m, nstep, f_root(n), f_coeff(n) !// ��������,��Сn��module��ʼ������
    Integer :: i, j
    Real(kind=DP), parameter :: h = 1.d-6
    j = 0   !// ��ֵ����ѭ�������ĳ�ֵ           
    m = -1.d0 - h   !// ���ü�����[-1��1] �����ޣ�������-1 
    nstep = nint(2.d0/h)
    Do i = 1, nstep   !// ���ѭ������Ӧ�����ɲ���0.000001�� ��,���㷽����2000000=2/0.000001     
      If ( N_Legendre(m)*N_Legendre(m+h) < 0 ) then   !// �����޴���ʼ�������ۼ�
        j = j + 1    !// ��¼���ǵڼ�����
        f_root(j) = NewtonIteration( m, m+h )!// ����ţ�ٷ��������ڷֺõ�һС������⣬����洢��fn��j��
        f_coeff(j) = 2.0_DP / ( dble(n) * N1_Legendre(f_root(j)) * DN_Legendre(f_root(j)) ) !// ���ù�ʽ�����˹���Ȩ��
      End if
      m = m + h !// ִ����һ���ж�m��ǰ�ƽ�һ��
    End Do
  End subroutine root_coeff

End module Gauss_Legendre
  
Module getdBz
  implicit none
  Integer :: nn, fileid
  Real(kind=8), allocatable :: t(:), Derfunc(:)
contains
Subroutine getDerBz
  implicit none
  Integer :: i, info
  
  nn = 0
  open( newunit = fileid, file = 'FWD.dat' )
  Do 
    read( fileid, *, iostat = info )
    if ( info /= 0 ) exit
    nn = nn + 1
  End do

  rewind( fileid )
  allocate( t(nn), Derfunc(nn) )
  
  do i = 1, nn
    read( fileid, * ) t(i), Derfunc(i)
  end do

End subroutine getDerBz

End module getdBz
    

Program GaussianIntegral 
  use getdBz
  use Gauss_Legendre
  Implicit none
  
  Real (Kind=DP) :: f_root(n), f_coeff(n)
  Real (kind=DP) :: x, a, b, answer, ss, tmpt(1), tmpans(1)
  Real (kind=DP) :: init = 1.d0  !// ��ʼʱ�̵�ֵ
  Integer :: i, j
  
  write( *,'(a)' ) '===================================����˵��======================================='
  write( *,'(a)' ) '================getDerBz�е��ļ�FWD.dat�洢�Ա����뺯���ڸô���һ�׵���==========='
  write( *,'(a)' ) '============================�������е�init��ʾ��һ���㴦��ֵ======================'
  write( *,'(a)' ) '============================ans.dat�д洢�õ㴦ԭ������ֵ========================='
  write( *,'(a)' ) "=====================���Ժ���: y' = -e^(-x), ԭ����: y = e^(-x)==================="
  write( *,'(a)' ) ' �밴�س�����ʼ����......'
  read*
  
  call getDerBz()

  Call root_coeff ( f_root, f_coeff ) !// �������˹����Ȩ�ص��Ӻ���
  
  open ( 1000, file = 'ans.dat' )
  write( 1000,* ) t(1), init
  
  Do i = 2, nn
    write( *,'(1x,a,g0,a,g0,a)' ) "���ڼ���� ", i, " ��,�� ", nn, " ��."
    a = t(i-1)
    b = t(i)
    answer = 0.d0
    do j = 1, n
      tmpt(1) = (a+b) / 2.0_DP + (b-a) / 2.0_DP * f_root(j)
      call func( t, Derfunc, nn, tmpt(1), tmpans(1), 1, nn-1, nn-2 )
      answer = answer + f_coeff(j) * tmpans(1)
    end do
    answer = answer * ( b-a ) / 2.d0
    
    answer = answer + init
    write( 1000,* ) t(i), answer
    init = answer
  End do
  close( 1000 )

End program GaussianIntegral
  
  
Subroutine func( x, y, n, t, f, m, n1, n2 )
  Implicit none
  Integer :: i, j, k, m, n, n1, n2
  Real(kind=8) :: x(n), y(n), t(m), f(m), f1(m)
  Real(kind=8) :: s2(n), h(n1), dy(n1), s(n1), e(n2)
  Real(kind=8) :: z, h1, h2, h3, h4
  
  Do i = 1, n1
    h(i) = x(i+1) - x(i)
    dy(i) = ( y(i+1) - y(i) ) / h(i)
  End do
  s2(1) = 0.d0; s2(n) = 0.d0
  Do i = 2, n1
    s2(i) = 6.d0 * ( dy(i) - dy(i-1) )
  End do
  z = 0.5d0 / ( h(1) + h(2) )
  s(1) = -h(2) * z
  e(1) = s2(2) * z
  Do i = 2, n2
    k = i - 1
    j = i + 1
    z = 1.d0 / ( 2.d0*( h(i)+h(j) ) + h(i)*s(k) )
    s(i) = -h(j) * z
    e(i) = ( s2(j)-h(i)*e(k) ) * z
  End do
  s2(n1) = e(n2)
  Do i = n2, 2, -1
    k = i - 1
    s2(i) = s(k)*s2(i+1) + e(k)
  End do
  Do i = 1, n1
    s(i) = ( s2(i+1) - s2(i) ) / h(i)
  End do
  i = 2
  k = 1
  Do j = 1, m
    do 
      If ( t(j) > x(i) ) then
        k = i
        i = i + 1
      else
        exit
      End if
    End do
    h1 = t(j) - x(k)
    h2 = t(j) - x(i)
    h3 = h1 * h2
    h4 = s2(k) + h1*s(k)
    z = ( s2(i) + s2(k) + h4 ) / 6.d0
    f(j) = y(k) + h1*dy(k) + h3*Z
    f1(j) = dy(k) + z*( h1+h2 ) + h3 * s(k) / 6.d0
  End do
End subroutine func