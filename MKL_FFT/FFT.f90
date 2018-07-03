Module MKL_FFT !// ���´��븴��ճ����ֱ��ʹ��
  Use MKL_DFTI
  private
  Type , public :: CLS_FFT
    type(DFTI_DESCRIPTOR), Pointer :: h => NULL()
    Integer :: Err
  contains
    Procedure :: Create
    Procedure :: Forward
    Procedure :: Backward
    Procedure :: Destory
  End Type CLS_FFT

contains

  Subroutine Create( this , N )
    class( CLS_FFT ) :: this
    Integer , Intent( IN ) :: N
    this%Err = DftiCreateDescriptor( this%h , DFTI_SINGLE , DFTI_REAL , 1 , N )
    this%Err = DftiSetValue( this%h , DFTI_PLACEMENT , DFTI_NOT_INPLACE )
    this%Err = DftiCommitDescriptor( this%h )
  End Subroutine Create

  Function Forward( this , X ) result( F )
    class( CLS_FFT ) :: this
    Real :: X(:)
    Complex :: F( size(X)/2+1 )
    this%Err = DftiComputeForward( this%h , X , F )
  End Function Forward

  Function Backward( this , X ) result( T )
    class( CLS_FFT ) :: this
    Complex :: X(:)
    Real    :: T( size(X)*2-1 )
    this%Err = DftiComputeBackward( this%h , X , T )
  End Function Backward

  Subroutine Destory( this )
    class( CLS_FFT ) :: this
    this%Err = DftiFreeDescriptor( this%h )
  End Subroutine Destory

End Module MKL_FFT
!// ���ϴ��븴��ճ����ֱ��ʹ��

Program Test_FFT  !// ��Ҫ��MKL��װĿ¼�µ� MKL_DFTI.f90 �ļ����빤��
  use MKL_FFT   !// ��������� *************
  Type( CLS_FFT ) :: FFT !// ��䶨��һ�� FFT �Ĺ��� ************
  Integer , parameter :: N = 30 !//����Ҫ2^k �η�����
  Real     :: r(N) = & !// ʱ����
    [3,2,4,6,2,5,7,8,5,3,7,8,4,2,4,7,8,9,3,2,5,7,8,4,2,5,8,9,4,6]
  Complex  :: f(N/2+1) !// Ƶ����Ϊ����,��СΪ n/2 + 1

  call FFT%Create( N ) !// ����FFT���̡�N �� FFT �ĵ��� ************
  f  = FFT%Forward( r )  !// ���� FFT �任  *************
  Do i = 1 , size(f)
    Write( * , * ) i , f( i ) !// ���Ƶ����
  End Do
  r = FFT%Backward( f ) / n !// ��һ������������ϱ任 *************
  Do i = 1 , size(r)
    Write( * , * ) i , r(i) !// ������任
  End Do
  call FFT%Destory() !// ���� FFT ����*************
  Read(*,*)
End Program Test_FFT