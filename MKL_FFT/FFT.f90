Module MKL_FFT !// 以下代码复制粘贴，直接使用
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
!// 以上代码复制粘贴，直接使用

Program Test_FFT  !// 需要将MKL安装目录下的 MKL_DFTI.f90 文件加入工程
  use MKL_FFT   !// 这句代码必须 *************
  Type( CLS_FFT ) :: FFT !// 这句定义一个 FFT 的过程 ************
  Integer , parameter :: N = 30 !//不需要2^k 次方整幂
  Real     :: r(N) = & !// 时间域
    [3,2,4,6,2,5,7,8,5,3,7,8,4,2,4,7,8,9,3,2,5,7,8,4,2,5,8,9,4,6]
  Complex  :: f(N/2+1) !// 频率域，为复数,大小为 n/2 + 1

  call FFT%Create( N ) !// 创建FFT过程。N 是 FFT 的点数 ************
  f  = FFT%Forward( r )  !// 正向 FFT 变换  *************
  Do i = 1 , size(f)
    Write( * , * ) i , f( i ) !// 输出频率域
  End Do
  r = FFT%Backward( f ) / n !// 这一句可以做反傅氏变换 *************
  Do i = 1 , size(r)
    Write( * , * ) i , r(i) !// 输出反变换
  End Do
  call FFT%Destory() !// 销毁 FFT 过程*************
  Read(*,*)
End Program Test_FFT