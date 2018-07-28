!.. purpose: compute the convolution of the array xa and xb
Module testxcorr
  implicit none
  integer, parameter :: n = 16
  real(kind=8), parameter :: a = 0.84, b = 0.92
  real(kind=8) :: xa(n) = 0.d0, xb(n) = 0.d0
  real(kind=8) :: r(2*n-1) = 0.d0
Contains 

  subroutine getxa_xb
    implicit none
    integer :: i

    do i = 1, n 
      xa(i) = a**dble(i-1)
      xb(i) = b**dble(i-1)
    end do
  end subroutine getxa_xb
      
  subroutine getxcorr()
    implicit none
    integer :: i, j, k 
    real(kind=8) :: temp

    outdo: Do i = 1, 2*n-1
      j = i - n 
      temp = 0.d0 
      indo: do k = 1, n 
        if ( (j+k) >= 1 .and. (j+k) <= n ) then 
            temp = temp + xa(j+k) * xb(k)
        end if
      end do indo
      r(i) = temp
    End do outdo 
  end subroutine getxcorr

End module testxcorr

Program main
  use testxcorr
  call getxa_xb
  call getxcorr
End program main