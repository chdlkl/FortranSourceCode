Program test
!DIR$ if defined(_WIN64)
  Logical, parameter :: IsWin = .true.
  Character(*), parameter :: filename = 'out\1.txt'
  Print*, ' The system is Windows!'
!DIR$ else
  Logical, parameter :: IsWin = .false.
  Character(*), parameter :: filename = './out/1.txt'
!DIR$ end if
  Print*, ' The system is linux!'
  Pause
End program test
