program main
  include 'link_fnl_shared.h' 
  use lin_sol_gen_int
  implicit none
  real :: A(3,3) = [2,-1,0,-1,2,-1,0,-1,2]
  real :: B(3,1) = [-1./16,-1./16,15./16]
  real :: X(3,1) = 0.

  call lin_sol_gen(A,B,X) ! A*X=B,解X
  write(*,"(3F6.3)") X
  stop
end program