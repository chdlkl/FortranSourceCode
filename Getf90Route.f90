!// 要开预处理：解决方案>属性>fortran>Preprocessor>Preprocess Source File改为yes
!// 命令行下/fpp或-fpp
Program test
  Implicit none
  Write ( *,* ) __FILE__  
End program test