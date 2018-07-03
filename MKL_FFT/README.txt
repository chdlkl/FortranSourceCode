说明:
如果是在IDE(VS+IVF)中运行此程序，要开启mkl函数库

如果在win下用命令行运行此程序，相应的命令为: ifort /Qmkl mkl_dfti.f90 FFT.f90

如果在linux下用命令行运行程序，相应的命令为: ifort -mkl mkl_dfti.f90 FFT.f90