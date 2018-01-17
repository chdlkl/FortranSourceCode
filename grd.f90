!//---------------
!//输出为grd文件
!//---------------
Subroutine Output_grd(detaG_file,m,n,Dg,Xmin,Xmax,Ymin,Ymax,dx,dy)
 Character*(*) detaG_file
 Integer m,n
 Real Xmin,Xmax,Ymin,Ymax,zmin,zmax,Dg(m,n),dx,dy

 zmin=HUGE(zmin)
 zmax=-HUGE(zmax)

 Do i=1,m
      Do j=1,n
       Zmin=MIN(Zmin,Dg(i,j))
       Zmax=MAX(Zmax,Dg(i,j))
      End do
 End do  

 Open(70,file=detaG_file,status='old')
   Write(70,'(a)')'DSAA'
   Write(70,*)m,n
   Write(70,*)Xmin,Xmax
   Write(70,*)Ymin,Ymax  
   Write(70,*)Zmin,Zmax
 
 Do j=1,n
    Write(70,*)  (Dg(i,j),i=1,m)
 End do

 Close(70)

End subroutine Output_grd