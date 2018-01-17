!----------------------------------------------------------------------------------------
!
!------------------此程序的主要功能是对二维平面进行规则三角网格剖分---------------------------
!
!----------------------------------------------------------------------------------------
Program TriGrd
  Implicit None
  Integer             :: i,j,k,ii,jj !//循环变量
  Integer             :: NumCol  !//存储nb1.txt文件中的个数
  !//剖分区域的范围以及步长
  Real                :: Xmin,Xmax,dx,Ymin,Ymax,dy
  !//X方向和Y方向的节点
  Integer             :: nx,ny
  Integer,Parameter   :: FileId = 11, FileId1 = 12, FileId2 = 13  !//通道号
  Integer,Allocatable :: node(:) !//节点
  Real,Allocatable    :: nb(:)  !//第一类边界条件场值节点
  Real                :: temp1,temp2   !//临时变量
  !//存放单元编号的自定义类型数据
  Type :: Element
    Integer :: x,y,z
  End Type Element
  Type(Element),Allocatable :: e(:,:,:)


  Write(*,*) '!-----------------------------------------------------------------!'
  Write(*,*) '!----------此程序的主功能是对二维平面进行规则三角网格剖分---------!'
  Write(*,*) '!---------------------------作者：白南先生------------------------!'
  Write(*,*) '!------------请输入X方向的计算范围Xmin,Xmax以及步长dx-------------!'
  Read(*,*) Xmin,Xmax,dx
  nx = Nint( (Xmax-Xmin)/dx )
  Write(*,*) '!------------请输入Y方向的计算范围Ymin,Ymax以及步长dy-------------!'
  Read(*,*) Ymin,Ymax,dy
  ny = Nint( (Ymax-Ymin)/dy )
  Allocate(e(ny,nx,4))

  Write( *,"('单元个数为:',g0)" ) nx*ny*4
  Allocate( node((nx+1)*(ny+1)+nx*ny) )
  !//输出节点坐标
  k = 0
  Open( FileId,file='xy.txt',status='unknown' )
  Do j = 0,ny
    temp1 = j*dy
    Do i = 0,nx
      temp2 = i*dx
      k = k + 1
      Write( FileId,'(I10,2f15.8)' ) k,temp2,temp1
    End Do
    If( j<ny ) Then
      temp1 = dy/2. + j*dy
      Do ii = 0, nx-1
        temp2 = dx/2. + ii*dx
        k = k + 1
        Write( FileId,'(I10,2f15.8)' ) k,temp2,temp1
      End Do
    End If
  End Do
  Write( *,"('总结点数为:',g0)" ) k
  Close( FileId )

  !//输出每个单元的编号
  !//对前四个基单元进行编号处理，以便进行后续编号计算
  e(1,1,1)%x = nx+1+1; e(1,1,1)%y = 1; e(1,1,1)%z = 2
  e(1,1,2)%x = nx+1+1; e(1,1,2)%y = 2; e(1,1,2)%z = nx+nx+1+2
  e(1,1,3)%x = nx+1+1; e(1,1,3)%y = nx+nx+1+2; e(1,1,3)%z = nx+1+nx+1
  e(1,1,4)%x = nx+1+1; e(1,1,4)%y = nx+1+nx+1; e(1,1,4)%z = 1
  Do i = 1,ny
    Do j = 2,nx
      Do k = 1,4
        e(i,j,k)%x = e(i,j-1,k)%x + 1
        e(i,j,k)%y = e(i,j-1,k)%y + 1
        e(i,j,k)%z = e(i,j-1,k)%z + 1
      End Do
    End Do
    !//如果循环到最后一行，无需向后计算
    If( i==ny ) Exit
    Do k = 1,4
      e(i+1,1,k)%x = e(i,1,k)%x + nx+1+nx
      e(i+1,1,k)%y = e(i,1,k)%y + nx+1+nx
      e(i+1,1,k)%z = e(i,1,k)%z + nx+1+nx
    End Do
  End Do
  !//输出至I3.txt文件
  Open( FileId,file='I3.txt',status='unknown' )
  Do i = 1,ny
    Do j = 1,nx
      Do k =1,4
        Write( FileId,'(3I10)' ) e(i,j,k)%x, e(i,j,k)%y, e(i,j,k)%z
      End Do
    End Do
  End Do
  Close( FileId )

  !//存储第一类边界场值的节点号
  NumCol = 0  !//k存储nb1.txt文件中的行数
  Open( fileId,file='nb1.txt',status='unknown' )
  !//输入第一行边界节点号
  Do i = 1,nx+1
    NumCol = NumCol + 1
    Write( FileId,'(I10)' ) i
  End Do
  temp1 = 1 + nx+1+nx
  temp2 = nx+1 + nx+1+nx
  !//输入两边的节点号
  Do j = 2,ny
    NumCol = NumCol + 2
    Write( FileId,'(I10)' ) Int(temp1)
    Write( fileId,'(I10)' ) Int(temp2)
    temp1 = temp1 + nx+1+nx
    temp2 = temp2 + nx+1+nx
  End Do
  !//输入最后一行的节点号
  Do i = 1,nx+1
    NumCol = NumCol + 1
    Write( FileId,'(I10)' ) Int(temp1 + i-1)
  End Do
  Close( FileId )
  Write( *,"('边界节点总数为:',g0)" ) Numcol
  Allocate( nb(NumCol) )

  !//存储第一类边界条件的场值
  !//设方程u(x,y)=sin(y)*Exp(-x)
  Open( FileId,file='nb1.txt',status='unknown' )
  Do i = 1,NumCol
    Read( FileId,* ) nb(i)
  End Do
  Close( FileId )

  !//查询边界节点的坐标，赋予边界场值
  Open( FileId,file='xy.txt',status='unknown' )
  Open( FileId1,file='U1.txt',status='unknown' )
  Open( FileId2,file='AnaSol.dat',status='unknown' )  !//存放解析解，与有限元解f(x,y)=sin(y)*Exp(-x)进行比较

  loop1: Do i = 1,(nx+1)*(ny+1) + nx*ny
    Read( FileId,* ) node(i),temp1,temp2
    Write( FileId2,'(2f12.8,Es23.13)' ) temp1,temp2,sind(temp2)*Exp(-1.d0*temp1)
    loop2: Do j = 1,NumCol
      If( node(i)==nb(j) ) Then
        Write( FileId1,'(Es20.13)' ) sind(temp2)*Exp(-1.d0*temp1)  !//存储边界条件
      End If
    End Do loop2
  End Do loop1

  Close( FileId )
  Close( FileId1 )
  Close( FileId2 )
  Deallocate( nb )
  Deallocate( e )
  Call System( "pause" )

End Program TriGrd