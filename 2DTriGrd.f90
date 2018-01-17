!----------------------------------------------------------------------------------------
!
!------------------�˳������Ҫ�����ǶԶ�άƽ����й������������ʷ�---------------------------
!
!----------------------------------------------------------------------------------------
Program TriGrd
  Implicit None
  Integer             :: i,j,k,ii,jj !//ѭ������
  Integer             :: NumCol  !//�洢nb1.txt�ļ��еĸ���
  !//�ʷ�����ķ�Χ�Լ�����
  Real                :: Xmin,Xmax,dx,Ymin,Ymax,dy
  !//X�����Y����Ľڵ�
  Integer             :: nx,ny
  Integer,Parameter   :: FileId = 11, FileId1 = 12, FileId2 = 13  !//ͨ����
  Integer,Allocatable :: node(:) !//�ڵ�
  Real,Allocatable    :: nb(:)  !//��һ��߽�������ֵ�ڵ�
  Real                :: temp1,temp2   !//��ʱ����
  !//��ŵ�Ԫ��ŵ��Զ�����������
  Type :: Element
    Integer :: x,y,z
  End Type Element
  Type(Element),Allocatable :: e(:,:,:)


  Write(*,*) '!-----------------------------------------------------------------!'
  Write(*,*) '!----------�˳�����������ǶԶ�άƽ����й������������ʷ�---------!'
  Write(*,*) '!---------------------------���ߣ���������------------------------!'
  Write(*,*) '!------------������X����ļ��㷶ΧXmin,Xmax�Լ�����dx-------------!'
  Read(*,*) Xmin,Xmax,dx
  nx = Nint( (Xmax-Xmin)/dx )
  Write(*,*) '!------------������Y����ļ��㷶ΧYmin,Ymax�Լ�����dy-------------!'
  Read(*,*) Ymin,Ymax,dy
  ny = Nint( (Ymax-Ymin)/dy )
  Allocate(e(ny,nx,4))

  Write( *,"('��Ԫ����Ϊ:',g0)" ) nx*ny*4
  Allocate( node((nx+1)*(ny+1)+nx*ny) )
  !//����ڵ�����
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
  Write( *,"('�ܽ����Ϊ:',g0)" ) k
  Close( FileId )

  !//���ÿ����Ԫ�ı��
  !//��ǰ�ĸ�����Ԫ���б�Ŵ����Ա���к�����ż���
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
    !//���ѭ�������һ�У�����������
    If( i==ny ) Exit
    Do k = 1,4
      e(i+1,1,k)%x = e(i,1,k)%x + nx+1+nx
      e(i+1,1,k)%y = e(i,1,k)%y + nx+1+nx
      e(i+1,1,k)%z = e(i,1,k)%z + nx+1+nx
    End Do
  End Do
  !//�����I3.txt�ļ�
  Open( FileId,file='I3.txt',status='unknown' )
  Do i = 1,ny
    Do j = 1,nx
      Do k =1,4
        Write( FileId,'(3I10)' ) e(i,j,k)%x, e(i,j,k)%y, e(i,j,k)%z
      End Do
    End Do
  End Do
  Close( FileId )

  !//�洢��һ��߽糡ֵ�Ľڵ��
  NumCol = 0  !//k�洢nb1.txt�ļ��е�����
  Open( fileId,file='nb1.txt',status='unknown' )
  !//�����һ�б߽�ڵ��
  Do i = 1,nx+1
    NumCol = NumCol + 1
    Write( FileId,'(I10)' ) i
  End Do
  temp1 = 1 + nx+1+nx
  temp2 = nx+1 + nx+1+nx
  !//�������ߵĽڵ��
  Do j = 2,ny
    NumCol = NumCol + 2
    Write( FileId,'(I10)' ) Int(temp1)
    Write( fileId,'(I10)' ) Int(temp2)
    temp1 = temp1 + nx+1+nx
    temp2 = temp2 + nx+1+nx
  End Do
  !//�������һ�еĽڵ��
  Do i = 1,nx+1
    NumCol = NumCol + 1
    Write( FileId,'(I10)' ) Int(temp1 + i-1)
  End Do
  Close( FileId )
  Write( *,"('�߽�ڵ�����Ϊ:',g0)" ) Numcol
  Allocate( nb(NumCol) )

  !//�洢��һ��߽������ĳ�ֵ
  !//�跽��u(x,y)=sin(y)*Exp(-x)
  Open( FileId,file='nb1.txt',status='unknown' )
  Do i = 1,NumCol
    Read( FileId,* ) nb(i)
  End Do
  Close( FileId )

  !//��ѯ�߽�ڵ�����꣬����߽糡ֵ
  Open( FileId,file='xy.txt',status='unknown' )
  Open( FileId1,file='U1.txt',status='unknown' )
  Open( FileId2,file='AnaSol.dat',status='unknown' )  !//��Ž����⣬������Ԫ��f(x,y)=sin(y)*Exp(-x)���бȽ�

  loop1: Do i = 1,(nx+1)*(ny+1) + nx*ny
    Read( FileId,* ) node(i),temp1,temp2
    Write( FileId2,'(2f12.8,Es23.13)' ) temp1,temp2,sind(temp2)*Exp(-1.d0*temp1)
    loop2: Do j = 1,NumCol
      If( node(i)==nb(j) ) Then
        Write( FileId1,'(Es20.13)' ) sind(temp2)*Exp(-1.d0*temp1)  !//�洢�߽�����
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