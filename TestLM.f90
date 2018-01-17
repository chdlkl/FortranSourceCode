!----------------------------------------------------------------------------
!                                     LM�㷨
!                      ˵���������еı���������������ʱ�ѳ�ʼ��
!                           ����ģ�ͣ�y = A*Exp( -B*x )
!                           Ŀ�꣺����LM�㷨���ݲ���A��B
!                         ԭ���̣�y = 20.5*Exp( -0.24*x )
!                                   ���ߣ�³����
!                              ������ڣ�2017��5��18��
!----------------------------------------------------------------------------
Module VarLM
  Implicit None
  !//NparasΪ����������NdataΪ���ݸ���,NitersΪ����������
  Integer     ,Parameter :: Nparas = 2,Ndata = 9,Niters = 50,Fileid = 11
  !//��ʱ������order=1����������������ֹͣ����
  Integer                :: order
  !//ѭ��������itΪ��������ѭ������
  Integer                :: it,i,ii
  !//x_1Ϊ�۲������Ա�����y_1Ϊ�۲���������������߾�Ϊ��֪����
  Real(kind=8),Parameter :: x_1(Ndata) = [0.25,0.5,1.0,1.5,2.0,3.0,4.0,6.0,8.0]
  Real(kind=8),Parameter :: y_1(Ndata)  = [19.306,18.182,16.126,14.302,12.685,9.978,7.849,4.857,3.005]   
  !//a0��b0�ֱ�Ϊ�����²��ʼֵ,RealA��RealBΪ��ֵ
  Real(kind=8),Parameter :: a0 = 0.,b0 = 0.,RealA = 20.5,RealB = 0.24
	!//����޶�
  Real(kind=4)           :: eps = 1e-8
  !//y_estΪ����ֵ��dΪ����ֵ��ʵ��ֵy_1֮�����rdΪ����d�ı���
  Real(kind=8)           :: y_est(Ndata) = 0.,d(Ndata) = 0.,rd(Ndata,1) = 0.
  !//JΪ�ſɱȾ���JTΪJ��ת�þ���HΪ��ɭ����
  Real(kind=8)           :: J(Ndata,Nparas) = 0.,JT(Nparas,Ndata) = 0.,H(Nparas,Nparas) = 0.
  !//Inv_H_1mΪH_1m�������
  Real(kind=8)           :: H_Lm(Nparas,Nparas)=0.,Inv_H_Lm(Nparas,Nparas)=0.
  !//eyeΪ��λ����deltaΪ��������
  Real(kind=8)           :: eye(Nparas,Nparas) = 0.0,delta(Nparas,1) = 0.0
  !//a_est��b_est�ֱ�Ϊ���ݲ���
  Real(kind=8)           :: a_est = 0.,b_est = 0.,e = 0.
  !//�����¶�Ӧ��ֵ
  Real(kind=8)           :: y_est_Lm(Ndata) = 0.,d_Lm(Ndata) = 0.,e_Lm = 0.
  !//��ʱ������lamdaΪLM�㷨������ϵ����ʼֵ
  Real(kind=8)           :: a_Lm,b_Lm,lamda = 0.01,v = 10.d0,temp
 
End Module VarLM
  

Program TestLM
  Include 'link_fnl_shared.h'
  Use VarLM
  Use LINRG_INT
  Implicit None
  
  Write(*,"('------------------------------------------------')") 
  Write(*,"('    ����ģ�ͣ�y = A*Exp( -B*x )     ')")
  Write(*,"('   Ŀ�꣺����LM�㷨���ݲ���A��B     ')")
  Write(*,"('          ���ߣ���������            ')") 
  Write(*,"('       ������ڣ�2017��5��18��      ')") 
  !//���ɵ�λ����
  Forall( i = 1:Nparas, ii = 1:Nparas, i==ii ) eye(i,ii) = 1.d0
  
  !//��һ����������ֵ
  order = 1
  a_est = a0
  b_est = b0
  
  !//�ڶ���������
  Write(*,"('------------------------------------------------')") 
  loop1: Do it = 1,Niters
    If ( order==1 ) Then  !//���ݵ�ǰ����ֵ�������ſɱȾ���
      
      y_est = a_est*Exp( -b_est*x_1 ) !//���ݵ�ǰa_est��b_est��x_1���õ�����ֵy_est
      d = y_1 - y_est   !//������ֵ֪y_1��y_est�����
      
      loop2: Do i = 1,Ndata  !//�����ſɱȾ���dy/da = Exp(-b*x)��dy/db = -a*x*Exp(-b*x)
        J(i,1) = Exp( -b_est*x_1(i) ) 
        J(i,2) = -a_est*x_1(i)*Exp( -b_est*x_1(i) )
      End Do loop2    
      
      JT=Transpose(J)   
      H = Matmul( JT,J )  !//���㺣ɭ����
      
    End If
		
		If ( it==1 ) e = Dot_Product(d,d)  !//���ǵ�һ�ε������������epsilon
    H_Lm = H + lamda*eye   !//��������ϵ��lamda��ϵõ�H����
    
    !//���㲽��delta�������ݲ��������µĲ�������ֵ
    Call LINRG( H_Lm,Inv_H_Lm )   !//ʹ��imsl�����⣬����H_Lm�������Inv_H_Lm
    
    rd = Reshape( d,[Ndata,1] )  !//Ϊ�������ڲ�����Matmul�ļ��㷨�򣬶�d��������״���иı�
    delta = Matmul( Inv_H_Lm,matmul( JT,rd ) )  !//deltaΪ����
    a_Lm = a_est + delta(1,1)
    b_Lm = b_est + delta(2,1)
    
		!//���||delta||<1e-8����ֹ����
		If ( Dot_Product(delta(:,1),delta(:,1))<eps ) Exit
		
    !//�����µĿ��ܹ���ֵ��Ӧ��y�ͼ���в�e
    y_est_Lm = a_Lm*Exp( -b_Lm*x_1 )
    d_Lm = y_1 - y_est_Lm
    e_Lm = Dot_Product( d_Lm,d_Lm )  !//e_LM����||y_1 - y_est_LM||
    
    !//������������θ��²���������ϵ��
		!//�����ɹ�ʱ��lamda��С����������lamda
    If ( e_Lm<e ) Then
		  lamda = lamda/v
      a_est = a_Lm
      b_est = b_Lm
      e = e_Lm
      order = 1
    Else
      order = 0
      lamda = lamda*v
    End If
    
    Write(*,"('a_est=',g0,2X,'b_est=',g0)") a_est,b_est
  End Do loop1
  Write(*,"('------------------------------------------------')") 
  Write(*,"('ֹͣ�������ܹ�����',g0,'��')") it - 1
  !//��������������Լ��ٷֱ����������ļ�,�ܹ�100�����ݵ�,��������Ϊ[0-50]
  Open(Fileid,file='ԭʼ�����뷴������.dat',status='unknown')
  Do i = 0,100
    temp = i/2.d0
    Write(Fileid,'(f7.3,3f15.8)') temp,RealA*Exp( -RealB*temp ),a_est*Exp( -b_est*temp ),&
      Abs( a_est*Exp( -b_est*temp )-RealA*Exp( -RealB*temp ) )/RealA/Exp( -RealB*ii )*100
  End Do
  Close(Fileid)
  !//������ݲ���a_est��b_est�Լ�ԭʼ���ݺ��������
  Write(*,"(/,'���ݲ���Ϊ��')")
  Write(*,"('a_est=',g0)") a_est
  Write(*,"('b_est=',g0)") b_est
  Write(*,"(/,'ԭʼ����Ϊ��')") 
  Write(*,'(3f9.4/)') y_1
  Write(*,"('�������Ϊ��')") 
  Write(*,'(3f9.4/)') a_est*Exp( -b_est*x_1 )
  Write(*,"('------------------------------------------------')") 
  
End Program TestLM