Program GetFileNum
   Implicit None
   integer :: n
   External ToDoOneFile
   call DoWithWildcard( "*.txt" , ToDoOneFile , n )
   write(*,*) '��',n,'���ļ�'
End Program GetFileNum

Subroutine ToDoOneFile( cFile , iLoop )
   Character( Len = * ) , Intent( IN ) :: cFile
   Integer , Intent( IN ) :: iLoop
   Write( * , * ) '��',iLoop,'���ļ���',cFile
End Subroutine ToDoOneFile

   
Subroutine DoWithWildcard(cWildcard,CallBack,iTotal)
   !// ��һ����룬����� Compaq �� Digital�����Ϊ Use DFLib
   Use IFPort , only : GetFileInfoQQ , GetLastErrorQQ , FILE$INFO , FILE$LAST , FILE$ERROR , FILE$FIRST , ERR$NOMEM , ERR$NOENT , FILE$DIR
   Implicit None
   Interface 
     Subroutine CallBack( cFile , iLoop )
       Character( Len = * ) , Intent( IN ) :: cFile
       Integer , Intent( IN ) :: iLoop
     End Subroutine CallBack
   End Interface
   Character( Len = * ) , Intent( IN ) :: cWildcard
   Integer , Intent( OUT ) :: iTotal
   Type (FILE$INFO) :: stInfo
   Integer(KIND=INT_PTR_KIND()) iWildhandle
   Integer(4) :: iLength , iRet  
   iWildhandle = FILE$FIRST
   iTotal = 0
   Do While (.TRUE.)
       iLength = GetFileInfoQQ( cWildCard , stInfo , iWildhandle )
       If (( iWildhandle == FILE$LAST) .OR.( iWildhandle == FILE$ERROR )) then
         Select Case (GetLastErrorQQ())
         Case (ERR$NOMEM)  !//�ڴ治��
           iTotal = - 1
           return
         Case (ERR$NOENT)  !//����ͨ�������β
           return
         Case Default
           iTotal = 0
           return
         End Select
       End If
       If ( ( stInfo%permit.AND.FILE$DIR ) == 0 ) then
         call CallBack( Trim(stInfo%Name) , iTotal + 1 )
         iTotal = iTotal + 1
       End If
   End Do
 End Subroutine DoWithWildcard