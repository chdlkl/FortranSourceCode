Program GetFileNum
   Implicit None
   integer :: n
   External ToDoOneFile
   call DoWithWildcard( "*.txt" , ToDoOneFile , n )
   write(*,*) '共',n,'个文件'
End Program GetFileNum

Subroutine ToDoOneFile( cFile , iLoop )
   Character( Len = * ) , Intent( IN ) :: cFile
   Integer , Intent( IN ) :: iLoop
   Write( * , * ) '第',iLoop,'个文件：',cFile
End Subroutine ToDoOneFile

   
Subroutine DoWithWildcard(cWildcard,CallBack,iTotal)
   !// 下一句代码，如果是 Compaq 或 Digital，需改为 Use DFLib
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
         Case (ERR$NOMEM)  !//内存不足
           iTotal = - 1
           return
         Case (ERR$NOENT)  !//碰到通配符序列尾
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