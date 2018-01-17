Program GetexeRoute
  character(512) :: GetFileInAppDirectory
  write(*,*) Trim(GetFileInAppDirectory(""))
End program GetexeRoute

Character(512) Function GetFileInAppDirectory( cFile )
    use Kernel32 , only : GetModuleFileName
    Character(*) , Intent( IN ) :: cFile
    Integer i
    Character(512) :: cTemp
    i = GetModuleFileName( 0 , cTemp , 512 )
    i = Index( Trim(cTemp) , "\" , back = .true. )
    cTemp( i+1: ) = cFile
    GetFileInAppDirectory = cTemp
End Function GetFileInAppDirectory
