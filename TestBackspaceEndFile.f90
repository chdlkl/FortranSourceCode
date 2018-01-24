Program main
  Implicit none
  Integer :: fileid, i
  
  Open ( newunit = fileid, file = 'test.txt' )
  Do i = 1, 10
    Write ( fileid, * ) i
  End do 
  Backspace ( fileid )
  Endfile ( fileid )
  Close ( fileid )
  
End program main