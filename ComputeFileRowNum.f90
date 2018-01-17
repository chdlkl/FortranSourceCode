Program ComputeFileRowNum
  Implicit none 
  Character(*), parameter :: filename = '123.par'
  Integer :: fileid, Info, num

  num = 0
  Open ( newunit = fileid, file = filename )
  Do 
    Read ( fileid, fmt = *, iostat = info )
    If ( info /= 0 ) exit 
    num = num + 1
  End do 
  Print*, num 
  Read ( *,* ) 

End program ComputeFileRowNum
