Program main
  Implicit none
  Character(len=20) date,time

  Open ( 101, file = 'runtime.txt' )
  Call date_and_time(date,time)  
  Write(101,"('Date:',A8,/,'Time:',A2,':',A2,':',A2)") date,time(1:2),time(3:4),time(5:6)

  Call date_and_time(date,time)  
  Write(101,"('Date:',A8,/,'Time:',A2,':',A2,':',A2)") date,time(1:2),time(3:4),time(5:6)
  Close ( 101 )
  
End program main