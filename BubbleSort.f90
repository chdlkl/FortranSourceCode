Program BubbleSort
  Implicit none
  Integer :: a(6) = [ 5, 4, 2, 3, 1, 3 ]
  Integer :: i, j, temp
  
  Write (*,'(3x,g0)') '排序之前:'
  Write (*,*) a
  Do j = 1, size(a) - 1
    Do i = 1, size(a) - j
      If ( a(i)>a(i+1) ) Then
        temp = a(i)
        a(i) = a(i+1)
        a(i+1) = temp
      End If
    End Do
  End Do
  Write (*,'(3x,g0)') '排序之后:'
  Write (*,*) a
End Program BubbleSort