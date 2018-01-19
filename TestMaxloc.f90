Program main
  Integer :: a(5) = [ -1, 2, 4, -7, 0 ]
  Print*, ' 数组a中最大元素位置为 ', maxloc( a )  !// 应该是3
  Print*, ' 数组a(2:)中最大元素位置为 ', maxloc( a(2:) )  !// 答案是2
  Print*, ' maxloc只返回作用的数组片段中的位置 '
End program main