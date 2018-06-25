!-------------------------------------------------
!.. ԭʼ���ݴ洢��shuju.txt
!.. Ƶ�����ݴ洢��traned.txt
!.. ��任����洢��re_traned.txt
!.. trans_method=.true.ʱ������Ҷ���任
!.. trans_method=.false.ʱ������Ҷ��任
!-------------------------------------------------

Program trans
  Logical trans_method
  trans_method = .True.
  Call fft_pro('shuju.txt', 'traned.txt', trans_method)
  trans_method = .False.
  Call fft_pro('traned.txt', 're_traned.txt', trans_method)
End Program trans



Subroutine fft_pro(infile, outfile, trans_method)
  Character *(*) infile, outfile
  Logical trans_method
  Integer i, n, ig
  Real dt, temp, temp1
  Complex, Allocatable :: cx(:)
  n = 0
  temp = 0.0
  dt = 0.0
  If (trans_method) Then
    ig = -1
  Else
    ig = 1
  End If
  Open (40, File=infile, Status='old')
  Do While (.Not. eof(40))
    temp = dt
    Read (40, *) dt
    n = n + 1
  End Do
  Close (40)
  Allocate (cx(n))
  If (ig==-1) Then
    dt = dt - temp
  Else
    dt = 1.0/(dt-temp)/n
  End If
  Open (30, File=infile, Status='old')
  Do i = 1, n
    Read (30, *) temp1, cx(i)
  End Do
  Close (30)
  Call fft(n, cx, ig)
  Open (20, File=outfile)
  Do i = 1, n
    If (ig==-1) Then
      temp = 1.0*i/dt/n
      Write (20, *) temp, cx(i), abs(cx(i))
    Else
      temp = dt*i
      temp1 = cx(i)
      Write (20, *) temp, cx(i), temp1
    End If
  End Do
  Close (20)
End Subroutine fft_pro