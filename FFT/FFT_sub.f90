Subroutine fft(lx, cx, ig)
  Complex cx(lx), carg, cw, ctemp
  l = lx/2
  sc = sqrt(1./lx)
  a = acos(-1.d0)*ig
  j = 1
  Do i = 1, lx
    If (i<=j) Then
      ctemp = cx(j)*sc
      cx(j) = cx(i)*sc
      cx(i) = ctemp
    End If
    m = l
    20 If (j>m) Then
      j = j - m
      m = m/2
      If (m>=1) Goto 20
    End If
    j = j + m
  End Do
  l = 1
  40 istep = 2*l
  ab = a/l
  sa = 0.0
  ca = 1.0
  sb = sin(ab)
  cb = cos(ab)
  cw = cmplx(ca, sa)
  Do m = 1, l
    Do i = m, lx, istep
      ctemp = cw*cx(i+l)
      cx(i+l) = cx(i) - ctemp
      cx(i) = cx(i) + ctemp
    End Do
    Call swing(ca, sa, cb, sb)
    cw = cmplx(ca, sa)
  End Do
  l = istep
  If (l<lx) Goto 40
  Return
End Subroutine fft
  
  
Subroutine swing(ca, sa, cb, sb)
  d = cb - 1.0
  z1 = ca + d*ca - sa*sb
  z2 = sa + d*sa + ca*sb
  t = 1.5 - .5*(z1*z1+z2*z2)
  ca = t*z1
  sa = t*z2
  Return
End Subroutine swing

