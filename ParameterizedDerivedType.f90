Program ParameterizedDerivedType
  Implicit none
  Type :: vector(kind,n)
    Integer, kind :: kind = kind(0.)
    Integer, len :: n = 3
    Real(kind) :: v(n)
  End type vector
  type( vector(kind(0.),3) ) :: vec
End program ParameterizedDerivedType