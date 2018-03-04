!// 派生数据类型的动态内存分配
Program Derived_data_Types
  Implicit none
  Type :: person_info
    Character(len=20) :: name
    Character(len=20) :: school
    Integer :: age
  End type person_info
  type( person_info ), allocatable :: luk
  allocate( luk )
  luk = person_info( 'lukailiang', "Chang'an university", 25 )
  Write(*,*) luk
  Deallocate( luk )
End program Derived_data_Types