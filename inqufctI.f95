!!! Numerical inquiry functions for intrinsic INTEGER types    !!!
PROGRAM  inqufctI
  IMPLICIT NONE

! SELECTED_INT_KIND(R) asks for the KIND type parameter of the   !
! "smallest" INTEGER type in which all integers with R or less   !
! decimal digits (i.e. all integers in the open interval         !
! (-10^R, +10^R) are representable.                              !
! Note that some of these types may not be available, leading    !
! either to a compile-time error, e.g. if the 8 Byte = 64 bit    !
! INTEGER format is not available, or to multiple selections of  !
! the same type, e.g. if the 1 Byte = 8 bit format is not        !
! available (resulting in byte==half).                           !

  INTEGER, PARAMETER :: byte = SELECTED_INT_KIND(R=2)
  INTEGER, PARAMETER :: half = SELECTED_INT_KIND(R=4)
  INTEGER, PARAMETER :: full = SELECTED_INT_KIND(R=9)
  INTEGER, PARAMETER :: long = SELECTED_INT_KIND(R=18)

  INTEGER (KIND=byte) :: b     ! 1-byte integer
  INTEGER (KIND=half) :: h     ! 2-byte integer
  INTEGER (KIND=full) :: f     ! 4-byte integer
  INTEGER (KIND=long) :: l     ! 8-byte integer
  INTEGER             :: n     ! default integer

  WRITE(*,*)
  WRITE(*,*) ' ==== 1-Byte INTEGER Type ==== '
  WRITE(*,*) 'KIND(b)   : ', kind(b)      ! kind parameter value !
  WRITE(*,*) 'RADIX(b)  : ', radix(b)     ! base b of represent. !
  WRITE(*,*) 'DIGITS(b) : ', digits(b)    ! # of base b digits   !
  WRITE(*,*) 'HUGE(b)   : ', huge(b)      ! largest value        !
  WRITE(*,*) 'RANGE(b)  : ', range(b)     ! decimal range R  =>  !
  WRITE(*,*)     ! all R-digit decimal numbers are representable !
  WRITE(*,*) ' ==== 2-Byte INTEGER Type ==== '
  WRITE(*,*) 'KIND(h)   : ', kind(h)
  WRITE(*,*) 'RADIX(h)  : ', radix(h)
  WRITE(*,*) 'DIGITS(h) : ', digits(h)
  WRITE(*,*) 'HUGE(h)   : ', huge(h)
  WRITE(*,*) 'RANGE(h)  : ', range(h)
  WRITE(*,*)
  WRITE(*,*) ' ==== 4-Byte INTEGER Type ==== '
  WRITE(*,*) 'KIND(f)   : ', kind(f)
  WRITE(*,*) 'RADIX(f)  : ', radix(f)
  WRITE(*,*) 'DIGITS(f) : ', digits(f)
  WRITE(*,*) 'HUGE(f)   : ', huge(f)
  WRITE(*,*) 'RANGE(f)  : ', range(f)
  WRITE(*,*)
  WRITE(*,*) ' ==== 8-Byte INTEGER Type ==== '
  WRITE(*,*) 'KIND(l)   : ', kind(l)
  WRITE(*,*) 'RADIX(l)  : ', radix(l)
  WRITE(*,*) 'DIGITS(l) : ', digits(l)
  WRITE(*,*) 'HUGE(l)   : ', huge(l)
  WRITE(*,*) 'RANGE(l)  : ', range(l)
  WRITE(*,*)
  WRITE(*,*) ' Default INTEGER Type has KIND value:', kind(n)
  WRITE(*,*)
END PROGRAM  inqufctI

