!!! Numerical inquiry functions for intrinsic REAL types       !!!
PROGRAM  inqufctR
  IMPLICIT NONE

  INTEGER, PARAMETER :: sp = kind(1.0)
  INTEGER, PARAMETER :: dp = kind(1.0D0)
  INTEGER, PARAMETER :: xp = SELECTED_REAL_KIND(18,1000)
  INTEGER, PARAMETER :: qp = SELECTED_REAL_KIND(33,4931)

! SELECTED_REAL_KIND(P,R) asks for the KIND type parameter of the !
! "smallest" REAL type in which real numbers are representable as !
! floating-point numbers with a decimal precision of at least P   !
! (decimal mantissa digits) and a decimal range of at least R,    !
! i.e. numbers in the interval [10^-r, 10^+r] are (approximately) !
! representable.  Note that all Fortran systems must provide      !
! SINGLE and DOUBLE precision REALs, but double extended or       !
! quadruple precision REAL types may not be available, leading    !
! to a compile-time error.                                        !

  REAL (KIND=sp) :: s     ! single precision
  REAL (KIND=dp) :: d     ! double precision
  REAL (KIND=xp) :: x     ! double extended precision
  REAL (KIND=qp) :: q     ! quadruple precision
  REAL           :: r     ! DEFAULT REAL

  WRITE(*,*)
  WRITE(*,*) ' ===== Single Precision REAL Type (Default) ==== '
  WRITE(*,*) 'KIND(s)        :', kind(s)      ! kind parameter value !
  WRITE(*,*) 'RADIX(s)       :', radix(s) ! base b of representation !
  WRITE(*,*) 'DIGITS(s)      :', digits(s)      ! # of base-b digits !
  WRITE(*,*) 'MINEXPONENT(s) :', minexponent(s)   ! minimal exponent !
  WRITE(*,*) 'MAXEXPONENT(s) :', maxexponent(s)   ! maximal exponent !
  WRITE(*,*) 'HUGE(s)        :', huge(s) ! largest fl-pt # in format !
  WRITE(*,*) 'TINY(s)        :', tiny(s)! smallest pos. normalized # !
  WRITE(*,*) 'EPSILON(s)     :', epsilon(s)   ! distance 1 to next # !
  WRITE(*,*) 'PRECISION(s)   :', precision(s)    ! decimal precision !
  WRITE(*,*) 'RANGE(s)       :', range(s) ! decimal (exponent) range !
  WRITE(*,*)
  WRITE(*,*) ' ===== Double Precision REAL Type ===== '
  WRITE(*,*) 'KIND(d)        :', kind(d)
  WRITE(*,*) 'RADIX(d)       :', radix(d)
  WRITE(*,*) 'DIGITS(d)      :', digits(d)
  WRITE(*,*) 'MINEXPONENT(d) :', minexponent(d)
  WRITE(*,*) 'MAXEXPONENT(d) :', maxexponent(d)
  WRITE(*,*) 'HUGE(d)        :', huge(d)
  WRITE(*,*) 'TINY(d)        :', tiny(d)
  WRITE(*,*) 'EPSILON(d)     :', epsilon(d)
  WRITE(*,*) 'PRECISION(d)   :', precision(d)
  WRITE(*,*) 'RANGE(d)       :', range(d)
  WRITE(*,*)
  WRITE(*,*) ' ===== Double Extended Precision REAL Type ===== '
  WRITE(*,*) 'KIND(x)        :', kind(x)
  WRITE(*,*) 'RADIX(x)       :', radix(x)
  WRITE(*,*) 'DIGITS(x)      :', digits(x)
  WRITE(*,*) 'MINEXPONENT(x) :', minexponent(x)
  WRITE(*,*) 'MAXEXPONENT(x) :', maxexponent(x)
  WRITE(*,*) 'HUGE(x)        :', huge(x)
  WRITE(*,*) 'TINY(x)        :', tiny(x)
  WRITE(*,*) 'EPSILON(x)     :', epsilon(x)
  WRITE(*,*) 'PRECISION(x)   :', precision(x)
  WRITE(*,*) 'RANGE(x)       :', range(x)
  WRITE(*,*)
  WRITE(*,*) ' ===== Quadruple Precision REAL Type ===== '
  WRITE(*,*) 'KIND(q)        :', kind(q)
  WRITE(*,*) 'RADIX(q)       :', radix(q)
  WRITE(*,*) 'DIGITS(q)      :', digits(q)
  WRITE(*,*) 'MINEXPONENT(q) :', minexponent(q)
  WRITE(*,*) 'MAXEXPONENT(q) :', maxexponent(q)
  WRITE(*,*) 'HUGE(q)        :', huge(q)
  WRITE(*,*) 'TINY(q)        :', tiny(q)
  WRITE(*,*) 'EPSILON(q)     :', epsilon(q)
  WRITE(*,*) 'PRECISION(q)   :', precision(q)
  WRITE(*,*) 'RANGE(q)       :', range(q)
  WRITE(*,*)
  WRITE(*,*) ' Default REAL Type has KIND value:', kind(r)
  WRITE(*,*)
END PROGRAM  inqufctR

