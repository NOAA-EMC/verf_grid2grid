PROGRAM BITMAP_OUT_UNDEFINE_NESDIS_AOD
IMPLICIT NONE
!
! 06/29/2007 Ho-Chun Huang
!  This program check nesdis aod data and reset the proper bitmap for 'copygb'
!  (1) separate real aod value and index value, assign .flase. to bitmap for
!      null datalocation
!  (2) apply copygb on two records separately (ip=2, neighbor; width=2)
!      choose interpolation method, and area of search as well as minimum
!      percentage of valid data to do mapping carefully
!  (3) recombine two record using interpolated aod data and index values 
!
INTEGER, PARAMETER :: kind_io4 = 4
INTEGER, PARAMETER :: kind_io8 = 8
integer, parameter :: imax = 801,jmax = 534
INTEGER, PARAMETER :: maxdata=imax*jmax
integer, parameter :: NMON = 12, NDAY = 31, MAXDAY = NMON*NDAY
integer, parameter :: NHR = 24,  NAREA = 3
INTEGER, PARAMETER :: JPDS_DIM = 200    ! default SIZE for JPDS array in libw3.a
REAL(kind=kind_io4), DIMENSION(MAXDATA)  :: VTMP, VC8, VDIAG
LOGICAL*1, DIMENSION(MAXDATA)  :: LB, L0, L1
INTEGER, DIMENSION(JPDS_DIM) :: JPDS, JGDS, KPDS, KGDS, KPDSGG, KGDSGG, KENS

character (len=200) :: idir, xdir, filein, filex, cdum, file1, file2, fmt, cmt
integer :: i, j, k, l, m, n, a, b, c, idum, iin, iout, idiag
INTEGER :: NUMDATA, NUMOUT, IRET
integer :: iost, iargc, iargs, ierr
integer :: iyr, imb, idy, ihr
logical :: check = .true.
logical :: chkfle, ALL_NEG

integer iini

check = .false.
check = .true.

ALL_NEG = .true.
!
! Select mapping option
!
cmt="-K-1 -B-1 -g148 -i'2,2' -x"

iargs = iargc()
if (iargs<1) STOP 'USAGE: *.x YYYYMMDD HH'
call getarg(1,cdum)
read(cdum,'(i4,i2,i2)') iyr, imb, idy
call getarg(2,cdum)
read(cdum,'(i2)') ihr

iin  = 1
iout = 2
idiag = 3

!ZZ idir='/meso/noscrub/wx20jh/aod-smoke/aod/'
!ZZ write(xdir,'(a,''nesdis.'',i4,i2.2,i2.2,''/'')') trim(idir), iyr, imb, idy
JPDS(:) = -1
JGDS(:) = -1
write(filein,'(''G13.'',i4,i2.2,i2.2,i2.2,''15.all.aod_conc.NAM3.grd'')') &
          iyr, imb, idy, ihr
!ZZ write(filex,'(a,a)') trim(xdir), trim(filein)
write(filex,'(a)') trim(filein)

INQUIRE(EXIST=CHKFLE,FILE=TRIM(FILEX))
IF ( CHKFLE ) THEN
  if (check) write(*,'(''FOUND '', a)') trim(filein)
  CALL BAOPENR(IIN,TRIM(FILEX),IRET)
  print*,'iret=',iret
  IF (IRET /= 0 ) STOP 'INPUT FILE OPEN FAILED'
!ZZ  write(file1,'(a,''A'',i2.2)') trim(xdir),ihr
  write(file1,'(''A'',i2.2)') ihr
  INQUIRE(EXIST=CHKFLE,FILE=TRIM(FILE1))
  IF ( CHKFLE ) THEN
    WRITE(FMT,"('/bin/rm -f ',a)") TRIM(FILE1)
    CALL SYSTEM(FMT)
  END IF
  JPDS(5)  = 129       ! indicator of parameter and unit
  JPDS(6)  = 200       ! type of level or height, sfc = 1, level = 105
  JPDS(7)  = 0         ! value of level or height
  JPDS(16) = 0         ! time range indicator, inst = 10, avg = 3, accum = 4
  VTMP = 0.
  j=-1
  iini=0
  CALL GETGB(IIN,iini,MAXDATA,j,JPDS,JGDS,NUMOUT,k,KPDS,KGDS,LB,VTMP,IERR)
  IF ( IERR /= 0 ) STOP 'ERROR READ IN GETGB 1'
  L0=.FALSE.
  L1=.FALSE.
  KPDS(4) = 64
  DO I = 1, NUMOUT
    IF ( VTMP(I) >= 0. ) THEN
      L0(I)   = .TRUE.
      ALL_NEG = .FALSE.
    ELSE
      L1(I)   = .TRUE.
    END IF
  END DO
!! 12/26/2007 Ho-Chun if all negative value, i.e., L0(:) = .FALSE.
!!                    PUTGB will return error code 
!!                    ==== 8, W3FI73 ERROR, ALL VALUES IN IBMAP ARE ZERO ====
!!                    NO record will be wrote to file A that lead to read in
!!                    error of file H (after copygb)
!!                    SOLUTION: treat this hour as missing and stop 
!!                              producing G13 file
!! 12/26/2007 Ho-Chun Second situation leading to read H GB faild is the
!!                    post-copygb H file contains no mapping record of 
!!                    corresponding rec of A file.  Reason is unknown so far
!!                    It maybe due to lacking number of valid grid value within
!!                    searching box or minimum percentage of data.
!!                    SOLUTION: treat this hour as missing and stop
!!                              producing G13 file
!!
  IF ( .NOT. ALL_NEG ) THEN       !! WRITE OUTPUT FILE
    CALL BAOPENW(IOUT,TRIM(FILE1),IRET)
    IF (IRET /= 0 ) STOP 'OUTPUT FILE OPEN FAILED'
    CALL PUTGB(IOUT,NUMOUT,KPDS,KGDS,L0,VTMP,IRET)
    IF ( IRET  /= 0 ) WRITE(*,'(''ERROR PUTGB A 129 L0 CODE = '', I5)') IRET
    KPDS(5)  = 254
    CALL PUTGB(IOUT,NUMOUT,KPDS,KGDS,L1,VTMP,IRET)
    IF ( IRET  /= 0 ) WRITE(*,'(''ERROR PUTGB A 254 L1 CODE = '', I5)') IRET
!
    JPDS(5)  = 89        ! indicator of parameter and unit
    JPDS(6)  = 105       ! type of level or height, sfc = 1, level = 105
    JPDS(7)  = 5000      ! value of level or height
    JPDS(16) = 0         ! time range indicator, inst = 10, avg = 3, accum = 4
    VTMP = 0.
    iini=0
    j=0
    CALL GETGB(IIN,iini,MAXDATA,j,JPDS,JGDS,NUMOUT,k,KPDS,KGDS,LB,VTMP,IERR)
    IF ( IERR  /= 0 ) WRITE(*,'(''ERROR GETGB 89 CODE = '', I5)') IERR
    KPDS(4) = 64
!!
!! WRITE OUTPUT FILE
!!
    CALL PUTGB(IOUT,NUMOUT,KPDS,KGDS,L0,VTMP,IRET)
    IF ( IRET  /= 0 ) WRITE(*,'(''ERROR PUTGB A 89 L0 CODE = '', I5)') IRET
!
    CALL BACLOSE(IIN,IRET)
    IF (IRET /= 0 ) STOP 'INPUT FILE CLOSE FAILED AOD'
    CALL BACLOSE(IOUT,IRET)
    IF (IRET /= 0 ) STOP 'OUTPUT FILE CLOSE FAILED'
!!
!!  CHANGE GRID
!!
    write(file2,'(''H'',i2.2)') ihr
    INQUIRE(EXIST=CHKFLE,FILE=TRIM(FILE2))
    IF ( CHKFLE ) THEN
      WRITE(FMT,"('/bin/rm -f ',a)") TRIM(FILE2)
      CALL SYSTEM(FMT)
    END IF
    WRITE(FMT,"('/nwprod/util/exec/copygb ',a,TR1,a,TR1,a)") TRIM(CMT),TRIM(FILE1), TRIM(FILE2)
    CALL SYSTEM(FMT)
!!
!! COMBINE POST_MAPPING VALUES FOR GRID2GRID
!!
    CALL BAOPENR(IIN,TRIM(FILE2),IRET)
    IF (IRET /= 0 ) STOP 'INPUT FILE OPEN FAILED POST-COPYGB'
    JPDS(5)  = 129       ! indicator of parameter and unit
    JPDS(6)  = 200       ! type of level or height, sfc = 1, level = 105
    JPDS(7)  = 0         ! value of level or height
    JPDS(16) = 0         ! time range indicator, inst = 10, avg = 3, accum = 4
    VTMP = 0.
    L1=.FALSE.
    iini=0
    j=-1
    CALL GETGB(IIN,iini,MAXDATA,j,JPDS,JGDS,NUMOUT,k,KPDS,KGDS,L1,VTMP,IERR)
!
    IF ( IERR  == 0 )   THEN    !! OUTPUT FILE
!
!ZZ      write(filex,'(a,''G13.t'',i2.2,''z.f00'')') trim(xdir),ihr
      write(filex,'(''G13.t'',i2.2,''z.f00'')') ihr
      INQUIRE(EXIST=CHKFLE,FILE=TRIM(FILEX))
      IF ( CHKFLE ) THEN
        WRITE(FMT,"('/bin/rm -f ',a)") TRIM(FILEX)
        CALL SYSTEM(FMT)
      END IF
      CALL BAOPENW(IOUT,TRIM(FILEX),IRET)
!ZZ      write(filex,'(a,''X13.t'',i2.2,''z.f00'')') trim(xdir),ihr
      write(filex,'(''X13.t'',i2.2,''z.f00'')') ihr
      CALL BAOPENW(IDIAG,TRIM(FILEX),IRET)
      IF (IRET /= 0 ) STOP 'OUTPUT FILE OPEN FAILED POST-COPYGB'
!
! IF OUTPUT L1=.F., VTMP=0.
!
!!
!! USE INTERPOLATED INDEX FOR NULL DATA POINT  <=0. .and. LB
!!
      JPDS(5)  = 254       ! indicator of parameter and unit
      VC8  = 0.
      LB=.FALSE.
      iini=0
      j=-1 
      CALL GETGB(IIN,iini,MAXDATA,j,JPDS,JGDS,NUMOUT,k,KPDS,KGDS,LB,VC8,IERR)
      IF ( IERR  /= 0 )   STOP 'ERROR READ IN GETGB H 2'

      L0=.FALSE.
      VDIAG = VTMP
      DO I = 1, NUMOUT
        IF ( VTMP(I) > 0. ) THEN
          L0(I)   = .TRUE.
        ELSE IF ( VTMP(I) == 0. ) THEN
          L0(I)   = .TRUE.
          IF ( LB(I) )  VTMP(I) = -9.
          IF ( LB(I) .and. VC8(I) == -9. )  VDIAG(I) = -9.
          IF ( LB(I) .and. VC8(I) == -1. )  VDIAG(I) = -1.
          IF ( LB(I) .and. VC8(I) > -9. .and. VC8(I) < -1. )  VDIAG(I) = -6.
        ELSE
          WRITE(*, '( ''VC = '',F10.5,TR5,''INDEX = '',F10.5)') VTMP(I), VC8(I)
        END IF
      END DO
      KPDS(4) = 64
      KPDS(5) = 129
      CALL PUTGB(IOUT,NUMOUT,KPDS,KGDS,L0,VTMP,IRET)
      CALL PUTGB(IDIAG,NUMOUT,KPDS,KGDS,L0,VDIAG,IRET)
      CALL BACLOSE(IDIAG,IRET)
      IF (IRET /= 0 ) STOP 'DIAG   FILE CLOSE FAILED AOD POST-COPYGB'
!
      JPDS(5)  = 89        ! indicator of parameter and unit
      JPDS(6)  = 105       ! type of level or height, sfc = 1, level = 105
      JPDS(7)  = 5000      ! value of level or height
      JPDS(16) = 0         ! time range indicator, inst = 10, avg = 3, accum = 4
      VTMP = 0.
      iini=0
      j=0
      CALL GETGB(IIN,iini,MAXDATA,j,JPDS,JGDS,NUMOUT,k,KPDS,KGDS,LB,VTMP,IERR)
      IF ( IERR  /= 0 )   STOP 'ERROR READ IN GETGB 2'
!!
!! WRITE OUTPUT FILE
!!
      KPDS(4) = 64
      CALL PUTGB(IOUT,NUMOUT,KPDS,KGDS,L0,VTMP,IRET)
!
      CALL BACLOSE(IIN,IRET)
      IF (IRET /= 0 ) STOP 'INPUT FILE CLOSE FAILED AOD POST-COPYGB'
      CALL BACLOSE(IOUT,IRET)
      IF (IRET /= 0 ) STOP 'OUTPUT FILE CLOSE FAILED AOD POST-COPYGB'
!!
!! Remove Intermediated FILEs
!!
    ELSE
      WRITE(*,'(''ERROR READ IN POST copygb H-files, code = '', I5)') IERR
      WRITE(*,'(''Code 99 request not found'')')
      WRITE(*,'(''NO FILE GENERATED FOR '',A)') TRIM(FILEIN)
    END IF
    WRITE(FMT,"('/bin/rm -f ',a,TR1,a)") TRIM(FILE1), TRIM(FILE2)
    CALL SYSTEM(FMT)
  ELSE  !! STOP PROCESSING
    WRITE(*,'(''ALL MISSING INDEX, NO FILE GENERATED FOR '',A)') TRIM(FILEIN)
  END IF
ELSE
  WRITE(*,'('' FILE NOT FOUND, PROGRAM STOP'')')
END IF
END PROGRAM BITMAP_OUT_UNDEFINE_NESDIS_AOD
