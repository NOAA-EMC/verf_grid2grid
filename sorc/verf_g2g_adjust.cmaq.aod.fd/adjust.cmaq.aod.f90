Program adjust_cmaq_aod
implicit none
!
! Adjust the CMAQ AOD with column depth; remove the background
! atmospheric igaseous Raleigh scattering
!
! 07/17/2007 Ho-Chun Huang  Starting from July 17 06z run, the CMAQ AOD
!                           atmos bkgrnd AOD is removed before vertical
!                           integration in Pius Lee's post processing.
!                           Thus, read and write the record without any
!                           adjustment

INTEGER, PARAMETER :: kind_io4 = 4
INTEGER, PARAMETER :: kind_io8 = 8
integer, parameter :: imax = 442, jmax = 265, maxdata=imax*jmax, kmax=22
integer, parameter :: NMON = 12, NDAY = 31, MAXDAY = NMON*NDAY
integer, parameter :: nhour = 48
INTEGER, PARAMETER :: JPDS_DIM = 200    ! default SIZE for JPDS array in libw3.a

REAL(kind=kind_io8), DIMENSION(IMAX,JMAX) :: VC
REAL(kind=kind_io8), DIMENSION(IMAX,JMAX)  :: VTMP, ZTOP, ZBOT
REAL(kind=kind_io8), DIMENSION(IMAX,JMAX)  :: ZDEP
REAL(kind=kind_io8), DIMENSION(NHOUR)  :: ZMAX, ZMIN

character (len=200) :: idir, xdir, filex, cdum, FMT
LOGICAL, DIMENSION(MAXDATA)  :: LB,L2, LB0
INTEGER, DIMENSION(JPDS_DIM) :: JPDS, JGDS, KPDS, KGDS, KPDSGG, KGDSGG, KENS
integer, dimension(nmon) :: mday = &
         (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
INTEGER, DIMENSION(3) :: count
character (len=120), DIMENSION(3) :: bin
REAL(kind=kind_io4), DIMENSION(3,31) :: per
integer :: i, j, k, l, m, n, a, b, c, idum, iin, iout, gk
INTEGER :: IB2, NUMDATA, NUMOUT, IRET
integer :: iost, iargc, iargs, fcst_hour, ierr
integer :: iseek, mseek, lskip, lgrib, iyr, imb, idb, ide, ileap
real :: rdum, bmax, bmin, dmax, dmin
logical :: check = .true.
logical :: flag_aod, flag_ztop, flag_zbot

check = .true.
check = .false.

iargs = iargc()
if (iargs<1) STOP 'USAGE: *.x YYYYMMDD'
call getarg(1,cdum)
read(cdum,'(i4,i2,i2)') iyr, imb, idb
ide=idb

ILEAP     = 0
IF ( MOD(IYR,400) == 0 ) THEN
  ILEAP = 1
ELSE
  IF ( MOD(IYR,100) == 0 ) THEN
    ILEAP = 0
  ELSE
    IF ( MOD(IYR, 4) == 0 ) THEN
      ILEAP = 1
    END IF
  END IF
END IF

if ( ileap == 1 ) mday(2)=29

iin  = 1
iout = 2

fcst_hour = 6

!ZZ idir='/meso/noscrub/wx20jh/aod-smoke/aod/'


DO A = idb, ide
!ZZ  write(xdir,'(a,''cmaq.'',i4,i2.2,i2.2,''/'')') trim(idir), iyr, imb, A
  count = 0
!!
!! AOD
!!
  vc = 0.
  do L = 1, NHOUR
    JPDS(:) = -1
    JGDS(:) = -1
    ISEEK = 0
    LSKIP = 0
    LGRIB = 0
    MSEEK = 128
!ZZ    write(filex,'(a,''aqm.t'',i2.2,''z.aot'',i2.2)') trim(xdir),fcst_hour,L
    write(filex,'(''aqm.t'',i2.2,''z.aot'',i2.2)') fcst_hour,L
    if (check) write(*,*) trim(filex)
    CALL BAOPENR(IIN,TRIM(FILEX),IRET)
    IF (IRET /= 0 ) STOP 'INPUT FILE OPEN FAILED'
    flag_aod  = .false.
    flag_ztop = .false.
    flag_zbot = .false.

!jp    do k = 1, kmax+4
     do k = 1, 1 
      ISEEK=ISEEK+LGRIB
      CALL SKGB(IIN,ISEEK,MSEEK,LSKIP,LGRIB)
      IF ( CHECK ) WRITE(*,'(''ISEEK = '', I8, '' MSEEK = '', I8, '' LSKIP = '', I8, '' LGRIB = '', I8)' ) ISEEK, MSEEK, LSKIP, LGRIB
!    CALL GETGB(IIN,0,MAXDATA,0,JPDS,JGDS,NUMOUT,0,KPDS,KGDS,LB,VTMP,IERR)
      CALL GETGB1R(IIN,LSKIP,LGRIB,NUMOUT,KPDS,KGDS,KENS,LB,VTMP,GK,IERR)
      IF ( IERR  /= 0 ) THEN
        IF ( NUMOUT == 0 ) THEN
          IF ( CHECK ) WRITE(*,'(''DATA AFTER GETGB = '', I10)') NUMOUT
          IF ( CHECK ) WRITE(*,'(''DATA EXPECTED    = '', I10)') IMAX*JMAX
        ELSE
          PRINT *, IERR, 'ERROR READ IN GETGB PM K = ', K
        END IF
      END IF
      IF ( NUMOUT /= IMAX*JMAX ) THEN
!
! sometime files contain different record but we may already read what we want
!
!jp        IF ( NUMOUT == 0 .AND. flag_aod .AND. flag_ztop .AND. flag_zbot ) THEN
        IF ( NUMOUT == 0 .AND. flag_aod ) THEN
          EXIT
        ELSE
          WRITE(*,'(''ERROR READ IN GETGB # DATA /= NUMGRID '', ''DATA SIZE = '', I10)') NUMOUT
          STOP 
        END IF
      END IF
!!      IF ( KPDS(5) == 255 .AND. KPDS(6) == 200 .AND. KPDS(7) == 0 ) THEN
      IF ( KPDS(5) == 129 .AND. KPDS(6) == 200 .AND. KPDS(7) == 0 ) THEN
        VC=VTMP
        KPDSGG = KPDS
        KGDSGG = KGDS
        LB0 = LB
        flag_aod  = .true.
      END IF
!jp      IF ( KPDS(5) == 7 .AND. KPDS(6) == 107 .AND. KPDS(7) == 0 ) THEN
!jp        ZTOP=VTMP 
!jp        flag_ztop = .true.
!jp      END IF
!jp      IF ( KPDS(5) == 7 .AND. KPDS(6) == 107 .AND. KPDS(7) == 10000 ) THEN
!jp        ZBOT=VTMP 
!jp        flag_zbot = .true.
!jp     END IF
    end do
    CALL BACLOSE(IIN,IRET)
    IF (IRET /= 0 ) STOP 'INPUT FILE CLOSE FAILED'
!jp    ZDEP=ZTOP-ZBOT
    IF ( 1 == 2 ) WRITE(*,*) VC(IMAX/2,JMAX/2), ZDEP(IMAX/2,JMAX/2)
!!
!! ADJUSTED AOD  - 0.01 per km
!!
!! NOTE : THE negative value in the grid2grid FHO program means null data point
!!        and will not be counted.  Thus, reset negative value to a tiny value
!!
    IF ( 1 == 2 ) THEN    ! REMOVE ADJUSTMENT, see note above
      DO I = 1, IMAX
        DO J = 1, JMAX
!jp          VTMP(I,J) = VC(I,J) - ZDEP(I,J) * 1.0E-5
          VTMP(I,J) = VC(I,J) 
          IF ( VTMP(I,J) < 0. ) VTMP(I,J) = 1.0E-6
        END DO
      END DO
    END IF
!ZZ    write(filex,'(a,''aod.t'',i2.2,''z.f'',i2.2)') trim(xdir),fcst_hour,L
    write(filex,'(''aod.t'',i2.2,''z.f'',i2.2)') fcst_hour,L
    CALL BAOPENW(IOUT,TRIM(FILEX),IRET)
    IF (IRET /= 0 ) STOP 'OUTPUT FILE OPEN FAILED'
    
!    call putgb(iout,imax*jmax,kpdsgg,kgdsgg,LB0,VTMP,iret)
    call putgb(iout,imax*jmax,kpdsgg,kgdsgg,LB0,VC,iret)
    CALL BACLOSE(IOUT,IRET)
    IF (IRET /= 0 ) STOP 'OUTPUT FILE CLOSE FAILED'
  END DO
END DO
CLOSE(IOUT)
!! WRITE(*,'(''PROGRAM FINISH SUCCESSFULLY'')')
END Program adjust_cmaq_aod
