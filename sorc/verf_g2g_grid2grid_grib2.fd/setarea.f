      SUBROUTINE setarea(iar,namarea,nchr, usrmk,
     +                   model,obstype)
C************************************************************************
      INCLUDE 'parm.inc'
      CHARACTER*24 namarea, nam24
      CHARACTER*1 nam1(24)
      EQUIVALENCE (nam24,nam1(1))
      CHARACTER*3 regnam(mxarea), regions(100)
      COMMON /grdef/ig104(147,110),numreg(mxarea),mode(mxarea),
     +  imax(mxarea),imin(mxarea),jmax(mxarea),jmin(mxarea),
     +  alat1(mxarea),elon1(mxarea), dxx(mxarea), dyy(mxarea),
     +  elonv(mxarea), alatan(mxarea), latlong(mxarea),
     +  lambert(mxarea), polarstereo(mxarea),regions

      LOGICAL*1 latlong, lambert, polarstereo
      CHARACTER*8 dumstn
      CHARACTER*8 stnlist
      COMMON /stndef/ nstns (mxarea), stnlist (mxarea,maxj)
      integer p, len   !add by Binbin to get "/" position  
      integer usrmk    !add by Binbin to set user defined region

      character*24 model,obstype 
C-------------------------------------------------------------------------

      nam24 = namarea
      mode(iar) = 0
      iqstn = INDEX ( nam24, '.STNS' )
      IF (nam1(1).eq.'G' .and. iqstn .eq. 0 ) THEN

c  Add by Binbin Zhou------------------------------------------------
       open (21, file='regions', status='old')
c--------------------------------------------------------------------
        p = index( nam24, '/' )
        len = len_trim(nam24)

         igrid = ID (nam24) 

czhou        READ (nam24(2:4),'(I3)') igrid

        write(*,*) 'in setarea: igrid=',igrid

        CALL gtgdef(igrid,istat,imax(iar),jmax(iar),alat1(iar),
     +              elon1(iar),dxx(iar),dyy(iar),elonv(iar),alatan(iar),
     +              latlong(iar),lambert(iar),polarstereo(iar),
     +              model, obstype)

        write(*,*)'in setarea: after call gtgdef, imax=',imax(iar)
        write(*,*)'in setarea: after call gtgdef, jmax=',jmax(iar)

        imin(iar) = 1
        jmin(iar) = 1

czhou   IF (nam24(5:5).eq.'/') THEN
czhou      regnam(iar) = nam24(6:8)

        IF(usrmk.eq.1) then        !for user defined region
          numreg(iar) = 33        
          mode(iar) = 2 
          PRINT *, nam24, ' is region id # ', numreg(iar)        
         return                     
        END IF

        IF (p .gt. 0) THEN
          regnam(iar) = trim ( nam24(p+1:len) )
          IF (ig104(75,55).eq.0) THEN
czhou            READ (20,'(20I4)') ig104
Czhou           PRINT*,'IG104: ',IG104
            do i = 1, 32
             READ (21,'(4X,A3)') regions(i)
            end do
            close(21)
         PRINT*,'REGIONS: ',REGIONS
          END IF
          DO nr = 1, 32                             !g2g: add N. hemisphere (NHM),  S. hemisphere(SHM) 
            IF (regnam(iar).eq.regions(nr)) THEN
              numreg(iar) = nr
              mode(iar) = 2
              PRINT *, regnam(iar), ' is region id # ', numreg(iar) 
              RETURN
            END IF
          END DO
          mode(iar) = 1
          numreg(iar) = 0   !add by Binbin
        ELSE
          mode(iar) = 1
          numreg(iar) = 0   !add by Binbin
        END IF
      ELSE IF ( iqstn .ne. 0 ) THEN
	nstns (iar) = 0
	OPEN ( UNIT=22, FILE=nam24, STATUS='OLD',
     +         IOSTAT=ios )
	IF ( ios .ne. 0 ) WRITE (6,*)
     +	  'Cannot open file ', nam24
	DO WHILE ( ios .eq. 0 )
	  READ ( 22, '(A)', IOSTAT=ios ) dumstn
	  IF ( ios .eq. 0 ) THEN
	    nstns (iar) = nstns (iar) + 1
	    stnlist ( iar, nstns(iar) ) = dumstn
          END IF
	END DO
	mode (iar) = 3
	namarea = nam24 (1:iqstn-1)
	nchr = iqstn - 1
	CLOSE ( UNIT = 22 )
      ELSE
        mode(iar) = 0
        PRINT *, ' NON-CONFORMING AREA PARAMETER NEEDS TO BE Gxxx'
	PRINT *, ' OR xxx.STNS, where xxx is an upper case name.'
      END IF

      RETURN
      END
