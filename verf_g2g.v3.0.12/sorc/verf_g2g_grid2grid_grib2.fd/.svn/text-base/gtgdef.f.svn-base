      SUBROUTINE gtgdef(igrid,istat,imax,jmax,alat1,elon1,dxx,dyy,elonv,
     +            alatan,latlong,lambert,polarstereo,model,obstype)
C                .      .    .                                       .
C SUBPROGRAM:    GTGDEF      RETRIEVE grid definition parameters
C   PRGMMR: Geoff  DiMego    ORG: W/NP22     DATE: 97-12-29
C
C ABSTRACT: RETRIEVES grid definition parameters 
C
C PROGRAM HISTORY LOG:
C   98-01-04  Geoff DiMego   Brand new code
C   2006-10-10 Binbin Z.     Add grid#255 for HYSPLIT MODEL
C   2012--9-15 Binbin Z.     Add RTMA 2.5 grid
C   2014-5-6   Binbin Z.     Add Firewx grid 

C USAGE:    CALL GTGDEF(IGRID,ISTAT,IMAX,JMAX,ALAT1,ELON1,
C    1    DXX,DYY,ELONV,ALATAN,LATLONG,LAMBERT,POLARSTEREO)
C
C   INPUT ARGUMENT LIST:
C     IGRID    - INTEGER NUMBER OF desired grid
C
C   OUTPUT ARGUMENT LIST:
C     ISTAT    - INTEGER =0  MEANS SUCCESSFUL COMPLETION
C                        = 1 MEANS GRID NOT currently supported
C     IMAX,JMAX- DIMENSIONS OF GRID
C     ALAT1,ELON1 LOCATION OF ORIGIN PT(1,1)
C     DXX,DYY  - MESH LENGTHS
C     ELONV    - VERTICAL LONGITUDE
C     ALATAN   - REFERENCE LATITUDE (FOR LAMBERT)
C     LATLONG,LAMBERT,POLARSTEREO  PROJECTION-TYPE SWITCHES
C            WHERE LATLONG,LAMBERT,POLARSTEREO are type LOGICAL
C
C REMARKS:

C ATTRIBUTES:
C   LANGUAGE: FORTRAN-77
C   MACHINE:  CRAY C-90
C$$$
      LOGICAL latlong, lambert, polarstereo

      INTEGER kgds(91)
      character*24 model, obstype, ens
      integer  dumy(20) 
C
      istat = 0
C     
C     USE W3FI71 WITH THE INPUT GRID NUMBER TO GET THE PDS
C     FROM WHICH TO EXTRACT THE GRID DEFINITION PARAMETERS
C
      ens=model(1:7) 
      write(*,*) 'In gtgdef: igrid, model, obstype=', 
     +   igrid, trim(model), '  ', trim(obstype),' ',trim(ens)
    
      if(igrid.lt.255) then     
       CALL w3fi71(igrid,kgds,istat)
       IF (istat.ne.0) RETURN
      else if(igrid.eq.255) then

        if(trim(model).eq.'HRRR'.and.
     +     trim(obstype).ne.'FIREWX' ) then
            kgds(2+2)=1799
            kgds(2+3)=1059

        else if (model(1:4).eq.'href' ) then
            kgds(2+2)=1121
            kgds(2+3)=881

        else if(trim(obstype).eq.'RTMA' ) then 
            kgds(2+2)=1073   !RTMA
            kgds(2+3)=689    !RTMA
            kgds(2+4)=23200
            kgds(2+5)=-108900
            kgds(2+6)=0
            kgds(2+7)=-91000
            kgds(2+8)=4000
            kgds(2+9)=4000
            kgds(2+10)=0
            kgds(2+11)=64
            kgds(2+12)=0
            kgds(2+13)=38000
            kgds(2+14)=0
            kgds(2+15)=0
            kgds(2+16)=0
            kgds(2+17)=-1
            kgds(2+18)=-1
            kgds(2+19)=0
            kgds(2+20)=255
            kgds(2+21)=-1
            kgds(2+22)=-1
            kgds(2+23)=-1
            kgds(2+24)=-1
            kgds(2+25)=-1
          else if(trim(obstype).eq.'RTMA2' .or.
     +        trim(obstype).eq.'URMA') then
            kgds(2+2)=2145   !2.5km RTMA
            kgds(2+3)=1377
            kgds(2+4)=20192
            kgds(2+5)=-238446
            kgds(2+6)=8
            kgds(2+7)=26500
            kgds(2+8)=2540
            kgds(2+9)=2540
            kgds(2+10)=0
            kgds(2+11)=64
            kgds(2+12)=25000
            kgds(2+13)=25000
            kgds(2+14)=0
            kgds(2+15)=0
            kgds(2+16)=0
            kgds(2+17)=-1
            kgds(2+18)=-1
            kgds(2+19)=0
            kgds(2+20)=255
            kgds(2+21)=-1
            kgds(2+22)=-1
            kgds(2+23)=-1
            kgds(2+24)=-1
            kgds(2+25)=-1

           else if(trim(obstype).eq.'FIREWX') then
            read(15,*) (dumy(i),i=1,8), kgds(2+2),kgds(2+3)

           else if(trim(obstype).eq.'SMOKE/255') then
         kgds(2+1)=0 
            kgds(2+2)=801
            kgds(2+3)=534
         kgds(2+4)=0
         kgds(2+5)=-175000
         kgds(2+6)=128
         kgds(2+7)=80000
         kgds(2+8)=-55000
         kgds(2+9)=150
         kgds(2+10)=150
         kgds(2+11)=64
         kgds(2+12)=0
         kgds(2+13)=-1
         kgds(2+14)=-1
         kgds(2+15)=-1
         kgds(2+16)=-1
         kgds(2+17)=-1
         kgds(2+18)=-1
         kgds(2+19)=0
         kgds(2+20)=255
         kgds(2+21)=-1
         kgds(2+22)=-1
         kgds(2+23)=-1
         kgds(2+24)=-1
         kgds(2+25)=-1

           else if(trim(obstype).eq.'DUST/255' ) then
         kgds(2+1)=0
            kgds(2+2)=601
            kgds(2+3)=251
         kgds(2+4)=25000
         kgds(2+5)=-125000
         kgds(2+6)=0
         kgds(2+7)=50000
         kgds(2+8)=-65000
         kgds(2+9)=65535
         kgds(2+10)=65535
         kgds(2+11)=64
         kgds(2+12)=0
         kgds(2+13)=-1
         kgds(2+14)=-1
         kgds(2+15)=-1
         kgds(2+16)=-1
         kgds(2+17)=-1
         kgds(2+18)=-1
         kgds(2+19)=1
         kgds(2+20)=255
         kgds(2+21)=-1
         kgds(2+22)=-1
         kgds(2+23)=-1
         kgds(2+24)=-1
         kgds(2+25)=-1

           else if(trim(obstype).eq.'NDAS'.or.
     +             trim(obstype).eq.'LAPS' ) then  !Isidora: modify here !for your system
            kgds(2+2)=801
            kgds(2+3)=581

           else if(trim(obstype).eq.'RAPANL3') then
            kgds(2+2)=1799
            kgds(2+3)=1059

           else if(trim(obstype).eq.'RAPANL4') then
            kgds(2+2)=1473
            kgds(2+3)=1025
           end if

           !even if model is HRRR, if verif is on MOSAIC grid, overwrite it
           if(trim(obstype).eq.'ONMOSAIC') then
            kgds(2+2)=1401
            kgds(2+3)=701
           end if

       end if
C     
C     FILL IN GRIDEF COMMON BLOCK
C     W3FI71 RETURNS 2 EXTRA VALUES AT BEGINNING OF ARRAY KGDS
C     THE FOLLOWING DEFINED REGARDLESS OF GRID PROJECTION
C     
      imax = kgds(2+2)
      jmax = kgds(3+2)

      write(*,*) 'imax,jmax=', imax,jmax
C     
C     USE KGDS(1+2) TO DETERMINE GRID PROJECTION
C     
C     KGDS(1+2) = 0 ----> LATITUDE/LONGITUDE
C     KGDS(1+2) = 1 ----> MERCATOR (NOT YET USED)
C     KGDS(1+2) = 3 ----> LAMBERT CONFORMAL
C     KGDS(1+2) = 5 ----> POLAR STEREOGRAPHIC
C     
      IF (kgds(1+2).eq.0) THEN
        latlong = .true.
        lambert = .false.
        polarstereo = .false.
      ELSE IF (kgds(1+2).eq.3) THEN
        latlong = .false.
        lambert = .true.
        polarstereo = .false.
      ELSE IF (kgds(1+2).eq.5) THEN
        latlong = .false.
        lambert = .false.
        polarstereo = .true.
      ELSE
        iret = 99
        WRITE (6,*) ' KGDS(1+2) = ', kgds(1+2)
        WRITE (6,*) ' GRID CAN NOT BE USED IN THIS CODE IRET= ', iret
        istat = 1
        RETURN
      END IF
C     
C     SET THE REST OF THE GRID PARAMETERS BASED ON PROJECTION TYPE
C     
C     Change has been made for LATLON definition --- Yuejian Zhu
C     Changed back with checking -- K. Brill
C     
      IF (latlong) THEN
        alat1 = kgds(4+2) * 0.001
        elon1 = kgds(5+2) * 0.001
	IF ( elon1 .lt. 0.0 ) elon1 = elon1 + 360
        elonv = 0.0
        alatan = 0.0
        dyy = kgds(9+2) * 0.001
        dxx = kgds(10+2) * 0.001
      END IF
C     
      IF (lambert) THEN
        alat1 = kgds(4+2) * 0.001
        elon1 = kgds(5+2) * 0.001
	IF ( elon1 .lt. 0.0 ) elon1 = elon1 + 360
        elonv = kgds(7+2) * 0.001
	IF ( elonv .lt. 0.0 ) elonv = elonv + 360
        alatan = kgds(12+3) * 0.001
        dxx = kgds(8+2) * 0.001
        dyy = kgds(9+2) * 0.001
      END IF
C     
      IF (polarstereo) THEN
        alat1 = kgds(4+2) * 0.001
        elon1 = kgds(5+2) * 0.001
	IF ( elon1 .lt. 0.0 ) elon1 = elon1 + 360
        elonv = kgds(7+2) * 0.001
	IF ( elonv .lt. 0.0 ) elonv = elonv + 360
        alatan = 0.0
        dxx = kgds(8+2) * 0.001
        dyy = kgds(9+2) * 0.001
      END IF
C     
      PRINT *, 'gridspecs ', lambert, alat1, elon1, elonv, alatan, dxx,
     +            dyy
C     
      WRITE (6,*) ' GREETINGS FROM THE GRID-DEFINITION CODE! '
      WRITE (6,*) ' THE GRID YOU HAVE CHOSEN IS NUMBER ', igrid
      IF (latlong) THEN
        WRITE (6,*) ' A LAT/LON GRID WITH RES= ', dxx, ' BY ', dyy, 
     +              ' DEG'
      ELSE IF (polarstereo) THEN
        WRITE (6,*) ' A POLAR STEREO GRID CENTERED AT ', elonv, ' DEG E'
        WRITE (6,*) ' AND A HORIZONTAL RESOLUTION OF ', dxx, ' KM'
      ELSE IF (lambert) THEN
        WRITE (6,*) ' A LAMBERT CONFORMAL GRID CENTERED AT ', elonv, 
     +              ' DEG E'
        WRITE (6,*) ' AND A HORIZONTAL RESOLUTION OF ', dxx, ' KM'
      END IF
      WRITE (6,*) ' HORIZONTAL DIMENSIONS ARE ', imax, ' X', jmax

      RETURN
      END
