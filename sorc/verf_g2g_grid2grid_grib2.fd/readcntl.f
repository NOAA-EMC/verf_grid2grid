c
c
c    11/2012 B.Zhou Move from CCS to Zeus/WCOSS, ST_RMBL modified
c

      SUBROUTINE readcntl(numodel,numfcst,numvfdate,numvfyobs,numarea,
     +            numstat,numvarbl,numlevel,numvector)
C
      INCLUDE 'parm.inc'
C
      DIMENSION nchrmodel(maxmod), nchrfcst(mxfcst), nchrvfdate(mxdate),
     +            nchrvfyobs(maxobs), nchrarea(mxarea), 
     +            nchrstat(mxstat), nchrvarbl(mxvrbl), 
     +            nchrlevel(maxlvl)
      CHARACTER*24 namodel(maxmod), namfcst(mxfcst), 
     +            namvfdate(mxdate), namvfyobs(maxobs), 
     +            namarea(mxarea), namstat(mxstat), 
     +            namvarbl(mxvrbl), namlevel(maxlvl)
      CHARACTER*150 input, substr (26)
C
      COMMON /names/ namodel, namfcst, namvfdate, namvfyobs, namarea, 
     +            namstat, namvarbl, namlevel
      COMMON /nchrs/ nchrmodel, nchrfcst, nchrvfdate, nchrvfyobs, 
     +            nchrarea, nchrstat, nchrvarbl, nchrlevel
      LOGICAL vtflg
      COMMON /cnvrsns/ vtflg, nmbgrd (maxmod), concon (maxmod),
     +		       cenlon (maxmod)

C  For grid2grid 
      CHARACTER*80 str
      CHARACTER*24 fcst_ymdhf(mxfcst), obsv_ymdhf(maxobs)      !store YYYYMMDDHHFF string for fcst and obsv read from control
      CHARACTER*4 cyyyyfcst(mxfcst),cyyyyobsv(maxobs)
      CHARACTER*2 cmmfcst(mxfcst),cmmobsv(maxobs)
      CHARACTER*2 cddfcst(mxfcst),cddobsv(maxobs)
      CHARACTER*2 chhfcst(mxfcst),chhobsv(maxobs)
      CHARACTER*3 cfffcst(mxfcst),cffobsv(maxobs) ,gribid
      integer yyyyfcst(mxfcst), mmfcst(mxfcst), 
     +        ddfcst(mxfcst), hhfcst(mxfcst), fffcst(mxfcst),
     +        yyyyobsv(maxobs), mmobsv(maxobs),  
     +        ddobsv(maxobs), hhobsv(maxobs), ffobsv(maxobs)
      integer k5(mxvrbl),k6(mxvrbl),k7(mxvrbl),k4(mxvrbl),
     +        region_id(maxpts), igribid
      real region_latlon(2,maxpts), ptr1(2,mxarea), ptr2(2,mxarea)
      integer vectormrk(mxvrbl)
      CHARACTER*6 ck7(mxvrbl)

      CHARACTER*24 namlvl(mxvrbl,maxlvl)
      integer nchrlvl(mxvrbl,maxlvl) 
      CHARACTER*24 fho(mxvrbl),fhothr(mxvrbl,20)
      CHARACTER*24 afho(mxvrbl),afhothr(mxvrbl,20)
      CHARACTER*24 sfho(mxvrbl),sfhothr(mxvrbl,20)                      !sfho() array will not be used, only fho() is used
      CHARACTER*24 ffho(mxvrbl),ffhothr(mxvrbl,20)                      !ffho() array will not be used, only fho() is used
      integer  nchrfho(mxvrbl),nchrfhothr(mxvrbl,20),fhomrk(mxvrbl)
      integer  nchrafho(mxvrbl),nchrafhothr(mxvrbl,20),afhomrk(mxvrbl)
      integer  nchrsfho(mxvrbl),nchrsfhothr(mxvrbl,20),sfhomrk(mxvrbl) 
      integer  nchrffho(mxvrbl),nchrffhothr(mxvrbl,20),ffhomrk(mxvrbl) 
      real rfhothr(mxvrbl,20),rafhothr(mxvrbl,20),rsfhothr(mxvrbl,20),
     +     rffhothr(mxvrbl,20)
      integer usrmrk(100), continue_mrk(mxvrbl)
      integer anomly_mrk(mxvrbl),anomlylev(mxvrbl,maxlvl)

      COMMON /g2g/cyyyyfcst,cmmfcst,cddfcst,chhfcst,cfffcst,
     +            cyyyyobsv,cmmobsv,cddobsv,chhobsv,cffobsv,
     +             yyyyfcst, mmfcst, ddfcst, hhfcst, fffcst,
     +             yyyyobsv, mmobsv, ddobsv, hhobsv, ffobsv,
     +             k4,k5,k6,k7,ck7,vectormrk,namlvl,nchrlvl,
     +      fhomrk,fho,nchrfho,fhothr,nchrfhothr,rfhothr,
     +             continue_mrk,anomly_mrk,anomlylev,
     +     afhomrk,afho,nchrafho,afhothr,nchrafhothr,rafhothr
      COMMON /FRC/
     +     sfhomrk,sfho,nchrsfho,sfhothr,nchrsfhothr,rsfhothr,
     +     ffhomrk,ffho,nchrffho,ffhothr,nchrffhothr,rffhothr

C     for tendency:--------------------------------------------------
      DIMENSION  nchrfcst2(4,mxfcst), nchrvfdate2(4,mxdate)
      CHARACTER*24  namfcst2(4,mxfcst),namvfdate2(4,mxdate)

      CHARACTER*24 fcst_ymdhf2(4,mxfcst), obsv_ymdhf2(4,maxobs)      !store YYYYMMDDHHFF string for fcst and obsv read from control
      CHARACTER*4 cyyyyfcst2(4,mxfcst),cyyyyobsv2(4,maxobs)
      CHARACTER*2 cmmfcst2(4,mxfcst),cmmobsv2(4,maxobs)
      CHARACTER*2 cddfcst2(4,mxfcst),cddobsv2(4,maxobs)
      CHARACTER*2 chhfcst2(4,mxfcst),chhobsv2(4,maxobs)
      CHARACTER*3 cfffcst2(4,mxfcst),cffobsv2(4,maxobs) 
      CHARACTER*2 cdt(mxvrbl)
      CHARACTER*1 updown(mxvrbl,20) 
      CHARACTER*20 abc
      integer yyyyfcst2(4,mxfcst), mmfcst2(4,mxfcst),
     +        ddfcst2(4,mxfcst), hhfcst2(4,mxfcst), fffcst2(4,mxfcst),
     +        yyyyobsv2(4,maxobs), mmobsv2(4,maxobs),
     +        ddobsv2(4,maxobs), hhobsv2(4,maxobs), ffobsv2(4,maxobs)
      integer tendencymrk(mxvrbl), dt(mxvrbl)
      COMMON /tnd/cyyyyfcst2,cmmfcst2,cddfcst2,chhfcst2,cfffcst2,
     +            cyyyyobsv2,cmmobsv2,cddobsv2,chhobsv2,cffobsv2,
     +            yyyyfcst2, mmfcst2, ddfcst2, hhfcst2, fffcst2,
     +            yyyyobsv2, mmobsv2, ddobsv2, hhobsv2, ffobsv2,
     +            namfcst2,namvfdate2,nchrfcst2,nchrvfdate2,
     +            cdt, dt, tendencymrk, updown     
c--------------------------------------------------------------------

      integer wavemrk(mxvrbl), wv1(mxvrbl), wv2(mxvrbl)      
      CHARACTER*2 cwv1(mxvrbl),cwv2(mxvrbl)      
      COMMON  /wave/wavemrk,cwv1,cwv2,wv1,wv2
      CHARACTER*1 cwvdim                                        !cwvdim: wave dimension, 1 or 2 (1D or 2D)
 
      COMMON /grb/igribid       

 
      COMMON /reg/region_id, region_latlon, ptr1,ptr2

      integer p, lens, nt
      character*24 nam24


      CHARACTER*10 blocksize(mxvrbl)
      CHARACTER*1 tag(mxvrbl)
      integer nxy(mxvrbl) 

      COMMON /fss/tag,nxy

      CHARACTER*1 blank
      DATA blank /' '/

      anomly_mrk = 0     
      anomlylev = 0     

C
C   READ NUMBER OF MODELS TO BE VERIFIED, first model name, and
C   optional wind rotation grid # for the model.
C
C   Note:  no adherence to format is necessary for this input.
C
        READ (5,'(A)') input
        CALL ST_CLST ( input, ' ', ' ', 3, substr, num, ier )
	CALL ST_NUMB ( substr (1), numodel, ier )                !convert substr(1) to nummodel(integer)
CCzeus	CALL ST_RMBL ( substr (2), namodel (1), lng, ier )       !convert substr(2) to namodel (string) and delete
        lng = len_trim(substr(2))
        namodel(1) = substr(2)(1:lng)
	nchrmodel (1) = lng                                      !space as well within the string and get its length
	IF ( substr (3) .ne. ' ' ) THEN
	   CALL ST_NUMB ( substr (3), nmbgrd (1), ier )
	ELSE
	   nmbgrd (1) = -1
        END IF
C
C     NUMBER OF VERIFYING MODELS IS LIMITED TO MAXMOD
      IF (numodel.gt.maxmod) THEN
        PRINT '("  NUMBER OF VERIFYING MODELS EXCEEDS LIMIT OF",2I5)', 
     +              maxmod, numodel
        STOP 16
      END IF
C     
C     READ NUMODEL MODEL PNEMONICS AND GET CHARACTER COUNT FOR EACH
C     Also read in optional wind rotation flag (false if missing).
C     
C   Note:  no adherence to format is necessary for this input.
C
      DO 30 n = 1, numodel
        IF ( n .ne. 1 ) THEN
	    READ (5,'(A)') input
      	    CALL ST_CLST ( input, ' ', ' ', 2, substr, num, ier )
CCzeus	    CALL ST_RMBL ( substr (1), namodel (n), lng, ier )
            lng = len_trim(substr(1))
            namodel(n) = substr(1)(1:lng)
	    nchrmodel (n) = lng
	    IF ( substr (2) .ne. ' ' ) THEN
	       CALL ST_NUMB ( substr (2), nmbgrd (n), ier )
	    ELSE
	       nmbgrd (n) = -1
            END IF
	END IF
	CALL SETMODEL(N,NAMODEL(N),NCHRMODEL(N),namvfyobs(n))
	PRINT *, ' GRD # for wind rotation = ', nmbgrd (n)
   30 CONTINUE
       write(*,*) 'setmodel done'

C---------------------------------------------------------------------------------------
C  Following is modified by Binbin Zhou for retrieve fcst times and obsv dates
C  and also get YYYY, MM, DD, HH for retrieve data from fcst GRIB and obsv GRIB later-on

C   
C     READ NUMBER OF FORECAST HOURS TO BE VERIFIED AND first hour.
C     
c       CALL ST_READ  ( namfcst, nchrfcst, numfcst, iret )
c       IF ( iret .ne. 0 ) THEN
c    WRITE (6,*)
c     +		' End of file encountered reading forecast hours.'
c	    STOP
c	END IF
C
C*      NUMBER OF FORECAST HOURS IS LIMITED TO MXFCST
c        IF (numfcst.gt.mxfcst) THEN
c          PRINT '("  NUMBER OF FORECAST HOURS EXCEEDS LIMIT OF",2I5)', 
c     +              mxfcst, numfcst
c
c          STOP 17
c        END IF
C     
C       SET NUMFCST FORECAST HOUR PNEMONICS
C     
c        DO 60 n = 1, numfcst
c          CALL setfcst(n,namfcst(n),nchrfcst(n))
c   60   CONTINUE
C     
C     READ NUMBER OF VERIFICATION DATES
C     
c	CALL ST_READ  ( namvfdate, nchrvfdate, numvfdate, iret )
c	IF ( iret .ne. 0 ) THEN
c	    WRITE (6,*)
c     +		' End of file encountered reading verifying dates.'
c	    STOP
c	END IF
C
C*      NUMBER OF VERIFYING DATES IS LIMITED TO MXDATE
c        IF (numvfdate.gt.mxdate) THEN
c          PRINT '("  NUMBER OF VERIFYING DATES EXCEEDS LIMIT OF",2I5)', 
c     +              mxdate, numvfdate
c          STOP 18
c        END IF

C  Modified as following:       
                                                                                                                                       
       READ (5,*) numfcst, str
       write(*,*) numfcst, str
                                                                                                                                             
       numvfdate=numfcst

       do n = 1, numfcst
         READ (5, '(A)') input
         write(*,*)'fcst# = ', n, input
         call ST_CLST(input, ' ', ' ', 20, substr, num, ier )
         write(*,*) 'substr=', substr(1), substr(2),num 

CCzeus         CALL ST_RMBL ( substr (1), fcst_ymdhf(n), lng1, ier )
         lng1 = len_trim(substr(1))
         fcst_ymdhf(n) = substr(1)(1:lng1)
         namfcst(n)=fcst_ymdhf(n)(11:lng1)
         nchrfcst(n)=lng1-10
         CALL setfcst(n,namfcst(n),nchrfcst(n))
         cyyyyfcst(n)=fcst_ymdhf(n)(1:4)
         cmmfcst(n)=fcst_ymdhf(n)(5:6)
         cddfcst(n)=fcst_ymdhf(n)(7:8)
         chhfcst(n)=fcst_ymdhf(n)(9:10)
         cfffcst(n)=fcst_ymdhf(n)(11:13)
           
         CALL ST_NUMB (cyyyyfcst(n),yyyyfcst(n),iet)
         CALL ST_NUMB (cmmfcst(n),mmfcst(n),iet)
         CALL ST_NUMB (cddfcst(n),ddfcst(n),iet)
         CALL ST_NUMB (chhfcst(n),hhfcst(n),iet)
         !suppose 999 is not the forecast time
         if(trim(cfffcst(n)).eq.'NN') then
            fffcst(n)=999
         else
           CALL ST_NUMB (cfffcst(n),fffcst(n),iet)
         end if
          
         write(*,*)n,' yyyyfcst,mmfcst,ddfcst,hhfcst,fffcst=',
     +    yyyyfcst(n),mmfcst(n),ddfcst(n),hhfcst(n),fffcst(n)
                                                                                                                                   
CCzeus         CALL ST_RMBL ( substr (2), obsv_ymdhf(n), lng2, ier )
         lng2 = len_trim(substr(2))
         obsv_ymdhf(n) = substr(2)(1:lng2)
         namvfdate(n)=obsv_ymdhf(n)(1:10)
         nchrvfdate(n)=10
         cyyyyobsv(n)=obsv_ymdhf(n)(1:4)
         cmmobsv(n)=obsv_ymdhf(n)(5:6)
         cddobsv(n)=obsv_ymdhf(n)(7:8)
         chhobsv(n)=obsv_ymdhf(n)(9:10)
         cffobsv(n)=obsv_ymdhf(n)(11:12)

         CALL ST_NUMB (cyyyyobsv(n),yyyyobsv(n),iet)
         CALL ST_NUMB (cmmobsv(n),mmobsv(n),iet)
         CALL ST_NUMB (cddobsv(n),ddobsv(n),iet)
         CALL ST_NUMB (chhobsv(n),hhobsv(n),iet)

         if(trim(cffobsv(n)).eq.'NN') then
            ffobsv(n)=999
         else
           CALL ST_NUMB (cffobsv(n),ffobsv(n),iet)
         end if

         write(*,*)n,'yyyyobsv,mmobsv,ddobsv,hhobsv,ffobsv=',
     +    yyyyobsv(n),mmobsv(n),ddobsv(n),hhobsv(n),ffobsv(n) 

         do nt=1,4
 
CCzeus           CALL ST_RMBL(substr(2*nt+1),fcst_ymdhf2(nt,n),lng1,ier )
           lng1 = len_trim(substr(2*nt+1))
           fcst_ymdhf2(nt,n) = substr(2*nt+1)(1:lng1)
           namfcst2(nt,n)=fcst_ymdhf2(nt,n)(11:lng1)
           nchrfcst2(nt,n)=lng1-10
           CALL setfcst(n,namfcst2(nt,n),nchrfcst2(nt,n))
           cyyyyfcst2(nt,n)=fcst_ymdhf2(nt,n)(1:4)
           cmmfcst2(nt,n)=fcst_ymdhf2(nt,n)(5:6)
           cddfcst2(nt,n)=fcst_ymdhf2(nt,n)(7:8)
           chhfcst2(nt,n)=fcst_ymdhf2(nt,n)(9:10)
           cfffcst2(nt,n)=fcst_ymdhf2(nt,n)(11:13)
           CALL ST_NUMB (cyyyyfcst2(nt,n),yyyyfcst2(nt,n),iet)
           CALL ST_NUMB (cmmfcst2(nt,n),mmfcst2(nt,n),iet)
           CALL ST_NUMB (cddfcst2(nt,n),ddfcst2(nt,n),iet)
           CALL ST_NUMB (chhfcst2(nt,n),hhfcst2(nt,n),iet)

           if(trim(cfffcst2(nt,n)).eq.'NN') then
            fffcst2(nt,n)=999
           else
            CALL ST_NUMB (cfffcst2(nt,n),fffcst2(nt,n),iet)
           end if

c           write(*,*) fcst_ymdhf2(nt,n),
c     +                yyyyfcst2(nt,n),mmfcst2(nt,n),ddfcst2(nt,n),
c     +                hhfcst2(nt,n),fffcst2(nt,n)

CCzeus           CALL ST_RMBL(substr(2*nt+2),obsv_ymdhf2(nt,n),lng2,ier )
           lng2 = len_trim(substr(2*nt+2))
           obsv_ymdhf2(nt,n) = substr(2*nt+2)(1:lng2)
           namvfdate2(nt,n)=obsv_ymdhf2(nt,n)(1:10)
           nchrvfdate2(nt,n)=10
           cyyyyobsv2(nt,n)=obsv_ymdhf2(nt,n)(1:4)
           cmmobsv2(nt,n)=obsv_ymdhf2(nt,n)(5:6)
           cddobsv2(nt,n)=obsv_ymdhf2(nt,n)(7:8)
           chhobsv2(nt,n)=obsv_ymdhf2(nt,n)(9:10)
           cffobsv2(nt,n)=obsv_ymdhf2(nt,n)(11:12)
           CALL ST_NUMB (cyyyyobsv2(nt,n),yyyyobsv2(nt,n),iet)
           CALL ST_NUMB (cmmobsv2(nt,n),mmobsv2(nt,n),iet)
           CALL ST_NUMB (cddobsv2(nt,n),ddobsv2(nt,n),iet)
           CALL ST_NUMB (chhobsv2(nt,n),hhobsv2(nt,n),iet)
           if(trim(cffobsv2(nt,n)).eq.'NN') then
              ffobsv2(nt,n)=999
           else
              CALL ST_NUMB (cffobsv2(nt,n),ffobsv2(nt,n),iet)
           end if

c           write(*,*) obsv_ymdhf2(nt,n), 
c     +                yyyyobsv2(nt,n),mmobsv2(nt,n),ddobsv2(nt,n),
c     +                hhobsv2(nt,n),ffobsv2(nt,n)

         end do
       end do

        write(*,*) 'Forecast time and Obvervation time done!'


C---------------------------------------------------------------------------

C     
C       READ NUMVFDATE VERIFICATION PNEMONICS AND GET CHARACTER COUNT
C     
C     READ NUMBER OF VERIFYING OB TYPES
C     
	CALL ST_READ  ( namvfyobs, nchrvfyobs, numvfyobs, iret )
	IF ( iret .ne. 0 ) THEN
	    WRITE (6,*)
     +		' End of file encountered reading verifying ob types.'
	    STOP
	END IF
C
C*      NUMBER OF VERIFYING OBS IS LIMITED TO MAXOBS
        IF (numvfyobs.gt.maxobs) THEN
          PRINT '("  NUMBER OF VERIFYING OBS EXCEEDS LIMIT OF",2I5)', 
     +              maxobs, numvfyobs
          write(*,*) numvfyobs, maxobs
          STOP 19
        END IF
C     
C       SET NUMVFYOBS VERIFYING OBS PNEMONICS
C     
        DO 110 n = 1, numvfyobs
          CALL setobtyp(n,namvfyobs(n),nchrvfyobs(n))
  110   CONTINUE

      write(*,*) 'setobtyp done'

C     
C     READ NUMBER OF VERIFICATION AREAS
C     
	CALL READ_AREA(namarea,nchrarea,numarea,ptr1,ptr2,usrmrk,iret )
	IF ( iret .ne. 0 ) THEN
	    WRITE (6,*)
     +		' End of file encountered reading verification areas.'
	    STOP
	END IF

        write(*,*) 'In readcntl.f: User defined 4 points:'
        do n=1,numarea
         write(*,*) ptr1(1,n),ptr1(2,n),ptr2(1,n),ptr2(2,n)
        end do 
             
C
C*      NUMBER OF VERIFYING AREAS IS LIMITED TO MXAREA
        IF (numarea.gt.mxarea) THEN
          PRINT '("  NUMBER OF VERIFYING AREAS EXCEEDS LIMIT OF",2I5)', 
     +              mxarea, numarea
          STOP 20
        END IF

        region_id = -1
         igribid = ID (namarea (1))

c        CALL ST_NUMB ( gribid, igribid, ier )

        write(*,*) 'grib id =', igribid
        CALL getregionid(region_id,region_latlon,igribid,
     +          namodel(1), namvfyobs(1) )

c        do i = 1,30000 
c         write(*,*) i, region_latlon(1,i),region_latlon(2,i),
c     +              region_id(i)
c        end do
        
C     
C       SET NUMAREA DOMAIN PNEMONICS
C     
        DO 140 n = 1, numarea
          CALL setarea(n,namarea(n),nchrarea(n),usrmrk(n),
     +          namodel(n), namvfyobs(n) )
  140   CONTINUE


        write(*,*) 'setarea done'

C     
C     READ NUMBER OF STATISTICAL SCORES
C     
	CALL ST_READ  ( namstat, nchrstat, numstat, iret )
	IF ( iret .ne. 0 ) THEN
	    WRITE (6,*)
     +		' End of file encountered reading statistic types.'
	    STOP
	END IF
C
C       NUMBER OF STATISTICS IS LIMITED TO MXSTAT
        IF (numstat.gt.mxstat) THEN
          PRINT '("  NUMBER OF STATISTICS EXCEEDS LIMIT OF",2I5)',
     +		    mxstat, numstat
          STOP 21
        END IF

      do n = 1, numstat
       write(*,*) n, namstat(n)
      end do 

      write(*,*) 'STATISTICS TYPE done'

C-------------------------------------------------------------------------------
C Following are modified by Binbin Zhou to read both variable name and GRIB id 
C from control file

C     
C     READ VARIABLES TO BE VERIFIED
C     
c	CALL ST_READ  ( namvarbl, nchrvarbl, numvarbl, iret )
c	IF ( iret .ne. 0 ) THEN
c	    WRITE (6,*)
c     +		' End of file encountered reading parameters.'
c	    STOP
c	END IF
C 
C  Modified as:   

        numvector = 0
        vectormrk = 0
        fhomrk = 0
        afhomrk = 0
        tendencymrk=0
        wavemrk=0

        READ (5, '(A)') input
        CALL ST_CLST( input, ' ', ' ', 26, substr, num, ier)
        CALL ST_NUMB( substr(1), numvarbl, ier)
        nchrvarbl(1) = len_trim(substr(2))
        namvarbl(1) = substr(2)(1:nchrvarbl(1))
        CALL ST_NUMB( substr(3), k4(1), ier)
        CALL ST_NUMB( substr(4), k5(1), ier)
        CALL ST_NUMB( substr(5), k6(1), ier)
        CALL ST_NUMB( substr(6), k7(1), ier)
        ck7(1) = trim (substr(6))
        write(*,*) numvarbl, namvarbl(1), nchrvarbl(1),
     +     k4(1),k5(1),k6(1),k7(1)

        ntnd = index (namvarbl(1),'_TND')
        if (ntnd.gt.0) then
         nslash=index(namvarbl(1),'/')
         nlen=len_trim(namvarbl(1))
         cdt(1)=namvarbl(1)(nslash+1:nlen)
         CALL ST_NUMB( cdt(1), dt(1), ier)
         tendencymrk(1)=dt(1)       
        end if

        nwv = index (namvarbl(1),'_WV')
        if(nwv.gt.0) then
         nslash=index(namvarbl(1),'/')
         ndash=index(namvarbl(1),'-')
         nlen=len_trim(namvarbl(1))
         cwv1(1)=namvarbl(1)(nslash+1:ndash-1)
         cwv2(1)=namvarbl(1)(ndash+1:nlen)
         cwvdim=namvarbl(1)(nslash-1:nslash-1)
         CALL ST_NUMB( cwv1(1), wv1(1), ier)
         CALL ST_NUMB( cwv2(1), wv2(1), ier)
         if(cwvdim.eq.'1') then
          wavemrk(1)=1
         else if(cwvdim.eq.'2') then
          wavemrk(1)=2
         end if
        end if

        nM=-1
        nA=-1
        nF=-1
        nM = index (namvarbl(1),'_MAX')
        nA = index (namvarbl(1),'_AVG')
        nF = index (namvarbl(1),'_FSS')
        if(nM.gt.0 .or. nA.gt.0.or.nF.gt.0) then
         if (nM.gt.0) tag(1)='M'
         if (nA.gt.0) tag(1)='A'
         if (nF.gt.0) tag(1)='F'
         nslash=index(namvarbl(1),'/')
         nlen=len_trim(namvarbl(1))
         blocksize(1)=namvarbl(1)(nslash+1:nlen)        
         CALL ST_NUMB(blocksize(1), nxy(1), ier)

          write(*,*) 'nM, nA, nF, nxy(1)=', nM, nA,nF,nxy(1),tag(1)
          

        end if

        if (num.gt.7) then        !has FHO or AFHO or EFHO or SFHO
          nchrfho(1) = len_trim(substr(7))
          fho(1) = substr(7)(1:nchrfho(1))
          
         if(fho(1)(1:1).eq.'F' .or.
     +      fho(1)(1:1).eq.'E' ) then      !FHO or EFHO
          fhomrk(1) = num - 7
          do nx = 8, num 
            nchrfhothr(1,nx-7) = len_trim(substr(nx))
            fhothr(1,nx-7) = substr(nx)(1:nchrfhothr(1,nx-7))
              if(tendencymrk(1).eq.0) then
                rfhothr(1,nx-7) = ChartoReal(fhothr(1,nx-7))
              else
                updown(1,nx-7)=fhothr(1,nx-7)
     +           (nchrfhothr(1,nx-7):nchrfhothr(1,nx-7))
                abc=fhothr(1,nx-7)(1:(nchrfhothr(1,nx-7)-1))
                rfhothr(1,nx-7)=ChartoReal(abc)
              end if
            end do

         else if (fho(1)(1:1).eq.'S'  ) then   !SFHO
          sfhomrk(1) = num - 7
          do nx = 8, num
            nchrsfhothr(1,nx-7) = len_trim(substr(nx))
            sfhothr(1,nx-7) = substr(nx)(1:nchrsfhothr(1,nx-7))
              if(tendencymrk(1).eq.0) then
                rsfhothr(1,nx-7) = ChartoReal(sfhothr(1,nx-7))
              else
                updown(1,nx-7)=sfhothr(1,nx-7)
     +           (nchrsfhothr(1,nx-7):nchrsfhothr(1,nx-7))
                abc=sfhothr(1,nx-7)(1:(nchrsfhothr(1,nx-7)-1))
                rsfhothr(1,nx-7)=ChartoReal(abc)
              end if
            end do

         else if (fho(1)(1:1).eq.'P'  ) then   ! FSS (or PFHO)
          ffhomrk(1) = num - 7
          do nx = 8, num
            nchrffhothr(1,nx-7) = len_trim(substr(nx))
            ffhothr(1,nx-7) = substr(nx)(1:nchrffhothr(1,nx-7))
              if(tendencymrk(1).eq.0) then
                rffhothr(1,nx-7) = ChartoReal(ffhothr(1,nx-7))
              else
                updown(1,nx-7)=ffhothr(1,nx-7)
     +           (nchrffhothr(1,nx-7):nchrffhothr(1,nx-7))
                abc=ffhothr(1,nx-7)(1:(nchrffhothr(1,nx-7)-1))
                rffhothr(1,nx-7)=ChartoReal(abc)
              end if
            end do
 

         else                           !AFHO
          afhomrk(1) = num - 7
          do nx = 8, num
            nchrafhothr(1,nx-7) = len_trim(substr(nx))
            afhothr(1,nx-7) = substr(nx)(1:nchrafhothr(1,nx-7))
              if(tendencymrk(1).eq.0) then
                rafhothr(1,nx-7) = ChartoReal(afhothr(1,nx-7))
              else
                updown(1,nx-7)=afhothr(1,nx-7)
     +           (nchrafhothr(1,nx-7):nchrafhothr(1,nx-7))
                abc=afhothr(1,nx-7)(1:(nchrafhothr(1,nx-7)-1))
                rafhothr(1,nx-7)=ChartoReal(abc)
              end if
            end do
          end if
        end if 

        IF(numvarbl.GE.2) THEN
         DO n=2,numvarbl
           READ (5, '(A)') input
           CALL ST_CLST( input, ' ', ' ', 26, substr, num, ier)
           write(*,*) input
           write(*,*)  substr(1)
           nchrvarbl(n) = len_trim(substr(1))
           namvarbl(n) = substr(1)(1:nchrvarbl(n))
           write(*,*) nchrvarbl(n), namvarbl(n)

           CALL ST_NUMB( substr(2), k4(n), ier)
           CALL ST_NUMB( substr(3), k5(n), ier)
           CALL ST_NUMB( substr(4), k6(n), ier)
           CALL ST_NUMB( substr(5), k7(n), ier)
           ck7(n) = trim (substr(5))

           write(*,*)namvarbl(n),nchrvarbl(n),k4(n),k5(n),k6(n),k7(n)

           ntnd = index (namvarbl(n),'_TND')

          if (ntnd.gt.0) then
             nslash=index(namvarbl(n),'/')
             nlen=len_trim(namvarbl(n))
             cdt(n)=namvarbl(n)(nslash+1:nlen)
             CALL ST_NUMB( cdt(n), dt(n), ier)
             tendencymrk(n)=dt(n)
           end if


           nwv = index (namvarbl(n),'_WV')
           if(nwv.gt.0) then
            nslash=index(namvarbl(n),'/')
            ndash=index(namvarbl(n),'-')
            nlen=len_trim(namvarbl(n))
            cwv1(n)=namvarbl(n)(nslash+1:ndash-1)
            cwv2(n)=namvarbl(n)(ndash+1:nlen)
            cwvdim=namvarbl(n)(nslash-1:nslash-1)
            CALL ST_NUMB( cwv1(n), wv1(n), ier)
            CALL ST_NUMB( cwv2(n), wv2(n), ier)
            if(cwvdim.eq.'1') then
             wavemrk(n)=1
            else if(cwvdim.eq.'2') then
             wavemrk(n)=2
            end if
           end if

           nM=-1
           nA=-1
           nF=-1
           nM = index (namvarbl(n),'_MAX')
           nA = index (namvarbl(n),'_AVG')
           nF = index (namvarbl(n),'_FSS')
           write(*,*) n, nM,nA,nF
           if(nM.gt.0 .or. nA.gt.0.or.nF.gt.0) then
             if (nM.gt.0) tag(n)='M'
             if (nA.gt.0) tag(n)='A'
             if (nF.gt.0) tag(n)='F'
             nslash=index(namvarbl(n),'/')
             nlen=len_trim(namvarbl(n))
             blocksize(n)=namvarbl(n)(nslash+1:nlen)
             CALL ST_NUMB(blocksize(n), nxy(n), ier)
             write(*,*) 'tag(n),nxy(n)=',tag(n),nxy(n)
           end if

           if (num.gt.6) then

             nchrfho(n) = len_trim(substr(6))
             fho(n) = substr(6)(1:nchrfho(n))
            if(fho(n)(1:1).eq.'F' .or.
     +         fho(n)(1:1).eq.'E' ) then     !FHO or EFHO
            
             fhomrk(n) =  num - 6
             do nx = 7, num
              nchrfhothr(n,nx-6) = len_trim(substr(nx))
              fhothr(n,nx-6) = substr(nx)(1:nchrfhothr(n,nx-6))

              if(tendencymrk(n).eq.0) then
                rfhothr(n,nx-6) = ChartoReal(fhothr(n,nx-6))
              else 
                updown(n,nx-6)=fhothr(n,nx-6)
     +           (nchrfhothr(n,nx-6):nchrfhothr(n,nx-6))
                abc=fhothr(n,nx-6)(1:(nchrfhothr(n,nx-6)-1))
                rfhothr(n,nx-6) = ChartoReal(abc)
              end if
             end do

            else if (fho(n)(1:1).eq.'S'  ) then    ! SFHO

             sfhomrk(n) =  num - 6
             do nx = 7, num
              nchrsfhothr(n,nx-6) = len_trim(substr(nx))
              sfhothr(n,nx-6) = substr(nx)(1:nchrsfhothr(n,nx-6))

              if(tendencymrk(n).eq.0) then
                rsfhothr(n,nx-6) = ChartoReal(sfhothr(n,nx-6))
              else
                updown(n,nx-6)=sfhothr(n,nx-6)
     +           (nchrsfhothr(n,nx-6):nchrsfhothr(n,nx-6))
                abc=sfhothr(n,nx-6)(1:(nchrsfhothr(n,nx-6)-1))
                rsfhothr(n,nx-6) = ChartoReal(abc)
              end if
             end do

           else if (fho(n)(1:1).eq.'P'  ) then    ! FSS (or PFHO)

             ffhomrk(n) =  num - 6
             do nx = 7, num
              nchrffhothr(n,nx-6) = len_trim(substr(nx))
              ffhothr(n,nx-6) = substr(nx)(1:nchrffhothr(n,nx-6))

              if(tendencymrk(n).eq.0) then
                rffhothr(n,nx-6) = ChartoReal(ffhothr(n,nx-6))
              else
                updown(n,nx-6)=ffhothr(n,nx-6)
     +           (nchrffhothr(n,nx-6):nchrffhothr(n,nx-6))
                abc=ffhothr(n,nx-6)(1:(nchrffhothr(n,nx-6)-1))
                rffhothr(n,nx-6) = ChartoReal(abc)
              end if
             end do


            else                          !for AFHO

             afhomrk(n) =  num - 6
             do nx = 7, num
              nchrafhothr(n,nx-6) = len_trim(substr(nx))
              afhothr(n,nx-6) = substr(nx)(1:nchrafhothr(n,nx-6))
              if(tendencymrk(n).eq.0) then
                rafhothr(n,nx-6) = ChartoReal(afhothr(n,nx-6))
              else
                updown(n,nx-6)=afhothr(n,nx-6)
     +           (nchrafhothr(n,nx-6):nchrafhothr(n,nx-6))
                abc=afhothr(n,nx-6)(1:(nchrafhothr(n,nx-6)-1))
                rafhothr(n,nx-6) = ChartoReal(abc)
              end if
             end do
        
            end if
    
           end if
         END DO
        END IF


        !set mask to exclude undefined Soil values (0.0) over ocean points
      do n = 1, numvarbl
       if(k5(n).eq.11.and.k6(n).eq.112) continue_mrk(n)=9   !Soil Temperature
       if(k5(n).eq.114.and.k6(n).eq.112) continue_mrk(n)=10  !Soil Moisture,  old 9 is wrong
      enddo

      !set continueing variable mark
      do n = 1, numvarbl
       if(k4(n).eq.3.and.k5(n).eq.5.and.k6(n).eq.2)continue_mrk(n)=1
       if(k4(n).eq.3.and.k5(n).eq.5.and.k6(n).eq.3) continue_mrk(n)=1
       if(k4(n).eq.6.and.k5(n).eq.1) continue_mrk(n)=1  !total cloud
       if(k4(n).eq.6.and.k5(n).eq.2) continue_mrk(n)=1  !convective cloud
       if(k4(n).eq.6.and.k5(n).eq.3) continue_mrk(n)=1  !hi cloud
       if(k4(n).eq.6.and.k5(n).eq.4) continue_mrk(n)=1  !mid cloud
       if(k4(n).eq.6.and.k5(n).eq.5) continue_mrk(n)=1  !low cloud
       if(k4(n).eq.3.and.k5(n).eq.10) continue_mrk(n)=2   ! set for smoke/aod (density)

       if(k4(n).eq.19.and.k5(n).eq.0.and.k6(n).eq.1)continue_mrk(n)=6   !for visibility 
       !if(k4(n).eq.19.and.k5(n).eq.0.and.k6(n).eq.13)continue_mrk(n)=6   !for visibility prob < 400
       !if(k4(n).eq.19.and.k5(n).eq.0.and.k6(n).eq.14)continue_mrk(n)=6  !for visibility prob < 800
       !if(k4(n).eq.19.and.k5(n).eq.0.and.k6(n).eq.15)continue_mrk(n)=6  !for visibility prob < 1600
       !if(k4(n).eq.19.and.k5(n).eq.0.and.k6(n).eq.16)continue_mrk(n)=6  !for visibility prob < 3200
       !if(k4(n).eq.19.and.k5(n).eq.0.and.k6(n).eq.17)continue_mrk(n)=6  !for visibility prob < 6400
       !if(k4(n).eq.19.and.k5(n).eq.0.and.k6(n).eq.18)continue_mrk(n)=4  !for vsref fog prob

       if(k4(n).eq.19.and.k5(n).eq.0.and.k6(n).eq.13)continue_mrk(n)=4   !for visibility prob < 400
       if(k4(n).eq.19.and.k5(n).eq.0.and.k6(n).eq.14)continue_mrk(n)=4  !for visibility prob < 800
       if(k4(n).eq.19.and.k5(n).eq.0.and.k6(n).eq.15)continue_mrk(n)=4  !for visibility prob < 1600
       if(k4(n).eq.19.and.k5(n).eq.0.and.k6(n).eq.16)continue_mrk(n)=4  !for visibility prob < 3200
       if(k4(n).eq.19.and.k5(n).eq.0.and.k6(n).eq.17)continue_mrk(n)=4  !for visibility prob < 6400
       if(k4(n).eq.19.and.k5(n).eq.0.and.k6(n).eq.18)continue_mrk(n)=4  !for vsref fog prob

       if(k4(n).eq.3.and.k5(n).eq.5.and.k6(n).eq.215)continue_mrk(n)=5 !for ceiling



       if(k4(n).eq.16.and.k5(n).eq.196.and.k6(n).eq.200) 
     +                             continue_mrk(n)=4          !for composite reflectivity

       if(k4(n).eq.16.and.k5(n).eq.196.and.k6(n).eq.241) 
     +                             continue_mrk(n)=4          !for ref > 10
       if(k4(n).eq.16.and.k5(n).eq.196.and.k6(n).eq.242) 
     +                             continue_mrk(n)=4          !for ref > 20
       if(k4(n).eq.16.and.k5(n).eq.196.and.k6(n).eq.243) 
     +                             continue_mrk(n)=4          !for ref > 30
       if(k4(n).eq.16.and.k5(n).eq.196.and.k6(n).eq.244) 
     +                             continue_mrk(n)=4          !for ref > 40

       if(k4(n).eq.16.and.k5(n).eq.195.and.k6(n).eq.103)
     +                             continue_mrk(n)=4          !for hybrid scan reflectivity

       if(k4(n).eq.13.and.k5(n).eq.195.and.k6(n).eq.103)      !for smoke, 20151109, New added
     +                             continue_mrk(n)=2 


       if(k4(n).eq.16.and.k5(n).eq.197.and.k6(n).eq.200) 
     +                                     continue_mrk(n)=4  !for echo top
       !+                                     continue_mrk(n)=7  !for echo top

       if(k4(n).eq.16.and.k5(n).eq.197.and.k6(n).eq.245) 
     +                                     continue_mrk(n)=4  !for echo top>2000
       !+                                     continue_mrk(n)=7  !for echo top>2000
       if(k4(n).eq.16.and.k5(n).eq.197.and.k6(n).eq.246) 
     +                                     continue_mrk(n)=4  !for echo top>4000
       !+                                     continue_mrk(n)=7  !for echo top>4000
       if(k4(n).eq.16.and.k5(n).eq.197.and.k6(n).eq.247) 
     +                                     continue_mrk(n)=4  !for echo top>6000
       !+                                     continue_mrk(n)=7  !for echo top>6000
       if(k4(n).eq.16.and.k5(n).eq.197.and.k6(n).eq.248) 
     +                                     continue_mrk(n)=4  !for echo top>8000
       !+                                     continue_mrk(n)=7  !for echo top>8000
       if(k4(n).eq.16.and.k5(n).eq.197.and.k6(n).eq.249)
     +                                     continue_mrk(n)=4  !for echo top>10000
       !+                                     continue_mrk(n)=7  !for echo top>10000

       if(k4(n).eq.4.and.k5(n).eq.8.and.k6(n).eq.103) continue_mrk(n)=4  !for hibrid scan reflectivity 

      end do

        !set anomly mark
      do n = 1, numvarbl
       if(k4(n).eq.3.and.k5(n).eq.5.and.k6(n).eq.100) anomly_mrk(n)=1
       if(k4(n).eq.3.and.k5(n).eq.1.and.k6(n).eq.101) anomly_mrk(n)=1
       if(k4(n).eq.0.and.k5(n).eq.4.and.k6(n).eq.103) anomly_mrk(n)=1
       if(k4(n).eq.0.and.k5(n).eq.5.and.k6(n).eq.103) anomly_mrk(n)=1
       if(k4(n).eq.0.and.k5(n).eq.0.and.k6(n).eq.100) anomly_mrk(n)=1
       if(k4(n).eq.0.and.k5(n).eq.0.and.k6(n).eq.103) anomly_mrk(n)=1
       if(k4(n).eq.2.and.k5(n).eq.2.and.k6(n).eq.100) anomly_mrk(n)=1
       if(k4(n).eq.2.and.k5(n).eq.3.and.k6(n).eq.100) anomly_mrk(n)=1
       if(k4(n).eq.2.and.k5(n).eq.1.and.k6(n).eq.100) anomly_mrk(n)=1
      end do

      do n = 1, numvarbl
        write(*,*) 'variable:', n
        write(*,*)'readcntl,  fhomrk =',fhomrk(n),trim(fho(n)),
     +  (rfhothr(n,i),i=1,fhomrk(n))
        write(*,*)'  afhomrk =',afhomrk(n),trim(fho(n)),nchrafho(n),
     +  (rafhothr(n,i),i=1,afhomrk(n))
        write(*,*)'  tendencymrk,updown,dt, =',tendencymrk(n),
     +  (updown(n,i),i=1,10), dt(n)
        write(*,*)'  anomly_mrk=',anomly_mrk(n)
        write(*,*)'  wavemrk, wv1, wv2 =',wavemrk(n),wv1(n),wv2(n)
        write(*,*)'  continue_mrk=',continue_mrk(n) 
      end do


C---------------------------------------------------------------------------
C       NUMBER OF VARIABLES IS LIMITED TO MXVRBL
        IF (numvarbl.gt.mxvrbl) THEN
          PRINT '("  NUMBER OF VARIABLES EXCEEDS LIMIT OF",2I5)',
     +		mxvrbl, numvarbl
          STOP 22
        END IF
C     
C       FIND VECTOR VARIABLE (must be last in list)
C     
c	DO ivr = 1, numvarbl
c          IF (nchrvarbl(ivr).eq.4.and.namvarbl(ivr).eq.'VWND') THEN
c            numvector = ivr
c	    IF ( numvector .ne. numvarbl ) THEN
c		WRITE (6,*) ' VWND must be last in parm list.'
c		STOP 23
c	    END IF
c            numvarbl = numvarbl + 1
c            namvarbl(ivr+1) = namvarbl(ivr) (1:1)
c            nchrvarbl(ivr+1) = 1
c          END IF
c	END DO

       DO ivr = 1, numvarbl
c        IF (nchrvarbl(ivr).eq.4.and.namvarbl(ivr).eq.'VWND') THEN
        IF (k5(ivr).eq.32) THEN
          numvector = numvector + 1
          vectormrk(ivr) = 1
        END IF
       END DO         

       write(*,*) 'Read var done'

C     
C     READ NUMBER OF LEVEL DESCRIPTIONS
C     
	CALL ST_READ  ( namlevel, nchrlevel, numlevel, iret )
	IF ( iret .ne. 0 ) THEN
	    WRITE (6,*)
     +		' End of file encountered reading verifying levels.'
	    STOP
	END IF
C
C*      NUMBER OF VERIFYING LEVELS IS LIMITED TO MAXLVL
        IF (numlevel.gt.maxlvl) THEN
          PRINT '(" NUMBER OF VERIFYING LEVELS EXCEEDS LIMIT OF",2I5)', 
     +              maxlvl, numlevel
          STOP 24
        END IF
C     
C       READ NUMLEVEL LEVEL PNEMONICS AND GET CHARACTER COUNT
C     
        DO 220 n = 1, numlevel
          CALL setlevel(n,namlevel(n),nchrlevel(n))
  220   CONTINUE

c  add by Binbin:
        do n = 1, numvarbl
         write(*,*) 'k6(n) ck7(n)=',k6(n),trim(ck7(n))
         if(k6(n).eq.100.or.k6(n).eq.104) then
            namlvl(n,:) = namlevel(:)
            nchrlvl(n,:) = nchrlevel(:)
         else if (k6(n).eq.108) then
            namlvl(n,1) = 'P'//trim(ck7(n))
            nchrlvl(n,1) = len_trim(namlvl(n,1))
         else if (k6(n).eq.103) then
            namlvl(n,1) = 'H'//trim(ck7(n))
            nchrlvl(n,1) = len_trim(namlvl(n,1))
         else if (k6(n).eq.101) then 
            namlvl(n,1) = 'MSL'
            nchrlvl(n,1) = 3
         else if (k6(n).eq.200) then
            namlvl(n,1) = 'ATMOS'
            nchrlvl(n,1) = 5
         else if (k6(n).eq.1.or.k6(n).eq.215) then  !ceiling's k6=215 
            namlvl(n,1) = 'SFC'
            nchrlvl(n,1) = 3
         else if (k6(n).eq.2) then
            namlvl(n,1) = 'CLDBS'
            nchrlvl(n,1) = 5
         else if (k6(n).eq.3) then
            namlvl(n,1) = 'CLDTP'
            nchrlvl(n,1) = 5
         else if (k6(n).eq.7) then
            namlvl(n,1) = 'TROP'
            nchrlvl(n,1) = 4
         else if (k6(n).eq.4) then
            namlvl(n,1) = '0DEG'
            nchrlvl(n,1) = 4                                                                                                                     
         end if
           write(*,*) 'namlvl nchrlvl=', namlvl(n,1),nchrlvl(n,1)
        end do

       write(*,*) 'setlevel done'


      RETURN
      END
