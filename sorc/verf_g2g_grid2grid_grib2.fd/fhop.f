c  subroutine gtFHOP is for verifying Probability vs Event      
c  The output is F.H.O record 
c  Author: Binbin Zhou, created on  Mar. 14, 2016
c   FHO>(or FHO< or FHO=) p1  p2  p3  p4 ...
c   The first one "p1" is event threshold,
c   p2, p3 .... are probability thresholds (all are larger than) 
c
  
      subroutine gtFHOP(nvsdb,imodel,ist,numfcst,numvfyobs,numarea,
     +  numvarbl,numlevel,ngrid,levels,tendencymrk,updown,hasdata,
     +  fcstdata,obsvdata)


      INCLUDE 'parm.inc'
      
      INTEGER, intent(IN) :: imodel, ist, numvfyobs,numarea,
     +         numfcst,numvarbl,numlevel,ngrid, levels(mxvrbl),
     +         hasdata(mxfcst,mxvrbl,maxlvl)

      REAL,dimension(numfcst,numvarbl,numlevel,ngrid),intent(INOUT) ::
     +          fcstdata, obsvdata
                                                                                                                                                        
      DIMENSION nchrmodel(maxmod), nchrfcst(mxfcst), nchrvfdate(mxdate),
     +            nchrvfyobs(maxobs), nchrarea(mxarea),
     +            nchrstat(mxstat), nchrvarbl(mxvrbl),
     +            nchrlevel(maxlvl)
      CHARACTER*24 namodel(maxmod), namfcst(mxfcst), namvfdate(mxdate),
     +            namvfyobs(maxobs), namarea(mxarea), namstat(mxstat),
     +            namvarbl(mxvrbl), namlevel(maxlvl)
                                                                                                                                                         
      COMMON /names/ namodel, namfcst, namvfdate, namvfyobs, namarea,
     +            namstat, namvarbl, namlevel
      COMMON /nchrs/ nchrmodel, nchrfcst, nchrvfdate, nchrvfyobs,
     +            nchrarea, nchrstat, nchrvarbl, nchrlevel
      CHARACTER*3 regions (100)
      COMMON /grdef/ig104(147,110),numreg(mxarea),mode(mxarea),
     +  imax(mxarea),imin(mxarea),jmax(mxarea),jmin(mxarea),
     +  alat1(mxarea),elon1(mxarea), dxx(mxarea), dyy(mxarea),
     +  elonv(mxarea), alatan(mxarea), latlong(mxarea),
     +  lambert(mxarea), polarstereo(mxarea),regions



      real*8, allocatable, dimension(:,:,:,:,:,:) :: sumf,sumo,sumh,
     +                                             count,weigh

      CHARACTER*3 namversion
      CHARACTER*132 vdbhdr132, input, substr (3)
                                                                                                                                                         
      CHARACTER*1 blank, equal
C

      LOGICAL*1 latlong, lambert, polarstereo                  !Add  by Binbin, otherwise, they are can not be retrieved
      CHARACTER*24 fcst_ymdhf(mxfcst), obsv_ymdhf(maxobs)      !store YYYYMMDDHHFF string for fcst and obsv read from control
      CHARACTER*4 cyyyyfcst(mxfcst),cyyyyobsv(maxobs)
      CHARACTER*2 cmmfcst(mxfcst),cmmobsv(maxobs)
      CHARACTER*2 cddfcst(mxfcst),cddobsv(maxobs)
      CHARACTER*2 chhfcst(mxfcst),chhobsv(maxobs)
      CHARACTER*3 cfffcst(mxfcst),cffobsv(maxobs)
      CHARACTER*6 ck7(mxvrbl)

      CHARACTER*24 namlvl(mxvrbl,maxlvl) 
      integer nchrlvl(mxvrbl,maxlvl) 

      integer yyyyfcst(mxfcst), mmfcst(mxfcst),
     +        ddfcst(mxfcst), hhfcst(mxfcst), fffcst(mxfcst),
     +        yyyyobsv(maxobs), mmobsv(maxobs),
     +        ddobsv(maxobs), hhobsv(maxobs), ffobsv(maxobs)
      integer k5(mxvrbl),k6(mxvrbl),k7(mxvrbl),vectormrk(mxvrbl)
      integer k4(mxvrbl)
 
      CHARACTER*24 fho(mxvrbl),    fhothr(mxvrbl,20)
      CHARACTER*24 afho(mxvrbl),  afhothr(mxvrbl,20)
      integer  nchrfho(mxvrbl),nchrfhothr(mxvrbl,20),fhomrk(mxvrbl)
      integer  nchrafho(mxvrbl),nchrafhothr(mxvrbl,20),afhomrk(mxvrbl)
      real rfhothr(mxvrbl,20)
      real rafhothr(mxvrbl,20)
      CHARACTER*1 updown(mxvrbl,20)
      integer tendencymrk(mxvrbl), continue_mrk(mxvrbl)
      integer anomlylev(mxvrbl,maxlvl),anomly_mrk(mxvrbl)

      CHARACTER*24 sfho(mxvrbl),sfhothr(mxvrbl,20)
      CHARACTER*24 ffho(mxvrbl),ffhothr(mxvrbl,20)
      integer  nchrsfho(mxvrbl),nchrsfhothr(mxvrbl,20),sfhomrk(mxvrbl)
      integer  nchrffho(mxvrbl),nchrffhothr(mxvrbl,20),ffhomrk(mxvrbl)
      real rsfhothr(mxvrbl,20)
      real rffhothr(mxvrbl,20)


      COMMON /g2g/cyyyyfcst,cmmfcst,cddfcst,chhfcst,cfffcst,
     +            cyyyyobsv,cmmobsv,cddobsv,chhobsv,cffobsv,
     +             yyyyfcst, mmfcst, ddfcst, hhfcst, fffcst,
     +             yyyyobsv, mmobsv, ddobsv, hhobsv, ffobsv,
     +             k4,k5,k6,k7,ck7,vectormrk,namlvl,nchrlvl,
     +      fhomrk,fho,nchrfho,fhothr,nchrfhothr,rfhothr, 
     +             continue_mrk,anomly_mrk,anomlylev,
     +   afhomrk,afho,nchrafho,afhothr,nchrafhothr,rafhothr
      COMMON /FRC/
     +       sfhomrk,sfho,nchrsfho,sfhothr,nchrsfhothr,rsfhothr,
     +       ffhomrk,ffho,nchrffho,ffhothr,nchrffhothr,rffhothr




      integer plevel(maxlvl)
      integer region_id(maxpts)
      real region_latlon(2,maxpts),ptr1(2,mxarea), ptr2(2,mxarea)
 
      COMMON /reg/region_id, region_latlon,ptr1,ptr2
      COMMON /layer/ modelvl(maxlvl), iplevel(maxlvl,2)

      integer nodata(mxfcst,mxvrbl,maxlvl)
      COMMON /nofile/nodata

      real grid_area(maxpts)


      real area_factor(maxpts)
      COMMON /weight/area_factor

      DATA blank /' '/
      DATA equal /'='/
      DATA namversion /'V01'/
      DATA bmiss /10E10/
      DATA rmiss /99999./


      write(*,*) 'In gtFHOP:',imodel,ist,numfcst,numvfyobs,numarea,
     +  numvarbl,numlevel,ngrid
      write(*,*) 'vectormrk', (vectormrk(i),i=1,numvarbl)
      write(*,*) 'fhomrk', (fhomrk(i),i=1,numvarbl)
      write(*,*) 'afhomrk',(afhomrk(i),i=1,numvarbl)
      write(*,*)  'anomly_mrk',(anomly_mrk(i),i=1,numvarbl)     
      write(*,*)  'continue_mrk',(continue_mrk(i),i=1,numvarbl)     

      write(*,*)'weight factor=', area_factor(10000)
      

      allocate(sumf(numfcst,numvarbl,numlevel,numarea,numvfyobs,10))
      allocate(sumo(numfcst,numvarbl,numlevel,numarea,numvfyobs,10))
      allocate(sumh(numfcst,numvarbl,numlevel,numarea,numvfyobs,10))
      allocate(count(numfcst,numvarbl,numlevel,numarea,numvfyobs,10))
      allocate(weigh(numfcst,numvarbl,numlevel,numarea,numvfyobs,10))

          count = 0.0
          weigh = 0.0
          sumf = 0.0
          sumo = 0.0
          sumh = 0.0

      do ivr = 1, numvarbl
       write(*,*)ivr, 
     +    fhomrk(ivr),fho(ivr),(rfhothr(ivr,j),j=1,fhomrk(ivr)), 
     +    ' tendencymrk=',tendencymrk(ivr),' updown=',
     +    (updown(ivr,j),j=1,fhomrk(ivr)) 

       if (trim(namvarbl(ivr)).eq.'FOGEVNT') then
         continue_mrk(ivr)=5
       end if
       write(*,*) continue_mrk(ivr)
      end do

c       do ifh = 1,1 
c         do ivr = 1, numvarbl
c           write(*,*) namvarbl(ivr), continue_mrk(ivr)
c           do ilv = 1, levels(ivr)
c        write(*,111) (fcstdata(ifh,ivr,ilv,i),i=1001,1010)
c 111    format(10f8.2)
c        write(*,111) (obsvdata(ifh,ivr,ilv,i),i=1001,1010)
c            end do
c           end do
c         end do

      do 90 ifh = 1, numfcst
        do 80 iob = 1, numvfyobs
          do 70 iar = 1, numarea
           do 60 ivr = 1, numvarbl
             if(fhomrk(ivr).eq.0) goto 60
              event=rfhothr(ivr,1) 
             do 55 ifo = 2, fhomrk(ivr)
               do 50 ilv = 1, levels(ivr)
                if(nodata(ifh,ivr,ilv).eq.1) goto 50

         IF (mode(iar).eq.1) THEN                          !all GRID domain (mode 1)
           do 501 i = 1,ngrid
            if(continue_mrk(ivr).eq.1 .and.
     +         obsvdata(ifh,ivr,ilv,i).lt.0.0 ) goto 501    !for cloud-like non-continous parameters

            if(continue_mrk(ivr).eq.2 .and.(
     +         obsvdata(ifh,ivr,ilv,i).lt.0.0 .or.
     +         fcstdata(ifh,ivr,ilv,i).lt.0.0) ) goto 501  !for aerosol/smoke

            if(continue_mrk(ivr).eq.3 .and.
     +      obsvdata(ifh,ivr,ilv,i).eq.0.0) goto 501  !for RTMA RH case

            if (continue_mrk(ivr).eq.4 .and.
     +       obsvdata(ifh,ivr,ilv,i).le.-100.0 ) goto 501


            if(continue_mrk(ivr).eq.6 .and.
     +         obsvdata(ifh,ivr,ilv,i).lt. 1.0) goto 501 ! for visibility
            if(continue_mrk(ivr).eq.6 .and.
     +         fcstdata(ifh,ivr,ilv,i).le. 0.0) goto 501 ! for visibility
            if(continue_mrk(ivr).eq.5 .and.
     +         obsvdata(ifh,ivr,ilv,i).le. 0.0) goto 501 ! for fog 
            if(continue_mrk(ivr).eq.5 .and.
     +         fcstdata(ifh,ivr,ilv,i).le. 0.0) goto 501 ! for fog

            if(continue_mrk(ivr).eq.5 .and.
     +        (obsvdata(ifh,ivr,ilv,i).le.0.0.or.
     +         fcstdata(ifh,ivr,ilv,i).le.0.0) ) goto 501 ! for ceiling

           
              f=getF_prob(fcstdata(ifh,ivr,ilv,i),rfhothr(ivr,ifo))
              o=getFO(obsvdata(ifh,ivr,ilv,i),event,fho(ivr))
              if(f.eq.1.0.and.o.eq.1.0) then
                 h=1.
              else
               h=0.
              end if

c              if ( fcstdata(ifh,ivr,ilv,i).ge.30.0.and.
c     +             obsvdata(ifh,ivr,ilv,i).gt.10.0) then
c                    write(*,'(a5,i10,5f8.3)') 'FOUND', i,
c     +              fcstdata(ifh,ivr,ilv,i),obsvdata(ifh,ivr,ilv,i), 
c     +              f, o, h
c              end if 

                    sumf(ifh,ivr,ilv,iar,iob,ifo) = 
     +              sumf(ifh,ivr,ilv,iar,iob,ifo) + f*area_factor(i)

                    sumo(ifh,ivr,ilv,iar,iob,ifo) =
     +              sumo(ifh,ivr,ilv,iar,iob,ifo) + o*area_factor(i)

                    sumh(ifh,ivr,ilv,iar,iob,ifo) =
     +              sumh(ifh,ivr,ilv,iar,iob,ifo) + h*area_factor(i)
                   
                    count(ifh,ivr,ilv,iar,iob,ifo) =
     +              count(ifh,ivr,ilv,iar,iob,ifo) + 1.0
                    weigh(ifh,ivr,ilv,iar,iob,ifo) =
     +              weigh(ifh,ivr,ilv,iar,iob,ifo) + area_factor(i)
 
501         continue  

          ELSE IF (mode(iar).eq.2) THEN                    ! GRID#104  (mode 2)

            IF (numreg(iar).le.30) then                    !         sub-region

             do 502 i = 1,ngrid

               if(continue_mrk(ivr).eq.1.and.
     +           obsvdata(ifh,ivr,ilv,i).lt.0.0 ) goto 502

               if(continue_mrk(ivr).eq.2 .and.(
     +           obsvdata(ifh,ivr,ilv,i).lt.0.0 .or.
     +             fcstdata(ifh,ivr,ilv,i).lt.0.0) ) goto 502  !for aerosol/smoke
 
               if(continue_mrk(ivr).eq.3 .and.
     +            obsvdata(ifh,ivr,ilv,i).eq.0.0) goto 502  !for RTMA RH case

               if (continue_mrk(ivr).eq.4 .and.
     +            obsvdata(ifh,ivr,ilv,i).le.-100.0 )goto 502  !for reflectivity

               if(continue_mrk(ivr).eq.4 .and.
     +            obsvdata(ifh,ivr,ilv,i).lt. 1.0) goto 502 ! for visibility

               if(continue_mrk(ivr).eq.5 .and. 
     +            (obsvdata(ifh,ivr,ilv,i).le.0.0.or.
     +             fcstdata(ifh,ivr,ilv,i).le.0.0) ) goto 502 ! for ceiling


               if(region_id(i).eq.numreg(iar)) then

                 if(tendencymrk(ivr).eq.0) then
                   f=getF_prob(fcstdata(ifh,ivr,ilv,i),rfhothr(ivr,ifo))
                   o=getFO(obsvdata(ifh,ivr,ilv,i),event,fho(ivr))
                   if(f.eq.1.0.and.o.eq.1.0) then
                     h=1.
                   else
                     h=0.
                   end if
                 end if
                                                                                                                                                                 
                 sumf(ifh,ivr,ilv,iar,iob,ifo) =
     +            sumf(ifh,ivr,ilv,iar,iob,ifo) + f*area_factor(i)
                                                                                                                                                                 
                 sumo(ifh,ivr,ilv,iar,iob,ifo) =
     +            sumo(ifh,ivr,ilv,iar,iob,ifo) + o*area_factor(i)
                                                            
                 sumh(ifh,ivr,ilv,iar,iob,ifo) =
     +            sumh(ifh,ivr,ilv,iar,iob,ifo) + h*area_factor(i)
                                                                                                                                                                 
                       count(ifh,ivr,ilv,iar,iob,ifo) =
     +                 count(ifh,ivr,ilv,iar,iob,ifo) + 1.0
                    weigh(ifh,ivr,ilv,iar,iob,ifo) =
     +              weigh(ifh,ivr,ilv,iar,iob,ifo) + area_factor(i)

              end if                                                                    
502                  continue  
         ELSE IF (numreg(iar).eq.31 ) then             !        N. hemisphere
 
            do 503 i = 1,ngrid
               if(continue_mrk(ivr).eq.1.and.
     +                 obsvdata(ifh,ivr,ilv,i).lt.0.0 ) goto 503

               if(continue_mrk(ivr).eq.2 .and.(
     +                 obsvdata(ifh,ivr,ilv,i).lt.0.0 .or.
     +                  fcstdata(ifh,ivr,ilv,i).lt.0.0) ) goto 503  !for aerosol/smoke

              if(continue_mrk(ivr).eq.3 .and.
     +          obsvdata(ifh,ivr,ilv,i).eq.0.0) goto 503  !for RTMA RH case


              if(continue_mrk(ivr).eq.6 .and.
     +         obsvdata(ifh,ivr,ilv,i).lt. 1.0) goto 503 ! for visibility

              if(continue_mrk(ivr).eq.5 .and.
     +          (obsvdata(ifh,ivr,ilv,i).le.0.0.or.
     +           fcstdata(ifh,ivr,ilv,i).le.0.0) ) goto 503 ! for ceiling

              if(region_latlon(1,i).gt.0.0) then

                if(tendencymrk(ivr).eq.0) then
                 f=getF_prob(fcstdata(ifh,ivr,ilv,i),rfhothr(ivr,ifo))
                 o=getFO(obsvdata(ifh,ivr,ilv,i),event,fho(ivr))
                 if(f.eq.1.0.and.o.eq.1.0) then
                  h=1.
                 else
                  h=0.
                 end if
               end if
                          
                sumf(ifh,ivr,ilv,iar,iob,ifo) =
     +            sumf(ifh,ivr,ilv,iar,iob,ifo) + f*area_factor(i)
                          
                sumo(ifh,ivr,ilv,iar,iob,ifo) =
     +            sumo(ifh,ivr,ilv,iar,iob,ifo) + o*area_factor(i)
                          
                sumh(ifh,ivr,ilv,iar,iob,ifo) =
     +            sumh(ifh,ivr,ilv,iar,iob,ifo) + h*area_factor(i)
                          
                       count(ifh,ivr,ilv,iar,iob,ifo) =
     +                 count(ifh,ivr,ilv,iar,iob,ifo) + 1.0
                    weigh(ifh,ivr,ilv,iar,iob,ifo) =
     +              weigh(ifh,ivr,ilv,iar,iob,ifo) + area_factor(i)

              end if
503                 continue   
        ELSE IF (numreg(iar).eq.32 ) then             !        S. hemisphere
 
           do 504 i = 1,ngrid
              if(continue_mrk(ivr).eq.1.and.
     +                 obsvdata(ifh,ivr,ilv,i).lt.0.0 ) goto 504

             if(continue_mrk(ivr).eq.2 .and.(
     +                 obsvdata(ifh,ivr,ilv,i).lt.0.0 .or.
     +                  fcstdata(ifh,ivr,ilv,i).lt.0.0) ) goto 504  !for aerosol/smoke

                    
             if(continue_mrk(ivr).eq.3 .and.
     +          obsvdata(ifh,ivr,ilv,i).eq.0.0) goto 504  !for RTMA RH case


             if(continue_mrk(ivr).eq.4 .and.
     +          obsvdata(ifh,ivr,ilv,i).le. -120.0) goto 504  !for reflectivity

             if(continue_mrk(ivr).eq.4 .and.
     +          obsvdata(ifh,ivr,ilv,i).le. 1.0) goto 504  !for visibility

             if(continue_mrk(ivr).eq.5 .and.
     +         (obsvdata(ifh,ivr,ilv,i).le.0.0.or.
     +          fcstdata(ifh,ivr,ilv,i).le.0.0) ) goto 504 ! for ceiling


             if(region_latlon(1,i).lt.0.0) then

              if(tendencymrk(ivr).eq.0) then
                 f=getF_prob(fcstdata(ifh,ivr,ilv,i),rfhothr(ivr,ifo))
                 o=getFO(obsvdata(ifh,ivr,ilv,i),event,fho(ivr))
                 if(f.eq.1.0.and.o.eq.1.0) then
                  h=1.
                 else
                  h=0.
                 end if
               end if
 
                       sumf(ifh,ivr,ilv,iar,iob,ifo) =
     +                 sumf(ifh,ivr,ilv,iar,iob,ifo) +
     +                       f*area_factor(i)
                          
                       sumo(ifh,ivr,ilv,iar,iob,ifo) =
     +                 sumo(ifh,ivr,ilv,iar,iob,ifo) +
     +                       o*area_factor(i)
                          
                       sumh(ifh,ivr,ilv,iar,iob,ifo) =
     +                 sumh(ifh,ivr,ilv,iar,iob,ifo) +
     +                       h*area_factor(i)
                          
                       count(ifh,ivr,ilv,iar,iob,ifo) =
     +                 count(ifh,ivr,ilv,iar,iob,ifo) + 1.0
                    weigh(ifh,ivr,ilv,iar,iob,ifo) =
     +              weigh(ifh,ivr,ilv,iar,iob,ifo) + area_factor(i)
 
             end if
504                  continue            

        ELSE IF (numreg(iar).eq.33) then             ! User defined region


         if (ptr1(1,iar).eq.ptr2(1,iar).and.         !user defined case 1, all points along latitude
     +       ptr1(2,iar).ne.ptr2(2,iar)) then                !

           do 505 i = 1,ngrid                                !  x(ptr2(1), ptr2(2))
             if(continue_mrk(ivr).eq.1.and.                       !  |
     +             obsvdata(ifh,ivr,ilv,i).lt.0.0 ) goto 505         !  |
                       
             if(continue_mrk(ivr).eq.2 .and.(
     +             obsvdata(ifh,ivr,ilv,i).lt.0.0 .or.
     +              fcstdata(ifh,ivr,ilv,i).lt.0.0) ) goto 505  

             if(continue_mrk(ivr).eq.3 .and.
     +              obsvdata(ifh,ivr,ilv,i).eq.0.0) goto 505  !for RTMA RH case

             if(continue_mrk(ivr).eq.4 .and.
     +             obsvdata(ifh,ivr,ilv,i).le. -120.0) goto 505  !for reflectivity

             if(continue_mrk(ivr).eq.4 .and.
     +            obsvdata(ifh,ivr,ilv,i).le. 1.0) goto 505  !for visibility

                       
             if(continue_mrk(ivr).eq.5 .and.
     +             (obsvdata(ifh,ivr,ilv,i).le.0.0.or.
     +              fcstdata(ifh,ivr,ilv,i).le.0.0) ) goto 505 ! for ceiling

                                                               !  |
             if(region_latlon(1,i).ge.ptr1(2,iar).and.         !  |
     +             region_latlon(1,i).le.ptr2(2,iar)) then     !  |
                                                               !  x(ptr1(1), ptr1(2))  
                                                               !  where ptr1(1) = ptr2(1) 
                  if(tendencymrk(ivr).eq.0) then
                   f=getF_prob(fcstdata(ifh,ivr,ilv,i),rfhothr(ivr,ifo))
                   o=getFO(obsvdata(ifh,ivr,ilv,i),event,fho(ivr))
                   if(f.eq.1.0.and.o.eq.1.0) then
                     h=1.
                   else
                    h=0.
                   end if
                  end if
                          
                       sumf(ifh,ivr,ilv,iar,iob,ifo) =
     +                 sumf(ifh,ivr,ilv,iar,iob,ifo) +
     +                       f*area_factor(i)
                          
                       sumo(ifh,ivr,ilv,iar,iob,ifo) =
     +                 sumo(ifh,ivr,ilv,iar,iob,ifo) +
     +                       o*area_factor(i)
                          
                       sumh(ifh,ivr,ilv,iar,iob,ifo) =
     +                 sumh(ifh,ivr,ilv,iar,iob,ifo) +
     +                       h*area_factor(i)
                          
                       count(ifh,ivr,ilv,iar,iob,ifo) =
     +                 count(ifh,ivr,ilv,iar,iob,ifo) + 1.0
                    weigh(ifh,ivr,ilv,iar,iob,ifo) =
     +              weigh(ifh,ivr,ilv,iar,iob,ifo) + area_factor(i)

              end if
505                   continue
           else if (ptr1(1,iar).ne.ptr2(1,iar).and.    !user defined case 2, all points along longitude 
     +                ptr1(2,iar).eq.ptr2(2,iar)) then
            do 506 i = 1,ngrid  
              if(continue_mrk(ivr).eq.1.and.                       !  
     +           obsvdata(ifh,ivr,ilv,i).lt.0.0 ) goto 506      !  

              if(continue_mrk(ivr).eq.2 .and.(
     +           obsvdata(ifh,ivr,ilv,i).lt.0.0 .or.
     +           fcstdata(ifh,ivr,ilv,i).lt.0.0) ) goto 506  !for aerosol/smoke

              if(continue_mrk(ivr).eq.3 .and.
     +           obsvdata(ifh,ivr,ilv,i).eq.0.0) goto 506  !for RTMA RH case


              if(continue_mrk(ivr).eq.4 .and.
     +           obsvdata(ifh,ivr,ilv,i).le. -120.0) goto 506  !for reflectivity

              if(continue_mrk(ivr).eq.4 .and.
     +           obsvdata(ifh,ivr,ilv,i).le. 1.0) goto 506  !for visibility

              if(continue_mrk(ivr).eq.5 .and.
     +          (obsvdata(ifh,ivr,ilv,i).le.0.0.or.
     +           fcstdata(ifh,ivr,ilv,i).le.0.0) ) goto 506 ! for ceiling

                                                                      !  
              if(region_latlon(2,i).ge.ptr1(1,iar).and.        !       x -----------------x 
     +           region_latlon(2,i).le.ptr2(1,iar)) then       ! (ptr1(1), ptr1(2))   (ptr2(1), ptr2(2))
                                                                         !  where ptr1(2) = ptr2(2)  
                 if(tendencymrk(ivr).eq.0) then
                  f=getF_prob(fcstdata(ifh,ivr,ilv,i),rfhothr(ivr,ifo))
                  o=getFO(obsvdata(ifh,ivr,ilv,i),event,fho(ivr))
                  if(f.eq.1.0.and.o.eq.1.0) then
                    h=1.
                  else
                    h=0.
                  end if
                 end if
                          
                       sumf(ifh,ivr,ilv,iar,iob,ifo) =
     +                 sumf(ifh,ivr,ilv,iar,iob,ifo) +
     +                       f*area_factor(i)
                          
                       sumo(ifh,ivr,ilv,iar,iob,ifo) =
     +                 sumo(ifh,ivr,ilv,iar,iob,ifo) +
     +                       o*area_factor(i)
                          
                       sumh(ifh,ivr,ilv,iar,iob,ifo) =
     +                 sumh(ifh,ivr,ilv,iar,iob,ifo) +
     +                       h*area_factor(i)
                          
                       count(ifh,ivr,ilv,iar,iob,ifo) =
     +                 count(ifh,ivr,ilv,iar,iob,ifo) + 1.0
                    weigh(ifh,ivr,ilv,iar,iob,ifo) =
     +              weigh(ifh,ivr,ilv,iar,iob,ifo) + area_factor(i)

                end if
506                   continue  
            else if (ptr1(1,iar).ne.ptr2(1,iar).and.    !user defined case 3, all points in a regtangular
     +               ptr1(2,iar).ne.ptr2(2,iar)) then        

              do 507 i = 1,ngrid           
                                                 
               if(continue_mrk(ivr).eq.1.and.                      
     +             obsvdata(ifh,ivr,ilv,i).lt.0.0 ) goto 507

               if(continue_mrk(ivr).eq.2 .and.(
     +             obsvdata(ifh,ivr,ilv,i).lt.0.0 .or.
     +             fcstdata(ifh,ivr,ilv,i).lt.0.0) ) goto 507  !for aerosol/smoke

               if(continue_mrk(ivr).eq.3 .and.
     +            obsvdata(ifh,ivr,ilv,i).eq.0.0) goto 507  !for RTMA RH case

               if (continue_mrk(ivr).eq.4 .and.
     +             obsvdata(ifh,ivr,ilv,i).le.-100.0) goto 507  !for reflectivity

               if(continue_mrk(ivr).eq.4 .and.
     +             obsvdata(ifh,ivr,ilv,i).le. 1.0) goto 507  !for visibility


               if(continue_mrk(ivr).eq.6 .and.
     +             obsvdata(ifh,ivr,ilv,i).le. 1.0) goto 507  !for visibility

               if(continue_mrk(ivr).eq.5 .and.
     +            (obsvdata(ifh,ivr,ilv,i).le.0.0.or.
     +             fcstdata(ifh,ivr,ilv,i).le.0.0) ) goto 507 ! for ceiling

                                                                      !             (ptr2(1), ptr2(2))
               if(region_latlon(2,i).ge.ptr1(1,iar).and.     !      ----------------x
     +            region_latlon(2,i).le.ptr2(1,iar).and.     !     |                |
     +            region_latlon(1,i).ge.ptr1(2,iar).and.     !     |    x           |
     +            region_latlon(1,i).le.ptr2(2,iar)) then    !     |                |
                                                             !    x----------------
                                                             !  (ptr1(1), ptr1(2))

                  if(tendencymrk(ivr).eq.0) then
                   f=getF_prob(fcstdata(ifh,ivr,ilv,i),rfhothr(ivr,ifo))
                   o=getFO(obsvdata(ifh,ivr,ilv,i),event,fho(ivr))
                   if(f.eq.1.0.and.o.eq.1.0) then
                     h=1.
                   else
                     h=0.
                   end if
                  end if
                       sumf(ifh,ivr,ilv,iar,iob,ifo) =
     +                 sumf(ifh,ivr,ilv,iar,iob,ifo) +
     +                       f*area_factor(i)
                          
                       sumo(ifh,ivr,ilv,iar,iob,ifo) =
     +                 sumo(ifh,ivr,ilv,iar,iob,ifo) + o*area_factor(i)
                          
                       sumh(ifh,ivr,ilv,iar,iob,ifo) =
     +                 sumh(ifh,ivr,ilv,iar,iob,ifo) + h*area_factor(i)
                          
                       count(ifh,ivr,ilv,iar,iob,ifo) =
     +                 count(ifh,ivr,ilv,iar,iob,ifo) + 1.0
                    weigh(ifh,ivr,ilv,iar,iob,ifo) =
     +              weigh(ifh,ivr,ilv,iar,iob,ifo) + area_factor(i)

               end if
507                  continue  
                      
           end if   !end of user defined

          END IF    !end of numreg 

         END IF    !end of region mode
 
50               continue
55              continue
60             continue
70            continue
80           continue
90         continue

      do 91 ifh = 1, numfcst
       do 81 iob = 1, numvfyobs
        do 71 iar = 1, numarea
         do 61 ivr = 1, numvarbl
          if(fhomrk(ivr).eq.0) goto 61
          do 56 ifo = 2, fhomrk(ivr)
            do 51 ilv = 1, levels(ivr)
             if(nodata(ifh,ivr,ilv).eq.1) goto 51
             if(weigh(ifh,ivr,ilv,iar,iob,ifo).ne.0.0) then

c             write(*,*) weigh(ifh,ivr,ilv,iar,iob,ifo),
c     +       sumf(ifh,ivr,ilv,iar,iob,ifo),
c     +       sumh(ifh,ivr,ilv,iar,iob,ifo),
c     +       sumo(ifh,ivr,ilv,iar,iob,ifo)
     

              sumf(ifh,ivr,ilv,iar,iob,ifo)=
     +        sumf(ifh,ivr,ilv,iar,iob,ifo)/
     +        weigh(ifh,ivr,ilv,iar,iob,ifo)
                                                                                                                                                                                   
              sumo(ifh,ivr,ilv,iar,iob,ifo)=
     +        sumo(ifh,ivr,ilv,iar,iob,ifo)/
     +        weigh(ifh,ivr,ilv,iar,iob,ifo)
                                                                                                                                                                                   
              sumh(ifh,ivr,ilv,iar,iob,ifo)=
     +        sumh(ifh,ivr,ilv,iar,iob,ifo)/
     +        weigh(ifh,ivr,ilv,iar,iob,ifo)
                                                                                                                                                                                   
              if(sumf(ifh,ivr,ilv,iar,iob,ifo).lt.0.0)
     +           sumf(ifh,ivr,ilv,iar,iob,ifo)=0.0
              if(sumo(ifh,ivr,ilv,iar,iob,ifo).lt.0.0)
     +           sumo(ifh,ivr,ilv,iar,iob,ifo)=0.0
              if(sumh(ifh,ivr,ilv,iar,iob,ifo).lt.0.0)
     +           sumh(ifh,ivr,ilv,iar,iob,ifo)=0.0
                                                                                                                                                                                   

c             write(*,*) weigh(ifh,ivr,ilv,iar,iob,ifo),
c     +       sumf(ifh,ivr,ilv,iar,iob,ifo),
c     +       sumh(ifh,ivr,ilv,iar,iob,ifo),
c     +       sumo(ifh,ivr,ilv,iar,iob,ifo)


             end if
51          continue
56        continue
61       continue
71      continue
81     continue
91    continue
       
           do 190 iob = 1, numvfyobs
            do 180 iar = 1, numarea
             do 170 ifh = 1, numfcst
                ivfdate = ifh
              do 160 ivr = 1, numvarbl
               if(fhomrk(ivr).eq.0) goto 160
               do 150 ifo = 2, fhomrk(ivr)
                 do 150 ilv = 1, levels(ivr)
                  if(nodata(ifh,ivr,ilv).eq.1) goto 150
c                   if(count(ifh,ivr,ilv,iar,iob,ifo).le.0) goto 150

                    IF (count(ifh,ivr,ilv,iar,iob,ifo).gt.0.0) THEN
                      iend = 3
                      vdbhdr132(1:iend) = namversion(:3)
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrmodel(imodel)
                      vdbhdr132(istrt:iend) =
     +                            namodel(imodel)(:nchrmodel(imodel))
 
                     iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrfcst(ifh)
                      vdbhdr132(istrt:iend) =
     +                            namfcst(ifh)(:nchrfcst(ifh))
 
                     iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrvfdate(ivfdate)
                      vdbhdr132(istrt:iend) =
     +                            namvfdate(ivfdate)(:
     +                            nchrvfdate(ivfdate))
 
                     iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrvfyobs(iob)
                      vdbhdr132(istrt:iend) =
     +                            namvfyobs(iob)(:nchrvfyobs(iob))
 
                     iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrarea(iar)
                      vdbhdr132(istrt:iend) =
     +                            namarea(iar)(:nchrarea(iar))

                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrfho(ivr)
                      vdbhdr132(istrt:iend) =
     +                            fho(ivr)(:nchrfho(ivr))
                       
                      istrt = iend + 1
                      iend = iend + nchrfhothr(ivr,1)
                      vdbhdr132(istrt:iend) =
     +                      fhothr(ivr,1)(:nchrfhothr(ivr,1))

                      istrt = iend + 1
                      iend = iend + 6
                      vdbhdr132(istrt:iend) = ' Prob>'


                      istrt = iend + 1
                      iend = iend + nchrfhothr(ivr,ifo)
                      vdbhdr132(istrt:iend) =
     +                      fhothr(ivr,ifo)(:nchrfhothr(ivr,ifo))
                     
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrvarbl(ivr)
                      vdbhdr132(istrt:iend) =
     +                            namvarbl(ivr)(:nchrvarbl(ivr))
                     iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrlevel(ilv)
                      vdbhdr132(istrt:iend) =
     +                       namlvl(ivr,ilv)(:nchrlvl(ivr,ilv))
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      iend = iend + 1
                      vdbhdr132(iend:iend) = equal
                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                       if(hasdata(ifh,ivr,ilv).gt.0) then
                        WRITE (nvsdb,1000) vdbhdr132(:iend),
     +                              count(ifh,ivr,ilv,iar,iob,ifo),
     +                              sumf(ifh,ivr,ilv,iar,iob,ifo),
     +                              sumh(ifh,ivr,ilv,iar,iob,ifo),
     +                              sumo(ifh,ivr,ilv,iar,iob,ifo)
 1000                   FORMAT (A,F10.0,3E18.9)
                       end if
                    END IF
150             continue
155            continue
160           continue
170          continue
180         continue
190        continue

           deallocate(sumf)
           deallocate(sumo)
           deallocate(sumh)
           deallocate(count)
           deallocate(weigh)

           return
 
          end
