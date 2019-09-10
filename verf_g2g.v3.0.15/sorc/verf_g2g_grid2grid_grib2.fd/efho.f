c
c  This gtEFHO is to verify event-ensemble probability (E-P) 
c  Author: Binbin Zhou
c  July 17, 2009
c
c    
      subroutine gtEFHO(nvsdb,imodel,ist,numfcst,numvfyobs,numarea,
     +  numvarbl,numlevel,ngrid,levels,tendencymrk,updown,hasdata,
     +  fcstdata,obsvdata)

c    This program is to generate FHO vsdb records for all fcst times, 
c    all variables, all levels, all requested sub-regions, then write them 
c    into the vsdb file
c    Author: Binbin Zhou
c            March, 2005

      INCLUDE 'parm.inc'
      
      INTEGER, intent(IN) :: imodel, ist, numvfyobs,numarea,
     +         numfcst,numvarbl,numlevel,ngrid, levels(mxvrbl),
     +         hasdata(mxfcst,mxvrbl,maxlvl)

      REAL,dimension(numfcst,numvarbl,numlevel,ngrid),intent(IN) ::
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
      real*8, allocatable, dimension(:,:,:,:,:,:) :: pfield

      CHARACTER*3 namversion
      CHARACTER*200 vdbhdr132, input, substr (3)
                                                                                                                                                         
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

      CHARACTER*24 sfho(mxvrbl),sfhothr(mxvrbl,20)
      CHARACTER*24 ffho(mxvrbl),ffhothr(mxvrbl,20)
      integer  nchrsfho(mxvrbl),nchrsfhothr(mxvrbl,20),sfhomrk(mxvrbl)
      integer  nchrffho(mxvrbl),nchrffhothr(mxvrbl,20),ffhomrk(mxvrbl)
      real rsfhothr(mxvrbl,20) 
      real rffhothr(mxvrbl,20)



      CHARACTER*1 updown(mxvrbl,20)
      integer tendencymrk(mxvrbl), continue_mrk(mxvrbl)
      integer anomlylev(mxvrbl,maxlvl),anomly_mrk(mxvrbl)

      CHARACTER*6 threshold
      integer numodel
     

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
      real thresh,obsevnt


      DATA blank /' '/
      DATA equal /'='/
      DATA namversion /'V01'/
      DATA bmiss /10E10/
      DATA rmiss /99999./

      numodel=21

      write(*,*) 'In gtEFHO:',imodel,ist,numfcst,numvfyobs,numarea,
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

      allocate(pfield(numfcst,numvarbl,numlevel,numarea,numvfyobs,24))

          count = 0.0
          weigh = 0.0
          sumf = 0.0
          sumo = 0.0
          sumh = 0.0

      do ivr = 1, numvarbl
       write(*,*)ivr, 
     +    fhomrk(ivr),'fho=',fho(ivr),(rfhothr(ivr,j),j=1,fhomrk(ivr)), 
     +    ' tendencymrk=',tendencymrk(ivr),' updown=',
     +    (updown(ivr,j),j=1,fhomrk(ivr)) 
       write(*,*) ivr, mode(ivr)
      end do

      do 90 ifh = 1, numfcst
        do 80 iob = 1, numvfyobs
          do 70 iar = 1, numarea
           do 60 ivr = 1, numvarbl

             if(namvarbl(ivr).eq.'FOGEVNT') then
               thresh=1.0
             else if(namvarbl(ivr).eq.'ICETRC') then
               thresh=1.0
             else if(namvarbl(ivr).eq.'ICELGT') then
               thresh=2.0
             else if(namvarbl(ivr).eq.'ICEMID') then
               thresh=3.0
             else if(namvarbl(ivr).eq.'ICESVR') then
               thresh=4.0
             else
               thresh=1.0
             end if

             if(fhomrk(ivr).eq.0) goto 60
             do 55 ifo = 1, fhomrk(ivr)
               do 50 ilv = 1, levels(ivr)
                if(nodata(ifh,ivr,ilv).eq.1) goto 50
                if (mode(iar).eq.1) then                          !all GRID domain (mode 1)
                  do 501 i = 1,ngrid
                    if(continue_mrk(ivr).eq.1 .and.
     +                 obsvdata(ifh,ivr,ilv,i).lt.0.0 ) goto 501    !for cloud-like non-continous parameters and scipuff prob-event

                    if(continue_mrk(ivr).eq.3 .and.
     +                 obsvdata(ifh,ivr,ilv,i).eq.0.0) goto 501  !for RTMA RH case


                    if(continue_mrk(ivr).eq.4 .and.
     +              obsvdata(ifh,ivr,ilv,i).lt. -90.0) goto 501    !for reflectivity, echo top  

                     if(continue_mrk(ivr).eq.6 .and.
     +              obsvdata(ifh,ivr,ilv,i).lt. -90.0) goto 501    !for visibility
                     if(continue_mrk(ivr).eq.6 .and.
     +              fcstdata(ifh,ivr,ilv,i).le. 0.0) goto 501      !for fcst visibility

                     if(trim(namvarbl(ivr)).eq.'FOGEVNT') then     !for SREF and VSREF's FOGEVNT P-E veification
                        if (obsvdata(ifh,ivr,ilv,i).gt.0.0 .and.
     +                       obsvdata(ifh,ivr,ilv,i).le.1000.0) then
                          obsevnt=1.0
                        else
                          obsevnt=0.0
                        end if
                     else
                          obsevnt=obsvdata(ifh,ivr,ilv,i)         !for echo-top or reflectivity of SREF, obsevnt values (mosaic data) have 
                     end if                                       !been changed to 0/1 based on different echo-top thresholds
                                                                  !for icing, already 0/1/2/3/4
                       f=getFO(fcstdata(ifh,ivr,ilv,i),
     +                    rfhothr(ivr,ifo),fho(ivr))
                       o=getFO(obsevnt,thresh,fho(ivr))
                       h=getHitE(fcstdata(ifh,ivr,ilv,i),obsevnt,
     +                     rfhothr(ivr,ifo),thresh,fho(ivr))
 
                  
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
          
501               continue  

                  end if    !end of region mode
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
          do 56 ifo = 1, fhomrk(ivr)
            do 51 ilv = 1, levels(ivr)
             if(nodata(ifh,ivr,ilv).eq.1) goto 51
             if(weigh(ifh,ivr,ilv,iar,iob,ifo).gt.0.0) then

              write(*,*) 'Before Sum=',
     +       sumf(ifh,ivr,ilv,iar,iob,ifo),
     +       sumh(ifh,ivr,ilv,iar,iob,ifo),
     +       sumo(ifh,ivr,ilv,iar,iob,ifo)

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
               do 150 ifo = 1, fhomrk(ivr)
                 do 150 ilv = 1, levels(ivr)
                  if(nodata(ifh,ivr,ilv).eq.1) goto 150

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
 1000                   FORMAT (A,F12.0,3E18.9)
                       end if
                    END IF
150             continue
155            continue
160           continue
170          continue
180         continue
190        continue




       pfield = 0.

      do 902 ifh = 1, numfcst
        do 802 iob = 1, numvfyobs
          do 702 iar = 1, numarea
           do 602 ivr = 1, numvarbl
             if(fhomrk(ivr).eq.0) goto 602
               do 502 ilv = 1, levels(ivr)
                if(nodata(ifh,ivr,ilv).eq.1) goto 502
                if (mode(iar).eq.1) then                          !all GRID domain (mode 1)
           if (trim(namvarbl(ivr)).eq.'FOGEVNT') then
             thresh=1.0
            else if(trim(namvarbl(ivr)).eq.'ICETRC') then
             thresh=1.0
            else if(trim(namvarbl(ivr)).eq.'ICELGT') then
             thresh=2.0
            else if(trim(namvarbl(ivr)).eq.'ICEMID') then
             thresh=3.0
            else if(trim(namvarbl(ivr)).eq.'ICESRV') then
             thresh=4.0
            else
             thresh=1.0
           end if

           write(*,*) 'thresh=', thresh

          do 5011 i = 1,ngrid
                    if(continue_mrk(ivr).eq.1 .and.
     +                 obsvdata(ifh,ivr,ilv,i).lt.0.0 ) goto 5011    !for cloud-like non-continous parameters and scipuff prob-event

                    if(continue_mrk(ivr).eq.3 .and.
     +                 obsvdata(ifh,ivr,ilv,i).eq.0.0) goto 5011  !for RTMA RH case

                    if(continue_mrk(ivr).eq.4 .and.
     +              obsvdata(ifh,ivr,ilv,i).lt. -90.0) goto 5011    !for reflectivity, echo top

                     if(continue_mrk(ivr).eq.6 .and.
     +              obsvdata(ifh,ivr,ilv,i).lt. -90.0) goto 5011    !for visibility
                     if(continue_mrk(ivr).eq.6 .and.
     +              fcstdata(ifh,ivr,ilv,i).le. 0.0) goto 5011      !for fcst visibility


           prob=fcstdata(ifh,ivr,ilv,i)


           if(trim(namvarbl(ivr)).eq.'FOGEVNT') then
             if (obsvdata(ifh,ivr,ilv,i).gt.0.0 .and.
     +              obsvdata(ifh,ivr,ilv,i).le.1000.0) then
                 obsv=1.0
             else
                 obsv=0.0
             end if
           else
              obsv=obsvdata(ifh,ivr,ilv,i)
           end if


           pfield(ifh,ivr,ilv,iar,iob,1)=
     +           pfield(ifh,ivr,ilv,iar,iob,1)+1.0

          !To see&count the prob belongs to which probability range pfield(2~24)

          if(prob.eq.100.) then
            pfield(ifh,ivr,ilv,iar,iob,12)=
     +         pfield(ifh,ivr,ilv,iar,iob,12)+1.

          else if(prob.gt.0.0 .and. prob.le.10.) then
            pfield(ifh,ivr,ilv,iar,iob,2)=
     +         pfield(ifh,ivr,ilv,iar,iob,2)+1.
          else if(prob.gt.10. .and. prob.le.20.) then
            pfield(ifh,ivr,ilv,iar,iob,3)=
     +         pfield(ifh,ivr,ilv,iar,iob,3)+1.
          else if(prob.gt.20. .and. prob.le.30.) then
            pfield(ifh,ivr,ilv,iar,iob,4)=
     +         pfield(ifh,ivr,ilv,iar,iob,4)+1.
          else if(prob.gt.30. .and. prob.le.40.) then
            pfield(ifh,ivr,ilv,iar,iob,5)=
     +         pfield(ifh,ivr,ilv,iar,iob,5)+1.
          else if(prob.gt.40. .and. prob.le.50.) then
            pfield(ifh,ivr,ilv,iar,iob,6)=
     +         pfield(ifh,ivr,ilv,iar,iob,6)+1.
          else if(prob.gt.50. .and. prob.le.60.) then
            pfield(ifh,ivr,ilv,iar,iob,7)=
     +         pfield(ifh,ivr,ilv,iar,iob,7)+1.
          else if(prob.gt.60. .and. prob.le.70.) then
            pfield(ifh,ivr,ilv,iar,iob,8)=
     +         pfield(ifh,ivr,ilv,iar,iob,8)+1.
          else if(prob.gt.70. .and. prob.le.80.) then
            pfield(ifh,ivr,ilv,iar,iob,9)=
     +         pfield(ifh,ivr,ilv,iar,iob,9)+1.
          else if(prob.gt.80. .and. prob.le.90.) then
            pfield(ifh,ivr,ilv,iar,iob,10)=
     +         pfield(ifh,ivr,ilv,iar,iob,10)+1.
          else if(prob.gt.90. .and. prob.lt.100.) then
            pfield(ifh,ivr,ilv,iar,iob,11)=
     +         pfield(ifh,ivr,ilv,iar,iob,11)+1.
          end if


          if(prob.eq.100. .and. obsv.ge.thresh) then
             pfield(ifh,ivr,ilv,iar,iob,24)=
     +         pfield(ifh,ivr,ilv,iar,iob,24)+1.

           else if(prob.eq.0.0.and.obsv.ge.thresh) then
             pfield(ifh,ivr,ilv,iar,iob,13)=
     +         pfield(ifh,ivr,ilv,iar,iob,13)+1.

           else if(prob.gt.0.0.and.prob.le.10..and.
     +             obsv.ge.thresh) then
             pfield(ifh,ivr,ilv,iar,iob,14)=
     +         pfield(ifh,ivr,ilv,iar,iob,14)+1.
           else if(prob.gt.10..and.prob.le.20..and.
     +             obsv.ge.thresh) then
             pfield(ifh,ivr,ilv,iar,iob,15)=
     +         pfield(ifh,ivr,ilv,iar,iob,15)+1.
           else if(prob.gt.20..and.prob.le.30..and.
     +             obsv.ge.thresh) then
             pfield(ifh,ivr,ilv,iar,iob,16)=
     +         pfield(ifh,ivr,ilv,iar,iob,16)+1.
           else if(prob.gt.30..and.prob.le.40..and.
     +             obsv.ge.thresh) then
             pfield(ifh,ivr,ilv,iar,iob,17)=
     +         pfield(ifh,ivr,ilv,iar,iob,17)+1.
           else if(prob.gt.40..and.prob.le.50..and.
     +             obsv.ge.thresh) then
             pfield(ifh,ivr,ilv,iar,iob,18)=
     +         pfield(ifh,ivr,ilv,iar,iob,18)+1.
           else if(prob.gt.50..and.prob.le.60..and.
     +             obsv.ge.thresh) then
             pfield(ifh,ivr,ilv,iar,iob,19)=
     +         pfield(ifh,ivr,ilv,iar,iob,19)+1.
c             if(i.ge.5000.and.i.le.7000) then
c              write(*,*)'p50-60%:',i, prob, obsv, thresh,
c     +           pfield(ifh,ivr,ilv,iar,iob,19)
c             end if

           else if(prob.gt.60..and.prob.le.70..and.
     +             obsv.ge.thresh) then
             pfield(ifh,ivr,ilv,iar,iob,20)=
     +         pfield(ifh,ivr,ilv,iar,iob,20)+1.
           else if(prob.gt.70..and.prob.le.80..and.
     +             obsv.ge.thresh) then
             pfield(ifh,ivr,ilv,iar,iob,21)=
     +         pfield(ifh,ivr,ilv,iar,iob,21)+1.
           else if(prob.gt.80..and.prob.le.90..and.
     +             obsv.ge.thresh) then
             pfield(ifh,ivr,ilv,iar,iob,22)=
     +         pfield(ifh,ivr,ilv,iar,iob,22)+1.
           else if(prob.gt.90..and.prob.le.100..and.
     +             obsv.ge.thresh) then
             pfield(ifh,ivr,ilv,iar,iob,23)=
     +         pfield(ifh,ivr,ilv,iar,iob,23)+1.
           end if


5011               continue

         end if    !end of region mode
502               continue
602             continue
702            continue
802           continue
902         continue

          do ifh = 1, numfcst
           do ivr = 1, numvarbl
            do ilv = 1, numlevel
             do iar = 1, numarea
               do iob = 1, numvfyobs
                  do ip = 2,24
                    if (pfield(ifh,ivr,ilv,iar,iob,1).gt.0.0) then
                     pfield(ifh,ivr,ilv,iar,iob,ip) =
     +               pfield(ifh,ivr,ilv,iar,iob,ip)/
     +               pfield(ifh,ivr,ilv,iar,iob,1)
                    end if
                  end do
                end do
              end do
             end do
            end do
          end do

          do ifh = 1, numfcst
           do ivr = 1, numvarbl
            do ilv = 1, numlevel
             do iar = 1, numarea
               do iob = 1, numvfyobs
               if(pfield(ifh,ivr,ilv,iar,iob,1).gt.0.0) then 

              if(trim(namvarbl(ivr)).eq.'DSG7') threshold='1.0E-7'
              if(trim(namvarbl(ivr)).eq.'DSG8') threshold='1.0E-8'
              if(trim(namvarbl(ivr)).eq.'DSG9') threshold='1.0E-9'
              if(trim(namvarbl(ivr)).eq.'RF10') threshold='10'
              if(trim(namvarbl(ivr)).eq.'RF20') threshold='20'
              if(trim(namvarbl(ivr)).eq.'RF30') threshold='30'
              if(trim(namvarbl(ivr)).eq.'RF40') threshold='40'
              if(trim(namvarbl(ivr)).eq.'FOGEVNT') threshold='yes'
              if(trim(namvarbl(ivr)).eq.'ICETRC') threshold='1'
              if(trim(namvarbl(ivr)).eq.'ICELGT') threshold='2'
              if(trim(namvarbl(ivr)).eq.'VIS0400') threshold='400'
              if(trim(namvarbl(ivr)).eq.'VIS0800') threshold='800'
              if(trim(namvarbl(ivr)).eq.'VIS1600') threshold='1600'
              if(trim(namvarbl(ivr)).eq.'VIS3200') threshold='3200'
              if(trim(namvarbl(ivr)).eq.'VIS6400') threshold='6400'


              write(*,*)'trim(namvarbl(ivr)=',trim(namvarbl(ivr)),
     +                  ' ',threshold

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

                       istrt = iend + 1
                       iend = iend + 9
                       vdbhdr132(istrt:iend) = ' PBS_ENS:'

                       istrt = iend + 1
                       if(numodel .lt. 10)then
                        iend = iend + 1
                        write(vdbhdr132(istrt:iend),'(i1)')numodel
                       else if(numodel .lt. 100)then
                        iend = iend + 2
                        write(vdbhdr132(istrt:iend),'(i2)')numodel
                       else
                        iend = iend + 3
                        write(vdbhdr132(istrt:iend),'(i3)')numodel
                       end if

                      istrt = iend + 1
                      iend = iend + 1
                      vdbhdr132(istrt:iend) = '/'

                      istrt = iend + 1
                      iend = iend + 1
                      vdbhdr132(istrt:iend) = '>'

                      istrt = iend + 1
                      iend = iend + 6
                      vdbhdr132(istrt:iend) = threshold(1:6)
                      iend = iend + 1

                      istrt = iend + 1
                      vdbhdr132(iend:iend) = blank
                      iend = iend + 8
                      vdbhdr132(istrt:iend) =
     +                            namvarbl(ivr)(1:8)

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

                      WRITE (nvsdb,1555) vdbhdr132(:iend),
     +                  int(pfield(ifh,ivr,ilv,iar,iob,1)),
     +                   (pfield(ifh,ivr,ilv,iar,iob,ip),ip=2,24)
1555                  FORMAT (A,I10, 23(1X,F10.5))

                end if
               end do
              end do
             end do
            end do
           end do


           deallocate(sumf)
           deallocate(sumo)
           deallocate(sumh)
           deallocate(count)
           deallocate(weigh)

           return
 
          end
