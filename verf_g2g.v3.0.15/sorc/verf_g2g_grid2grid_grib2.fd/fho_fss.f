      
      subroutine gtSFHO(nvsdb,imodel,ist,numfcst,numvfyobs,numarea,
     +  numvarbl,numlevel,ngrid,levels,tendencymrk,updown,
     +  hasdata,fcstdata,obsvdata)

c    This program is to generate FHO vsdb records for pre-defined small blocks
c    (nxy*nxy, neighborhood method) to deal with hi-resolution models.   
c    Author: Binbin Zhou
c            Nov 2013

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
      CHARACTER*24 sfho(mxvrbl),  sfhothr(mxvrbl,20)
      CHARACTER*24 ffho(mxvrbl),  ffhothr(mxvrbl,20)
      integer  nchrfho(mxvrbl),nchrfhothr(mxvrbl,20),fhomrk(mxvrbl)
      integer  nchrafho(mxvrbl),nchrafhothr(mxvrbl,20),afhomrk(mxvrbl)
      integer  nchrsfho(mxvrbl),nchrsfhothr(mxvrbl,20),sfhomrk(mxvrbl)
      integer  nchrffho(mxvrbl),nchrffhothr(mxvrbl,20),ffhomrk(mxvrbl)
      real rfhothr(mxvrbl,20)
      real rafhothr(mxvrbl,20)
      real rsfhothr(mxvrbl,20)
      real rffhothr(mxvrbl,20)

      CHARACTER*1 updown(mxvrbl,20)
      integer tendencymrk(mxvrbl), continue_mrk(mxvrbl)
      integer anomlylev(mxvrbl,maxlvl),anomly_mrk(mxvrbl)

      COMMON /g2g/cyyyyfcst,cmmfcst,cddfcst,chhfcst,cfffcst,
     +            cyyyyobsv,cmmobsv,cddobsv,chhobsv,cffobsv,
     +             yyyyfcst, mmfcst, ddfcst, hhfcst, fffcst,
     +             yyyyobsv, mmobsv, ddobsv, hhobsv, ffobsv,
     +             k4,k5,k6,k7,ck7,vectormrk,namlvl,nchrlvl,
     +      fhomrk,fho,nchrfho,fhothr,nchrfhothr,rfhothr, 
     +             continue_mrk,anomly_mrk,anomlylev,
     +   afhomrk,afho,nchrafho,afhothr,nchrafhothr,rafhothr
      COMMON /FRC/
     +   sfhomrk,sfho,nchrsfho,sfhothr,nchrsfhothr,rsfhothr,
     +   ffhomrk,ffho,nchrffho,ffhothr,nchrffhothr,rffhothr



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

      real fcstij(imax(1),jmax(1)),obsvij(imax(1),jmax(1))
      integer ix, jy, mgrid,  mx,my

      real, allocatable ,dimension(:,:) :: fcstijblock,obsvijblock
      real, allocatable ,dimension(:,:) :: fblock,oblock
      real, allocatable ,dimension(:) :: fcstblock,obsvblock    

      CHARACTER*1 tag(mxvrbl)
      integer nxy(mxvrbl)

      COMMON /fss/tag,nxy



      DATA blank /' '/
      DATA equal /'='/
      DATA namversion /'V01'/
      DATA bmiss /10E10/
      DATA rmiss /99999./


      write(*,*) 'In gtFSS:',imodel,ist,numfcst,numvfyobs,numarea,
     +  numvarbl,numlevel,ngrid
      write(*,*) 'vectormrk', (vectormrk(i),i=1,numvarbl)
      write(*,*) 'fhomrk', (fhomrk(i),i=1,numvarbl)
      write(*,*) 'afhomrk',(afhomrk(i),i=1,numvarbl)
      write(*,*) 'sfhomrk',(sfhomrk(i),i=1,numvarbl)
      write(*,*)  'anomly_mrk',(anomly_mrk(i),i=1,numvarbl)     
      write(*,*)  'continue_mrk',(continue_mrk(i),i=1,numvarbl)     

      write(*,*) ' fho ', (trim(fho(i)),i=1,numvarbl)
      write(*,*) 'sfho ', (trim(sfho(i)),i=1,numvarbl)
      write(*,*) 'afho ', (trim(afho(i)),i=1,numvarbl)

      write(*,*)'weight factor=', area_factor(10000)
      write(*,*) 'tag',(tag(i),i=1,numvarbl)     
      write(*,*) 'nxy',(nxy(i),i=1,numvarbl)     

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

       do ifh = 1,1 
         do ivr = 1, numvarbl
           write(*,*) namvarbl(ivr), continue_mrk(ivr)
c           do ilv = 1, levels(ivr)
c              do i = 1,ngrid
c                if(fcstdata(ifh,ivr,ilv,i).gt.0.0 .and.
c     +             fcstdata(ifh,ivr,ilv,i).le.1000.) then
c                write(*,*) i, obsvdata(ifh,ivr,ilv,i),
c     +          fcstdata(ifh,ivr,ilv,i)
c                end if
c              end do
c            end do
           end do
         end do



      do 90 ifh = 1, numfcst
        do 80 iob = 1, numvfyobs
          do 70 iar = 1, numarea
           do 60 ivr = 1, numvarbl
             if(sfhomrk(ivr).eq.0) goto 60
             do 55 ifo = 1, sfhomrk(ivr)
               do 50 ilv = 1, levels(ivr)

                if(nodata(ifh,ivr,ilv).eq.1) goto 50

                if (mode(iar).eq.1) then                          !all GRID domain (mode 1)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
cccccccc 
           mx = INT (imax(1)/nxy(ivr))     !nxy: grids in x,y-directions for small square, mx/my: number of small areas in x/y-direction
           my = INT (jmax(1)/nxy(ivr))
           mgrid= mx * my
           allocate (fcstijblock(mx,my))
           allocate (obsvijblock(mx,my))
           allocate (fcstblock(mgrid))
           allocate (obsvblock(mgrid))
           allocate (fblock(nxy(ivr),nxy(ivr)))
           allocate (oblock(nxy(ivr),nxy(ivr)))
 
           !write(*,*) ifh,ifo,'mgrid, mx, my =',mgrid, mx, my

 
              do j = 1, jmax(1)
               do i = 1, imax(1)
                ij=(j-1)*imax(1) + i
                fcstij(i,j)=fcstdata(ifh,ivr,ilv,ij)
                obsvij(i,j)=obsvdata(ifh,ivr,ilv,ij)
               end do
              end do

              
              fblock=-9999.0             
              oblock=-9999.0             

              do im=1,mx
               do jm=1,my

                 do ib=1,nxy(ivr)
                  do 9001 jb=1,nxy(ivr)
                    ik=(im-1)*nxy(ivr)+ib
                    jk=(jm-1)*nxy(ivr)+jb

                    if(continue_mrk(ivr).eq.4.and.
     +                 (obsvij(ik,jk).le.-100.0.or.
     +                  fcstij(ik,jk).le.-100.0)) goto 9001

                    if(continue_mrk(ivr).eq.7.and.
     +                 (obsvij(ik,jk).le.-100.0.or.
     +                  fcstij(ik,jk).le.-9999.0)) goto 9001

                    if(continue_mrk(ivr).eq.4) then
                      if(obsvij(ik,jk).le.0.)
     +                  obsvij(ik,jk)=10.0**obsvij(ik,jk)
                      if(fcstij(ik,jk).le.0.)
     +                  fcstij(ik,jk)=10.0**fcstij(ik,jk)
                    end if

                    if(continue_mrk(ivr).eq.7) then
                      if(obsvij(ik,jk).le.0.)
     +                  obsvij(ik,jk)=0.0
                      if(fcstij(ik,jk).le.0.)
     +                  fcstij(ik,jk)=0.0
                    end if

                    !So, skipped undfined fblock and oblock has value of !-9999.0
                    fblock(ib,jb)=fcstij(ik,jk)
                    oblock(ib,jb)=obsvij(ik,jk)

9001              continue 
                 end do
                
                 if (tag(ivr).eq.'M') Then
                    fcstijblock(im,jm)=MAXVAL(fblock)
                    obsvijblock(im,jm)=MAXVAL(oblock)
                 else if (tag(ivr).eq.'A') then
                    fcstijblock(im,jm)=average(fblock,nxy(ivr))
                    obsvijblock(im,jm)=average(oblock,nxy(ivr))
                 else
                  write(*,*) 'No such operation', tag(ivr)
                  stop 100
                 end if

                 end do
               end do

                do im = 1,mx
                 do jm = 1, my
                  ij=(jm-1)*mx + im
                  fcstblock(ij)=fcstijblock(im,jm)
                  obsvblock(ij)=obsvijblock(im,jm)
                 end do
                end do

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

           do 501 i = 1,mgrid
               if(fcstblock(i).lt.0.0.or.obsvblock(i).lt.0.) goto 501    !skip undefined blocks

               if(tendencymrk(ivr).eq.0) then
                   f=getFO(fcstblock(i),
     +                rsfhothr(ivr,ifo),fho(ivr))
                   o=getFO(obsvblock(i),rsfhothr(ivr,ifo),fho(ivr))
                   h=getHit(fcstblock(i),obsvblock(i),
     +                rsfhothr(ivr,ifo),fho(ivr))
               else
                   f=getTND_FO(fcstblock(i),
     +               updown(ivr,ifo),rfhothr(ivr,ifo),fho(ivr))
                   o=getTND_FO(obsvblock(i),
     +                  updown(ivr,ifo),rsfhothr(ivr,ifo),fho(ivr))
                   h=getTND_Hit(fcstblock(i),obsvblock(i),
     +                  updown(ivr,ifo),rsfhothr(ivr,ifo),fho(ivr))
               end if
                  !if(i.eq.2966) then
                  !  write(*,*) 'grid point=',i,
     +            !  'fcst=',fcstblock(i),
     +            !  'obsv=',obsvblock(i), 
     +            !  'threhold=', fho(ivr), rsfhothr(ivr,ifo)
                  !  write(*,*)'f,o,h=', f, o, h
                  !end if

                    sumf(ifh,ivr,ilv,iar,iob,ifo) = 
     +              sumf(ifh,ivr,ilv,iar,iob,ifo) + f

                    sumo(ifh,ivr,ilv,iar,iob,ifo) =
     +              sumo(ifh,ivr,ilv,iar,iob,ifo) + o

                    sumh(ifh,ivr,ilv,iar,iob,ifo) =
     +              sumh(ifh,ivr,ilv,iar,iob,ifo) + h
                   
                    count(ifh,ivr,ilv,iar,iob,ifo) =
     +              count(ifh,ivr,ilv,iar,iob,ifo) + 1.0
                    weigh(ifh,ivr,ilv,iar,iob,ifo) =
     +              weigh(ifh,ivr,ilv,iar,iob,ifo) + 1.0
 
501               continue 
 
           deallocate (fcstijblock)
           deallocate (obsvijblock)
           deallocate (fcstblock)
           deallocate (obsvblock)
           deallocate (fblock)
           deallocate (oblock)

          end if
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
          if(sfhomrk(ivr).eq.0) goto 61
          do 56 ifo = 1, sfhomrk(ivr)
            do 51 ilv = 1, levels(ivr)
             if(nodata(ifh,ivr,ilv).eq.1) goto 51
             if(weigh(ifh,ivr,ilv,iar,iob,ifo).ne.0.0) then
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
                                                                                                                                                                                   

c            write(*,*)'sumf,sumo,sumh,count=',
c     +      count(ifh,ivr,ilv,iar,iob,ifo),
c     +      sumf(ifh,ivr,ilv,iar,iob,ifo),
c     +      sumo(ifh,ivr,ilv,iar,iob,ifo),
c     +      sumh(ifh,ivr,ilv,iar,iob,ifo) 

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
               if(sfhomrk(ivr).eq.0) goto 160
               do 150 ifo = 1, sfhomrk(ivr)
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
                      iend = iend + nchrfho(ivr) - 1
c                      vdbhdr132(istrt:iend) =
c     +                            fho(ivr)(:nchrfho(ivr))
                      vdbhdr132(istrt:iend) ='FHO>' 
                                                                                                                                                       
                      istrt = iend + 1
                      iend = iend + nchrsfhothr(ivr,ifo)
                      vdbhdr132(istrt:iend) =
     +                      sfhothr(ivr,ifo)(:nchrsfhothr(ivr,ifo))
                     
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
                        WRITE (nvsdb,1000) vdbhdr132(:iend),
     +                              count(ifh,ivr,ilv,iar,iob,ifo),
     +                              sumf(ifh,ivr,ilv,iar,iob,ifo),
     +                              sumh(ifh,ivr,ilv,iar,iob,ifo),
     +                              sumo(ifh,ivr,ilv,iar,iob,ifo)
 1000                   FORMAT (A,F10.0,3E18.9)
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

         FUNCTION average(A,n)
           real A(n,n)
            x=0.
            c=0.0
           do i=1,n
            do j=1,n
             if(A(i,j).ge.0.0) then
               x=x+A(i,j)
               c=c+1.0
             end if
            end do
           end do

           if(c.gt.0.0) then
            average=x/c
           else
            average=-99.0
           end if

           return
          END



