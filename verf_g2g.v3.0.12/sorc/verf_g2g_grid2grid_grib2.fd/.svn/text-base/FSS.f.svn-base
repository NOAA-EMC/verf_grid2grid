      subroutine gtFSS(nvsdb,imodel,ist,numfcst,numvfyobs,numarea,
     +  numvarbl,numlevel,ngrid,levels,tendencymrk,updown,
     +  hasdata,fcstdata,obsvdata)

c    This program is to generate FSS vsdb records for pre-defined small blocks
c    (nxy*nxy, neighborhood method) to deal with hi-resolution models based on
c     Robert and Lean (2008, MWR, 136, 78-97)   
c    Author: Binbin Zhou
c            Nov 2013

      INCLUDE 'parm.inc'
      
      INTEGER, intent(IN) :: imodel, ist, numvfyobs,numarea,
     +         numfcst,numvarbl,numlevel,ngrid, levels(mxvrbl),
     +         hasdata(mxfcst,mxvrbl,maxlvl)

      REAL,dimension(numfcst,numvarbl,numlevel,ngrid) ::
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



      real*8, allocatable, dimension(:,:,:,:,:,:) :: sumff,sumoo,sumfo,
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
     +   fhomrk,fho,nchrfho,fhothr,nchrfhothr,rfhothr, 
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
      integer nxy(mxvrbl),contain_point

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
      write(*,*) 'ffhomrk',(ffhomrk(i),i=1,numvarbl)
      write(*,*)  'anomly_mrk',(anomly_mrk(i),i=1,numvarbl)     
      write(*,*)  'continue_mrk',(continue_mrk(i),i=1,numvarbl)     

      write(*,*) ' fho ', (trim(fho(i)),i=1,numvarbl)
      write(*,*) 'sfho ', (trim(sfho(i)),i=1,numvarbl)
      write(*,*) 'afho ', (trim(afho(i)),i=1,numvarbl)
      write(*,*) 'ffho ', (trim(ffho(i)),i=1,numvarbl)

      write(*,*)'weight factor=', area_factor(10000)
      write(*,*) 'tag',(tag(i),i=1,numvarbl)     
      write(*,*) 'nxy',(nxy(i),i=1,numvarbl)     

      allocate(sumff(numfcst,numvarbl,numlevel,numarea,numvfyobs,10))
      allocate(sumoo(numfcst,numvarbl,numlevel,numarea,numvfyobs,10))
      allocate(sumfo(numfcst,numvarbl,numlevel,numarea,numvfyobs,10))
      allocate(count(numfcst,numvarbl,numlevel,numarea,numvfyobs,10))
      allocate(weigh(numfcst,numvarbl,numlevel,numarea,numvfyobs,10))

          count = 0.0
          weigh = 0.0
          sumff = 0.0
          sumoo = 0.0
          sumfo = 0.0

      do ivr = 1, numvarbl
       write(*,*)ivr,'threshold=',(rffhothr(ivr,j),j=1,ffhomrk(ivr)), 
     +    ' tendencymrk=',tendencymrk(ivr),' updown=',
     +    (updown(ivr,j),j=1,ffhomrk(ivr)) 

       if (trim(namvarbl(ivr)).eq.'FOGEVNT') then
         continue_mrk(ivr)=5
       end if
      end do


      do 90 ifh = 1, numfcst
        do 80 iob = 1, numvfyobs
          do 70 iar = 1, numarea
           do 60 ivr = 1, numvarbl
             if(ffhomrk(ivr).eq.0) goto 60
             do 55 ifo = 1, ffhomrk(ivr)
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

            !write(*,*)'Check FSS: mx=',mx,'my=',my,'nxy=',nxy(ivr)

               fblock=-9999.0
               oblock=-9999.0
               
                do im=1,mx
                 do jm=1,my

              contain_point=mcontain(im,jm,nxy(ivr),1140,165)
              !write(*,*)'im, jm, contain_point=',im,jm,contain_point

               
                   do ib=1,nxy(ivr)
                    do 9001 jb=1,nxy(ivr)
                      ik=(im-1)*nxy(ivr)+ib
                      jk=(jm-1)*nxy(ivr)+jb

                      if(continue_mrk(ivr).eq.4.and.
     +           (obsvij(ik,jk).le.-100.0.or.             !MOSAIC: -99 is clear sky, should be included,-999 in undefined 
     +            fcstij(ik,jk).le.-100.0)) goto 9001     !Fcst and MOSAIC: -999, -99999 are undefined so skipped   

                      if(continue_mrk(ivr).eq.7.and.
     +           (obsvij(ik,jk).le.-100.0.or.             !MOSAIC: -1 is clear sky, so included
     +            fcstij(ik,jk).le.-9999.0)) goto 9001    !NCEP & HRRR: -99999 undefined, skipped, HRRR: -999 clear sky, included 

                      if(continue_mrk(ivr).eq.4) then
                        if(obsvij(ik,jk).le.0.)
     +                    obsvij(ik,jk)=10.0**obsvij(ik,jk)
                        if(fcstij(ik,jk).le.0.)
     +                    fcstij(ik,jk)=10.0**fcstij(ik,jk)
                      end if

                      if(continue_mrk(ivr).eq.7) then    
                        if(obsvij(ik,jk).le.0.) 
     +                    obsvij(ik,jk)=0.0
                        if(fcstij(ik,jk).le.0.)
     +                    fcstij(ik,jk)=0.0
                      end if


                  ! So, the skipped fblock and oblock values are -9999.0
                      fblock(ib,jb)=fcstij(ik,jk)
                      oblock(ib,jb)=obsvij(ik,jk)

          
9001              continue
                    end do
                
                    fcstijblock(im,jm)=getfraction(fblock,oblock,
     +                      nxy(ivr),rffhothr(ivr,ifo),fho(ivr))
                    obsvijblock(im,jm)=getfraction(oblock,oblock,
     +                      nxy(ivr),rffhothr(ivr,ifo),fho(ivr))


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

           if(obsvblock(i).lt.0.0.or.fcstblock(i).lt.0.0 ) goto 501 !skip undefined blocks

            sumff(ifh,ivr,ilv,iar,iob,ifo) = 
     +          sumff(ifh,ivr,ilv,iar,iob,ifo) + 
     +          fcstblock(i)*fcstblock(i)*area_factor(i)

            sumoo(ifh,ivr,ilv,iar,iob,ifo) = 
     +          sumoo(ifh,ivr,ilv,iar,iob,ifo) + 
     +          obsvblock(i)*obsvblock(i)*area_factor(i)

            sumfo(ifh,ivr,ilv,iar,iob,ifo) = 
     +          sumfo(ifh,ivr,ilv,iar,iob,ifo) + 
     +          fcstblock(i)*obsvblock(i)*area_factor(i)
                   
                    count(ifh,ivr,ilv,iar,iob,ifo) =
     +              count(ifh,ivr,ilv,iar,iob,ifo) + area_factor(i)
                    weigh(ifh,ivr,ilv,iar,iob,ifo) =
     +              weigh(ifh,ivr,ilv,iar,iob,ifo) + area_factor(i)

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
          if(ffhomrk(ivr).eq.0) goto 61
          do 56 ifo = 1, ffhomrk(ivr)
            do 51 ilv = 1, levels(ivr)

             if(nodata(ifh,ivr,ilv).eq.1) goto 51
             if(weigh(ifh,ivr,ilv,iar,iob,ifo).ne.0.0) then
              sumff(ifh,ivr,ilv,iar,iob,ifo)=
     +        sumff(ifh,ivr,ilv,iar,iob,ifo)/
     +        weigh(ifh,ivr,ilv,iar,iob,ifo)
                                                                                                                                                                                   
              sumoo(ifh,ivr,ilv,iar,iob,ifo)=
     +        sumoo(ifh,ivr,ilv,iar,iob,ifo)/
     +        weigh(ifh,ivr,ilv,iar,iob,ifo)
                                                                                                                                                                                   
              sumfo(ifh,ivr,ilv,iar,iob,ifo)=
     +        sumfo(ifh,ivr,ilv,iar,iob,ifo)/
     +        weigh(ifh,ivr,ilv,iar,iob,ifo)
                                                                                                                                                                                   
              if(sumff(ifh,ivr,ilv,iar,iob,ifo).lt.0.0)
     +           sumff(ifh,ivr,ilv,iar,iob,ifo)=0.0
              if(sumoo(ifh,ivr,ilv,iar,iob,ifo).lt.0.0)
     +           sumoo(ifh,ivr,ilv,iar,iob,ifo)=0.0
              if(sumfo(ifh,ivr,ilv,iar,iob,ifo).lt.0.0)
     +           sumfo(ifh,ivr,ilv,iar,iob,ifo)=0.0
                                                                                                                                                                                   

c            write(*,*)'sumff,sumoo,sumfo,count=',
c     +      count(ifh,ivr,ilv,iar,iob,ifo),
c     +      sumff(ifh,ivr,ilv,iar,iob,ifo),
c     +      sumoo(ifh,ivr,ilv,iar,iob,ifo),
c     +      sumfo(ifh,ivr,ilv,iar,iob,ifo) 

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
               if(ffhomrk(ivr).eq.0) goto 160
               do 150 ifo = 1, ffhomrk(ivr)
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
                      iend = iend + nchrffhothr(ivr,ifo)
                      vdbhdr132(istrt:iend) =
     +                      ffhothr(ivr,ifo)(:nchrffhothr(ivr,ifo))
                     
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
     +                              sumff(ifh,ivr,ilv,iar,iob,ifo),
     +                              sumoo(ifh,ivr,ilv,iar,iob,ifo),
     +                              sumfo(ifh,ivr,ilv,iar,iob,ifo)
 1000                   FORMAT (A,F10.0,3E18.9)
                    END IF
150             continue
155            continue
160           continue
170          continue
180         continue
190        continue

           deallocate(sumff)
           deallocate(sumoo)
           deallocate(sumfo)
           deallocate(count)
           deallocate(weigh)

           return
 
          end



         FUNCTION getfraction(A,B,n,threshold,symbol)
           real A(n,n), B(n,n)               !B(n,n) is observation 
           character(len=10), intent(in) :: symbol
           real,              intent(in) :: threshold

           getfraction=-999.0

           if (trim(symbol).eq.'PFHO>') then
             c=0.0
             d=0.0   !total effective grids 
             do 101 i=1,n
              do 102  j=1,n

               if(B(i,j).gt.0) then !skip obsv missing grids (obsv<0 means cloud,or refectivity or precip is missing)
                 d=d+1.0
               else
                 goto 102
               end if

               if(A(i,j).gt.threshold) then
                c=c+1.0
               end if

 102          continue
 101         continue

             if (d.gt.0.0) then
                getfraction=c/d
              end if

            else if (trim(symbol).eq.'PFHO<') then
               c=0.0
               d=0.0
               do 103 i=1,n
                do 104 j=1,n
                 if(B(i,j).gt.0) then
                    d=d+1.0
                 else
                    goto 104
                 end if
                 if(A(i,j).lt.threshold) then
                  c=c+1.0
                 end if
 104            continue
 103           continue
              if (d.gt.0.0) then
                getfraction=c/d
              end if

             else if (trim(symbol).eq.'PFHO=') then
               c=0.0
               d=0.0
               do 105 i=1,n
                do 106  j=1,n

                 if(B(i,j).gt.0) then
                   d=d+1.0
                 else
                   goto 106
                 end if

                 if(A(i,j).eq.threshold) then
                  c=c+1.0
                 end if

 106            continue
 105           continue
               if (d.gt.0.0) then
                getfraction=c/d
               end if
             else
              write(*,*) 'Wrong FSS setting'
              stop 111
             end if

             return
           END

           function mcontain(im,jm,nxy,i,j)
             mcontain=0
             do ib=1,nxy
               do jb=1,nxy
                  ik=(im-1)*nxy+ib
                  jk=(jm-1)*nxy+jb
                  if(ik.eq.i.and.jk.eq.j) then
                   mcontain=1
                   goto 50
                  end if
                end do
              end do

 50            return
             end

          FUNCTION getfraction_New(A,B,n,threshold,symbol)
           real A(n,n), B(n,n)               !B(n,n) is observation 
           character(len=10), intent(in) :: symbol
           real,              intent(in) :: threshold
           
           getfraction_New=-1.0

           if (trim(symbol).eq.'PFHO>') then
             c=0.0
             d=0.0
             do 101 i=1,n
              do 102  j=1,n
                 if(B(i,j).lt.0.0.or.A(i,j).lt.0.0) then !skip undefined obsv and fcst blocks
                    goto 102
                 else
                    d=d+1
                 end if

               if(A(i,j).gt.threshold) then
                c=c+1.0
               end if

 102          continue 
 101         continue 
              if (d.gt.0.0) then
                getfraction_New=c/d
              end if

            else if (trim(symbol).eq.'PFHO<') then
               c=0.0
               d=0.0
               do 103 i=1,n
                do 104 j=1,n
                 if(B(i,j).lt.0.0.or.A(i,j).lt.0.0) then
                    goto 104
                 else
                    d=d+1
                 end if
                 if(A(i,j).lt.threshold) then
                  c=c+1.0
                 end if
 104            continue 
 103           continue 
              if (d.gt.0.0) then
                getfraction_New=c/d
              end if
 
             else if (trim(symbol).eq.'PFHO=') then
               c=0.0
               d=0.0
               do 105 i=1,n
                do 106  j=1,n
                 if(B(i,j).lt.0.0.or.A(i,j).lt.0.0) then
                    goto 106
                 else
                    d=d+1
                 end if

                 if(A(i,j).eq.threshold) then
                  c=c+1.0
                 end if

 106            continue 
 105           continue 
               if (d.gt.0.0) then
                getfraction_New=c/d
               end if
             else
              write(*,*) 'Wrong FSS setting'
              stop 111
             end if        

             return
           END

