	subroutine EFS(nvsdb,Nmodel,ensname,ist,numfcst,numvfyobs,
     +        numarea,numvarbl,numlevel,ngrid,levels,plevel,
     +        hasdata,fcstmdl,obsvdata,ensmbr,ib,kb)


c    This program is to generate PROB for ensemble forecast
c      adaped from Yuejian Zhu FVPROB_pub.f 
c
c    Author: Binbin Zhou
c          April 18 2011
c    Modification: B. Zhou, 20120614,  add Yuejian output format files  
c    Modification: B. Zhou, 20140110, add specified thresholds for reliability
c    Modification: B. Zhou, 20150507, make get climatology data for both 1.0 deg
c                                     and old 2.5 deg
c            and Hit-False alarm rate  PROB subroutine
c                         
                                                                                         
      INCLUDE 'parm.inc'
      
      INTEGER, intent(IN) :: Nmodel, ist, numvfyobs,numarea,
     +         numfcst,numvarbl,numlevel,ngrid, levels(mxvrbl),
     +         hasdata(mxfcst,mxvrbl,maxlvl)

       REAL,dimension(Nmodel,numfcst,numvarbl,numlevel,ngrid),
     +       intent(IN) :: fcstmdl
                                                                                                                                                            
      CHARACTER*20, intent(IN) ::  ensname           !such as WRF/6, ETA/10, RSM/5, SREF/21

      REAL,dimension(numfcst,numvarbl,numlevel,ngrid),intent(IN) ::
     +          obsvdata
                                                                                                                                        
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

      COMMON /grb/igribid

      integer nodata(mxfcst,mxvrbl,maxlvl)
      COMMON /nofile/nodata

      CHARACTER*3 namversion
      CHARACTER*132 vdbhdr132, input, substr (3)
                                                                                                                                                         
      CHARACTER*1 blank, equal

      CHARACTER*80 meangribfile,sprdgribfile


      LOGICAL*1 latlong, lambert, polarstereo    !Add  by Binbin, otherwise, they are can not be retrieved
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

      CHARACTER*24 fho(mxvrbl),    fhothr(mxvrbl,20), op
      CHARACTER*24 afho(mxvrbl),   afhothr(mxvrbl,20)

      integer  nchrfho(mxvrbl),nchrfhothr(mxvrbl,20),fhomrk(mxvrbl)
      integer  nchrafho(mxvrbl),nchrafhothr(mxvrbl,20),afhomrk(mxvrbl)

      CHARACTER*24 sfho(mxvrbl),sfhothr(mxvrbl,20)
      CHARACTER*24 ffho(mxvrbl),ffhothr(mxvrbl,20)
      integer  nchrsfho(mxvrbl),nchrsfhothr(mxvrbl,20),sfhomrk(mxvrbl)
      integer  nchrffho(mxvrbl),nchrffhothr(mxvrbl,20),ffhomrk(mxvrbl)
      real rsfhothr(mxvrbl,20)
      real rffhothr(mxvrbl,20) 

      real rfhothr(mxvrbl,20), thresholds(20) 
      real rafhothr(mxvrbl,20)
      integer continue_mrk(mxvrbl)
      integer anomlylev(mxvrbl,maxlvl),anomly_mrk(mxvrbl)


      COMMON /g2g/cyyyyfcst,cmmfcst,cddfcst,chhfcst,cfffcst,
     +            cyyyyobsv,cmmobsv,cddobsv,chhobsv,cffobsv,
     +             yyyyfcst, mmfcst, ddfcst, hhfcst, fffcst,
     +             yyyyobsv, mmobsv, ddobsv, hhobsv, ffobsv,
     +             k4,k5,k6,k7,ck7,vectormrk,namlvl,nchrlvl,
     +        fhomrk,fho,nchrfho,fhothr,nchrfhothr,rfhothr,
     +             continue_mrk, anomly_mrk, anomlylev,
     +       afhomrk,afho,nchrafho,afhothr,nchrafhothr,rafhothr
      COMMON /FRC/
     +       sfhomrk,sfho,nchrsfho,sfhothr,nchrsfhothr,rsfhothr,
     +       ffhomrk,ffho,nchrffho,ffhothr,nchrffhothr,rffhothr


      integer plevel(maxlvl)
      integer region_id(maxpts)
      real region_latlon(2,maxpts),ptr1(2,mxarea), ptr2(2,mxarea)
 
      COMMON /reg/region_id, region_latlon,ptr1,ptr2
      COMMON /layer/ modelvl(maxlvl), iplevel(maxlvl,2)


      real area_factor(maxpts)               !add for area weighting of grid area in high latitude
      COMMON /weight/area_factor

      CHARACTER*4 CNmodel
      integer d1,d2,d3,numCNmodel
      integer Nmodel1

      character*4 var, level
      real clip(ngrid,kb)                         !climatology data over entire domain
      real clim1(ngrid,101),clim2(ngrid,101)      !2.5g previous and next month climatology data over entire domain 
c      REAL rfcst(ngrid,Nmodel),rfanl(ngrid)
      real, allocatable, dimension(:,:) :: rfcst,rclim
      real, allocatable, dimension(:) :: rfanl 
      real wght(ngrid),clip2deg(101)
      character*4 score_name(8)
      real, allocatable, dimension(:,:,:,:,:,:) :: fit,fir,rmse 

      real, allocatable, dimension(:,:,:,:,:,:) :: obsv,fcst,
     +       hit,far,rps,bss,eco 
      integer nchr_ensname 
 
      character*24 ensmbr(100)
      COMMON /vscore/infow(500), probs(500), dists(500)
      character*40 file_yuejian

      real, allocatable, dimension(:,:) :: aloc         !position To use Yuejian's location weight
 

      DATA blank /' '/
      DATA equal /'='/
      DATA namversion /'V01'/
      DATA bmiss /10E10/
      DATA rmiss /99999./

      DATA (score_name(i),i=1,8) /'HIST','RELP','RMSE',
     + 'RELI','HTFR','RPS ','BSS ','ECON'/

      nchr_ensname=len_trim(ensname)
      Nmodel1=Nmodel+1

      if(Nmodel.lt.10) then
       CNmodel='/'//achar(Nmodel+48)
       numCNmodel = 2
      else if(Nmodel.ge.10.and.Nmodel.lt.100) then
       d1 = Nmodel/10
       d2 = Nmodel - d1*10
       CNmodel='/'//achar(d1+48)//achar(d2+48)
       numCNmodel = 3
      else
       d1 = Nmodel/100
       d2 = Nmodel/10 - d1*10
       d3 = Nmodel - d1*100 -d2*10 
       CNmodel='/'//achar(d1+48)//achar(d2+48)//achar(d3+48)
       numCNmodel = 4
      end if


      write(*,*) 'In EFS:',Nmodel,ist,numfcst,numvfyobs,numarea,
     +  numvarbl,numlevel,ngrid

      write(*,*) 'fho symbol', (fho(ivr),ivr=1,numvarbl)  
      write(*,*) 'fhomrk', (fhomrk(ivr),ivr=1,numvarbl)
      write(*,*) 'continue_mrk', (continue_mrk(ivr),ivr=1,numvarbl)
      write(*,*) 'rfhothr', (rfhothr(ivr,1),ivr=1,numvarbl)
      do i=1,numfcst
       write(*,*) 'nfcst=',i
       do j=1,numvarbl
        write(*,*) 'ivr=',j,' nodata', (nodata(i,j,k),k=1,numlevel)
       end do
      end do

      allocate(fit(numfcst,numvfyobs,numarea,numvarbl,numlevel,
     +  Nmodel+1))
      allocate(fir(numfcst,numvfyobs,numarea,numvarbl,numlevel,
     + Nmodel+1))
      allocate(rmse(numfcst,numvfyobs,numarea,numvarbl,numlevel,5))
 
      allocate(obsv(numfcst,numvfyobs,numarea,numvarbl,numlevel,
     +  Nmodel+1))
      allocate(fcst(numfcst,numvfyobs,numarea,numvarbl,numlevel,
     +  Nmodel+1))
      allocate(hit(numfcst,numvfyobs,numarea,numvarbl,numlevel,
     +  Nmodel+1))
      allocate(far(numfcst,numvfyobs,numarea,numvarbl,numlevel,
     +  Nmodel+1))
      allocate(eco(numfcst,numvfyobs,numarea,numvarbl,numlevel,18))
      allocate(rps(numfcst,numvfyobs,numarea,numvarbl,numlevel,9))
      allocate(bss(numfcst,numvfyobs,numarea,numvarbl,numlevel,9))

      allocate(aloc(imax(1),jmax(1)))

          ix=imax(1)
          iy=jmax(1)


       do 90 ifh = 1, numfcst
        do 80 iob = 1, numvfyobs
          do 70 iar = 1, numarea

          write(*,*) 'numreg(iar)=',numreg(iar)

           do 60 ivr = 1, numvarbl
            !if(fhomrk(ivr).ne.0)    goto 60
             do 50 ilv = 1, levels(ivr)
              if(nodata(ifh,ivr,ilv).eq.1) goto 50

c (1)   Get  climatology data at all grid points based on Yuejian code 
         
c         jmm=mmobsv(ifh)
c         jdd=ddobsv(ifh)
c         jhh=hhobsv(ifh)
        
          nthrs=fhomrk(ivr)
          op=fho(ivr)
          thresholds(:)=rfhothr(ivr,:)

          kk4=k4(ivr)
          kk5=k5(ivr)
          kk6=k6(ivr)
          if(kk6.eq.100) then
            kk7 = plevel(ilv)
          else
            kk7 = k7(ivr)
          end if

       !(I) get climate bin data

      IF (INDEX(ensname,'2P5').eq.0) THEN !Get 1d climatology or model as climatology data 

        if (ib.eq.1.or.ib.eq.0) then      !use a singgle model forecast data as climate reference
                                         !in this case, this single model forecast yyyy,mm,dd,hh,ff
                                         !are same as the ensemble forecast's yyyy,mm,dd,hh,ff
         jmm=mmfcst(ifh)
         jdd=ddfcst(ifh)
         jhh=hhfcst(ifh)

          meangribfile="clim.mean."//cyyyyfcst(ifh)//
     +         cmmfcst(ifh)//cddfcst(ifh)//chhfcst(ifh)//
     +         cfffcst(ifh)
          sprdgribfile="clim.sprd."//cyyyyfcst(ifh)//
     +         cmmfcst(ifh)//cddfcst(ifh)//chhfcst(ifh)//
     +         cfffcst(ifh)


        else

         jmm=mmobsv(ifh)
         jdd=ddobsv(ifh)
         jhh=hhobsv(ifh)


         meangribfile="clim.mean."//
     +         cmmobsv(ifh)//cddobsv(ifh)//chhobsv(ifh)

         sprdgribfile="clim.stdv."//
     +         cmmobsv(ifh)//cddobsv(ifh)//chhobsv(ifh)


        end if

          write(*,*) trim(meangribfile),' ',trim(sprdgribfile)

          clip = 0. 
          call get_HiresClimData(meangribfile,sprdgribfile,
     +      kk4,kk5,kk6,kk7,ngrid,Nmodel,kb,clip,noclim)

          if(noclim.eq.1) then
            nodata(ifh,ivr,ilv)=noclim
            goto 50
          end if

       ELSE    ! For 2.5x2.5 degree verification, use Yuejian 2.5deg old Binary clim data 

        write(*,*) 'Getting 2.5deg climatology data'
 
         jmm=mmobsv(1)
         jdd=ddobsv(1)
         var=trim(namvarbl(ivr))
         len=len_trim(namlvl(ivr,ilv))
         level=namlvl(ivr,ilv)(2:len)

         if(trim(level).eq.'2'.or.trim(level).eq.'10')
     +      level=trim(level)//'M'

          ix=imax(1)
          iy=jmax(1)

          write(*,*) jmm,jdd,var,level,ix,iy,ngrid

          call get_ClimaData(jmm,jdd,var,level,ix,iy,ngrid,
     +    clim1,clim2,noclim)

          if(noclim.eq.1) then
           nodata(ifh,ivr,ilv) = 1
           goto 50
          end if

          fac2 = abs((float(jdd)-15.0)/30.)
          fac1 = 1.0-fac2

       END if


      if (mode(iar).eq.1) then                  !all GRID domain (mode 1)
 
       if (ensname(1:4).eq.'HREF'.and.              !Specific for HREF
     +     ((kk4.eq.0.and.kk5.eq.0).or.             !For T2m    
     +      (kk4.eq.16.and.kk5.eq.196).or.          !For Reflectivity
     +      (kk4.eq.16.and.kk5.eq.197).or.          !For Etop
     +      (kk4.eq.19.and.kk5.eq.0).or.            !For VIS
     +      (kk4.eq.1.and.kk5.eq.8).or.             !For APCP
     +      (kk4.eq.6.and.kk5.eq.1) ) )  then       !For CLOUD 
                        
          nxy=0
          if(kk4.eq.16.and.kk5.eq.196) then
            do  i = 1,ngrid   !how many effective points for refl
              if(obsvdata(ifh,ivr,ilv,i).gt.-990.) then
                nxy=nxy+1
              end if
            end do
          write(*,*) 'In EFS, reflectivity nxy=',nxy
          else if(kk4.eq.0.and.kk5.eq.0) then
            do 7000 i = 1,ngrid   !how many effective points for t2m
              do iens=1,Nmodel
               if(fcstmdl(iens,ifh,ivr,ilv,i).eq.0.) goto 7000
              end do 
              nxy=nxy+1
7000        continue
          else ! All APCP and VIS grid points are effective!
            nxy=ngrid
          end if

          write(*,*) 'how many effective points nxy=',nxy

          allocate(rclim(nxy,kb))
          allocate(rfanl(nxy))
          allocate(rfcst(nxy,Nmodel))

           nxy=0
           swgt = 0.
           rclim = 0.

          do i = 1,ngrid 
           if(kk4.eq.16.and.kk5.eq.196) then
            if(obsvdata(ifh,ivr,ilv,i).gt.-990.) then
             nxy=nxy+1
             rclim(nxy,:) = clip(i,:)
             area_factor(nxy)=cos(datan(1.0d0)*region_latlon(1,i)/45.0)
             swgt=swgt +  area_factor(nxy)

              do iens=1,Nmodel
                rfcst(nxy,iens)=fcstmdl(iens,ifh,ivr,ilv,i)
                !set very small reflectivity to be 0 
                if(rfcst(nxy,iens).le.0.) rfcst(nxy,iens)=0.
              end do

              rfanl(nxy)=obsvdata(ifh,ivr,ilv,i)
              !set very small eflectivity (<0 but >-999) to be 0
              if (rfanl(nxy).le.0.) rfanl(nxy)=0.
            end if
           else if (kk4.eq.0.and.kk5.eq.0) then
             do iens=1,Nmodel                !As long as one member has T2m-0.0 at gridpoint i, skip this grid point
                if(fcstmdl(iens,ifh,ivr,ilv,i).eq.0.) goto 7100
             end do
             nxy=nxy+1
             do iens=1,Nmodel
                rfcst(nxy,iens)=fcstmdl(iens,ifh,ivr,ilv,i)
             end do
             rclim(nxy,:) = clip(i,:)
             area_factor(nxy)=cos(datan(1.0d0)*region_latlon(1,i)/45.0)
             swgt=swgt +  area_factor(nxy)
             rfanl(nxy)=obsvdata(ifh,ivr,ilv,i)
7100         continue
              
           else
             ! VIS and APCP need not to change'
             nxy=nxy+1
             rclim(nxy,:) = clip(i,:)
             area_factor(nxy)=cos(datan(1.0d0)*region_latlon(1,i)/45.0)
             swgt=swgt +  area_factor(nxy)
             do iens=1,Nmodel
               rfcst(nxy,iens)=fcstmdl(iens,ifh,ivr,ilv,i)
             end do
             rfanl(nxy)=obsvdata(ifh,ivr,ilv,i)
           end if

          end do

          write(*,*) 'nxy ib swgt=',nxy,ib,swgt

          do i = 1, nxy
            wght(i)=area_factor(i)*nxy/swgt
          end do

          dists = 0.
          probs = 0.
          scrf = 1.0/float(ib)

          if (ib.eq.1) then
           tprob=0.0
           cprob=0.0
           do i=1,nxy
             tprob=tprob+wght(i)
             if(rfanl(i).le.rclim(i,2)) then
                  cprob=cprob+wght(i)
             end if
           end do

           scrf = cprob/tprob

          end if

          if((kk4.eq.1.and.kk5.eq.8).or.
     +       (kk4.eq.19.and.kk5.eq.0).or.
     +       (kk4.eq.16.and.kk5.eq.196).or.
     +       (kk4.eq.16.and.kk5.eq.197).or.
     +       (kk4.eq.6.and.kk5.eq.1) ) then
            
          call dist1(rfcst,rfanl,rclim,wght,nxy,ib,Nmodel,kk4,kk5)
         else
          call dist(rfcst,rfanl,rclim,wght,nxy,ib,Nmodel)
         end if
          call prob(rfcst,rfanl,rclim,wght,nxy,ib,Nmodel,scrf,
     +    op,thresholds,nthrs)

          do k = 1, Nmodel+1
           fit(ifh,iob,iar,ivr,ilv,k)=dists(k)
          end do
          do k = 1, Nmodel
           fir(ifh,iob,iar,ivr,ilv,k)=
     +       dists(Nmodel+1+k)
          end do
          do k=1,5
           rmse(ifh,iob,iar,ivr,ilv,k)=
     +       dists(2*Nmodel+1+k)
          end do

          do k = 1, Nmodel1
           obsv(ifh,iob,iar,ivr,ilv,k)=probs(k)
           fcst(ifh,iob,iar,ivr,ilv,k)=probs(k+Nmodel1)
           hit(ifh,iob,iar,ivr,ilv,k)=probs(k+2*Nmodel1)
           far(ifh,iob,iar,ivr,ilv,k)=probs(k+3*Nmodel1)
          end do
          do k = 1, 18
           eco(ifh,iob,iar,ivr,ilv,k)=probs(k+17+4*Nmodel1)
          end do
          do k = 1,6
           rps(ifh,iob,iar,ivr,ilv,k)=probs(k+4*Nmodel1)
           bss(ifh,iob,iar,ivr,ilv,k)=probs(k+6+4*Nmodel1)
          end do
          do k = 7,9
           rps(ifh,iob,iar,ivr,ilv,k)=probs(k+29+4*Nmodel1)
          end do
           bss(ifh,iob,iar,ivr,ilv,7)=probs(14+4*Nmodel1)
           bss(ifh,iob,iar,ivr,ilv,8)=probs(16+4*Nmodel1)
           bss(ifh,iob,iar,ivr,ilv,9)=probs(17+4*Nmodel1)

         write(*,'(21F6.0)') (obsv(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(21F6.0)') (fcst(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(21F6.3)') (hit(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(21F6.3)') (far(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(9F10.5)') (rps(ifh,iob,iar,ivr,ilv,k),k=1,9)
         write(*,'(9F10.5)') (bss(ifh,iob,iar,ivr,ilv,k),k=1,9)
         write(*,'(18F6.2)') (eco(ifh,iob,iar,ivr,ilv,k),k=1,18)

         write(*,'(21F6.2)') (fit(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel+1)
         write(*,'(20F6.2)') (fir(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel)
         write(*,'(5F6.2)') (rmse(ifh,iob,iar,ivr,ilv,k),k=1,5)

         deallocate(rfcst)
         deallocate(rfanl)
         deallocate(rclim)
        
       else !End of HREF (Non-continous fields)
 
          write(*,*) 'All domain region points:', ngrid 
          allocate(rclim(ngrid,kb))
          allocate(rfanl(ngrid))
          allocate(rfcst(ngrid,Nmodel))

         IF (INDEX(ensname,'2P5').eq.0) THEN
          rclim = clip                                    !all clip data within domain are used          
         ELSE 
          do i=1,ngrid
           do n=1,101
             clip2deg(n)=clim1(i,n)*fac1+clim2(i,n)*fac2
           end do
           rfa = 100./ib
           rclim(i,1)=clip2deg(1)
           do nb = 2, ib
            rclim(i,nb)=clip2deg((nb-1)*rfa)
           enddo
           rclim(i,ib+1)=clip2deg(101)
          end do
         END IF
 
          !(II) Get weights
          swgt = 0.0
          do i=1,ngrid
           area_factor(i)=cos(datan(1.0d0)*region_latlon(1,i)/45.0)  !use Fanglin Yang's equation 
           swgt=swgt +  area_factor(i)
          end do

          wght(:)=area_factor(:)*ngrid/swgt

          if (igribid.eq.255) wght(:)=1.0

          !(III) get all members data and obsv data
          do iens=1,Nmodel
            rfcst(:,iens)=fcstmdl(iens,ifh,ivr,ilv,:)
          end do

          !(IV) get analysis data
          rfanl(:)=obsvdata(ifh,ivr,ilv,:)
 
          scrf = 1.0/float(ib)
                   
          if (ib.eq.1) then
           tprob=0.0
           cprob=0.0
          
           do i=1,ngrid
             tprob=tprob+wght(i)
             if(rfanl(i).le.rclim(i,2)) then 
               cprob=cprob+wght(i)
             end if
           end do
          
           scrf = cprob/tprob

          end if

          nxy = ngrid

          dists = 0.
          probs = 0.

          write(*,*) 'Entire:',iar,ivr
          do iens=1,Nmodel
           write(*,*) iens, rfcst(5000,iens)
          end do
           write(*,*) 'rfanl',rfanl(5000)
          do ibb=1,ib+1
           write(*,*) ibb, rclim(5000,ibb)
          end do


          call dist(rfcst,rfanl,rclim,wght,nxy,ib,Nmodel)
          call prob(rfcst,rfanl,rclim,wght,nxy,ib,Nmodel,scrf,
     +    op,thresholds,nthrs)      !op,thresholds nthrs are added by B.Zhou on 20140110

          do k = 1, Nmodel+1
           fit(ifh,iob,iar,ivr,ilv,k)=dists(k)
          end do
          do k = 1, Nmodel
           fir(ifh,iob,iar,ivr,ilv,k)=
     +       dists(Nmodel1+k)
          end do
          do k=1,5
           rmse(ifh,iob,iar,ivr,ilv,k)=
     +       dists(2*Nmodel+1+k)
          end do

          do k = 1, Nmodel1
           obsv(ifh,iob,iar,ivr,ilv,k)=probs(k)
           fcst(ifh,iob,iar,ivr,ilv,k)=probs(k+Nmodel1)
           hit(ifh,iob,iar,ivr,ilv,k)=probs(k+2*Nmodel1)
           far(ifh,iob,iar,ivr,ilv,k)=probs(k+3*Nmodel1)

           write(*,*) ifh,iob,iar,ivr,ilv,(probs(j),j=1,Nmodel1)
           write(*,*) (probs(j),j=Nmodel1+1,2*Nmodel1) 
          end do
          do k = 1, 18     
           eco(ifh,iob,iar,ivr,ilv,k)=probs(k+17+4*Nmodel1)
          end do
          do k = 1,6 
           rps(ifh,iob,iar,ivr,ilv,k)=probs(k+4*Nmodel1)
           bss(ifh,iob,iar,ivr,ilv,k)=probs(k+6+4*Nmodel1)
          end do
          do k = 7,9
           rps(ifh,iob,iar,ivr,ilv,k)=probs(k+29+4*Nmodel1)
          end do
           bss(ifh,iob,iar,ivr,ilv,7)=probs(14+4*Nmodel1)
           bss(ifh,iob,iar,ivr,ilv,8)=probs(16+4*Nmodel1)
           bss(ifh,iob,iar,ivr,ilv,9)=probs(17+4*Nmodel1)

         write(*,*)'Entire global: iar, ivr, ilv',iar, ivr, ilv
         write(*,'(21F6.0)') (obsv(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(21F6.0)') (fcst(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(21F6.3)') (hit(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(21F6.3)') (far(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(9F10.5)') (rps(ifh,iob,iar,ivr,ilv,k),k=1,9)
         write(*,'(9F10.5)') (bss(ifh,iob,iar,ivr,ilv,k),k=1,9)
         write(*,'(18F6.2)') (eco(ifh,iob,iar,ivr,ilv,k),k=1,18)

         write(*,'(21F6.2)') (fit(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel+1)
         write(*,'(20F6.2)') (fir(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel)
         write(*,'(5F6.2)') (rmse(ifh,iob,iar,ivr,ilv,k),k=1,5)
     
         deallocate(rfcst)
         deallocate(rfanl)
         deallocate(rclim)
 
       end if    
 
      else if (mode(iar).eq.2) then                  !Sub-region cases
 
         if (numreg(iar).le.30) then                     !Sub-region case 1: 30 small fixed regions

          nxy=0
          do  i = 1,ngrid   !how many points
           if(region_id(i).eq.numreg(iar)) then
             nxy=nxy+1
           end if
          end do  

          write(*,*) 'Sub-region#',numreg(iar),' points:', nxy

          allocate(rclim(nxy,kb))
          allocate(rfanl(nxy))
          allocate(rfcst(nxy,NModel))


           nxy=0
           swgt = 0.0

         do 5002 i = 1,ngrid
          if(region_id(i).eq.numreg(iar)) then

             nxy=nxy+1
            !(I) get climate bin data
 
            IF (INDEX(ensname,'2P5').eq.0) THEN
             rclim(nxy,:) = clip(i,:)              !Only clim data at grids within sub-region are used 
            ELSE
             do n=1,101
              clip2deg(n)=clim1(i,n)*fac1+clim2(i,n)*fac2
             end do
             rfa = 100./ib
             rclim(nxy,1)=clip2deg(1)
             do nb = 2, ib
              rclim(nxy,nb)=clip2deg((nb-1)*rfa)
             enddo
             rclim(nxy,ib+1)=clip2deg(101)
            END IF
            
            !(II) Get weights
             area_factor(nxy)=cos(datan(1.0d0)*region_latlon(1,i)/45.0)  !use Fanglin Yang's equation
             swgt=swgt +  area_factor(nxy)

            !(III) all members data and obsv data
             do iens=1,Nmodel
               rfcst(nxy,iens)=fcstmdl(iens,ifh,ivr,ilv,i)
             end do

             !(IV) get analysis data
              rfanl(nxy)=obsvdata(ifh,ivr,ilv,i)

          end if  !end if region_latlon
5002     continue

          do i = 1, nxy
            wght(i)=area_factor(i)*nxy/swgt
          end do
 
          dists=0.
          probs = 0.
          scrf = 1.0/float(ib)

          if (ib.eq.1) then
           tprob=0.0
           cprob=0.0
           do i=1,nxy
             tprob=tprob+wght(i)
             if(rfanl(i).le.rclim(i,2)) then
                  cprob=cprob+wght(i)
             end if
           end do

           scrf = cprob/tprob

          end if

          write(*,*) 'Call dist'
          call dist(rfcst,rfanl,rclim,wght,nxy,ib,Nmodel)
          write(*,*) 'Call prob'
          call prob(rfcst,rfanl,rclim,wght,nxy,ib,Nmodel,scrf,
     +    op,thresholds,nthrs)

          do k = 1, Nmodel+1
           fit(ifh,iob,iar,ivr,ilv,k)=dists(k)
          end do
          do k = 1, Nmodel
           fir(ifh,iob,iar,ivr,ilv,k)=
     +       dists(Nmodel+1+k)
          end do
          do k=1,5
           rmse(ifh,iob,iar,ivr,ilv,k)=
     +       dists(2*Nmodel+1+k)
          end do

          do k = 1, Nmodel1
           obsv(ifh,iob,iar,ivr,ilv,k)=probs(k)
           fcst(ifh,iob,iar,ivr,ilv,k)=probs(k+Nmodel1)
           hit(ifh,iob,iar,ivr,ilv,k)=probs(k+2*Nmodel1)
           far(ifh,iob,iar,ivr,ilv,k)=probs(k+3*Nmodel1)
          end do
          do k = 1, 18
           eco(ifh,iob,iar,ivr,ilv,k)=probs(k+17+4*Nmodel1)
          end do
          do k = 1,6
           rps(ifh,iob,iar,ivr,ilv,k)=probs(k+4*Nmodel1)
           bss(ifh,iob,iar,ivr,ilv,k)=probs(k+6+4*Nmodel1)
          end do
          do k = 7,9
           rps(ifh,iob,iar,ivr,ilv,k)=probs(k+29+4*Nmodel1)
          end do
           bss(ifh,iob,iar,ivr,ilv,7)=probs(14+4*Nmodel1)
           bss(ifh,iob,iar,ivr,ilv,8)=probs(16+4*Nmodel1)
           bss(ifh,iob,iar,ivr,ilv,9)=probs(17+4*Nmodel1)

         write(*,*)'Sub-region: iar, ivr, ilv',iar, ivr, ilv
         write(*,'(21F6.0)') (obsv(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(21F6.0)') (fcst(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(21F6.3)') (hit(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(21F6.3)') (far(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(9F10.5)') (rps(ifh,iob,iar,ivr,ilv,k),k=1,9)
         write(*,'(9F10.5)') (bss(ifh,iob,iar,ivr,ilv,k),k=1,9)
         write(*,'(18F6.2)') (eco(ifh,iob,iar,ivr,ilv,k),k=1,18)

         write(*,'(21F6.2)') (fit(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel+1)
         write(*,'(20F6.2)') (fir(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel)
         write(*,'(5F6.2)') (rmse(ifh,iob,iar,ivr,ilv,k),k=1,5)

         deallocate(rfcst)
         deallocate(rfanl)
         deallocate(rclim)

         else if (numreg(iar).eq.31) then             !Sub-region case 2: N. hemisphere
           
          nxy=0
          do  i = 1,ngrid   !how many points
           if(region_latlon(1,i).ge.0.0) then
            nxy=nxy+1
           end if
          end do

          allocate(rclim(nxy,kb))
          allocate(rfanl(nxy))
          allocate(rfcst(nxy,Nmodel))

           nxy=0
           fac2 = abs((float(jdd)-15.0)/30.)
           fac1 = 1.0-fac2
           swgt = 0.0
           rclim=0.0

           do 5003 i = 1,ngrid
           if(region_latlon(1,i).ge.0.0) then
             nxy=nxy+1

            !(I) get climate bin data

            IF (INDEX(ensname,'2P5').eq.0) THEN
             rclim(nxy,:) = clip(i,:)                 !Only clim data within N-hemispher are used
            ELSE
             do n=1,101
              clip2deg(n)=clim1(i,n)*fac1+clim2(i,n)*fac2
             end do
             rfa = 100./ib
             rclim(nxy,1)=clip2deg(1)
             do nb = 2, ib
              rclim(nxy,nb)=clip2deg((nb-1)*rfa)
             enddo
             rclim(nxy,ib+1)=clip2deg(101)
            END IF


            !(II) Get weights
             area_factor(nxy)=cos(datan(1.0d0)*region_latlon(1,i)/45.0)  !use Fanglin Yang's equation
             swgt=swgt +  area_factor(nxy)

            !(III) all members data and obsv data
             do iens=1,Nmodel
               rfcst(nxy,iens)=fcstmdl(iens,ifh,ivr,ilv,i)
             end do

            !(IV) get analysis data
             rfanl(nxy)=obsvdata(ifh,ivr,ilv,i)

         end if   !end if region_latlon
5003     continue

          do i = 1, nxy
            wght(i)=area_factor(i)*nxy/swgt
          end do

          dists=0.
          probs = 0.
          scrf = 1.0/float(ib)

          if (ib.eq.1) then
           tprob=0.0
           cprob=0.0
           do i=1,nxy
             tprob=tprob+wght(i)
             if(rfanl(i).le.rclim(i,2)) then
                  cprob=cprob+wght(i)
             end if
           end do

           scrf = cprob/tprob

          end if

          call dist(rfcst,rfanl,rclim,wght,nxy,ib,Nmodel)
          call prob(rfcst,rfanl,rclim,wght,nxy,ib,Nmodel,scrf,
     +    op,thresholds,nthrs)

          do k = 1, Nmodel+1
           fit(ifh,iob,iar,ivr,ilv,k)=dists(k)
          end do
          do k = 1, Nmodel
           fir(ifh,iob,iar,ivr,ilv,k)=
     +       dists(Nmodel+1+k)
          end do
          do k=1,5
           rmse(ifh,iob,iar,ivr,ilv,k)=
     +       dists(2*Nmodel+1+k)
          end do

          do k = 1, Nmodel1
           obsv(ifh,iob,iar,ivr,ilv,k)=probs(k)
           fcst(ifh,iob,iar,ivr,ilv,k)=probs(k+Nmodel1)
           hit(ifh,iob,iar,ivr,ilv,k)=probs(k+2*Nmodel1)
           far(ifh,iob,iar,ivr,ilv,k)=probs(k+3*Nmodel1)
          end do
          do k = 1, 18
           eco(ifh,iob,iar,ivr,ilv,k)=probs(k+17+4*Nmodel1)
          end do
          do k = 1,6
           rps(ifh,iob,iar,ivr,ilv,k)=probs(k+4*Nmodel1)
           bss(ifh,iob,iar,ivr,ilv,k)=probs(k+6+4*Nmodel1)
          end do
          do k = 7,9
           rps(ifh,iob,iar,ivr,ilv,k)=probs(k+29+4*Nmodel1)
          end do
           bss(ifh,iob,iar,ivr,ilv,7)=probs(14+4*Nmodel1)
           bss(ifh,iob,iar,ivr,ilv,8)=probs(16+4*Nmodel1)
           bss(ifh,iob,iar,ivr,ilv,9)=probs(17+4*Nmodel1)

         write(*,*)'NHM: iar, ivr, ilv',iar, ivr, ilv
         write(*,'(21F6.0)') (obsv(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(21F6.0)') (fcst(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(21F6.3)') (hit(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(21F6.3)') (far(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(9F10.5)') (rps(ifh,iob,iar,ivr,ilv,k),k=1,9)
         write(*,'(9F10.5)') (bss(ifh,iob,iar,ivr,ilv,k),k=1,9)
         write(*,'(18F6.2)') (eco(ifh,iob,iar,ivr,ilv,k),k=1,18)

         write(*,'(21F6.2)') (fit(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel+1)
         write(*,'(20F6.2)') (fir(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel)
         write(*,'(5F6.2)') (rmse(ifh,iob,iar,ivr,ilv,k),k=1,5)

         deallocate(rfcst)
         deallocate(rfanl)
         deallocate(rclim)

         else if (numreg(iar).eq.32) then                    !Sub-region case 3: S. hemisphere

          nxy=0
          do  i = 1,ngrid   !how many points
           if(region_latlon(1,i).le.0.0) then
            nxy=nxy+1
           end if
          end do

          allocate(rclim(nxy,kb))
          allocate(rfanl(nxy))
          allocate(rfcst(nxy,Nmodel))

           nxy=0
           swgt = 0.0
           rclim = 0.0

           do 5004 i = 1,ngrid
           if(region_latlon(1,i).le.0.0) then
             nxy=nxy+1
            !(I) get climate bin data

            IF (INDEX(ensname,'2P5').eq.0) THEN 
              rclim(nxy,:) = clip(i,:)                 !Only clim data within S-Hemishp are used
            ELSE
             do n=1,101
              clip2deg(n)=clim1(i,n)*fac1+clim2(i,n)*fac2
             end do
             rfa = 100./ib
             rclim(nxy,1)=clip2deg(1)
             do nb = 2, ib
              rclim(nxy,nb)=clip2deg((nb-1)*rfa)
             enddo
             rclim(nxy,ib+1)=clip2deg(101)
            END IF

            !(II) Get weights
             area_factor(nxy)=cos(datan(1.0d0)*region_latlon(1,i)/45.0)  !use Fanglin Yang's equation
             swgt=swgt +  area_factor(nxy)

            !(III) all members data and obsv data
             do iens=1,Nmodel
               rfcst(nxy,iens)=fcstmdl(iens,ifh,ivr,ilv,i)
             end do

             !(IV) get analysis data
             rfanl(nxy)=obsvdata(ifh,ivr,ilv,i)

         end if   !end if region_latlon
5004     continue

          do i = 1, nxy
            wght(i)=area_factor(i)*nxy/swgt
          end do

         dists = 0.
          probs = 0.
          scrf = 1.0/float(ib)

          if (ib.eq.1) then
           tprob=0.0
           cprob=0.0
           do i=1,nxy
             tprob=tprob+wght(i)
             if(rfanl(i).le.rclim(i,2)) then
                  cprob=cprob+wght(i)
             end if
           end do

           scrf = cprob/tprob

          end if

          call dist(rfcst,rfanl,rclim,wght,nxy,ib,Nmodel)
          call prob(rfcst,rfanl,rclim,wght,nxy,ib,Nmodel,scrf,
     +    op,thresholds,nthrs)

          do k = 1, Nmodel+1
           fit(ifh,iob,iar,ivr,ilv,k)=dists(k)
          end do
          do k = 1, Nmodel
           fir(ifh,iob,iar,ivr,ilv,k)=
     +       dists(Nmodel+1+k)
          end do
          do k=1,5
           rmse(ifh,iob,iar,ivr,ilv,k)=
     +       dists(2*Nmodel+1+k)
          end do

          do k = 1, Nmodel1
           obsv(ifh,iob,iar,ivr,ilv,k)=probs(k)
           fcst(ifh,iob,iar,ivr,ilv,k)=probs(k+Nmodel1)
           hit(ifh,iob,iar,ivr,ilv,k)=probs(k+2*Nmodel1)
           far(ifh,iob,iar,ivr,ilv,k)=probs(k+3*Nmodel1)
          end do
          do k = 1, 18
           eco(ifh,iob,iar,ivr,ilv,k)=probs(k+17+4*Nmodel1)
          end do
          do k = 1,6
           rps(ifh,iob,iar,ivr,ilv,k)=probs(k+4*Nmodel1)
           bss(ifh,iob,iar,ivr,ilv,k)=probs(k+6+4*Nmodel1)
          end do
          do k = 7,9
           rps(ifh,iob,iar,ivr,ilv,k)=probs(k+29+4*Nmodel1)
          end do
           bss(ifh,iob,iar,ivr,ilv,7)=probs(14+4*Nmodel1)
           bss(ifh,iob,iar,ivr,ilv,8)=probs(16+4*Nmodel1)
           bss(ifh,iob,iar,ivr,ilv,9)=probs(17+4*Nmodel1)

         write(*,*)'SHM: iar, ivr, ilv',iar, ivr, ilv
         write(*,'(21F6.0)') (obsv(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(21F6.0)') (fcst(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(21F6.3)') (hit(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(21F6.3)') (far(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(9F10.5)') (rps(ifh,iob,iar,ivr,ilv,k),k=1,9)
         write(*,'(9F10.5)') (bss(ifh,iob,iar,ivr,ilv,k),k=1,9)
         write(*,'(18F6.2)') (eco(ifh,iob,iar,ivr,ilv,k),k=1,18)

         write(*,'(21F6.2)') (fit(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel+1)
         write(*,'(20F6.2)') (fir(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel)
         write(*,'(5F6.2)') (rmse(ifh,iob,iar,ivr,ilv,k),k=1,5)

         deallocate(rfcst)
         deallocate(rfanl)
         deallocate(rclim)


        else if(numreg(iar).eq.33) then                     !Sub-region case 4: user defined case

         write(*,*) 'In EFS.f, User definde 4 points'
         write(*,*) ptr1(1,iar),ptr1(2,iar),
     +              ptr2(1,iar),ptr2(2,iar)

         if (ptr1(1,iar).ne.ptr2(1,iar).and.           
     +         ptr1(2,iar).ne.ptr2(2,iar)) then        

         write(*,*) 'For ',trim(namarea(iar))

          nxy=0
           do  i = 1,ngrid    !  find how many points within user-defined regtangular region
             im=mod(i,ix)
             if (im.eq.0) im=144

             if (im.ne.144) then
              jm=int(i/ix) + 1
             else
               jm=int(i/ix)
             end if
 
             !ptr1(2),ptr2(2) are lat of ptr1 and ptr2
             !ptr1(1),ptr2(1) are lon of ptr1 and ptr2
             !region_latlon(1,n) is lat of ptr
             !region_latlon(2,n) is lon of ptr

            if(region_latlon(2,i).ge.ptr1(1,iar).and.    !      ----------------x  (ptr2(1), ptr2(2))
     +        region_latlon(2,i).le.ptr2(1,iar).and.     !     |                |
     +        region_latlon(1,i).ge.ptr1(2,iar).and.     !     |    ptr *       |
     +        region_latlon(1,i).le.ptr2(2,iar)) then    !     |                |
                                                         !     x----------------
                                                         !  (ptr1(1), ptr1(2))


              nxy=nxy+1

!            write(*,'(3i8, a5,3f8.2,a5,3f8.2)') i,im,jm,
!     +      " Lon:",ptr1(1,iar),region_latlon(2,i),ptr2(1,iar),
!     +      " Lat:",ptr1(2,iar),region_latlon(1,i),ptr2(2,iar)

             end if
           end do
       
          write(*,*) 'In EFS, user-defined region nxy=',nxy  

          allocate(rclim(nxy,kb))
          allocate(rfanl(nxy))
          allocate(rfcst(nxy,Nmodel))


           nxy=0
           swgt = 0.
           rclim = 0.

          do 5007 i = 1,ngrid
           !Search points within user-defined region    
            if(region_latlon(2,i).ge.ptr1(1,iar).and.    
     +        region_latlon(2,i).le.ptr2(1,iar).and.     
     +        region_latlon(1,i).ge.ptr1(2,iar).and.     
     +        region_latlon(1,i).le.ptr2(2,iar)) then
   
            if (INDEX(ensname,'2P5').ne.0) then 
               im=mod(i,ix)
               if (im.eq.0) im=144
               if (im.ne.144) then
                jm=int(i/ix) + 1
               else
                jm=int(i/ix)
               end if
            end if
            
             nxy=nxy+1

             !(I) get climate bin data

            IF (INDEX(ensname,'2P5').eq.0) THEN
             rclim(nxy,:) = clip(i,:)    !new clim data used 
            ELSE                         !2.5x2.5 deg, use old clim data  
             do n=1,101
              clip2deg(n)=clim1(i,n)*fac1+clim2(i,n)*fac2
             end do
             rfa = 100./ib
             rclim(nxy,1)=clip2deg(1)
             do nb = 2, ib
              rclim(nxy,nb)=clip2deg((nb-1)*rfa)
             enddo
             rclim(nxy,ib+1)=clip2deg(101)
            END IF

             !(II) Get weights
             if (INDEX(ensname,'2P5').eq.0 ) then
              area_factor(nxy)=cos(datan(1.0d0)*region_latlon(1,i)/45.0)  !use Fanglin Yang's equation  
             else
              aloc(im,jm)=90.0-float(jm-1)*2.5                            !for 2.5x2.5 deg, use Yuejian's method
              area_factor(nxy)=cos(aloc(im,jm)*3.1415926/180.)
             end if

            swgt=swgt +  area_factor(nxy)

             !(III) all members data and obsv data
             do iens=1,Nmodel
               rfcst(nxy,iens)=fcstmdl(iens,ifh,ivr,ilv,i)
             end do

             !(IV) get analysis data
             rfanl(nxy)=obsvdata(ifh,ivr,ilv,i)
             
            end if !end if region_latlon
5007    continue                          
            
          do i = 1, nxy
            wght(i)=area_factor(i)*nxy/swgt
          end do
 
          dists = 0.
          probs = 0.
          scrf = 1.0/float(ib)

          if (ib.eq.1) then
           tprob=0.0
           cprob=0.0
           do i=1,nxy
             tprob=tprob+wght(i)
             if(rfanl(i).le.rclim(i,2)) then
                  cprob=cprob+wght(i)
             end if
           end do

           scrf = cprob/tprob

          end if

          call dist(rfcst,rfanl,rclim,wght,nxy,ib,Nmodel)
          call prob(rfcst,rfanl,rclim,wght,nxy,ib,Nmodel,scrf,
     +    op,thresholds,nthrs)
 
          do k = 1, Nmodel+1
           fit(ifh,iob,iar,ivr,ilv,k)=dists(k)
          end do
          do k = 1, Nmodel
           fir(ifh,iob,iar,ivr,ilv,k)=
     +       dists(Nmodel+1+k)
          end do
          do k=1,5
           rmse(ifh,iob,iar,ivr,ilv,k)=
     +       dists(2*Nmodel+1+k)
          end do

          do k = 1, Nmodel1
           obsv(ifh,iob,iar,ivr,ilv,k)=probs(k)
           fcst(ifh,iob,iar,ivr,ilv,k)=probs(k+Nmodel1)
           hit(ifh,iob,iar,ivr,ilv,k)=probs(k+2*Nmodel1)
           far(ifh,iob,iar,ivr,ilv,k)=probs(k+3*Nmodel1)
          end do
          do k = 1, 18
           eco(ifh,iob,iar,ivr,ilv,k)=probs(k+17+4*Nmodel1)
          end do
          do k = 1,6
           rps(ifh,iob,iar,ivr,ilv,k)=probs(k+4*Nmodel1)
           bss(ifh,iob,iar,ivr,ilv,k)=probs(k+6+4*Nmodel1)
          end do
          do k = 7,9
           rps(ifh,iob,iar,ivr,ilv,k)=probs(k+29+4*Nmodel1)
          end do
           bss(ifh,iob,iar,ivr,ilv,7)=probs(14+4*Nmodel1)
           bss(ifh,iob,iar,ivr,ilv,8)=probs(16+4*Nmodel1)
           bss(ifh,iob,iar,ivr,ilv,9)=probs(17+4*Nmodel1)

         write(*,*)'User-defined: iar, ivr, ilv',iar, ivr, ilv
         write(*,'(21F6.0)') (obsv(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(21F6.0)') (fcst(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(21F6.3)') (hit(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(21F6.3)') (far(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
         write(*,'(9F10.5)') (rps(ifh,iob,iar,ivr,ilv,k),k=1,9)
         write(*,'(9F10.5)') (bss(ifh,iob,iar,ivr,ilv,k),k=1,9)
         write(*,'(18F6.2)') (eco(ifh,iob,iar,ivr,ilv,k),k=1,18)

         write(*,'(21F6.2)') (fit(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel+1)
         write(*,'(20F6.2)') (fir(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel)
         write(*,'(5F6.2)') (rmse(ifh,iob,iar,ivr,ilv,k),k=1,5)

         deallocate(rfcst)
         deallocate(rfanl)
         deallocate(rclim)



          end if   !end if ptr1 
         end if    !end if numreg(iar)
       end if      !end if mode


50       continue
60       continue
70       continue
80       continue
90       continue

          
          do 200 ids = 1, 8             !Hist, Rel.P, rmase, respectively
           do 190 iob = 1, numvfyobs
            do 180 iar = 1, numarea
             do 170 ifh = 1, numfcst
              do 160 ivr = 1, numvarbl
                 do 150 ilv = 1, levels(ivr)
                   if(nodata(ifh,ivr,ilv).eq.1) goto 150
                 
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
                      iend = iend + nchr_ensname
                      vdbhdr132(istrt:iend) =
     +                            ensname(:nchr_ensname)


                     iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrfcst(ifh)
                      vdbhdr132(istrt:iend) =
     +                            namfcst(ifh)(:nchrfcst(ifh))

                     iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + nchrvfdate(ifh)
                      vdbhdr132(istrt:iend) =
     +                            namvfdate(ifh)(:nchrvfdate(ifh))

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
                      iend = iend + 4
                      vdbhdr132(istrt:iend) = score_name(ids)(:4)

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
                if(ids.eq.1) then
                   WRITE (nvsdb,1001) vdbhdr132(:iend),
     +               (fit(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel+1)     
                else if (ids.eq.2) then
                   WRITE (nvsdb,1002) vdbhdr132(:iend),
     +               (fir(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel)     
                else if (ids.eq.3) then
                   WRITE (nvsdb,1003) vdbhdr132(:iend),
     +               (rmse(ifh,iob,iar,ivr,ilv,k),k=1,5) 
                else if (ids.eq.4) then
                   WRITE (nvsdb,1004) vdbhdr132(:iend),
     +               (obsv(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1),
     +               (fcst(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
                else if (ids.eq.5) then
                   WRITE (nvsdb,1005) vdbhdr132(:iend),
     +               (hit(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1),
     +               (far(ifh,iob,iar,ivr,ilv,k),k=1,Nmodel1)
                else if (ids.eq.6) then
                   WRITE (nvsdb,1006) vdbhdr132(:iend),
     +               (rps(ifh,iob,iar,ivr,ilv,k),k=1,9)              
                else if (ids.eq.7) then
                   WRITE (nvsdb,1006) vdbhdr132(:iend),
     +               (bss(ifh,iob,iar,ivr,ilv,k),k=1,9)
                else if (ids.eq.8) then
                   WRITE (nvsdb,1008) vdbhdr132(:iend),
     +               (eco(ifh,iob,iar,ivr,ilv,k),k=1,18)
                end if

1001    FORMAT (A,<Nmodel+1>(1x,F6.2))
1002    FORMAT (A,<Nmodel>(1x,F6.2))
1003    FORMAT (A,5F12.3)
1004    FORMAT (A,<2*Nmodel1>(1x,F12.0))
1005    FORMAT (A,<2*Nmodel1>(1x,F6.3))
1006    FORMAT (A,9F12.5)
1007    FORMAT (A,9F12.5)
1008    FORMAT (A,18F7.2)

150             continue
160           continue
170          continue
180         continue
190        continue
200       continue

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cc
cc  Following is for output in Yuejian format
cc 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
          goto 777

          write(*,*) "Write in Yuejian format"
          
          do 701 ivr = 1, numvarbl
           do 702 ilv = 1, levels(ivr)
             file_yuejian=trim(namlvl(ivr,ilv))//
     +                    trim(namvarbl(ivr))//
     +         '.'//trim(namvfdate(1))

              write(*,*) 'file_yuejian=',trim(file_yuejian)
               open(705, file=file_yuejian,status='unknown')


              do 703 iar = 1, numarea
                do 704 ifh = 1, numfcst

              write(705,*) "###", ensname(:nchr_ensname),
     +         " PROB for Region: ", namarea(iar)(:nchrarea(iar))
              write(705,*) "VAR:", trim(namlvl(ivr,ilv)),
     +            trim(namvarbl(ivr)), " AT VALID TIME ",
     +            trim(namvfdate(ifh)), " (ic: ",
     +  cyyyyfcst(ifh)//cmmfcst(ifh)//cddfcst(ifh)//chhfcst(ifh),"  ", 
     +    trim(namfcst(ifh)), "hrs)" 

              write(705,*) "RELIABILITY DIAGRAM:"
              write(705,7004)(obsv(ifh,1,iar,ivr,ilv,k),k=1,Nmodel1),
     +                       (fcst(ifh,1,iar,ivr,ilv,k),k=1,Nmodel1)

               write(705,*) "HIT RATE and FALSE ALARM RATE:"
               write(705,7005)(hit(ifh,1,iar,ivr,ilv,k),k=1,Nmodel1),
     +                        (far(ifh,1,iar,ivr,ilv,k),k=1,Nmodel1)

               write(705,*) "RPS:"
               write(705,7006)(rps(ifh,1,iar,ivr,ilv,k),k=1,9)

               write(705,*) "BSS:"
               write(705,7006)(bss(ifh,1,iar,ivr,ilv,k),k=1,9)

               write(705,*) "ECONOMIC VALUES 18 COST/LOSS RATIOS:" 
               write(705,7008)(eco(ifh,1,iar,ivr,ilv,k),k=1,18)

               write(705,*) "SPREAD AND RMSE"
               write(705,7003)(rmse(ifh,1,iar,ivr,ilv,k),k=1,5)

               write(705,*) "HISTOGRAM AND RELATIVE POSITION:"
               write(705,7001)(fit(ifh,1,iar,ivr,ilv,k),k=1,Nmodel+1)
               write(705,7002)(fir(ifh,1,iar,ivr,ilv,k),k=1,Nmodel)
               write(705,*)"   "
                    
7001    FORMAT (<Nmodel+1>(1x,F6.2))
7002    FORMAT (<Nmodel>(1x,F6.2))
7003    FORMAT (5F12.5)
7004    FORMAT (<2*Nmodel1>(1x,F8.0))
7005    FORMAT (<2*Nmodel1>(1x,F10.5))
7006    FORMAT (9F12.5)
7007    FORMAT (9F12.5)
7008    FORMAT (18F7.2)

704             continue
703           continue

              close (705)

702       continue
701       continue            

777       continue

           deallocate(fit) 
           deallocate(fir) 
           deallocate(rmse)
 
         return
          end


