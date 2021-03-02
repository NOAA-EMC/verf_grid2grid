      subroutine EVL1L2(nvsdb,Nmodel,ensname,ist,numfcst,
     +  numvfyobs,numarea, numvarbl,numlevel,ngrid,levels,
     +  plevel,hasdata,ufcstmdl,vfcstmdl,uobsv,vobsv)

c    This program is to generate VL1L2 vsdb records for all fcst times,
c    all vector variables, all levels, all requested sub-regions, then write them
c    into the vsdb file
c
c    Author: Binbin Zhou
c            March, 2005
                                                                                                                 


      INCLUDE 'parm.inc'
      
      INTEGER, intent(IN) :: Nmodel, ist, numvfyobs,numarea,
     +         numfcst,numvarbl,numlevel,ngrid, levels(mxvrbl),
     +         hasdata(mxfcst,mxvrbl,maxlvl)

      REAL,dimension(Nmodel,numfcst,numvarbl,numlevel,ngrid),
     + intent(IN)                           ::ufcstmdl,vfcstmdl

      REAL,dimension(numfcst,numvarbl,numlevel,ngrid),intent(IN) 
     +                                      ::uobsv,vobsv

      CHARACTER*20, intent(IN) ::  ensname           !such as WRF/6, ETA/10, RSM/5, SREF/21

 
       real*8, allocatable, dimension(:,:,:,:,:) :: uf,vf,uo, vo,
     +       ufuo_vfvo,u2v2f,u2v2o,usumensvar, usumfrcle3, 
     +       vsumensvar, vsumfrcle3,count,weigh
     
      DIMENSION nchrmodel(maxmod), nchrfcst(mxfcst), nchrvfdate(mxdate),
     +            nchrvfyobs(maxobs), nchrarea(mxarea),
     +            nchrstat(mxstat), nchrvarbl(mxvrbl),
     +            nchrlevel(maxlvl)
      CHARACTER*24 namodel(maxmod), namfcst(mxfcst), namvfdate(mxdate),
     +            namvfyobs(maxobs), namarea(mxarea), namstat(mxstat),
     +            namvarbl(mxvrbl), namlevel(maxlvl)
                                                                                                                                                         
      COMMON /names/namodel, namfcst, namvfdate, namvfyobs, namarea,
     +            namstat, namvarbl, namlevel
      COMMON /nchrs/nchrmodel, nchrfcst, nchrvfdate, nchrvfyobs,
     +            nchrarea, nchrstat, nchrvarbl, nchrlevel
      CHARACTER*3 regions (100)
      COMMON /grdef/ig104(147,110),numreg(mxarea),mode(mxarea),
     +  imax(mxarea),imin(mxarea),jmax(mxarea),jmin(mxarea),
     +  alat1(mxarea),elon1(mxarea), dxx(mxarea), dyy(mxarea),
     +  elonv(mxarea), alatan(mxarea), latlong(mxarea),
     +  lambert(mxarea), polarstereo(mxarea),regions


      integer nodata(mxfcst,mxvrbl,maxlvl)
      COMMON /nofile/nodata

      CHARACTER*3 namversion
      CHARACTER*132 vdbhdr132, input, substr (3)
                                                                                                                                                         
      CHARACTER*1 blank, equal
C

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
                                                                                                                            
      CHARACTER*24 fho(mxvrbl),    fhothr(mxvrbl,20)
      CHARACTER*24 afho(mxvrbl),  afhothr(mxvrbl,20)
      integer  nchrfho(mxvrbl),nchrfhothr(mxvrbl,20),fhomrk(mxvrbl)
      integer  nchrafho(mxvrbl),nchrafhothr(mxvrbl,20),afhomrk(mxvrbl)
      real rfhothr(mxvrbl,20)
      real rafhothr(mxvrbl,20)
      integer  continue_mrk(mxvrbl)
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
     +             continue_mrk,anomly_mrk, anomlylev,
     +      afhomrk,afho,nchrafho,afhothr,nchrafhothr,rafhothr
      COMMON /FRC/
     +       sfhomrk,sfho,nchrsfho,sfhothr,nchrsfhothr,rsfhothr,
     +       ffhomrk,ffho,nchrffho,ffhothr,nchrffhothr,rffhothr

      integer plevel(maxlvl)
      integer region_id(maxpts)
      real region_latlon(2,maxpts), ptr1(2,mxarea),ptr2(2,mxarea)
 
      COMMON /reg/region_id, region_latlon, ptr1,ptr2
      COMMON /layer/ modelvl(maxlvl), iplevel(maxlvl,2)

      real area_factor(maxpts)
      COMMON /weight/area_factor

      CHARACTER*4 CNmodel
      integer d1,d2,d3,numCNmodel


      DATA blank /' '/
      DATA equal /'='/
      DATA namversion /'V01'/
      DATA bmiss /10E10/
      DATA rmiss /99999./

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



      write(*,*) 'In EVL1L2:',Nmodel,ist,numfcst,numvfyobs,numarea,
     +  numvarbl,numlevel,ngrid
      write(*,*) 'vectormrk', (vectormrk(ivr),ivr=1,numvarbl)
      write(*,*) 'fhomrk', (fhomrk(ivr),ivr=1,numvarbl)
      write(*,*) 'afhomrk', (afhomrk(ivr),ivr=1,numvarbl)
      write(*,*) 'rafhothr=', (rafhothr(6,i),i=1,5)     
      write(*,*) 'nchrafho=', (nchrafho(ivr),ivr=1,numvarbl)  
      write(*,*) 'anomly_mrk=',(anomly_mrk(ivr),ivr=1,numvarbl)   

       allocate(uf(numfcst,numvarbl,numlevel,numarea,numvfyobs))
       allocate(vf(numfcst,numvarbl,numlevel,numarea,numvfyobs))
       allocate(uo(numfcst,numvarbl,numlevel,numarea,numvfyobs))
       allocate(vo(numfcst,numvarbl,numlevel,numarea,numvfyobs))
       allocate(ufuo_vfvo(numfcst,numvarbl,numlevel,numarea,numvfyobs))
       allocate(u2v2f(numfcst,numvarbl,numlevel,numarea,numvfyobs))
       allocate(u2v2o(numfcst,numvarbl,numlevel,numarea,numvfyobs))
       allocate(count(numfcst,numvarbl,numlevel,numarea,numvfyobs))
       allocate(weigh(numfcst,numvarbl,numlevel,numarea,numvfyobs))
       allocate (usumensvar(numfcst,numvarbl,numlevel,numarea,
     +            numvfyobs))
       allocate (usumfrcle3(numfcst,numvarbl,numlevel,numarea,
     +            numvfyobs))
       allocate (vsumensvar(numfcst,numvarbl,numlevel,numarea,
     +            numvfyobs))
       allocate (vsumfrcle3(numfcst,numvarbl,numlevel,numarea,
     +            numvfyobs))


c      do nfcst=1,numfcst
c       do nvar=1,numvarbl
c        write(*,*) 'nfcst,nvar=',nfcst
c        if(vectormrk(nvar).eq.1) then
c        do np =1,levels(nvar)
c         write(*,'(i3,10f10.2)')np, (ufcst(nfcst,nvar,np,i),i=1,10) 
c         write(*,'(i3,10f10.2)')np, (vfcst(nfcst,nvar,np,i),i=1,10) 
c        end do
c        end if
c       end do
c      end do


          count = 0.0
          weigh = 0.0
          uf = 0
          vf = 0
          uo = 0
          vo = 0
          ufuo_vfvo = 0
          u2v2f = 0
          u2v2o = 0
          usumensvar=0.0
          vsumensvar=0.0
          usunfrcle3=0.0
          vsunfrcle3=0.0


      do 90 ifh = 1, numfcst
        do 80 iob = 1, numvfyobs
          do 70 iar = 1, numarea
           do 60 ivr = 1, numvarbl
             if(vectormrk(ivr).eq.0) goto 60                     !scaler in SL1L2
             if(fhomrk(ivr).ne.0)    goto 60                     !fho in FHO
             if(afhomrk(ivr).ne.0)   goto 60                     !afho in AFHO
             do 50 ilv = 1, levels(ivr)
              if(nodata(ifh,ivr,ilv).eq.1) goto 50
                if (mode(iar).eq.1) then                          !all GRID domain (mode 1)
                  do i = 1,ngrid

CCCCCCCCCCC      Prepare Ensemble mean/spread CCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                  
              uforcst = 0.
              vforcst = 0.
              do imodel=1,Nmodel                                 !mean of fcst
                uforcst = uforcst + ufcstmdl(imodel,ifh,ivr,ilv,i)
     +                             *area_factor(i)
                vforcst = vforcst + vfcstmdl(imodel,ifh,ivr,ilv,i)
     +                             *area_factor(i)
              end do

              uforcst = uforcst / float(Nmodel)
              vforcst = vforcst / float(Nmodel)

              uobsval=uobsv(ifh,ivr,ilv,i)*area_factor(i)
              vobsval=vobsv(ifh,ivr,ilv,i)*area_factor(i)
  

              ufcstva = 0.
              ufcstva = 0.
              ufcstdiffsq=0.0
              vfcstdiffsq=0.0
              do imodel=1,Nmodel
               ufcstdiffsq=ufcstdiffsq+(ufcstmdl(imodel,ifh,ivr,ilv,i)
     +             *area_factor(i)  - uforcst)**2.0
               vfcstdiffsq=vfcstdiffsq+(vfcstmdl(imodel,ifh,ivr,ilv,i)
     +             *area_factor(i)  - vforcst)**2.0
              end do
              ufcstva=(ufcstdiffsq/float(Nmodel-1))**0.5 
              vfcstva=(vfcstdiffsq/float(Nmodel-1))**0.5 

              ufrcle3 = 0.
              vfrcle3 = 0.
              do imodel=1,Nmodel
                ufcstdiff=abs(ufcstmdl(imodel,ifh,ivr,ilv,i)
     +               *area_factor(i) - uforcst)
                if(ufcstdiff.ge. (3.0*ufcstva)) ufrcle3=ufrcle3+1.0
                vfcstdiff=abs(vfcstmdl(imodel,ifh,ivr,ilv,i)
     +               *area_factor(i)  -vforcst)
                if(vfcstdiff.ge. (3.0*vfcstva)) vfrcle3=vfrcle3+1.0
              end do
              ufrcle3 = ufrcle3/float(Nmodel)
              vfrcle3 = vfrcle3/float(Nmodel)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC              


                       uf(ifh,ivr,ilv,iar,iob) = 
     +                  uf(ifh,ivr,ilv,iar,iob) + uforcst
    
                       vf(ifh,ivr,ilv,iar,iob) =
     +                  vf(ifh,ivr,ilv,iar,iob) + vforcst

                       uo(ifh,ivr,ilv,iar,iob) =
     +                  uo(ifh,ivr,ilv,iar,iob) + uobsval
                                                                                                                                                           
                       vo(ifh,ivr,ilv,iar,iob) =
     +                  vo(ifh,ivr,ilv,iar,iob) + vobsval

                       ufuo_vfvo(ifh,ivr,ilv,iar,iob) =
     +                  ufuo_vfvo(ifh,ivr,ilv,iar,iob) 
     +                  + (uforcst*uobsval+vforcst*vobsval)

                       u2v2f(ifh,ivr,ilv,iar,iob) =
     +                  u2v2f(ifh,ivr,ilv,iar,iob) 
     +                  + (uforcst*uforcst+vforcst*vforcst)

                       u2v2o(ifh,ivr,ilv,iar,iob) =
     +                  u2v2o(ifh,ivr,ilv,iar,iob)
     +                  + (uobsval*uobsval+vobsval*vobsval)                                        

                       usumensvar(ifh,ivr,ilv,iar,iob) =         !variance
     +                        usumensvar(ifh,ivr,ilv,iar,iob)
     +                        + ufcstva**2
                       
                                                                                                                                                       
                       vsumensvar(ifh,ivr,ilv,iar,iob) =         !variance
     +                        vsumensvar(ifh,ivr,ilv,iar,iob)
     +                        + vfcstva**2

                                                                                                                           
                       usumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        usumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + ufrcle3

                      
                       vsumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        vsumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + vfrcle3
 

                       count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

                  end do
                else if (mode(iar).eq.2) then                    ! GRID#104  (mode 2)
                 if (numreg(iar).le.30) then                    !         sub-region

                   do i = 1,ngrid
                    if(region_id(i).eq.numreg(iar)) then

CCCCCCCCCCC      Prepare Ensemble mean/spread CCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                       
              uforcst = 0.
              vforcst = 0.
              do imodel=1,Nmodel                                 !mean of fcst
                uforcst = uforcst + ufcstmdl(imodel,ifh,ivr,ilv,i)
     +                             *area_factor(i)
                vforcst = vforcst + vfcstmdl(imodel,ifh,ivr,ilv,i)
     +                             *area_factor(i)
              end do
      
              uforcst = uforcst / float(Nmodel)
              vforcst = vforcst / float(Nmodel)
      
              uobsval=uobsv(ifh,ivr,ilv,i)*area_factor(i)
              vobsval=vobsv(ifh,ivr,ilv,i)*area_factor(i)
      
      
              ufcstva = 0.
              ufcstva = 0.
              ufcstdiffsq=0.0
              vfcstdiffsq=0.0
              do imodel=1,Nmodel
               ufcstdiffsq=ufcstdiffsq+(ufcstmdl(imodel,ifh,ivr,ilv,i)
     +             *area_factor(i)  - uforcst)**2.0
               vfcstdiffsq=vfcstdiffsq+(vfcstmdl(imodel,ifh,ivr,ilv,i)
     +             *area_factor(i)  - vforcst)**2.0
              end do
              ufcstva=(ufcstdiffsq/float(Nmodel-1))**0.5
              vfcstva=(vfcstdiffsq/float(Nmodel-1))**0.5
      
              ufrcle3 = 0.
              vfrcle3 = 0.
              do imodel=1,Nmodel
                ufcstdiff=abs(ufcstmdl(imodel,ifh,ivr,ilv,i)
     +               *area_factor(i) - uforcst)
                if(ufcstdiff.ge. (3.0*ufcstva)) ufrcle3=ufrcle3+1.0
                vfcstdiff=abs(vfcstmdl(imodel,ifh,ivr,ilv,i)
     +               *area_factor(i)  -vforcst)
                if(vfcstdiff.ge. (3.0*vfcstva)) vfrcle3=vfrcle3+1.0
              end do
              ufrcle3 = ufrcle3/float(Nmodel)
              vfrcle3 = vfrcle3/float(Nmodel)
      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                       
                       uf(ifh,ivr,ilv,iar,iob) =
     +                  uf(ifh,ivr,ilv,iar,iob) + uforcst
                                                                                                                                                       
                       vf(ifh,ivr,ilv,iar,iob) =
     +                  vf(ifh,ivr,ilv,iar,iob) + vforcst
                                                                                                                                                       
                       uo(ifh,ivr,ilv,iar,iob) =
     +                  uo(ifh,ivr,ilv,iar,iob) + uobsval
                                                                                                                                                       
                                                                                                                                                       
                       vo(ifh,ivr,ilv,iar,iob) =
     +                  vo(ifh,ivr,ilv,iar,iob) + vobsval
                                                                                                                                                       
                       ufuo_vfvo(ifh,ivr,ilv,iar,iob) =
     +                  ufuo_vfvo(ifh,ivr,ilv,iar,iob)
     +                  + (uforcst*uobsval+vforcst*vobsval)
                                                                                                                                                       
                       u2v2f(ifh,ivr,ilv,iar,iob) =
     +                  u2v2f(ifh,ivr,ilv,iar,iob)
     +                  + (uforcst*uforcst+vforcst*vforcst)
                                                                                                                                                       
                       u2v2o(ifh,ivr,ilv,iar,iob) =
     +                  u2v2o(ifh,ivr,ilv,iar,iob)
     +                  + (uobsval*uobsval+vobsval*vobsval)
                                                                                                                                                       
                       usumensvar(ifh,ivr,ilv,iar,iob) =         !variance
     +                        usumensvar(ifh,ivr,ilv,iar,iob)
     +                        + ufcstva**2
                                                                                                                                                       
                                                                                                                                                       
                       vsumensvar(ifh,ivr,ilv,iar,iob) =         !variance
     +                        vsumensvar(ifh,ivr,ilv,iar,iob)
     +                        + vfcstva**2
                                                                                                                                                       
                                                                                                                                                       
                       usumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        usumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + ufrcle3
                                                                                                                                                       
                                                                                                                                                       
                       vsumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        vsumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + vfrcle3
                                                                                                                                                       
                                                                                                                                                       
                       count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)


                    end if
                   end do

                  else if(numreg(iar).eq.31 ) then             !        N. hemisphere
 
                     do i = 1,ngrid
                      if(region_latlon(1,i).gt.0.0) then
CCCCCCCCCCC      Prepare Ensemble mean/spread CCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                       
              uforcst = 0.
              vforcst = 0.
              do imodel=1,Nmodel                                 !mean of fcst
                uforcst = uforcst + ufcstmdl(imodel,ifh,ivr,ilv,i)
     +                             *area_factor(i)
                vforcst = vforcst + vfcstmdl(imodel,ifh,ivr,ilv,i)
     +                             *area_factor(i)
              end do
      
              uforcst = uforcst / float(Nmodel)
              vforcst = vforcst / float(Nmodel)
      
              uobsval=uobsv(ifh,ivr,ilv,i)*area_factor(i)
              vobsval=vobsv(ifh,ivr,ilv,i)*area_factor(i)
      
      
              ufcstva = 0.
              ufcstva = 0.
              ufcstdiffsq=0.0
              vfcstdiffsq=0.0
              do imodel=1,Nmodel
               ufcstdiffsq=ufcstdiffsq+(ufcstmdl(imodel,ifh,ivr,ilv,i)
     +             *area_factor(i)  - uforcst)**2.0
               vfcstdiffsq=vfcstdiffsq+(vfcstmdl(imodel,ifh,ivr,ilv,i)
     +             *area_factor(i)  - vforcst)**2.0
              end do
              ufcstva=(ufcstdiffsq/float(Nmodel-1))**0.5
              vfcstva=(vfcstdiffsq/float(Nmodel-1))**0.5
      
              ufrcle3 = 0.
              vfrcle3 = 0.
              do imodel=1,Nmodel
                ufcstdiff=abs(ufcstmdl(imodel,ifh,ivr,ilv,i)
     +               *area_factor(i) - uforcst)
                if(ufcstdiff.ge. (3.0*ufcstva)) ufrcle3=ufrcle3+1.0
                vfcstdiff=abs(vfcstmdl(imodel,ifh,ivr,ilv,i)
     +               *area_factor(i)  -vforcst)
                if(vfcstdiff.ge. (3.0*vfcstva)) vfrcle3=vfrcle3+1.0
              end do
              ufrcle3 = ufrcle3/float(Nmodel)
              vfrcle3 = vfrcle3/float(Nmodel)
      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                       
                       uf(ifh,ivr,ilv,iar,iob) =
     +                  uf(ifh,ivr,ilv,iar,iob) + uforcst
                                                                                                                                                       
                       vf(ifh,ivr,ilv,iar,iob) =
     +                  vf(ifh,ivr,ilv,iar,iob) + vforcst
                                                                                                                                                       
                       uo(ifh,ivr,ilv,iar,iob) =
     +                  uo(ifh,ivr,ilv,iar,iob) + uobsval
                                                                                                                                                       
                                                                                                                                                       
                       vo(ifh,ivr,ilv,iar,iob) =
     +                  vo(ifh,ivr,ilv,iar,iob) + vobsval
                                                                                                                                                       
                       ufuo_vfvo(ifh,ivr,ilv,iar,iob) =
     +                  ufuo_vfvo(ifh,ivr,ilv,iar,iob)
     +                  + (uforcst*uobsval+vforcst*vobsval)
                                                                                                                                                       
                       u2v2f(ifh,ivr,ilv,iar,iob) =
     +                  u2v2f(ifh,ivr,ilv,iar,iob)
     +                  + (uforcst*uforcst+vforcst*vforcst)
                                                                                                                                                       
                       u2v2o(ifh,ivr,ilv,iar,iob) =
     +                  u2v2o(ifh,ivr,ilv,iar,iob)
     +                  + (uobsval*uobsval+vobsval*vobsval)
                                                                                                                                                       
                       usumensvar(ifh,ivr,ilv,iar,iob) =         !variance
     +                        usumensvar(ifh,ivr,ilv,iar,iob)
     +                        + ufcstva**2
                                                                                                                                                       
                                                                                                                                                       
                       vsumensvar(ifh,ivr,ilv,iar,iob) =         !variance
     +                        vsumensvar(ifh,ivr,ilv,iar,iob)
     +                        + vfcstva**2
                                                                                                                                                       
                                                                                                                                                       
                       usumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        usumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + ufrcle3
                                                                                                                                                       
                                                                                                                                                       
                       vsumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        vsumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + vfrcle3
                                                                                                                                                       
                                                                                                                                                       
                       count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

                      end if
                     end do

                   else if(numreg(iar).eq.32 ) then             !        S. hemisphere
                     do i = 1,ngrid
                      if(region_latlon(1,i).gt.0.0) then

CCCCCCCCCCC      Prepare Ensemble mean/spread CCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                       
              uforcst = 0.
              vforcst = 0.
              do imodel=1,Nmodel                                 !mean of fcst
                uforcst = uforcst + ufcstmdl(imodel,ifh,ivr,ilv,i)
     +                             *area_factor(i)
                vforcst = vforcst + vfcstmdl(imodel,ifh,ivr,ilv,i)
     +                             *area_factor(i)
              end do
      
              uforcst = uforcst / float(Nmodel)
              vforcst = vforcst / float(Nmodel)
      
              uobsval=uobsv(ifh,ivr,ilv,i)*area_factor(i)
              vobsval=vobsv(ifh,ivr,ilv,i)*area_factor(i)
      
      
              ufcstva = 0.
              ufcstva = 0.
              ufcstdiffsq=0.0
              vfcstdiffsq=0.0
              do imodel=1,Nmodel
               ufcstdiffsq=ufcstdiffsq+(ufcstmdl(imodel,ifh,ivr,ilv,i)
     +             *area_factor(i)  - uforcst)**2.0
               vfcstdiffsq=vfcstdiffsq+(vfcstmdl(imodel,ifh,ivr,ilv,i)
     +             *area_factor(i)  - vforcst)**2.0
              end do
              ufcstva=(ufcstdiffsq/float(Nmodel-1))**0.5
              vfcstva=(vfcstdiffsq/float(Nmodel-1))**0.5
      
              ufrcle3 = 0.
              vfrcle3 = 0.
              do imodel=1,Nmodel
                ufcstdiff=abs(ufcstmdl(imodel,ifh,ivr,ilv,i)
     +               *area_factor(i) - uforcst)
                if(ufcstdiff.ge. (3.0*ufcstva)) ufrcle3=ufrcle3+1.0
                vfcstdiff=abs(vfcstmdl(imodel,ifh,ivr,ilv,i)
     +               *area_factor(i)  -vforcst)
                if(vfcstdiff.ge. (3.0*vfcstva)) vfrcle3=vfrcle3+1.0
              end do
              ufrcle3 = ufrcle3/float(Nmodel)
              vfrcle3 = vfrcle3/float(Nmodel)
      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                       
                       uf(ifh,ivr,ilv,iar,iob) =
     +                  uf(ifh,ivr,ilv,iar,iob) + uforcst
                                                                                                                                                       
                       vf(ifh,ivr,ilv,iar,iob) =
     +                  vf(ifh,ivr,ilv,iar,iob) + vforcst
                                                                                                                                                       
                       uo(ifh,ivr,ilv,iar,iob) =
     +                  uo(ifh,ivr,ilv,iar,iob) + uobsval
                                                                                                                                                       
                                                                                                                                                       
                       vo(ifh,ivr,ilv,iar,iob) =
     +                  vo(ifh,ivr,ilv,iar,iob) + vobsval
                                                                                                                                                       
                       ufuo_vfvo(ifh,ivr,ilv,iar,iob) =
     +                  ufuo_vfvo(ifh,ivr,ilv,iar,iob)
     +                  + (uforcst*uobsval+vforcst*vobsval)
                                                                                                                                                       
                       u2v2f(ifh,ivr,ilv,iar,iob) =
     +                  u2v2f(ifh,ivr,ilv,iar,iob)
     +                  + (uforcst*uforcst+vforcst*vforcst)
                                                                                                                                                       
                       u2v2o(ifh,ivr,ilv,iar,iob) =
     +                  u2v2o(ifh,ivr,ilv,iar,iob)
     +                  + (uobsval*uobsval+vobsval*vobsval)
                                                                                                                                                       
                       usumensvar(ifh,ivr,ilv,iar,iob) =         !variance
     +                        usumensvar(ifh,ivr,ilv,iar,iob)
     +                        + ufcstva**2
                                                                                                                                                       
                                                                                                                                                       
                       vsumensvar(ifh,ivr,ilv,iar,iob) =         !variance
     +                        vsumensvar(ifh,ivr,ilv,iar,iob)
     +                        + vfcstva**2
                                                                                                                                                       
                                                                                                                                                       
                       usumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        usumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + ufrcle3
                                                                                                                                                       
                                                                                                                                                       
                       vsumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        vsumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + vfrcle3
                                                                                                                                                       
                                                                                                                                                       
                       count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

                      end if
                     end do

                  else if(numreg(iar).eq.33) then             ! User defined region
                   if (ptr1(1,iar).eq.ptr2(1,iar).and.         !user defined case 1, all points along latitude
     +                 ptr1(2,iar).ne.ptr2(2,iar)) then               
                    do i = 1,ngrid                                    !  x(ptr2(1), ptr2(2))
                     if(region_latlon(1,i).ge.ptr1(2,iar).and.        !  |
     +                     region_latlon(1,i).le.ptr2(2,iar)) then    !  |
                                                                      !  x(ptr1(1), ptr1(2))

CCCCCCCCCCC      Prepare Ensemble mean/spread CCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                       
              uforcst = 0.
              vforcst = 0.
              do imodel=1,Nmodel                                 !mean of fcst
                uforcst = uforcst + ufcstmdl(imodel,ifh,ivr,ilv,i)
     +                             *area_factor(i)
                vforcst = vforcst + vfcstmdl(imodel,ifh,ivr,ilv,i)
     +                             *area_factor(i)
              end do
      
              uforcst = uforcst / float(Nmodel)
              vforcst = vforcst / float(Nmodel)
      
              uobsval=uobsv(ifh,ivr,ilv,i)*area_factor(i)
              vobsval=vobsv(ifh,ivr,ilv,i)*area_factor(i)
      
      
              ufcstva = 0.
              ufcstva = 0.
              ufcstdiffsq=0.0
              vfcstdiffsq=0.0
              do imodel=1,Nmodel
               ufcstdiffsq=ufcstdiffsq+(ufcstmdl(imodel,ifh,ivr,ilv,i)
     +             *area_factor(i)  - uforcst)**2.0
               vfcstdiffsq=vfcstdiffsq+(vfcstmdl(imodel,ifh,ivr,ilv,i)
     +             *area_factor(i)  - vforcst)**2.0
              end do
              ufcstva=(ufcstdiffsq/float(Nmodel-1))**0.5
              vfcstva=(vfcstdiffsq/float(Nmodel-1))**0.5
      
              ufrcle3 = 0.
              vfrcle3 = 0.
              do imodel=1,Nmodel
                ufcstdiff=abs(ufcstmdl(imodel,ifh,ivr,ilv,i)
     +               *area_factor(i) - uforcst)
                if(ufcstdiff.ge. (3.0*ufcstva)) ufrcle3=ufrcle3+1.0
                vfcstdiff=abs(vfcstmdl(imodel,ifh,ivr,ilv,i)
     +               *area_factor(i)  -vforcst)
                if(vfcstdiff.ge. (3.0*vfcstva)) vfrcle3=vfrcle3+1.0
              end do
              ufrcle3 = ufrcle3/float(Nmodel)
              vfrcle3 = vfrcle3/float(Nmodel)
      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                       
                       uf(ifh,ivr,ilv,iar,iob) =
     +                  uf(ifh,ivr,ilv,iar,iob) + uforcst
                                                                                                                                                       
                       vf(ifh,ivr,ilv,iar,iob) =
     +                  vf(ifh,ivr,ilv,iar,iob) + vforcst
                                                                                                                                                       
                       uo(ifh,ivr,ilv,iar,iob) =
     +                  uo(ifh,ivr,ilv,iar,iob) + uobsval
                                                                                                                                                       
                                                                                                                                                       
                       vo(ifh,ivr,ilv,iar,iob) =
     +                  vo(ifh,ivr,ilv,iar,iob) + vobsval
                                                                                                                                                       
                       ufuo_vfvo(ifh,ivr,ilv,iar,iob) =
     +                  ufuo_vfvo(ifh,ivr,ilv,iar,iob)
     +                  + (uforcst*uobsval+vforcst*vobsval)
                                                                                                                                                       
                       u2v2f(ifh,ivr,ilv,iar,iob) =
     +                  u2v2f(ifh,ivr,ilv,iar,iob)
     +                  + (uforcst*uforcst+vforcst*vforcst)
                                                                                                                                                       
                       u2v2o(ifh,ivr,ilv,iar,iob) =
     +                  u2v2o(ifh,ivr,ilv,iar,iob)
     +                  + (uobsval*uobsval+vobsval*vobsval)
                                                                                                                                                       
                       usumensvar(ifh,ivr,ilv,iar,iob) =         !variance
     +                        usumensvar(ifh,ivr,ilv,iar,iob)
     +                        + ufcstva**2
                                                                                                                                                       
                                                                                                                                                       
                       vsumensvar(ifh,ivr,ilv,iar,iob) =         !variance
     +                        vsumensvar(ifh,ivr,ilv,iar,iob)
     +                        + vfcstva**2
                                                                                                                                                       
                                                                                                                                                       
                       usumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        usumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + ufrcle3
                                                                                                                                                       
                                                                                                                                                       
                       vsumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        vsumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + vfrcle3
                                                                                                                                                       
                                                                                                                                                       
                       count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

                      end if
                     end do
                   else if (ptr1(1,iar).ne.ptr2(1,iar).and.    !user defined case 2, all points along longitude
     +                     ptr1(2,iar).eq.ptr2(2,iar)) then
                    do i = 1,ngrid                                    !
                     if(region_latlon(2,i).ge.ptr1(1,iar).and.        !       x -----------------x
     +                     region_latlon(2,i).le.ptr2(1,iar)) then 

CCCCCCCCCCC      Prepare Ensemble mean/spread CCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                       
              uforcst = 0.
              vforcst = 0.
              do imodel=1,Nmodel                                 !mean of fcst
                uforcst = uforcst + ufcstmdl(imodel,ifh,ivr,ilv,i)
     +                             *area_factor(i)
                vforcst = vforcst + vfcstmdl(imodel,ifh,ivr,ilv,i)
     +                             *area_factor(i)
              end do
      
              uforcst = uforcst / float(Nmodel)
              vforcst = vforcst / float(Nmodel)
      
              uobsval=uobsv(ifh,ivr,ilv,i)*area_factor(i)
              vobsval=vobsv(ifh,ivr,ilv,i)*area_factor(i)
      
      
              ufcstva = 0.
              ufcstva = 0.
              ufcstdiffsq=0.0
              vfcstdiffsq=0.0
              do imodel=1,Nmodel
               ufcstdiffsq=ufcstdiffsq+(ufcstmdl(imodel,ifh,ivr,ilv,i)
     +             *area_factor(i)  - uforcst)**2.0
               vfcstdiffsq=vfcstdiffsq+(vfcstmdl(imodel,ifh,ivr,ilv,i)
     +             *area_factor(i)  - vforcst)**2.0
              end do
              ufcstva=(ufcstdiffsq/float(Nmodel-1))**0.5
              vfcstva=(vfcstdiffsq/float(Nmodel-1))**0.5
      
              ufrcle3 = 0.
              vfrcle3 = 0.
              do imodel=1,Nmodel
                ufcstdiff=abs(ufcstmdl(imodel,ifh,ivr,ilv,i)
     +               *area_factor(i) - uforcst)
                if(ufcstdiff.ge. (3.0*ufcstva)) ufrcle3=ufrcle3+1.0
                vfcstdiff=abs(vfcstmdl(imodel,ifh,ivr,ilv,i)
     +               *area_factor(i)  -vforcst)
                if(vfcstdiff.ge. (3.0*vfcstva)) vfrcle3=vfrcle3+1.0
              end do
              ufrcle3 = ufrcle3/float(Nmodel)
              vfrcle3 = vfrcle3/float(Nmodel)
      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                       
                       uf(ifh,ivr,ilv,iar,iob) =
     +                  uf(ifh,ivr,ilv,iar,iob) + uforcst
                                                                                                                                                       
                       vf(ifh,ivr,ilv,iar,iob) =
     +                  vf(ifh,ivr,ilv,iar,iob) + vforcst
                                                                                                                                                       
                       uo(ifh,ivr,ilv,iar,iob) =
     +                  uo(ifh,ivr,ilv,iar,iob) + uobsval
                                                                                                                                                       
                                                                                                                                                       
                       vo(ifh,ivr,ilv,iar,iob) =
     +                  vo(ifh,ivr,ilv,iar,iob) + vobsval
                                                                                                                                                       
                       ufuo_vfvo(ifh,ivr,ilv,iar,iob) =
     +                  ufuo_vfvo(ifh,ivr,ilv,iar,iob)
     +                  + (uforcst*uobsval+vforcst*vobsval)
                                                                                                                                                       
                       u2v2f(ifh,ivr,ilv,iar,iob) =
     +                  u2v2f(ifh,ivr,ilv,iar,iob)
     +                  + (uforcst*uforcst+vforcst*vforcst)
                                                                                                                                                       
                       u2v2o(ifh,ivr,ilv,iar,iob) =
     +                  u2v2o(ifh,ivr,ilv,iar,iob)
     +                  + (uobsval*uobsval+vobsval*vobsval)
                                                                                                                                                       
                       usumensvar(ifh,ivr,ilv,iar,iob) =         !variance
     +                        usumensvar(ifh,ivr,ilv,iar,iob)
     +                        + ufcstva**2
                                                                                                                                                       
                                                                                                                                                       
                       vsumensvar(ifh,ivr,ilv,iar,iob) =         !variance
     +                        vsumensvar(ifh,ivr,ilv,iar,iob)
     +                        + vfcstva**2
                                                                                                                                                       
                                                                                                                                                       
                       usumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        usumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + ufrcle3
                                                                                                                                                       
                                                                                                                                                       
                       vsumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        vsumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + vfrcle3
                                                                                                                                                       
                                                                                                                                                       
                       count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

                      end if
                     end do
                   else if(ptr1(1,iar).ne.ptr2(1,iar).and.    !user defined case 3, all points in a regtangular
     +                     ptr1(2,iar).ne.ptr2(2,iar)) then      !
                    do i = 1,ngrid                               !                     (ptr2(1), ptr2(2))
                     if(region_latlon(2,i).ge.ptr1(1,iar).and.   !              ----------------x
     +                  region_latlon(2,i).le.ptr2(1,iar).and.   !             |                |
     +                  region_latlon(1,i).ge.ptr1(2,iar).and.   !             |                |
     +                  region_latlon(1,i).le.ptr2(2,iar)) then  !             |                |
                                                                 !             x----------------
                                                                 !     (ptr1(1), ptr1(2))

CCCCCCCCCCC      Prepare Ensemble mean/spread CCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                       
              uforcst = 0.
              vforcst = 0.
              do imodel=1,Nmodel                                 !mean of fcst
                uforcst = uforcst + ufcstmdl(imodel,ifh,ivr,ilv,i)
     +                             *area_factor(i)
                vforcst = vforcst + vfcstmdl(imodel,ifh,ivr,ilv,i)
     +                             *area_factor(i)
              end do
      
              uforcst = uforcst / float(Nmodel)
              vforcst = vforcst / float(Nmodel)
      
              uobsval=uobsv(ifh,ivr,ilv,i)*area_factor(i)
              vobsval=vobsv(ifh,ivr,ilv,i)*area_factor(i)
      
      
              ufcstva = 0.
              ufcstva = 0.
              ufcstdiffsq=0.0
              vfcstdiffsq=0.0
              do imodel=1,Nmodel
               ufcstdiffsq=ufcstdiffsq+(ufcstmdl(imodel,ifh,ivr,ilv,i)
     +             *area_factor(i)  - uforcst)**2.0
               vfcstdiffsq=vfcstdiffsq+(vfcstmdl(imodel,ifh,ivr,ilv,i)
     +             *area_factor(i)  - vforcst)**2.0
              end do
              ufcstva=(ufcstdiffsq/float(Nmodel-1))**0.5
              vfcstva=(vfcstdiffsq/float(Nmodel-1))**0.5
      
              ufrcle3 = 0.
              vfrcle3 = 0.
              do imodel=1,Nmodel
                ufcstdiff=abs(ufcstmdl(imodel,ifh,ivr,ilv,i)
     +               *area_factor(i) - uforcst)
                if(ufcstdiff.ge. (3.0*ufcstva)) ufrcle3=ufrcle3+1.0
                vfcstdiff=abs(vfcstmdl(imodel,ifh,ivr,ilv,i)
     +               *area_factor(i)  -vforcst)
                if(vfcstdiff.ge. (3.0*vfcstva)) vfrcle3=vfrcle3+1.0
              end do
              ufrcle3 = ufrcle3/float(Nmodel)
              vfrcle3 = vfrcle3/float(Nmodel)
      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                                                                                                       
                       uf(ifh,ivr,ilv,iar,iob) =
     +                  uf(ifh,ivr,ilv,iar,iob) + uforcst
                                                                                                                                                       
                       vf(ifh,ivr,ilv,iar,iob) =
     +                  vf(ifh,ivr,ilv,iar,iob) + vforcst
                                                                                                                                                       
                       uo(ifh,ivr,ilv,iar,iob) =
     +                  uo(ifh,ivr,ilv,iar,iob) + uobsval
                                                                                                                                                       
                                                                                                                                                       
                       vo(ifh,ivr,ilv,iar,iob) =
     +                  vo(ifh,ivr,ilv,iar,iob) + vobsval
                                                                                                                                                       
                       ufuo_vfvo(ifh,ivr,ilv,iar,iob) =
     +                  ufuo_vfvo(ifh,ivr,ilv,iar,iob)
     +                  + (uforcst*uobsval+vforcst*vobsval)
                                                                                                                                                       
                       u2v2f(ifh,ivr,ilv,iar,iob) =
     +                  u2v2f(ifh,ivr,ilv,iar,iob)
     +                  + (uforcst*uforcst+vforcst*vforcst)
                                                                                                                                                       
                       u2v2o(ifh,ivr,ilv,iar,iob) =
     +                  u2v2o(ifh,ivr,ilv,iar,iob)
     +                  + (uobsval*uobsval+vobsval*vobsval)
                                                                                                                                                       
                       usumensvar(ifh,ivr,ilv,iar,iob) =         !variance
     +                        usumensvar(ifh,ivr,ilv,iar,iob)
     +                        + ufcstva**2
                                                                                                                                                       
                                                                                                                                                       
                       vsumensvar(ifh,ivr,ilv,iar,iob) =         !variance
     +                        vsumensvar(ifh,ivr,ilv,iar,iob)
     +                        + vfcstva**2
                                                                                                                                                       
                                                                                                                                                       
                       usumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        usumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + ufrcle3
                                                                                                                                                       
                                                                                                                                                       
                       vsumfrcle3(ifh,ivr,ilv,iar,iob) =
     +                        vsumfrcle3(ifh,ivr,ilv,iar,iob)
     +                        + vfrcle3
                                                                                                                                                       
                                                                                                                                                       
                       count(ifh,ivr,ilv,iar,iob) =
     +                        count(ifh,ivr,ilv,iar,iob)
     +                        + 1.0
                       weigh(ifh,ivr,ilv,iar,iob) =
     +                 weigh(ifh,ivr,ilv,iar,iob) + area_factor(i)

                      end if
                     end do
                    end if

                   end if   !end of mode=2 (# 104sub-region)
 
                  end if    !end of region mode
 
50               continue
60             continue
70            continue
80           continue
90         continue

      do 91 ifh = 1, numfcst
       do 81 iob = 1, numvfyobs
        do 71 iar = 1, numarea
         do 61 ivr = 1, numvarbl
          if(vectormrk(ivr).eq.0) goto 61
          if(fhomrk(ivr).ne.0)    goto 61
          if(afhomrk(ivr).ne.0)   goto 61
          do 51 ilv = 1, levels(ivr)
           if(nodata(ifh,ivr,ilv).eq.1) goto 51
           if(weigh(ifh,ivr,ilv,iar,iob).ne.0.0) then

           uf(ifh,ivr,ilv,iar,iob) =
     +     uf(ifh,ivr,ilv,iar,iob)/weigh(ifh,ivr,ilv,iar,iob)
                                                                                                                                                           
           vf(ifh,ivr,ilv,iar,iob) =
     +     vf(ifh,ivr,ilv,iar,iob)/weigh(ifh,ivr,ilv,iar,iob)
                                                                                                                                                           
           uo(ifh,ivr,ilv,iar,iob) =
     +     uo(ifh,ivr,ilv,iar,iob)/weigh(ifh,ivr,ilv,iar,iob)
                                                                                                                                                           
           vo(ifh,ivr,ilv,iar,iob) =
     +     vo(ifh,ivr,ilv,iar,iob)/weigh(ifh,ivr,ilv,iar,iob)
                                                                                                                                                           
           ufuo_vfvo(ifh,ivr,ilv,iar,iob) =
     +     ufuo_vfvo(ifh,ivr,ilv,iar,iob)/
     +             weigh(ifh,ivr,ilv,iar,iob)
                                                                                                                                                           
           u2v2f(ifh,ivr,ilv,iar,iob) =
     +     u2v2f(ifh,ivr,ilv,iar,iob)/
     +            weigh(ifh,ivr,ilv,iar,iob)
                                                                                                                                                           
           u2v2o(ifh,ivr,ilv,iar,iob) =
     +     u2v2o(ifh,ivr,ilv,iar,iob)/
     +            weigh(ifh,ivr,ilv,iar,iob)

           usumensvar(ifh,ivr,ilv,iar,iob)=
     +     usumensvar(ifh,ivr,ilv,iar,iob)/weigh(ifh,ivr,ilv,iar,iob)
                                                                                                                                                  
           usumfrcle3(ifh,ivr,ilv,iar,iob)=
     +     usumfrcle3(ifh,ivr,ilv,iar,iob)/weigh(ifh,ivr,ilv,iar,iob)

           vsumensvar(ifh,ivr,ilv,iar,iob)=
     +     vsumensvar(ifh,ivr,ilv,iar,iob)/weigh(ifh,ivr,ilv,iar,iob)
                                                                                                                                                  
           vsumfrcle3(ifh,ivr,ilv,iar,iob)=
     +     vsumfrcle3(ifh,ivr,ilv,iar,iob)/weigh(ifh,ivr,ilv,iar,iob)

           end if

           if(uf(ifh,ivr,ilv,iar,iob).lt. -99.0 .or.
     +        vf(ifh,ivr,ilv,iar,iob).lt. -99.0 .or.
     +        uo(ifh,ivr,ilv,iar,iob).lt. -99.0 .or.
     +        vo(ifh,ivr,ilv,iar,iob).lt. -99.0) then
              count(ifh,ivr,ilv,iar,iob) = 0.0
           end if

51         continue
61        continue
71       continue
81      continue
91     continue

           do 190 iob = 1, numvfyobs
            do 180 iar = 1, numarea
             do 170 ifh = 1, numfcst
                ivfdate = ifh
              do 160 ivr = 1, numvarbl
               if(vectormrk(ivr).eq.0) goto 160
               if(fhomrk(ivr).ne.0)    goto 160
               if(afhomrk(ivr).ne.0)    goto 160
               do 150 ilv = 1, levels(ivr)
                if(nodata(ifh,ivr,ilv).eq.1) goto 150
                    IF (count(ifh,ivr,ilv,iar,iob).gt.0.0) THEN

                      iend = 3
                      vdbhdr132(1:iend) = namversion(:3)

                      iend = iend + 1
                      vdbhdr132(iend:iend) = blank
                      istrt = iend + 1
                      iend = iend + 10
                      vdbhdr132(istrt:iend) = ensname

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
                      ivstrt = istrt
                      iend = iend + nchrstat(ist)
                      vdbhdr132(istrt:iend) =
     +                            namstat(ist)(:nchrstat(ist))
 
                      istrt = iend + 1
                      iend = iend + numCNmodel
                      vdbhdr132(istrt:iend) =
     +                           CNmodel(:numCNmodel)

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
     +                              count(ifh,ivr,ilv,iar,iob),
     +                              uf(ifh,ivr,ilv,iar,iob),
     +                              vf(ifh,ivr,ilv,iar,iob),
     +                              uo(ifh,ivr,ilv,iar,iob),
     +                              vo(ifh,ivr,ilv,iar,iob),
     +                              ufuo_vfvo(ifh,ivr,ilv,iar,iob),
     +                              u2v2f(ifh,ivr,ilv,iar,iob),
     +                              u2v2o(ifh,ivr,ilv,iar,iob),
     +                              usumensvar(ifh,ivr,ilv,iar,iob),     !vairance
     +                              vsumensvar(ifh,ivr,ilv,iar,iob),     !vairance
     +                              usumfrcle3(ifh,ivr,ilv,iar,iob),     !fraction
     +                              vsumfrcle3(ifh,ivr,ilv,iar,iob)      !fraction
 1000                   FORMAT (A,F7.0,11(1x,E13.6))
                       end if
                    END IF
150            continue
160           continue
170          continue
180         continue
190        continue

           deallocate(uf)
           deallocate(vf)
           deallocate(uo)
           deallocate(vo)
           deallocate(ufuo_vfvo)
           deallocate(u2v2f)
           deallocate(u2v2o)
           deallocate(count)
           deallocate(weigh)
           deallocate(usumensvar)
           deallocate(usumfrcle3)
           deallocate(vsumensvar)
           deallocate(vsumfrcle3)

           return
           end
