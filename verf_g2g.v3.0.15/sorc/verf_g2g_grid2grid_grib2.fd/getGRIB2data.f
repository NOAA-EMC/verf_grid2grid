c 
c   This program is to read data from GRIB2 file
c   Author: Binbin Zhou
c           June 2014 , NOAA/NCEP/EMC
c


      subroutine getGRIB2data (grbunit,grib2file,data,u,v,
     +         levels,numfcst,numvarbl,numlevel,ngrid,
     +         yy,mm,dd,hh,ff,k4,k5,k6,k7,plevel,namvarbl,
     +         Nmodel,model,obstype,fho,thr)

      use grib_mod
      include 'parm.inc'

      integer numfcst,numvarbl,numlevel,ngrid   

      real var(ngrid),
     + data(numfcst,numvarbl,numlevel,ngrid),
     + u(numfcst,numvarbl,numlevel,ngrid),
     + v(numfcst,numvarbl,numlevel,ngrid)

      integer k5(mxvrbl),k6(mxvrbl),k7(mxvrbl),k4(mxvrbl)
      integer plevel(maxlvl)
      integer levels(mxvrbl)          !levels for different variables
      integer yy(mxfcst), mm(mxfcst), dd(mxfcst), hh(mxfcst), 
     +        ff(mxfcst),yyyy 
      CHARACTER*24 namvarbl(mxvrbl),model,obstype 
                                                                                                                                                    
      integer yyfcst(mxfcst),yyobsv(maxobs)


      integer grbunit

      integer nodata(mxfcst,mxvrbl,maxlvl)
      COMMON /nofile/nodata

      integer igribid
      COMMON /grb/igribid


      type(gribfield) :: gfld
      integer jpdtn,jpd1,jpd2,jpd10,jpd12
      CHARACTER*80 grib2file

      CHARACTER*24 fho(mxvrbl)
      real thr(mxvrbl,20)

      write(*,*)'In getGRIB2data:',grbunit,grib2file,ngrid

      call baopenr(grbunit, grib2file, ierr)
        write(*,*) 'open ', grib2file, ' ierr=',ierr

      do 500 nfcst = 1, numfcst

         write(*,*) 'Forecast time = ',  nfcst

       do 600 nvar = 1, numvarbl

         write(*,*) 'Variable#', nvar

         write(*,'(a16,5i4)') 'yy,mm,dd,hh,ff=',yy(nfcst),
     +    mm(nfcst),dd(nfcst),hh(nfcst),ff(nfcst) 

        !jpdtn=-1            !Templete# wildcard has problem? repeat  read one field
        jpd1=k4(nvar)      !Category #
        jpd2=k5(nvar)      !Product # under this category
        jpd10=k6(nvar)     !Product vertical ID 
        jpd9=ff(nfcst)     !Forecast time or accumulation beginning time

        if (Nmodel.eq.1) then 
          jpdtn=0            !Product Templete# non-ensemble

          if(trim(grib2file).eq.'fcst.grib.GEFS') jpdtn=2  !GEFS icingmean case

          if(jpd1.eq.19.and.jpd2.eq.20.and.
     +       trim(grib2file).eq.'fcst.grib.GFS') jpdtn=1   !GEFS's control icing 

        else if (Nmodel.gt.1.and.grib2file(1:4).eq.'obsv') then
         if (model(1:7).eq.'gefs2p5'.or.model(1:7).eq.'cmce2p5'.or.
     +       model(1:8).eq.'naefs2p5'.or.model(1:7).eq.'ecme2p5') then
               jpdtn=1
         else
               jpdtn=0
         end if
        else if (Nmodel.gt.1.and.grib2file(1:4).eq.'fcst') then !Ensemble case
          if(model(1:4).eq.'sref') then
           !jpdtn=0
           jpdtn=1     !1n 2015 26 member SREF, jpdtn is corrected from 0 to 1
          else if ( model(1:4).eq.'gefs' .and.         !total cloud for gefs
     +              k4(nvar).eq.6.and.k5(nvar).eq.1  ) then
           jpdtn=11
           jpd9=ff(nfcst)-6    !Total cloud in gefs jpd9 is 6hr ahead of fcst time  
          else if ( model(1:4).eq.'cmce' .and.       !total cloud for Canaian ensemble cmce   
     +              k4(nvar).eq.6.and.k5(nvar).eq.1  ) then
           jpdtn=1
           jpd10=200
          else if ( grib2file(1:14).eq.'fcst.grib.href') then
           jpdtn=0 
          else
           jpdtn=1
          end if
        else
          write(*,*) "Check Product Template# in this grib2 file!"
          STOP 333
        end if


c        jpd1=k4(nvar)      !Category #
c        jpd2=k5(nvar)      !Product # under this category
c        jpd10=k6(nvar)     !Product vertical ID 
c        jpd9=ff(nfcst)     !Forecast time or accumulation beginning time
                           
        if(jpd1.eq.16.and.jpd2.eq.196.and.(
     +    trim(grib2file).eq.'fcst.grib.HRRR'.or.
     +    trim(grib2file).eq.'fcst.grib.RAP')) jpd10=10        !for HRRR/RAP composite reflectivity

        if(jpd1.eq.16.and.jpd2.eq.197.and.              !HRRR etop use non-standard product id
     +     trim(grib2file).eq.'fcst.grib.HRRR') then
          jpd1=16
          jpd2=3
          jpd10=3
        end if 

        if(jpd1.eq.6.and.jpd2.eq.1.and.              !HRRR total cloud  use non-standard product id
     +     grib2file(1:14).eq.'fcst.grib.HRRR') then
          jpd10=10
        end if

        if(jpd1.eq.13.and.jpd2.eq.195) then           
                                                 
           if(grib2file(1:14).eq.'obsv.grib.HYSP') then !HYSPLIT smoke/dust obsv  use different ID 
            jpd1=3
            jpd2=10
           end if

           if(grib2file(1:14).eq.'fcst.grib.HYSP') then !HYSPLIT smoke/dust fcst uses Template 4.8
            jpd9= ff(nfcst) -1               !For Template 4.8, substract accumulation time(1hr) from forecast hour        
            jpdtn=8
           end if

        end if

         if(jpd1.eq.1.and.jpd2.eq.8) then  ! APCP use Template 4.8, only verify 3hr accumulation
            jpdtn=8
           if(grib2file(1:4).eq.'obsv') then
            jpd9= 0         !Strange, Yin Lin's CCPA jpd9 is set to 0
            hh(nfcst)=hh(nfcst)-3 
           else
            jpd9= ff(nfcst) -3             
           end if
         end if


        if(jpd1.eq.6.and.jpd2.eq.1) then
          if (grib2file(1:13).eq.'fcst.grib.GFS') then
            jpd10=10                 !GFS total cloud jpd10 changed from 200 to 10 in i2015 new GFS
            jpdtn=8
            jpd9=ff(nfcst)-6         !GFS total cloud is 6 hour accumulated using Template 4.8. 
                                     !jpd9 is accumuation beginning time(= orecast time - interval 6)
           else if (grib2file(1:13).eq.'fcst.grib.HRR'.or. !HRRR/RAP total cloud  use non-standard product id
     +           grib2file(1:13).eq.'fcst.grib.RAP') then
            jpd10=10
           else if (grib2file(1:18).eq.'fcst.grib.GEFSMEAN') then !GEFS mean cloud 
            jpd10=10
            jpdtn=12
            jpd9=ff(nfcst)-6
           end if
         end if

        if ( grib2file(1:14).eq.'fcst.grib.NARR'.or.
     +       grib2file(1:14).eq.'fcst.grib.SREF'.or.
     +       grib2file(1:14).eq.'fcst.grib.HREF' ) then   ! NARREMEAN/SREFMEAN/HREFMEAN  uses Template 4.2
           jpdtn=2
        end if

        jp =  jpd10                                   !Binbin: these 2 lines are used to
        if(jpd10.eq.100.or.jpd10.eq.104) jp=100   !deal with both jpds=100 and jpds=104(sigma level)

        if (jp.eq.100) then
          levels(nvar) = numlevel
        else
          levels(nvar) = 1
        end if

        yyyy=yy(nfcst)+2000
 
        if(jpd1.eq.2.and.jpd2.eq.1) then         !Wind 

          do np = 1, levels(nvar)

            if (jp.eq.100) then
              jpd12 = plevel(np)
            else
              jpd12 = k7(nvar)
            end if

            write(*,*) 'Var for ', jpd1,jpd2,jpd10,jpd12

            if(fho(nvar)(1:4).eq.'FHOP'.and.
     +                            grib2file(1:4).eq.'fcst' ) then
               call readProbGB2(grbunit,jpdtn,2,2,jpd10,jpd12,0,
     +            fho(nvar),thr(nvar,1),yyyy,mm(nfcst),dd(nfcst),
     +            hh(nfcst),jpd9,ngrid,gfld,iret)
            else
               call readGB2(grbunit,jpdtn,2,2,jpd10,jpd12,yyyy,
     +           mm(nfcst),dd(nfcst),hh(nfcst),jpd9,ngrid,gfld,iret)
            end if

            if(iret.ne.0) then
              u(nfcst,nvar,np,:) = - 1.0E9
              nodata(nfcst,nvar,np) = 1
              write(*,*) '   read u error=',iret
            else
              u(nfcst,nvar,np,:)=gfld%fld(:)
            end if

            if(fho(nvar)(1:4).eq.'FHOP'.and.
     +                           grib2file(1:4).eq.'fcst' ) then
              call readProbGB2(grbunit,jpdtn,2,3,jpd10,jpd12,0,
     +           fho(nvar),thr(nvar,1),yyyy,mm(nfcst),dd(nfcst),
     +           hh(nfcst),jpd9,ngrid,gfld,iret)
            else
              call readGB2(grbunit,jpdtn,2,3,jpd10,jpd12,yyyy,
     +          mm(nfcst),dd(nfcst),hh(nfcst),jpd9,ngrid,gfld,iret)
            end if
 
            if(iret.ne.0) then
              v(nfcst,nvar,np,:) = - 1.0E9
              nodata(nfcst,nvar,np) = 1
              write(*,*) '   read u error=',iret
            else
              v(nfcst,nvar,np,:)=gfld%fld(:)
            end if
 
 
              data(nfcst,nvar,np,:) = sqrt(
     &           u(nfcst,nvar,np,:)*u(nfcst,nvar,np,:)+
     &           v(nfcst,nvar,np,:)*v(nfcst,nvar,np,:) )
 
          end do
  
        else                    !Non-wind

          do np = 1, levels(nvar)

            if (jp.eq.100) then
              jpd12 = plevel(np)
            else
              jpd12 = k7(nvar)
            end if
 
            if(jpd1.eq.16.and.jpd2.eq.195.and.jpd10.eq.103.and.
     +              grib2file(1:4).eq.'obsv')    then    !for verifying 1000m reflectivity against MOSAIC's hybrid scan reflectivity (hrs) 
              jpd1=15
              jpd2=15
              jpd10=200
              jpd12 =0
            end if

           if(jpd1.eq.19.and.jpd2.eq.0.and.jpd10.eq.3.and.
     +              grib2file(1:4).eq.'obsv')    then   !for GSD-VIS 
              jpd10=1
           end if

           if(jpd1.eq.13.and.jpd2.eq.195.and.jpd10.eq.103.and. !Dust obsv jpd12=5000 
     +        jpd12.eq.100.and.grib2file(1:4).eq.'obsv') then
              jpd12=5000
           end if  

           if(jpd1.eq.6.and.jpd2.eq.1.and.         !AFWA grib2 is converted from grib1, so jpd10 is still 200
     +                         grib2file(1:4).eq.'obsv') then
              jpd10=200 
           end if


            write(*,*) 'Var for ', jpdtn, jpd1,jpd2,jpd10,jpd12
            write(*,*) 'yyyy mm dd hh ff=', yyyy,mm(nfcst),dd(nfcst),
     +       hh(nfcst),ff(nfcst)


            if(fho(nvar)(1:4).eq.'FHOP'.and.
     +                           grib2file(1:4).eq.'fcst' ) then
              call readProbGB2(grbunit,jpdtn,jpd1,jpd2,jpd10,jpd12,0,
     +           fho(nvar),thr(nvar,1),yyyy,mm(nfcst),dd(nfcst),
     +           hh(nfcst),jpd9,ngrid,gfld,iret)
            else
              call readGB2(grbunit,jpdtn,jpd1,jpd2,jpd10,jpd12,yyyy,
     +           mm(nfcst),dd(nfcst),hh(nfcst),jpd9,ngrid,gfld,iret)
            end if 

            if(iret.ne.0) then
              data(nfcst,nvar,np,:) = - 1.0E9
              nodata(nfcst,nvar,np) = 1
              write(*,*) '   read error=',iret,'for ',jpd1,jpd2
            else
              data(nfcst,nvar,np,:)=gfld%fld(:)
            end if

            if(jpd1.eq.13.and.jpd2.eq.195.and.                          !convert HYSPLIT smoke/dust's unit 
     +       jpd10.eq.103.and.jpd12.eq.5000.and.                        !from log10 C -> concentration C
     +       grib2file(1:14).eq.'fcst.grib.HYSP') then                  !unit: micro-gram/m3
              do ng = 1, ngrid
               if (data(nfcst,nvar,np,ng).lt.-90.0) then
                data(nfcst,nvar,np,ng)=0.0
               else  
                data(nfcst,nvar,np,ng)=10.0**data(nfcst,nvar,np,ng)
               end if
              end do  
            end if

            if(jpd1.eq.3.and.jpd2.eq.10.and.                      !convert obsv dust/smoke's unit 
     +       jpd10.eq.103.and.jpd12.eq.5000.and.                  !from log10 C -> concentration C
     +       grib2file(1:14).eq.'obsv.grib.HYSP') then                !unit: micro-gram/m3
              data(nfcst,nvar,np,:)=data(nfcst,nvar,np,:)*1.0E9
              nd=0
              do ng = 1, ngrid
               vr=-99.
c               if (data(nfcst,nvar,np,ng).eq.-1.0) then
                if (data(nfcst,nvar,np,ng).gt.-1.5.and.
     +             data(nfcst,nvar,np,ng).lt.0.0) then
                 vr=data(nfcst,nvar,np,ng)
c               else if (data(nfcst,nvar,np,ng).eq.-2.0.or.
c     +                  data(nfcst,nvar,np,ng).eq.-3.0) then
                else if (data(nfcst,nvar,np,ng).ge.-3.5.and.
     +                   data(nfcst,nvar,np,ng).le.-1.5) then
                 vr=1.0E-6
c               else if (data(nfcst,nvar,np,ng).eq.-9.0) then
                else if (data(nfcst,nvar,np,ng).gt.-9.0.and.
     +                   data(nfcst,nvar,np,ng).lt.-3.5) then
                 vr=data(nfcst,nvar,np,ng)
                else
                 vr=data(nfcst,nvar,np,ng)
                end if
                 data(nfcst,nvar,np,ng)=vr
              end do
             end if

           if(jpd1.eq.1.and.jpd2.eq.8.and.
     +         grib2file(1:4).eq.'obsv') then  !CCPA's  hh set back 
              hh(nfcst)=hh(nfcst)+3    
           end if

          end do

        end if       

 600    continue
500   continue

       call baclose(grbunit,ierr)
       write(*,*) 'close ', grib2file, 'ierr=',ierr

      return
      end

        

      
