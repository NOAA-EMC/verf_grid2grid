c        Just for testing getregionid subroutine
c        integer region_id(427734), gribid
c        real region_latlon(2,427734)
c        gribid=255
c        call getregionid(region_id,region_latlon,gribid)
c        do i = 1,427734
c          write(*,'(2i10,2f10.2)') i, region_id(i), 
c     &       region_latlon(1,i),region_latlon(2,i)
c        end do       
c        stop
c        end




        subroutine getregionid(region_id, region_latlon, gribid,
     +              model, obstype)

c  This subroutine is to retrieve resion id (28) at each grid
c  for any requested gribid. The 28 region ids are predfined
c  in GRIB#104. The algorithm is first get each grid point's
C  Gr(i,j), ie. xpts212 and ypts212 in the code, then retrieve
c  its earth lat and long, ie rlon and rlat in the code
c  then from rlong a rlat to get (i,j) in grib104,
c  then from this new (i,j) to get region id fors point 
c  if the point is outside grib104 domain, set it to be -1
c
c  Mar. 10, 2005   Original, Binbin Zhou, SAIC@EMC/NCEP/NOAA
c  Oct. 12, 2006   Modification: Add 255 grid, Binbin Zhou 
c  Set. 15, 2012   Modification: add aditional model/obstype name to id NX, NY
c  2014-5-6        Binbin Z. Add Firewx grid reading

c  INPUT: gribid, integer, requested grib id number, such as 212, 223
c  OUTPUT: region_id(Ngrid), integer, region id for each grid point
c          in reference of grib104 
c

        use gdswzd_mod

        integer region_id(*) 
        real region_latlon(2,*)
        integer gribid,id
        
        integer kpds212(200), kpds104(200)
        integer kgds212(200), kgds104(200)
        real, allocatable, dimension(:) :: xpts212,ypts212
        real, allocatable, dimension(:) :: rlon,rlat,crot,srot 
        real, allocatable, dimension(:) :: xpts104,ypts104
        integer kgds(200)
        character*50 gds(400)

        integer ig104(147,110)
        character*3 regions
        character*24 model, obstype, ens
        integer dumy(20)                 !for reading firewx griddef file

        COMMON /for255/Ngrid


        ens=model(1:7) 

        write(*,*)'In getregion: model, obstype=', 
     +       trim(model),' ',trim(obstype),' ',trim(ens)
        ! get kgds array with requested gribid as input
        if(gribid.eq.255) then

          if (trim(model).eq.'HRRR'.and.
     +        trim(obstype).ne.'FIREWX' ) then          
            kgds212(2)=1799
            kgds212(3)=1059

          else if (model(1:4).eq.'href') then
            kgds212(2)=1121 
            kgds212(3)=881
            
          else if (trim(obstype).eq.'RTMA2'.or.
     +        trim(obstype).eq.'URMA' ) then 
            kgds212(2)=2145   !2.5km RTMA
            kgds212(3)=1377   
            kgds212(4)=20192
            kgds212(5)=-238446
            kgds212(6)=8
            kgds212(7)=26500
            kgds212(8)=2540
            kgds212(9)=2540
            kgds212(10)=0
            kgds212(11)=64
            kgds212(12)=25000
            kgds212(13)=25000
            kgds212(14)=0
            kgds212(15)=0
            kgds212(16)=0
            kgds212(17)=-1
            kgds212(18)=-1
            kgds212(19)=0
            kgds212(20)=255
            kgds212(21)=-1
            kgds212(22)=-1
            kgds212(23)=-1
            kgds212(24)=-1
            kgds212(25)=-1
           else if (trim(obstype).eq.'RTMA') then
            kgds212(2)=1073   !RTMA
            kgds212(3)=689    !RTMA
            kgds212(4)=23200
            kgds212(5)=-108900
            kgds212(6)=0
            kgds212(7)=-91000
            kgds212(8)=4000
            kgds212(9)=4000
            kgds212(10)=0
            kgds212(11)=64
            kgds212(12)=0
            kgds212(13)=38000
            kgds212(14)=0
            kgds212(15)=0
            kgds212(16)=0
            kgds212(17)=-1
            kgds212(18)=-1
            kgds212(19)=0
            kgds212(20)=255
            kgds212(21)=-1
            kgds212(22)=-1
            kgds212(23)=-1
            kgds212(24)=-1
            kgds212(25)=-1

          else if (trim(obstype).eq.'FIREWX') then   !For firewx, kgds212(2),kgds212(3) are dynamic, read fort.15 to get NX,NY
            read(15,*) (dumy(i),i=1,8),kgds212(2),kgds212(3)


           else if (trim(obstype).eq.'NDAS'.or.
     +              trim(obstype).eq.'LAPS') then   !Isidora: Suppose first 7 chars for  exp sref
                                                    !name is 'expsref'
            kgds212(2)=801
            kgds212(3)=581
 
          else if (trim(obstype).eq.'RAPANL3') then
            kgds212(2)=1799
            kgds212(3)=1059

           else if (trim(obstype).eq.'RAPANL4') then
            kgds212(2)=1473
            kgds212(3)=1025

           !to use user defined region, must use full array of kgds212 
           else if (trim(obstype).eq.'SMOKE/255'.or.
     +              trim(obstype).eq.'SMOKE') then
            kgds212(2)=801
            kgds212(3)=534
         kgds212(1)=0
         kgds212(4)=0
         kgds212(5)=-175000
         kgds212(6)=128
         kgds212(7)=80000
         kgds212(8)=-55000
         kgds212(9)=150
         kgds212(10)=150
         kgds212(11)=64
         kgds212(12)=0
         kgds212(13)=-1
         kgds212(14)=-1
         kgds212(15)=-1
         kgds212(16)=-1
         kgds212(17)=-1
         kgds212(18)=-1
         kgds212(19)=0
         kgds212(20)=255
         kgds212(21)=-1
         kgds212(22)=-1
         kgds212(23)=-1
         kgds212(24)=-1
         kgds212(25)=-1

           else if (trim(obstype).eq.'DUST'.or.
     +              trim(obstype).eq.'DUST/255' ) then
         kgds212(1)=0
            kgds212(2)=601
            kgds212(3)=251
         kgds212(4)=25000
         kgds212(5)=-125000
         kgds212(6)=0
         kgds212(7)=50000
         kgds212(8)=-65000
         kgds212(9)=65535
         kgds212(10)=65535
         kgds212(11)=64
         kgds212(12)=0
         kgds212(13)=-1
         kgds212(14)=-1
         kgds212(15)=-1
         kgds212(16)=-1
         kgds212(17)=-1
         kgds212(18)=-1
         kgds212(19)=1
         kgds212(20)=255
         kgds212(21)=-1
         kgds212(22)=-1
         kgds212(23)=-1
         kgds212(24)=-1
         kgds212(25)=-1   
                     
          end if  
            
          !even if model is HRRR, but if verif is on MOSAIC grid,overwite 
          if(trim(obstype).eq.'ONMOSAIC') then
            kgds212(2)=1401
            kgds212(3)=701
          end if

        else

          call makgds(gribid, kgds212, gds, lengds, ier)

        end if

        write(*,*) 'NX,NY=',kgds212(2),kgds212(3)

        Ngrid=kgds212(2)*kgds212(3)
        allocate(xpts212(ngrid))
        allocate(ypts212(ngrid))
        allocate(rlon(ngrid))
        allocate(rlat(ngrid))
        allocate(crot(ngrid))
        allocate(srot(ngrid))
        allocate(xpts104(ngrid))
        allocate(ypts104(ngrid))


        iopt=1
        fill=-9999.0

        !from grid sequence # -> (i,j)
        do N=1, Ngrid
          xpts212(n)=MOD(n, KGDS212(2)) 
           if (xpts212(n).eq.0) then
            xpts212(n)= KGDS212(2)
            a=n/KGDS212(2)
            ypts212(n)=NINT(A)
           else
           a=n/KGDS212(2)+1
           ypts212(n)=NINT(A)
        end if
        end do

        !from (i,j) to retrieve lat and long at all (i,j) points
c        call gdswiz(kgds212,iopt,Ngrid,fill,xpts212,ypts212,
c     &       rlon,rlat,nret,lrot,crot,srot)
 
        call gdswzd(kgds212,iopt,Ngrid,fill,xpts212,ypts212,
     &       rlon,rlat,nret,crot,srot)

        id=104
        call makgds(id, kgds104, gds, lengds, ier)
       
        !from array of lat and long of all points to get xpts104,ypts104, i.e. (i,j) of thse points
        iopt=-1
c        call gdswiz(kgds104,iopt,Ngrid,fill,xpts104,ypts104,
c     &       rlon,rlat,nret,lrot,crot,srot)

        call gdswzd(kgds104,iopt,Ngrid,fill,xpts104,ypts104,
     &       rlon,rlat,nret,crot,srot)
        open(20, file='grid#104', status='old') 
        read(20, '(20I4)') ig104

        do n=1,Ngrid
          if(xpts104(n).lt.0.0.or.ypts104(n).lt.0.0) then
           region_latlon(1,n) = rlat(n)
           region_latlon(2,n) = rlon(n)
           region_id(n)= -1
          else
           region_id(n)=ig104(NINT(xpts104(n)),NINT(ypts104(n)))
           region_latlon(1,n) = rlat(n)
           region_latlon(2,n) = rlon(n)
          end if
        end do
  
        if(allocated(xpts212)) deallocate(xpts212)
        if(allocated(ypts212)) deallocate(ypts212)
        if(allocated(rlon)) deallocate(rlon)
        if(allocated(rlat)) deallocate(rlat)
        if(allocated(crot)) deallocate(crot)
        if(allocated(srot)) deallocate(srot)
        if(allocated(xpts104)) deallocate(xpts104)
        if(allocated(ypts104)) deallocate(ypts104)

        close(20)

        return
        end

