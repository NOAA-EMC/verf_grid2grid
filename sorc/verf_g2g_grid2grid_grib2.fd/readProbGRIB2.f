      subroutine readProbGB2(iunit,jpdtn,jpd1,jpd2,jpd10,jpd12,jpd27,
     +    fho,thr,yy,mm,dd,cyc,ff,jf,gfld,iret)

        use grib_mod

        type(gribfield) :: gfld
 
        integer jids(200), jpdt(200), jgdt(200)
        integer jpd1,jpd2,jpd10,jpd12,jpdtn,jf,jpd27
        logical :: unpack=.true. 
        integer yy,mm,dd,cyc,ff
        character*24 fho
        real thr

        write(*,*) 'In readProbGB2'
        write(*,*) jpdtn,jpd1,jpd2,jpd10,jpd12,jpd27,fho,thr,
     +  yy,mm,dd,cyc,ff,jf

        jids=-9999  !array define center, master/local table, year,month,day, hour, etc, -9999 wildcard to accept any
        jpdt=-9999  !array define Product, to be determined
        jgdt=-9999  !array define Grid , -9999 wildcard to accept any

        jdisc=-1    !discipline#  -1 wildcard 
        jgdtn=-1    !grid template number,    -1 wildcard 
        jskp=0      !Number of fields to be skip, 0 search from beginning

        jpdt(1)=jpd1   !Category #     
        jpdt(2)=jpd2   !Product # under this category     
        jpdt(10)=jpd10
        jpdt(12)=jpd12
        jpdt(9)=ff    

        if (jpd1.eq.1.and.jpd2.eq.8) then
          jpdt(27)=jpd27
          jpdtn=9
        else
          jpdtn=5
        end if

        if(trim(fho).eq.'FHOP>') then
          jpdt(22)=int(thr*1000)
        else if (trim(fho).eq.'FHOP<') then
          jpdt(20)=int(thr*1000)
        else if (trim(fho).eq.'FHOP=') then
          jpdt(20)=int(thr*1000)
        else
          write(*,*) 'Wrong FHOP type'
          stop 2020
        end if

        if(jpdt(10).eq.100) then
         jpdt(12)=jpdt(12)*100
        end if


        !forecast starting time yy,mm,dd,cyc input (since jids(5)=1) 
        jids(6)=yy
        jids(7)=mm
        jids(8)=dd
        jids(9)=cyc

         call getgb2(iunit,0,jskp, jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,
     +        unpack, jskp1, gfld,iret)
         
        return
        end 
         
