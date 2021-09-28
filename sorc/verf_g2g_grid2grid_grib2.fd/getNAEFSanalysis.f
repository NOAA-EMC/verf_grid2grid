c
c     Return difference between NCEP analysis and Canadian analysis diff(jf) 
c

        subroutine get_naefs_analysis(iunit,kk4,kk5,kk6,kk7,jf,
     +        gfsanl,diff)

        use grib_mod

        type(gribfield) :: gfld

        real, dimension(jf),intent(IN) :: gfsanl
        real, dimension(jf),intent(OUT) :: diff
        real cmcanl(jf)

        jpdtn=1
        jpd1=kk4
        jpd2=kk5
        jpd10=kk6
        jpd12=kk7
        
        call readCMCanlGB2(iunit,jpdtn,jpd1,jpd2,jpd10,jpd12,
     +      gfld,iret)
        if(iret.eq.0) then
          cmcanl(:)=gfld%fld(:)
        else
          write(*,*) 'radGCMCanlB2 error ', iret, ' Skipped it'
        end if
        
        diff=0.
        do i=1,jf
         if (gfsanl(i).ne.-9999.99.and.cmcanl(i).ne.-9999.99) then
          diff(:)=gfsanl(:)-cmcanl(:)
         end if
        end do

c        write(*,*) 'In get_naefs_analysis: gfsanl, cmcanl, diff'
c        do i=1,jf
c         write(*,*) i,gfsanl(i),cmcanl(i),diff(i)
c        end do

        return
        end

      subroutine readCMCanlGB2(igrb2,jpdtn,jpd1,jpd2,jpd10,jpd12,
     +     gfld,iret)

        use grib_mod

        type(gribfield) :: gfld
        integer jids(200), jpdt(200), jgdt(200)
        integer igrb2,jpdtn,jpd1,jpd2,jpd10,jpd12
        logical :: unpack=.true.

        write(*,*) 'In readCMCanlGB2: ',
     +              igrb2, jpdtn,jpd1,jpd2,jpd10,jpd12

        jids=-9999  !array define center, master/local table, year,month,day, hour, etc, -9999 wildcard to accept any
        jpdt=-9999  !array define Product, to be determined
        jgdt=-9999  !array define Grid , -9999 wildcard to accept any

        jdisc=-1    !discipline#  -1 wildcard 
        jgdtn=-1    !grid template number,    -1 wildcard
        jskp=0      !Number of fields to be skip, 0 search from beginning

        jpdt(1)=jpd1   !Category #     
        jpdt(2)=jpd2   !Product # under this category     
        jpdt(10)=jpd10 !Product vertical ID      


        if(jpd10.eq.100) then
           jpdt(12)=jpd12*100   !pressure level     
        else
           jpdt(12)=jpd12
        end if


        call getgb2(igrb2,0,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,
     +     unpack, jskp1, gfld,iret)

        return
        end


        
        
              

