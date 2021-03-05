c       real HGTsfc(96673)
c       ngrid=96673
c       call getHGTsfc(HGTsfc,ngrid)
c       stop
c       end       

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  This subroutine is to get surface height
c  Author: Binbin Zhou
c          May, 2005   
c
       subroutine getHGTsfc(HGTsfc,ngrid)
  
       use grib_mod

       type(gribfield) :: gfld
       real HGTsfc(ngrid)

       integer grbunit, indxunit
       CHARACTER*80 grbfile, indxfile

        grbunit=35
        grbfile='sfc.grib'

       call baopenr(grbunit,grbfile, ierr)
       if(ierr.ne.0) then
          write(*,*) 'open grib file ',trim(grbfile), ' error'
          stop 98
       end if

        jpdtn=0
        jpd1=3
        jpd2=5
        jpd10=1
        jpd12=0

        call readGB2(iunit,jpdtn,jpd1,jpd2,jpd10,jpd12,
     +     -1,-1,-1,-1,-1,ngrid,gfld,iret)


        if(iret.ne.0) then
          write(*,*)'no sfc height data'
              stop 97 
        else
          HGTsfc=gfld%fld
        end if

        call baclose(grbunit, ierr)

        return
        end

