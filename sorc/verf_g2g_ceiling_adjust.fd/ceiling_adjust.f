        use grib_mod

        type(gribfield) :: gfld
C  raw data
       real,allocatable,dimension(:) :: var,ceil,hsfc

       character*50 gdss(400)
       integer GRIBID, kgdss(200), lengds,im,jm,jf
       character*60 fname, fnameout
       integer yy,mm,dd,cyc(4),ff(5)


       !GRIBID=227
       GRIBID=255


       if(GRIBID.eq.255) then   !For HRRR 255 grid
         im=1799
         jm=1059
         jf=im*jm
       else
         call makgds(GRIBID, kgdss, gdss, lengds, ier)
         im=kgdss(2)
         jm=kgdss(3)
         jf=kgdss(2)*kgdss(3)
       end if

       write(*,*) 'jf=',jf

       allocate(var(jf)) 
       allocate(hsfc(jf)) 
       allocate(ceil(jf)) 

        read (*,*) fname
         fnameout=trim(fname)//'.adjusted' 

        write (*,*) 'fname, fnameout=',trim(fname),'  ',trim(fnameout)  

        iunit=50                    !iunit must be < 100, larger than 100 (such as 101) will get ierr =96 error
        call baopenr(iunit,fname,ierr)
        write(*,*) 'open ', fname, ' ierr=',ierr


        jpdtn=0
        jpd1=3
        jpd2=5               
        jpd10=1
        jpd12=0

          var=0.0
          gfld%fld=0.0

          call readGB2(iunit,jpdtn,jpd1,jpd2,jpd10,jpd12,
     +      gfld,iret)
   
          if(iret.eq.0) then
            hsfc=gfld%fld
          else
           write(*,*) 'iret=', iret
          end if

        jpd10=215
         call readGB2(iunit,jpdtn,jpd1,jpd2,jpd10,jpd12,
     +      gfld,iret)

          if(iret.eq.0) then
            ceil=gfld%fld
          else
           write(*,*) 'iret=', iret
          end if

         call baclose(iunit,ierr)
         write(*,*) 'close ', fname, 'ierr=',ierr

         do i = 1,jf
          if (ceil(i).ge.13000.0) ceil(i)=20000.0 
         end do

         do i = 1,jf
          if(ceil(i).eq.20000.0) then
             gfld%fld(i)=ceil(i)
          else
             gfld%fld(i)=ceil(i)-hsfc(i)
          end if
         end do
        
          call baopen(60,fnameout,ierr)
           if(iret.ne.0) then
            write(*,*) 'open ', fnameout, ' error', iret
            stop
           end if
          call putgb2(60,gfld,iret) 
           if(iret.ne.0) then
            write(*,*) 'putgb2 error', iret
            stop
           end if
          call baclose(iunit,ierr)
              
      stop
      end


      subroutine readGB2(iunit,jpdtn,jpd1,jpd2,jpd10,jpd12,
     +    gfld,iret)

        use grib_mod

        type(gribfield) :: gfld
 
        integer jids(200), jpdt(200), jgdt(200)
        integer jpd1,jpd2,jpd10,jpd12,jpdtn
        logical :: unpack=.true. 

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
        if(jpdt(10).eq.100) then
         jpdt(12)=jpdt(12)*100
        end if

         call getgb2(iunit,0,jskp, jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,
     +        unpack, jskp1, gfld,iret)
         
        return
        end 
       
  
        
       
