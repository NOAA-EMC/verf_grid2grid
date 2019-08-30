        use grib_mod

        type(gribfield) :: gfld
C  raw data
       real,allocatable,dimension(:,:) :: var

       character*50 gdss(400)
       integer GRIBID, kgdss(200), lengds,im,jm,jf
       character*60 fname, fnameout
       integer iyyyy,imm,idd,icyc
       integer jpd1(10),jpd2(10),jpd10(10),jpd12(10),jpd27(10),
     +         jtmp(10)

       read (*,*) GRIBID,iyyyy,imm,idd,icyc,ifhr,fname 

cc     RAP has one-hour accumu precip, so only one file is used
cc     NAM has no one-hour accumu precip, so two files are needed

       if(GRIBID.eq.255) then   !For CCPA HRAP 255 grid
         im=1121
         jm=881
         jf=im*jm
       else
         call makgds(GRIBID, kgdss, gdss, lengds, ier)
         im=kgdss(2)
         jm=kgdss(3)
         jf=kgdss(2)*kgdss(3)
       end if


       jpd27=-9999

       if (GRIBID.eq.227) then
        nv=2
        !nv=4
        !REFC
        jpd1(1)=16
        jpd2(1)=196
        jpd10(1)=200
        jpd12(1)=0
        jtmp(1)=0
        !RETOP
        jpd1(2)=16
        jpd2(2)=197
        jpd10(2)=200
        jpd12(2)=0
        jtmp(2)=0
        !MXUPHL
        jpd1(3)=7
        jpd2(3)=199
        jpd10(3)=103
        jpd12(3)=5000
        jtmp(3)=8
        !REFD-1km
        jpd1(4)=16
        jpd2(4)=195
        jpd10(4)=103
        jpd12(4)=1000
        jtmp(1)=0
       else if (GRIBID.eq.184) then
        nv=4
        !VIS
        jpd1(1)=19
        jpd2(1)=0
        jpd10(1)=1
        jpd12(1)=0
        jtmp(1)=0
        !TCDC
        jpd1(2)=6
        jpd2(2)=1
        jpd10(2)=200
        jpd12(2)=0
        jtmp(2)=0
        !TMP 2m
        jpd1(3)=0
        jpd2(3)=0
        jpd10(3)=103
        jpd12(3)=2
        jtmp(3)=0
        !DPT 2m
        jpd1(4)=0
        jpd2(4)=6
        jpd10(4)=103
        jpd12(4)=2
        jtmp(4)=0
        !Ceiling
        jpd1(5)=3
        jpd2(5)=5
        jpd10(5)=215
        jpd12(5)=0
        jtmp(5)=0
       else if (GRIBID.eq.255) then
        !APCP
        nv=1
        jpd1(1)=1
        jpd2(1)=8
        jpd10(1)=1
        jpd12(1)=0
        jtmp(1)=8
        jpd27(1)=3
       else
        write(*,*)'nv=?'
       end if
        

       allocate(var(nv,jf)) 

        fnameout=trim(fname)//'.new' 

        write (*,*) 'fname, fnameout=',trim(fname),'  ',trim(fnameout)  

        iunit=50                    !iunit must be < 100, larger than 100 (such as 101) will get ierr =96 error
        call baopenr(iunit,fname,ierr)
        write(*,*) 'open ', fname, ' ierr=',ierr

        call baopen(60,fnameout,ierr)
         if(iret.ne.0) then
          write(*,*) 'open ', fnameout, ' error', iret
          stop
          end if


        var=0.0
        gfld%fld=0.0

        DO k=1,nv
         jpdtn=jtmp(k)
         call readGB2(iunit,jpdtn,jpd1(k),jpd2(k),jpd10(k),
     +               jpd12(k),jpd27(k),gfld,iret)
    
          if (iret.eq.0) then
            var(k,:)=gfld%fld(:) 
            if (jpd1(k).eq.16.and.jpd2(k).eq.196) then
             do i=1,jf
              if(var(k,i).lt.0.0.and.var(k,i).gt.-990.0) var(k,i)=0.
             end do
            end if
            if (jpd1(k).eq.3.and.jpd2(k).eq.5) then
             do i=1,jf
              if(var(k,i).lt.0.0) var(k,i)=20000.
             end do
            end if
          else
            write(*,*) 'iret=', iret
          end if

        !END DO

        !call baclose(iunit,ierr)
        !write(*,*) 'close ', fname, 'ierr=',ierr

           gfld%idsect(6)=iyyyy
           gfld%idsect(7)=imm
           gfld%idsect(8)=idd
           gfld%idsect(9)=icyc
          
          if (jpdtn.eq.0) then
           gfld%ipdtmpl(9)=ifhr
          else    !jpdtn=8
           gfld%ipdtmpl(9)=ifhr-3
          end if
        
          !call baopen(60,fnameout,ierr)
          ! if(iret.ne.0) then
          !  write(*,*) 'open ', fnameout, ' error', iret
          !  stop
          ! end if

          !DO k=1,nv

           !gfld%ipdtnum=jtmp(k)
           !gfld%ipdtmpl(1)=jpd1(k)
           !gfld%ipdtmpl(2)=jpd2(k)
           !gfld%ipdtmpl(10)=jpd10(k)
           !gfld%ipdtmpl(12)=jpd12(k)
           gfld%fld(:)=var(k,:)

          call putgb2(60,gfld,iret) 
           if(iret.ne.0) then
            write(*,*) 'putgb2 error', iret
            stop
           end if
          END DO

          call baclose(50,ierr)
          call baclose(60,ierr)
              
      stop
      end


      subroutine readGB2(iunit,jpdtn,jpd1,jpd2,jpd10,jpd12,
     +                  jpd27, gfld,iret)
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

        if(jpdtn.eq.8) jpdt(27)=jpd27

         call getgb2(iunit,0,jskp, jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,
     +        unpack, jskp1, gfld,iret)
         
        return
        end 
       
  
        
       
