	subroutine getMeanClimData(data,uclim,vclim,
     +         levels,numfcst,numvfyobs,numvarbl,numlevel,ngrid,
     +         yy,mm,dd,hh,ff, k4,k5,k6,k7,
     +         plevel, namvarbl,anomly_mrk,anomlylev,
     +         cmm,cdd)


      use grib_mod

      include 'parm.inc'
      
      type(gribfield) :: gfld                                                                                                                                       
      integer numvfyobs,numfcst,numvarbl,numlevel,ngrid
                                                                                                                                             
      real 
     + data(numfcst,numvarbl,numlevel,ngrid),
     + u(2,numlevel,ngrid),v(2,numlevel,ngrid)
     
      real uclim(numfcst,numvarbl,numlevel,ngrid),
     +     vclim(numfcst,numvarbl,numlevel,ngrid)
                                                                                                                                            
      integer k5(mxvrbl),k6(mxvrbl),k7(mxvrbl),k4(mxvrbl)
      integer plevel(maxlvl)
      integer levels(mxvrbl)          !levels for different variables
      integer yy(maxobs), mm(maxobs), dd(maxobs), hh(maxobs),
     +        ff(maxobs)
      CHARACTER*24 namvarbl(mxvrbl)
      CHARACTER*2  cmm(maxobs),cdd(maxobs)      
                                                                                                                                      
      integer yyfcst(mxfcst),yyobsv(maxobs)
                                                                                                                                             
      CHARACTER*80 grbfile, indxfile
      integer grbunit, indxunit

      integer h(5),anomly_mrk(mxvrbl),h12(2),
     +             anomlylev(mxvrbl,maxlvl)
      real cintp(2,numlevel,ngrid)


 
      data (h(i),i=1,5)
     + /0, 6, 12, 18, 24/

      data = 0.0

      write(*,*) ' In getMeanClimData' 

      do 2000 nobsv = 1, numvfyobs

       grbunit=201+nobsv

       write(*,*) 'Observ time = ', nobsv

       grbfile='climat.1959'//cmm(nobsv)//cdd(nobsv)

       write(*,*) trim(grbfile)

       call baopenr(grbunit,grbfile, ierr)
       if(ierr.ne.0) then
        write(*,*)'open climat grib file ',trim(grbfile), ' error'
        stop 118
       end if


       !get which two cycles        
       do i=1,4
        if(hh(nobsv).ge.h(i).and.hh(nobsv).lt.h(i+1)) then
          h12(1)=h(i)
          h12(2)=h(i+1)
          if(h12(2).eq.24) h12(2)=0
        end if
       end do
 
         iyy=59
         imm=mm(nobsv)

         write(*,*)'h1,h2=',h12(1),h12(2)
         write(*,*)'anomly_mrk=',(anomly_mrk(nvar),nvar=1,numvarbl)

        do 1000 nvar = 1, numvarbl 

         jpdtn=0
         jpd1=k4(nvar)
         jpd2=k5(nvar)
         jpd10=k6(nvar)


         if(anomly_mrk(nvar).eq.0) goto 1000

          write(*,*) 'nvar=', nvar

          jp = jpd10                                  !Binbin: these 2 lines are used to
          if(jpd10.eq.100.or.jpd10.eq.104) jp=100   !deal with both jpds=100 and jpds=107
          
          if(jp.eq.100) then
            levels(nvar) = numlevel
          else
            levels(nvar) = 1
          end if


          !get 2 cycle's climatologic data      

          do 800 mh=1,2

           cintp(mh,:,:) = 0.

           write(*,*) 'mh=',mh, 'h12(mh)=', h12(mh)  

           ihh=h12(mh)

           if(jpd1.eq.2.and.jpd2.eq.1)  then

              do 500 np = 1, levels(nvar)

               if(jp.eq.100) then
                jpd12 = plevel(np)
               else
                jpd12 = k7(nvar)
                !based on YueJian's data set, W10m are diagnositic,
                !and cycle-time should be substrcted by 6 hr 
                ihh = ihh - 6
                if (ihh.lt.0) ihh = 18
                !!!??? GRIB2??? jjpds(14) = 6
               end if

               if(anomlylev(nvar,np).eq.0) goto 500

               call readGB2(grbunit,0,2,2,jpd10,jpd12,1959,
     +            mm(nobsv),dd(nobsv),ihh,-1,ngrid,gfld,iret)

               if(iret.ne.0) then
                 u(mh,np,:) = - 1.0E9
               else
                 u(mh,np,:)=gfld%fld(:)
               end if

               call readGB2(grbunit,0,2,3,jpd10,jpd12,1959,
     +            mm(nobsv),dd(nobsv),ihh,-1,ngrid,gfld,iret)

               if(iret.ne.0) then
                 v(mh,np,:) = - 1.0E9
               else
                 v(mh,np,:)=gfld%fld(:)
               end if

               cintp(mh,np,:)=sqrt(
     &            u(mh,np,:)*u(mh,np,:)+
     &            v(mh,np,:)*v(mh,np,:) )

500           continue

           else
  
             do 400 np = 1, levels(nvar)

               if(anomlylev(nvar,np).eq.0) goto 400

               if(jp.eq.100) then
                jpd12 = plevel(np)
               else
                jpd12 = k7(nvar)
                if( (jpd1.eq.0.and.jpd2.eq.0).or.
     +              (jpd1.eq.0.and.jpd2.eq.4).or.
     +              (jpd1.eq.0.and.jpd2.eq.5).or.
     +              (jpd1.eq.2.and.jpd2.eq.2).or.
     +              (jpd1.eq.2.and.jpd2.eq.3) ) then

                   ihh = ihh - 6
                   if (ihh.lt.0) ihh = 18
                   !!!??? GRIB2 ????jpds(14) = 6
                 end if
               end if

               call readGB2(grbunit,0,jpd1,jpd2,jpd10,jpd12,1959,
     +            mm(nobsv),dd(nobsv),ihh,-1,ngrid,gfld,iret)

               if(iret.ne.0) then
                 cintp(mh,np,:) = - 1.0E9
                else
                 cintp(mh,np,:)=gfld%fld(:)
               end if

400          continue

           end if

800       continue !end of 2 mh
       
         !get weighted avearge of climatologic mean
 
          do 900 np = 1, levels(nvar)

           if(anomlylev(nvar,np).eq.0) goto 900

           data(nobsv,nvar,np,:)=cintp(1,np,:)+
     &      (cintp(2,np,:)-cintp(1,np,:))
     &      /(h12(2)-h12(1))*(hh(nobsv)-h12(1))

c       write(*,'(a14,i5, 5f10.2)') 'data at level ', np,
c     &     (data(nobsv,nvar,np,i),i=1,5)

           if(jpd1.eq.2.and.jpd2.eq.1) then
            uclim(nobsv,nvar,np,:)=u(1,np,:)+
     &      (u(2,np,:)-u(1,np,:))
     &      /(h12(2)-h12(1))*(hh(nobsv)-h12(1))

            vclim(nobsv,nvar,np,:)=v(1,np,:)+
     &      (v(2,np,:)-v(1,np,:))
     &      /(h12(2)-h12(1))*(hh(nobsv)-h12(1))

c       write(*,'(a21,i5,10f8.2)')'uclim,vclim at level ',np,
c     &       (uclim(nobsv,nvar,np,i),i=1,5),
c     &       (vclim(nobsv,nvar,np,i),i=1,5)
           end if

900        continue

1000      continue      !end of nvar
2000     continue       !end of nobsv          
 
        if(numvfyobs.eq.1) then
          do nfcst = 2, numfcst
            data(nfcst,:,:,:)=data(1,:,:,:)
            uclim(nfcst,:,:,:)=uclim(1,:,:,:)
            vclim(nfcst,:,:,:)=vclim(1,:,:,:)
          end do
        end if
 
               
        call baclose(grbunit, ierr)
                                                                                                                      

                
        return
        end

