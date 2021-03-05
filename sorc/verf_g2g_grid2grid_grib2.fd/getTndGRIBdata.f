c 
c   This program is to read data from GRIB file
c   Author: Binbin Zhou
c           Mar, 2005, NOAA/NCEP/EMC
c


      subroutine getTndGRIBdata (grbunit,
     +         grbfile, data,u,v,levels,
     +         numfcst,numvarbl,numlevel,ngrid,
     +         yy,mm,dd,hh,ff, k4,k5,k6,k7,
     +         plevel, namvarbl, tendencymrk)

      use grib_mod
      include 'parm.inc'

      type(gribfield) :: gfld

      integer numfcst,numvarbl,numlevel,ngrid   

      real data(numfcst,numvarbl,numlevel,ngrid),
     + u(numfcst,numvarbl,numlevel,ngrid),
     + v(numfcst,numvarbl,numlevel,ngrid)

      integer k5(mxvrbl),k6(mxvrbl),k7(mxvrbl),k4(mxvrbl)
      integer plevel(maxlvl)
      integer levels(mxvrbl)          !levels for different variables
      integer tendencymrk(mxvrbl)
      integer yy(mxfcst), mm(mxfcst), dd(mxfcst), hh(mxfcst), 
     +        ff(mxfcst) 
      CHARACTER*24 namvarbl(mxvrbl)
                                                                                                                                                    
      integer yyfcst(mxfcst),yyobsv(maxobs)

      integer grbunit
      CHARACTER*80 grbfile

      integer igribid
      COMMON /grb/igribid

              
        write(*,*) 'Tendency grib file:', 
     +        trim(grbfile)

        call baopenr(grbunit,grbfile, ierr)
        if(ierr.ne.0) then
          write(*,*) 'open grib file ',trim(grbfile), ' error'
          stop 98   
        end if 

        do nfcst = 1, numfcst
        
         iyy=yy(nfcst)
         imm=mm(nfcst)
         idd=dd(nfcst)
         ihh=hh(nfcst)
         iff=ff(nfcst)

         write(*,*) 'Forecast time = ', nfcst

         do 2000 nvar = 1, numvarbl
 
           write(*,*) 'tendencymrk for var ', nvar, '=',
     +                 tendencymrk(nvar)

           if(tendencymrk(nvar).eq.0) goto 2000       

           jpd1 = k4(nvar)
           jpd2 = k5(nvar)
           jpd10 = k6(nvar)
 
           jp = jpd10                                  !Binbin: these 2 lines are used to
           if(jpd10.eq.100.or.jpd10.eq.104) jp=100   !deal with both jpds=100 and jpds=107

           if(jp.eq.100) then
            levels(nvar) = numlevel
           else
            levels(nvar) = 1
           end if

            if(jpd1.eq.2.and.jpd2.eq.1) then

             do np = 1, levels(nvar)

               if(jp.eq.100) then
                 jpd12 = plevel(np)
               else
                 jpd12 = k7(nvar)
               end if

             call readGB2(iunit,jpdtn,2,2,jpd10,jpd12,
     +          iyy,imm,idd,ihh,iff,ngrid,gfld,iret)

               if(iret.ne.0) then
                 u(nfcst,nvar,np,:) = - 1.0E9
               else
                 u(nfcst,nvar,np,:)=gfld%fld(:)
               end if
                
             call readGB2(iunit,jpdtn,2,3,jpd10,jpd12,
     +          iyy,imm,idd,ihh,iff,ngrid,gfld,iret)
           
               if(iret.ne.0) then
                 v(nfcst,nvar,np,:) = - 1.0E9
               else
                 v(nfcst,nvar,np,:)=gfld%fld(:)
               end if
                           
                     
               data(nfcst,nvar,np,:) = sqrt(
     &            u(nfcst,nvar,np,:)*u(nfcst,nvar,np,:)+
     &            v(nfcst,nvar,np,:)*v(nfcst,nvar,np,:) )

                           
             end do

            else

             do np = 1, levels(nvar)
               if(jp.eq.100) then
                jpd12 = plevel(np)
               else
                jpd12 = k7(nvar)
               end if

               call readGB2(iunit,jpdtn,jpd1,jpd2,jpd10,jpd12,
     +            iyy,imm,idd,ihh,iff,ngrid,gfld,iret)


               if(iret.ne.0) then
                 data(nfcst,nvar,np,:) = - 1.0E9
               else
                 data(nfcst,nvar,np,:)=gfld%fld(:)
               end if

             end do

            end if
                           
2000     continue          

         end do
                
        call baclose(grbunit, ierr)

        return
        end

        


      
