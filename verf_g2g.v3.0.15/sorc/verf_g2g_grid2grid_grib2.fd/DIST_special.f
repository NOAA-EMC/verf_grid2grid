
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine      DIST1
c  Prgmmr: Binbin Zhou     03/15/2017
c
c This is main distribution sub-program for NAEFS project on IBM-SP
c
c  For: 1. Talagrand distribution
c       2. Relative position ( or near trues )
c       3. Ensemble spread
c       4. Ensemble mean RMS error
c       5. Perfection measurement ( option )
c
c   subroutine                                                    
c              IADDATE---> to add forecast hours to initial data    
c              GETGRB ---> to get GRIB format data                  
c              GETGRBE---> to get GRIB format ensemble data                  
c              WGT2D  ---> to create 2-dimensional weights
c              GRANGE ---> to calculate max. and min value of array
c              TALAGR ---> to calculate talagrand distribution and etc.
c   
c
c   special namelist parameter:
c      ictl  -- control parameter
c               1. standard grid to grid verification
c               2. grid to obs verfication
c   parameters:
c      ix    -- x-dimensional
c      iy    -- y-dimensional
c      im    -- ensemble members
c
c   Fortran 77 on IBMSP 
c
C--------+---------+---------+---------+---------+----------+---------+--
      subroutine dist1(fcst,fanl,clim,wght,len,ib,im,k4,k5)
      dimension fcst(len,im),fanl(len),clim(len,ib+1)
      dimension wght(len)
      dimension fst(im)
      dimension fit(im+1),it(im+1)
      dimension fir(im),ir(im)
      real rp(im)

      common /vscore/ infow(500),probs(500),dists(500)

      imp1 = im + 1
c ----
c to calculate talagrand histogram and N+1 distribution
c ----
      acwt = 0.0
      acwt1 = 0.0
      fit  = 0.0
      fir  = 0.0
      xx   = 0.0
      yy   = 0.0
      xy   = 0.0
      fsprd= 0.0
      frmsa= 0.0
      fmerr= 0.0
      fabse= 0.0
      faccs= 0.0

      write(*,*) 'DIST1:', len,ib,im
      do nxy = 1, len                  ! do loop for each latitude
       wfac = wght(nxy)
       acwt = acwt + wfac
       do ii = 1, im
        fst(ii) = fcst(nxy,ii)
         if (fst(ii).eq.-9999.99.or.fanl(nxy).eq.-9999.99) then
          dists(1+imp1+im) = -9.99 
          dists(2+imp1+im) = -9.99
          print *, 'fsprd=',fsprd
          go to 999
         endif
        if(k4.eq.19.and.k5.eq.0.and.fst(ii).ge.19999.) then ! VIS
         !just indicate no event for fcst 
         fst(ii)=0.
        end if

        if(k4.eq.16.and.k5.eq.196.and.fst(ii).le.0.0) then ! REF
         !just indicate no event for fcst, here all negative value 
         !-20 ~ 0 dBZ treated as no event  
         fst(ii)=0.
        end if

        if(k4.eq.16.and.k5.eq.197.and.fst(ii).le.0.0) then ! ETP
         !just indicate no event for fcst 
         fst(ii)=0.
        end if

       enddo
       anl      = fanl(nxy)
       !just indicate no event for obsv
       if(k4.eq.19.and.k5.eq.0.and.anl.ge.19999.) anl=0.   !VIS       
       if(k4.eq.16.and.k5.eq.196.and.anl.le.0.0) anl=0.    !REF
       if(k4.eq.16.and.k5.eq.197.and.anl.le.0.0) anl=0.    !ETP


       !write(*,*) (fst(ii),ii=1,im)

       call talagr1(im,im,fst,anl,it,rp,ave,xmed,sprd,rmsa,cnt)
       acwt1 = acwt1 + wfac*cnt  !just for hitogram

       do ii = 1, imp1
        fit(ii) = fit(ii) + it(ii)*wfac  
       enddo
       do ii = 1, im
        fir(ii) = fir(ii) + rp(ii)*wfac 
       enddo

       !write(*,'(i10,12f6.0,f3.0)') nxy, (fst(ii),ii=1,im),anl,cnt
       !write(*,'(a10,11f6.0)') '  fir     ', (fir(ii),ii=1,im)

       fsprd = fsprd + sprd*wfac
       frmsa = frmsa + rmsa*wfac
       fmerr = fmerr + (ave-anl)*wfac
       fabse = fabse + abs(ave-anl)*wfac

ccc    to calculate ACC scores
ccc    using climatelogical mean instead of medium
       clm = 0.0
       if (ib.gt.1) then
          do ii = 2, ib 
           clm = clm + clim(nxy,ii)/float(ib-1)
          enddo
       else                    !Add for single model as climate reference
           clm = clim(nxy,1)
       endif
 
       xy = xy + (ave-clm)*(anl-clm)*wfac
       xx = xx + (ave-clm)*(ave-clm)*wfac
       yy = yy + (anl-clm)*(anl-clm)*wfac

      enddo
 
      do ii = 1, imp1
       fit(ii) = fit(ii)*100.00/acwt1
      enddo
      do ii = 1, im     !relative position 
       !fir(ii) = fir(ii)*100.00/2.0/acwt
       fir(ii) = fir(ii)*100.00/acwt1
      enddo
      fsprd = sqrt(fsprd/float(im-1)/acwt)
      frmsa = sqrt(frmsa/acwt       )
      fmerr = fmerr/acwt
      fabse = fabse/acwt
      if (xx.eq.0.0.or.yy.eq.0.0) then
       faccs = 1.0
      else
       faccs = xy/sqrt(xx*yy)
      endif

      do ii = 1, imp1
       dists(ii) = fit(ii)
      enddo
      do ii = 1, im
       dists(ii+imp1) = fir(ii)
      enddo
      dists(1+imp1+im) = fsprd
      dists(2+imp1+im) = frmsa
      dists(3+imp1+im) = fmerr
      dists(4+imp1+im) = fabse
      dists(5+imp1+im) = faccs
 

      write (*,800) 
      write (*,801) (fit(ii),ii=1,imp1)         
      write (*,802) 
      write (*,801) (fir(ii),ii=1,im)         
      write (*,803) 
      write (*,801) fsprd,frmsa,fmerr,fabse,faccs         
  800 format('Histogram Distribution (Talagrand) (N+1)')
  802 format('Relative Position')
  803 format('Ensemble Spread and RMS Error of Ens. Mean')
  801 format(11f6.2)     
      write(*,*) 'DIST done!'     
      return
  999 print *, ' There is no data for this cycle (dist)!!!'
      return
      end

c      subroutine talagr1(n,m,en,ve,ital,irel,ave,xmed,sprd,rmsa,cnt)
      subroutine talagr1(n,m,en,ve,ital,relp,ave,xmed,sprd,rmsa,cnt)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                            C
C     USAGE: TO CALCULATE TALAGRAND HISTOGRAM FOR ENSEMBLE FORECASTS         C
C            ON ONE GRID POINT                                               C
C     CODE : F77 on IBMSP --- Yuejian Zhu (07/26/2004)                       C
C            3/15/2017  Binbin Zhou: Modified for non-continuos fields       C           
C                         (VIS, CLOUD, APCP, Reflectivity, etc)              C
C     INPUT: n    number of ensember forecasts                               C
C            m    number of ensember forecasts to be verify                  C
C            en   vector of n containning forecasts at gridpoint             C
C            ve   value of verification at gridpoint ( analysis )            C
C                                                                            C
C     OUTPUT: ital vector of n+1 containing zeroes except 1 for              C
C                  bin containing truth                                      C
C             irel vector of n containning the relative position             C
C                  between analysis and forecasts         
c             relp vector of n containning the relative position counts
C                  between analysis and forecasts 
C             ave  average of ensemble fcsts                                 C
C             xmed median of ensemble fcsts                                  C
C             sprd spread of ensemble fcsts                                  C
C             rmsa root mean square error for analysis and mean (ave)        C
C             cnt  count for effective points of relative position(irel)     C
C                                                                            C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c--------+----------+----------+----------+----------+----------+----------+--
      dimension en(n),em(m),enb(m,2),ena(m,3)
      dimension ital(m+1),irel(m), d(m)
      real relp(m) 
      relp=0.
      irel = 0
      ital = 0
      ave  = 0.0      
      xmed = 0.0             
      sprd = 0.0        
      rmsa = 0.0    

    
      do i = 1, m
       if (en(i).eq.-9999.99) goto 999
       em(i) = en(i)
      enddo
      
      nn=0
      do i = 1, m
       if(em(i).ne.0.) nn=nn+1
      end do
      !if nn =0 means all members are zero 
      if(nn.eq.0) then
       irel=0
       relp=0.
       ital=0
       cnt=0.
       return
      end if

      do i=1,m
       enb(i,1)=i
       enb(i,2)=em(i)
       ena(i,1)=i
       ena(i,2)=em(i)
       ital(i)=0
c ----
c calculate the average
c ----
       ave=ave+em(i)/float(m)
      enddo
c ----
c to calculate the spread
c ----
      do i=1,m
       sprd=sprd+(em(i)-ave)*(em(i)-ave)
      enddo
c ----
c to calculate the root mean square error for analysis and ensemble ave
c ----
      rmsa=(ve-ave)*(ve-ave)

      !!!write(*,'(14f8.2)') (em(i),i=1,m),ave,sprd,rmsa
c ----
c to order data
c ----
      call sortmm(ena,m,3,2)
c ----
c get relative position for analysis
c Binbin Note: original: 2 ends give "2", between give "1" respectively
c New design for relative position irel(:) array:
c
c Case 1: All members (m) is 0. (n=m)
c  if ve(obs)=0 or ve > 0 set 1/m for each mambers
c
c Case 2: Not all members =0., n of m members is 0 (n>0 but < m) 
c if ve=0, 1/n is set to those n memebrs 
c if ve>0, search for closet memebr and set 1 to irel for that member
c
c Case 3: No member is 0 (n=0) 
c if ve=0 set 1/m for each mambers
c if ve>0, search for closet memebr and set 1 to irel for that member
c
c For histgram: 
c Case 1, if (All members (m) is 0 and ve=0), skip
c        else not skip


      n0=0    !check how many members are 0 value
      do i = 1, m
       if (ena(i,2).eq.0.0) n0=n0+1
      end do
     
      if (n0.eq.m) then                  !All  m members are 0
        !relp(:)=1.0/m
         relp=0.
         cnt=0.
      else if (n0.gt.0.and.n0.lt.m) then  ! n of m memebrs are 0 
         if (ve.eq.0.0) then
           do i=1,m
            if (ena(i,2).eq.0.0) then
             !iii=ena(i,3)
             !relp(iii) = 1.0/n0
             relp=0.
             cnt=0.
            end if
           end do
         else
           ds=99999. 
           do i = 1, m
            d(i)=abs(ve-ena(i,2))
           end do
           do i=1,m
            if(d(i).lt.ds) then
             ds=d(i)
             iii=ena(i,3)
            end if
           end do
           relp(iii)=1.
           cnt=1.
         end if
       else                    ! if(n0=0) case: no members are 0. 
         if (ve.eq.0.) then
          relp(:)=1.0/m
          relp=0.
          cnt=0.
         else
          ds=99999.
           do i = 1, m
            d(i)=abs(ve-ena(i,2))
           end do
           do i=1,m
            if(d(i).lt.ds) then
             ds=d(i)
             iii=ena(i,3)
            end if
           end do
           relp(iii)=1.
           cnt=1.
         end if
       end if  

c        if (ve.le.ena(i,2)) then
c         if (i.eq.1) then
c          iii=ena(i,3)
c          irel(iii) = 2
c         else
c          iii=ena(i,3)
c          jjj=ena(i-1,3)
c          irel(iii) = 1
c          irel(jjj) = 1
c         endif
c         goto 100
c        endif
c       enddo
c       iii=ena(m,3)
c       irel(iii) = 2
c 100   continue
c       cnt=1.0
c      else
c       irel=0
c       ital=0
c       cnt=0.
c       return
c      end if 

c ----
c to calculate the talagrand histogram
c ----
      if (ve.ne.0.) then
       if (n0.ne.m) then !not all members are 0 
        do i=1,m
         if(ve.le.ena(i,2)) then
          ital(i)=1
          cnt=1.0
          goto 200
         endif
        enddo
         ital(m+1)=1
         cnt=1.
       else
        ital=0
        cnt=0.
       end if
      else
        ital=0
        cnt=0.
      end if 
 200  continue
c ----
c to calculate the median
c ----
      im=m/2
      if(mod(m,2).eq.1) then
       xmed=ena(im+1,2)
      else
       xmed=(ena(im,2)+ena(im+1,2))/2.
      endif
 999  continue

      return
      end


