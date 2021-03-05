
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine      DIST
c  Prgmmr: Yuejian Zhu           Org: np23          Date: 2004-08-01
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
      subroutine dist(fcst,fanl,clim,wght,len,ib,im)
c      parameter (mxlen=10512)             
c       parameter (mxlen=70000)             
      dimension fcst(len,im),fanl(len),clim(len,ib+1)
      dimension wght(len)
      dimension fst(im)
      dimension fit(im+1),it(im+1)
      dimension fir(im),ir(im)

      common /vscore/ infow(500),probs(500),dists(500)

      imp1 = im + 1
c ----
c to calculate talagrand histogram and N+1 distribution
c ----
      acwt = 0.0
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

      write(*,*) 'DIST:', len,ib,im
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
       enddo
       anl      = fanl(nxy)
       
       call talagr(im,im,fst,anl,it,ir,ave,xmed,sprd,rmsa)
c       if (anl.lt.20.0) then
c         write(*,*) 'In talagr: NXY=',nxy
c         write(*,*) 'fst=',fst
c         write(*,*) 'anl=',anl
c         write(*,*) 'ital=',it
c         write(*,*) 'ir=',ir  
c         write(*,*) 'ave=',ave
c         write(*,*) 'xmed=',xmed  
c         write(*,*) 'sprd=',sprd
c         write(*,*) 'rmsa=',rmsa  
c       end if   

       do ii = 1, imp1
        fit(ii) = fit(ii) + it(ii)*wfac  
       enddo
       do ii = 1, im
        fir(ii) = fir(ii) + ir(ii)*wfac 
       enddo

       fsprd = fsprd + sprd*wfac
       frmsa = frmsa + rmsa*wfac
       fmerr = fmerr + (ave-anl)*wfac
       fabse = fabse + abs(ave-anl)*wfac

c       if (mod(nxy,100).eq.0) then
c        write(*,*) 'nxy,fst,anl,sprd,rmsa=',nxy,fst,anl,sprd,rmsa
c        write(*,*) 'fsprd,frmsa=',fsprd,frmsa
c       end if

ccc    to calculate ACC scores
ccc    using climatelogical mean instead of medium
       clm = 0.0
       if (ib.gt.1) then
c         if (mod(ib,2).eq.0) then
c          mp=ib/2+1
c          clm=clim(nxy,mp)
c         else
c          mp=ib/2
c          clm=(clim(nxy,mp)+clim(nxy,mp+1))/2.0
c         endif
          do ii = 2, ib 
           clm = clm + clim(nxy,ii)/float(ib-1)
          enddo
       else                    !Add for single model as climate reference
           clm = clim(nxy,1)
       endif
 
c      if (nxy.eq.10000) then
c       print *, "clm=",(clim(nxy,ii),ii=1,ib+1)
c       print *, "clm=",clm, " anl=",anl
c      endif

       xy = xy + (ave-clm)*(anl-clm)*wfac
       xx = xx + (ave-clm)*(ave-clm)*wfac
       yy = yy + (anl-clm)*(anl-clm)*wfac

      enddo
 
      do ii = 1, imp1
       fit(ii) = fit(ii)*100.00/acwt
      enddo
      do ii = 1, im
       fir(ii) = fir(ii)*100.00/2.0/acwt
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

      subroutine talagr(n,m,en,ve,ital,irel,ave,xmed,sprd,rmsa)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                            C
C     USAGE: TO CALCULATE TALAGRAND HISTOGRAM FOR ENSEMBLE FORECASTS         C
C            ON ONE GRID POINT                                               C
C     CODE : F77 on IBMSP --- Yuejian Zhu (07/26/2004)                       C
C                                                                            C
C     INPUT: n    number of ensember forecasts                               C
C            m    number of ensember forecasts to be verify                  C
C            en   vector of n containning forecasts at gridpoint             C
C            ve   value of verification at gridpoint ( analysis )            C
C                                                                            C
C     OUTPUT: ital vector of n+1 containing zeroes except 1 for              C
C                  bin containing truth                                      C
C             irel vector of n containning the relative position             C
C                  between analysis and forecasts                            C
C             ave  average of ensemble fcsts                                 C
C             xmed median of ensemble fcsts                                  C
C             sprd spread of ensemble fcsts                                  C
C             rmsa root mean square error for analysis and mean (ave)        C
C                                                                            C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c--------+----------+----------+----------+----------+----------+----------+--
      dimension en(n),em(m),enb(m,2),ena(m,3)
      dimension ital(m+1),irel(m)
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
c Binbin Note: 2 ends give "2", between give "1" respectively 
c ----
      do i = 1, m
       if (ve.le.ena(i,2)) then
        if (i.eq.1) then
         iii=ena(i,3)
         irel(iii) = 2
        else
         iii=ena(i,3)
         jjj=ena(i-1,3)
         irel(iii) = 1
         irel(jjj) = 1
        endif
        goto 100
       endif
      enddo
      iii=ena(m,3)
      irel(iii) = 2
 100  continue
c ----
c to calculate the talagrand histogram
c ----
      do i=1,m
       if(ve.le.ena(i,2)) then
        ital(i)=1
        goto 200
       endif
      enddo
      ital(m+1)=1
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



      subroutine sortmm(a,n,nc,k)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                    C
C     USAGE: SORT ONE DIMENSION DATA WITH LENGTH N                   C
C     CODE : F77 on IBMSP --- Yuejian Zhu (07/08/99)                 C
C                                                                    C
C     INPUT: array a(n,nc)                                           C
C            n  -- input data length                                 C
C            nc -- variable second dimension (using nc=3)            C
C            k  -- input data location a(*,k)                        C
C                                                                    C
C     OUTPUT: re-order array a(n,nc)                                 C
C            a(n,1) -- original data (if k=1)                        C
C            a(n,2) -- new output after sorting (low -> high)        C
C            a(n,3) -- new output at original order index            C
C                                                                    C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
c--------+----------+----------+----------+----------+----------+----------+--
      dimension a(n,nc),b(n,nc),js(n)
      do i1 = 1, n
      iless = 0
      imore = 0
      ieq   = 0
      aa=a(i1,k)
      do i2 = 1, n
       bb=a(i2,k)
       if ( aa.lt.bb ) iless = iless + 1
       if ( aa.gt.bb ) imore = imore + 1
       if ( aa.eq.bb ) then
          ieq   = ieq   + 1
          js(ieq) = i2
       endif
      enddo
       if ( ieq.eq.1) then
          b(imore+1,2)=aa
          b(imore+1,1)=i1
       else
        do i3 = 1, ieq
          b(imore+i3,2)=aa
          b(imore+i3,1)=js(i3)
        enddo
       endif
      enddo
      do jj= 1, n
        a(jj,3) = b(jj,1)
        a(jj,2) = b(jj,2)
      enddo
      return
      end

