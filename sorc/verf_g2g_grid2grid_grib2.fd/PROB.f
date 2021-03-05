
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine      PROB(fcst,fanl,clim,wght,len,ib,im,cscrf)
c  Prgmmr: Yuejian Zhu           Org: np23          Date: 2004-08-01
c          Y. Zhu adding CRPS decomposition - 2008-02-15
c          B. Zhou adding specified a set of fixed thresholds to
c              compute reliability diagram  - 2014-01-09
c
c This is main ensemble based probabilistic verification sub-program
c         for NAEFS project on IBM-SP
c
c  For:   1. Reliability for m+1 probabilities ( m=members )
c         2. Ranked probability score ( and skill scores)
c         3. Brier score ( and Brier skill score )
c         4. Hit rate and false alarm rate
c         5. Resolution and reliability (option)
c         6. Economic values (option)
c         7. Information content (option)
c
c   subroutines                                                    
c              EVALUE ---> to calculate the economic value
c   
c   parameters:
c      len   -- total grid points/observations
c      im    -- ensemble members
c      ib    -- ib>=2: climatologically equally-a-likely-bin 
c               ib=1: climatological based, specified range consideration
c                 Binbin add: specified a set of fixed thresholds to compute
c                             reliability plot with (FHO> or FHO<) 
c               ib=-1: no climatology, specified value consideration
c      scrf  -- sample climatological relative frequence
c
c  1/10/2014, B.Zhou add: specified thresholds: 
c     op -- FHO operator (FHO>, FHO<, FHO=, FHO~
c     thrs -- thresholds array
c     nthrs -- number of thresholds:
c              1: for one specfied threshold (FHO>, FH<, FHO=)
c              3 and larger: range (bin) of threshold, number of bins is number thresholds + 1  
c              nthrs can not be 2 
c             
c   Fortran 77 on IBMSP
c
C--------+---------+---------+---------+---------+----------+---------+--
      subroutine prob(fcst,fanl,clim,wght,len,ib,im,cscrf,
     +   op, thrs, nthrs)
c      parameter (mxlen=10512)
c      parameter  (mxlen=70000)
      dimension fcst(len,im),fanl(len),clim(len,ib+1),wght(len)
      dimension fcp(ib),anp(ib)
      dimension sfcp(im+1),sanp(im+1),fp(im+1)
      dimension bsf(ib+1),bsc(ib+1),bss(ib+1)
      dimension fyy(im+1),fyn(im+1)
      dimension hra(im+1),fal(im+1)
      dimension fhk(ib)
      dimension fv(18)             ! 18 ratios has been set by subroutine
      dimension rp(im+1,2)
      dimension sp(ib,2),yy(im+1),yn(im+1)
      dimension fs(im),cl(ib+1),clrps(ib-1)
      dimension af(0:im),bt(0:im)
      dimension aaf(0:im),abt(0:im)
      dimension ag(0:im),ao(0:im),pp(0:im)
      character*10 op
      real thrs(20),pt(ib+1)   !Added by B.Zhou 
      integer nthrs      

      common /vscore/ infow(500),probs(500),dists(500)             

      Write(*,*) 'In PROB:',len,ib,im
      write(*,*) 'op ', op
      write(*,*) 'thrs', (thrs(i),i=1,nthrs)
      write(*,*) 'nthrs', nthrs
CCCCCC test print out
      write(*,444) fanl(720),(fcst(720,n),n=1,im)
      write(*,444) (clim(720,n),n=1,ib+1)
 444  format (11(f7.1))

      imp1=im+1
c ----
c     to creat climate based equally-likely-bin or specified values
c ----

      sanp = 0.0
      sfcp = 0.0
      bsf  = 0.0
      bsc  = 0.0
      rpfs = 0.0
      rpfc = 0.0
      fyy  = 0.0
      fyn  = 0.0
      crpsf= 0.0
      crpsc= 0.0
      crpsn= 0.0
      crpss= 0.0
      aaf  = 0.0
      abt  = 0.0
      bspec = cscrf 
c     print *, 'bspec=',bspec
      fhk  = 0.0
      ftl  = 0.0

      do nxy = 1, len  
       do i = 1, im
        fs(i) = fcst(nxy,i)
        if (fs(i).eq.-9999.99) go to 999
       enddo
       fa=fanl(nxy)
       if (fa.eq.-9999.99) go to 999

       do i = 1, ib+1
         cl(i) = clim(nxy,i)
       enddo

       if(nthrs.le.0) then
        do i = 1, ib+1
         pt(i) = clim(nxy,i)
        enddo
       else
        do i = 1,nthrs 
            pt(i) = thrs(i)
        end do
       end if

       wfac = wght(nxy)

c      print *, '++++++++++++++++++++++++++++++++++++'
c       print *, '        RELIABILITY DIAGRAM '
c B.Zhou: for non-climatology data reference case, use specified thresholds pt()
c         for climatology data as reference case, use climatology 10 bins  
c      print *, '++++++++++++++++++++++++++++++++++++'

       

       if(nthrs.ge.3) then                     !threshold range case (FHO~),at least 2 bins t1~t2~t3 (3 thresholds)
         call reli(fs,im,fa,pt,nthrs-1,rp,irt,nthrs,op)
       elseif (nthrs.eq.1) then                !0ne threshold (FHO> or FHO< or FHO=)case
          call reli(fs,im,fa,pt,1,rp,irt,nthrs,op)
       elseif (nthrs.le.0) then                !climatology data as ref case
         call reli(fs,im,fa,cl,ib,rp,irt,nthrs,op)
       else
         write(*,*) 'Number of thresholds is wrong, 1 or  at least 3'
         stop 1001
       end if

       !write(*,*) 'pt=',(pt(k),k=1,nthrs)
       !write(*,*) 'analy',(rp(k,2),k=1,imp1)
       !write(*,*) 'fcst ',(rp(k,1),k=1,imp1)

       do np = 1, imp1
        sanp(np) = sanp(np) + rp(np,2)*wfac
        sfcp(np) = sfcp(np) + rp(np,1)*wfac
       enddo

c      print *, '++++++++++++++++++++++++++++++++++++'
c       print *, '        BRIER SCORE        '
c      print *, '++++++++++++++++++++++++++++++++++++'

c       if(nthrs.ge.3) then 
c         call brsc(fs,im,fa,pt,nthrs-1,bspec,sp,irt,nthrs,op)
c         do nb = 1, nthrs-1
c          bsf(nb) = bsf(nb) + sp(nb,1)*wfac
c          bsc(nb) = bsc(nb) + sp(nb,2)*wfac
c         enddo

c       elseif (nthrs.eq.1) then
c         call brsc(fs,im,fa,pt,1,bspec,sp,irt,nthrs,op)
c         bsf(1) = bsf(1)+sp(1,1)*wfac
c         bsc(1) = bsc(1)+sp(1,2)*wfac
c       elseif(nthrs.le.0) then

        call brsc(fs,im,fa,cl,ib,bspec,sp,irt)
        do nb = 1, ib
         bsf(nb) = bsf(nb) + sp(nb,1)*wfac
         bsc(nb) = bsc(nb) + sp(nb,2)*wfac
        enddo

c       else
c        write(*,*) 'Number of thresholds is wrong, at least 3'
c        stop 1002
c       end if

c      print *, '++++++++++++++++++++++++++++++++++++'
c       print *, '       RANKED PROBABILITY SCORE    '
c      print *, '++++++++++++++++++++++++++++++++++++'

       call rrps(fs,im,fa,cl,ib,bspec,rpsf,rpsc,irt,nthrs,op)

       rpfs = rpfs + rpsf*wfac
       rpfc = rpfc + rpsc*wfac

c      print *, '++++++++++++++++++++++++++++++++++++'
c       print *, ' Continuous Ranked Probability Score'
c      print *, '++++++++++++++++++++++++++++++++++++'

       call crps(fs,im,fa,crpsff,0,irt)
       call crps(fs,im,fa,crpsfn,1,irt)
       if (ib.ne.1) then
        do nb = 1, ib-1
         clrps(nb)=cl(nb+1)
        enddo
        call crps(clrps,ib-1,fa,crpscc,0,irt)
       else
ccc    the crps is approximately in this calculation
        clrps(1)=cl(1)
        clrps(2)=cl(2)
        call crps(clrps,2,fa,crpscc,0,irt)
       endif

c      print *, 'crpsff=',crpsff,' crpscc=',crpscc
       crpsf = crpsf + crpsff*wfac
       crpsc = crpsc + crpscc*wfac
       crpsn = crpsn + crpsfn*wfac
       crpss = crpss + wfac*(crpscc-crpsff)/crpscc

c      print *, '++++++++++++++++++++++++++++++++++++'
c       print *, '  CRPS decomposition if m > 1       '
c      print *, '++++++++++++++++++++++++++++++++++++'

       call crpsd(fs,im,fa,af,bt,irt)
       do n = 0, im
        aaf(n) = aaf(n) + af(n)*wfac
        abt(n) = abt(n) + bt(n)*wfac
       enddo

c      print *, '++++++++++++++++++++++++++++++++++++'
c      print *, '  HIT RATE AND FALSE ALARM RATE     '
c      print *, '++++++++++++++++++++++++++++++++++++'

       
       if(nthrs.ge.3) then
         call hrfa(fs,im,fa,pt,nthrs-1,yy,yn,irt)
       elseif(nthrs.eq.1) then
         call hrfa(fs,im,fa,pt,1,yy,yn,irt)
       elseif (nthrs.le.0) then
         call hrfa(fs,im,fa,pt,ib,yy,yn,irt)
       else
         write(*,*) 'Number of thresholds is wrong, at least 3'
         stop 1003
       end if 

       do np = 1, imp1
        fyy(np) = fyy(np) + yy(np)*wfac
        fyn(np) = fyn(np) + yn(np)*wfac
       enddo

c     print *, '++++++++++++++++++++++++++++++++++++'
c      print *, '     HEIDKE SKILL SCORE if ib=3     '
c     print *, '++++++++++++++++++++++++++++++++++++'
 
       if (ib.eq.3) then
        call fapb(fst,m,obs,clm,ib,fcp,anp,irt)

        ftl = ftl + im*wfac
        do nb = 1, ib
         do np = 1, imp1
          if (fcp(nb).eq.float(np-1)) then
           if (anp(nb).eq.1.) then
            fhk(nb) = fhk(nb)+(np-1)*wfac
           endif
          endif
         enddo
        enddo
       endif

      enddo

C     print *, '++++++++++++++++++++++++++++++++++++'
c      print *, '     RELIABILITY DIAGRAM OUTPUT     '
C     print *, '++++++++++++++++++++++++++++++++++++'

C
C     Brier scores could be calculated from reliability diagram
C
      do np = 1, imp1
       if (sfcp(np).gt.0.1E-10) then
        fp(np) = (sanp(np)/sfcp(np))*100.0
       else
        fp(np) = 0.0
       endif
      enddo

      do ii = 1, imp1
       probs(ii)      = sanp(ii)
       probs(ii+imp1) = sfcp(ii)
      enddo

c     write (*,801) (sanp(ii),ii=1,im+1)         
c     write (*,801) (sfcp(ii),ii=1,im+1)         
c     write (*,801) (fp(ii),ii=1,im+1)         

C
C     Option: use reliability diagram to calculate information content
C      only for ib > 1 ?
      csfcp = 0.0
      cinfm = 0.0
      do np = 2, imp1
       csfcp = csfcp + sfcp(np)
       if (fp(np).gt.0.0.and.fp(np).lt.100..and.ib.gt.1) then
        xrate1 = fp(np)/100.0
        xrate2 = (1.0-xrate1)/float(ib-1)
        xinf = xrate1*alog10(xrate1)/alog10(float(ib)) 
     .       + (ib-1.0)*xrate2*alog10(xrate2)/alog10(float(ib))
        cinfm = cinfm + xinf*sfcp(np)
c      else
c      how about fp=0.0?
c       xrate  = fp(np)/100.0
c       xinf = xrate*alog10(xrate)/alog10(float(ib))
c       cinfm = cinfm + xinf*sfcp(np)
       endif
      enddo
      cinfm = 1.0 + cinfm/csfcp

      probs(14+4*imp1)= cinfm

c     print *, "Information Content: ", cinfm 

C
C     Option: use reliability diagram to calculate rel, res, and unc
c
c     Calculate the BS = reliability - resolution + uncertainty
c     --- Reference: Dianiel S. Wilks book, chapter 7 of
c        <<Statistical Methods in the Atmospheric Science>>
c        p262, equation (7.28)
c        p246, table 7.3
c
      tfst  = 0.0
      do np = 1, imp1
       tfst = tfst + sfcp(np)
      enddo

      creli = 0.0
      creso = 0.0
      cunce = 0.0

      do np = 1, imp1
       if (sfcp(np).ne.0.0) then
        cdis  = (np-1.0)/(imp1-1.0)-sanp(np)/sfcp(np)   ! distance for reli
        creli = creli + sfcp(np)/tfst * cdis*cdis       ! weight=sfc/tfst
        cdis  = sanp(np)/sfcp(np) - cscrf               ! distance for reso
        creso = creso + sfcp(np)/tfst * cdis*cdis       ! weight=sfc/tfst
       endif
      enddo

      cunce = cscrf*(1.0 - cscrf)
      cbss  = (creso - creli)/cunce
      cbs   = creli - creso + cunce

      probs(10+4*imp1) = creli
      probs(11+4*imp1) = creso
      probs(12+4*imp1) = cunce
      probs(13+4*imp1) = cscrf

c     print *, 'Reliability =',creli
c     print *, 'Resolution  =',creso
c     print *, 'Uncetainty  =',cunce
c     print *, 'S. Cli. Rel.=',cscrf
c     print *, 'BS          =',cbs 
c     print *, 'BSS         =',cbss  

c     Notes for ib=1:
c     when using bspec for specified case
c     usually, cbss (from decomposition) is not equal to bss (from Brier scores)
c     the different comes from bspec (pre-defined, not true in fact)
c     if we use cscrf instead of bspec when calculate Brier score
c     Then, the result will be the same.
c     therefore, BSS (from decomposition) is the right one.
c        --- Yuejian Zhu (08/31/2004)

c     print *, '++++++++++++++++++++++++++++++++++++'
c      print *, '        BRIER SCORES OUTPUT         '
c     print *, '++++++++++++++++++++++++++++++++++++'

      bsfa = 0.0
      bsca = 0.0
      do nb = 1, ib
       bsf(nb) = bsf(nb)/float(len)
       bsfa    = bsfa+bsf(nb)
       bsc(nb) = bsc(nb)/float(len)
       bsca    = bsca+bsc(nb)
       if (bsc(nb).ne.0.0) then
        bss(nb) = (bsc(nb)-bsf(nb))/bsc(nb)
        if (bss(nb).le.-10.0) bss(nb)=-9.99
       endif
      enddo
      bsfa = bsfa/float(ib)
      bsca = bsca/float(ib)
      if (bsca.ne.0.0) then
       bssa = (bsca-bsfa)/bsca
       if (bssa.le.-10.0) bssa=-9.99
      endif
      bsf(ib+1) = bsfa
      bsc(ib+1) = bsca
      bss(ib+1) = bssa

      probs(7+4*imp1) = bsfa 
      probs(8+4*imp1) = bsca 
      probs(9+4*imp1) = bssa 
ccc   adding bss for 10% and 90% (extreme forecast)
      probs(16+4*imp1) = bss(1)
      probs(17+4*imp1) = bss(ib)

c     write (*,802) bsfa,bsca,bssa               

c     print *, 'BSf         =',bsfa  
c     print *, 'BSc         =',bsca  
c     print *, 'BSS         =',bssa  

c     print *, '++++++++++++++++++++++++++++++++++++'
c      print *, ' RANKED PROBABILITY SCORES OUTPUT   '
c     print *, '++++++++++++++++++++++++++++++++++++'

      rpfs = rpfs/float(len)
      rpfc = rpfc/float(len)
      rpss = (rpfs-rpfc)/(1.0-rpfc)

      probs(1+4*imp1) = rpfs
      probs(2+4*imp1) = rpfc
      probs(3+4*imp1) = rpss 

c     write (*,802) rpfs,rpfc,rpss             

c     print *, '++++++++++++++++++++++++++++++++++++'
c      print *, ' Continuous Ranked Probability Score'
c     print *, '++++++++++++++++++++++++++++++++++++'

      crpsf = crpsf/float(len)
      crpsc = crpsc/float(len)
      crpss = crpss/float(len)
      crpsn = crpsn/float(len)
      crpss0= (crpsf-crpsc)/(0.0-crpsc)

      probs(4+4*imp1) = crpsf
      probs(5+4*imp1) = crpsc
c     probs(6+4*imp1) = crpss 
c     probs(4+4*imp1) = crpsn
c     probs(5+4*imp1) = crpss
      probs(6+4*imp1) = crpss0 

c     write (*,802) crpsf,crpsc,crpss             

c     print *, '++++++++++++++++++++++++++++++++++++'
c      print *, ' CRPS decomposition if m >= 1       '
c     print *, '++++++++++++++++++++++++++++++++++++'

      do n = 0, im
       aaf(n) = aaf(n)/float(len)
       abt(n) = abt(n)/float(len)
       ag(n)  = aaf(n) + abt(n)
       if (ag(n).eq.0.0) then
ccc     this is very special case, normaly ag is not equal to zero
        ao(n) = float(n)/float(im)
       else
        ao(n)  = abt(n)/ag(n)              ! ao <= 1.0
       endif
       pp(n)  = float(n)/float(im)
      enddo
       crprel = 0.0
       crpres = 0.0
       crpunc = 0.0
ccc    total distances (agt)
       agt = 0.0
       do n = 0, im
        agt = agt + ag(n)
       enddo
ccc
ccc   standard calculation from original definition
ccc
      do n = 0, im
       crprel = crprel + ag(n)*(ao(n)-pp(n))**2
       crpres = crpres + ag(n)*ao(n)**2 
       crpunc = crpunc + ag(n)*ao(n) 
      enddo
ccc
ccc   normalized by average distance (with weights)
ccc
c     do n = 0, im
c      crprel = crprel + ag(n)*(ao(n)-pp(n))**2 / agt
c      crpres = crpres + ag(n)*ao(n)**2 / agt
c      crpunc = crpunc + ag(n)*ao(n) /agt 
c     enddo
ccc
ccc   no weights (dimensionless, in probabilistic space, question???)
ccc
c     do n = 0, im
c      crprel = crprel + (ao(n)-pp(n))**2 / float(im+1)
c      crpres = crpres + ao(n)**2 / float(im+1)
c      crpunc = crpunc + ao(n) / float(im+1)
c     enddo

      probs(36+4*imp1) = crprel
      probs(37+4*imp1) = crpres
      probs(38+4*imp1) = crpunc

      write (*,802) crprel,crpres,crpunc

c     print *, '++++++++++++++++++++++++++++++++++++'
c      print *, '    HIT RATE, FALSE ALARM OUTPUT    '
c     print *, '++++++++++++++++++++++++++++++++++++'

      tnyy = .0
      tnyn = .0
      do np = 1, imp1
       tnyy = tnyy + fyy(np)    ! total analysis/observation
       tnyn = tnyn + fyn(np)
      enddo
      do np = 1, imp1  
       fnoh  = 0.0
       fnof  = 0.0
c      this is accumulative hit rate and false alarm rate
c      hr = h/(h+m), fa=f/(f+c)
       do npt = np, imp1
        fnoh = fnoh + fyy(npt)
        fnof = fnof + fyn(npt)
       enddo
       if (tnyy.ne.0.0) then
        hra(np) = fnoh/tnyy
       endif
       if (tnyn.ne.0.0) then
        fal(np) = fnof/tnyn
       endif
      enddo

      do np = 1, imp1
       probs(np+imp1*2) = hra(np)
       probs(np+imp1*3) = fal(np)
      enddo

c     write (*,802) (hra(np),np=1,imp1)           
c     write (*,802) (fal(np),np=1,imp1)           

c     print *, '++++++++++++++++++++++++++++++++++++'
c     print *, '    HEIDKE SKILL SCORES   OUTPUT    '
c     print *, '++++++++++++++++++++++++++++++++++++'

      hkss = -99.99
      if (ib.eq.3) then
       tfhk = 0.0
       do nb = 1, ib
        tfhk = tfhk + fhk(nb)
       enddo
       hkss = (tfhk - ftl/float(ib))/(ftl - ftl/float(ib))
c      print*, "HEIDKE SKILL SCORES = ",hkss
      endif
      probs(15+4*imp1) = hkss

C     
C     Option: to calculate economic value by use hit rate and false alarm
C
      write(*,*)'In PROB: hra,fal,imp1,cscrf=',
     + hra,fal,imp1,cscrf  
      call evalue(hra,fal,imp1,cscrf,fv)
       write(*,*)'In PROB: hra,fal,imp1,cscrf,fv=',
     + hra,fal,imp1,cscrf,fv

      do kk = 1, 18
       probs(kk+17+4*imp1)= fv(kk)
      enddo

c     print *, "==== Economic Values ===="
c     print *, "C   1.00    1.00    1.00    1.00    1.00    1.00    1.00
c    *    1.00    1.00"
c     print *, "--------------------------------------------------------
c    *----------------"
c     print *, "L   1.05    1.10    1.25    1.50    2.00    3.00    5.00
c    *    8.00    10.0"
c     print *
c     print "(' ',9f8.4)", (fv(kk),kk=1,9)
c     print *
c     print *, "C   1.00    1.00    1.00    1.00    1.00    1.00    1.00
c    *    1.00    1.00"
c     print *, "--------------------------------------------------------
c    *----------------"
c     print *, "L   18.0    27.0    40.0    60.0    90.0    140.    210.
c    *    350.    500."
c     print *
c     print "(' ',9f8.4)", (fv(kk),kk=10,18)

  800 format(' The Verification Scores for Region ',
     .       'la1=',i3,'la2=',i3,' lo1=',i3,'lo2=',i3)
  801 format(11f6.0)     
  802 format(11f6.3)     

      write(*,*) 'CALL PROB doen!'

      return   
 1020 print *, "there is a error to read namelist!!!"
      return
  999 print *, "there is no data for this cycle (PROB)!!!"
      end


c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine      RELI(fst,m,obs,clm,ib,rp,irt)
c  Prgmmr: Yuejian Zhu           Org: np23          Date: 2006-12-26
c     1/10/2014   MODIFY: B. Zhou, added specified thresholds for ib=1 case
c                   if nthrs=1, use specified one threshold 
c                   if nthrs>=3, 
c                         reliability is computed in different ranges
c                         between these thresholds:
c                         pt(1)~t(2), pt(2)~pt(3), ....,pt(nthrs-1)~pt(nthrs)
c     
c  This is the subroutine to calculete Contengent Rank Probability Scores
c         for NAEFS project on IBM-SP
c
c   parameters:
c      fst   -- forecasts for m ensemble members
c      m     -- ensemble members
c      obs   -- analysis or observation, or climatology (truth)                
c      clm   -- climatological data
c      ib    -- climatological data dimension (equally-a-likely-bin)
c      rp    -- reliability (self-category: m+1) 
c      irt   -- 0-successful
c               10-need to review ensemble forecast data
c
c    B.Zhou:
c      op   -- operator (FHO> or FHO<, but it is not used), input
c      thrs -- threshold array, input
c      nthrs -- number of thresholds, input

c   Fortran 77 on IBMSP
c
C--------+---------+---------+---------+---------+----------+---------+--
      subroutine reli(fst,m,obs,clm,ib,rp,irt,nthrs,op)
      dimension fst(m),clm(ib+1),rp(m+1,2) 

      character*10 op
      real thrs(20)
      integer nthrs      

c     print *, 'm=',m,' ib=',ib
c     write (*,'(26f6.0)') (fst(i),i=1,m),obs,(clm(i),i=1,ib+1)
      irt=0
      rp=0.0
      icnt=0

      if (ib.eq.1) then
       IF(nthrs.le.0) THEN
         do n = 1, m
          if (fst(n).gt.clm(1)) then
           icnt=icnt+1          
          endif
         enddo
         rp(icnt+1,1)=1.0
         if (obs.gt.clm(1)) then
          rp(icnt+1,2)=1.0
         endif
       ELSEIF(nthrs.eq.1) THEN                 !Added by B.Zhou 1/10/2014
           do n = 1, m
            if(trim(op).eq.'FHO>') then
              if (fst(n).ge.clm(1)) icnt=icnt+1
            elseif(trim(op).eq.'FHO=') then
              if (fst(n).eq.clm(1)) icnt=icnt+1
            elseif(trim(op).eq.'FHO<') then
              if (fst(n).le.clm(1)) icnt=icnt+1
            else
              write(*,*) 'Wrong setting for FHO'
              stop 222
            end if
           enddo
           rp(icnt+1,1)=1.0
           if(trim(op).eq.'FHO>') then
              if (obs.ge.clm(1)) rp(icnt+1,2)=1.0
           elseif(trim(op).eq.'FHO=') then
              if (obs.eq.clm(1)) rp(icnt+1,2)=1.0
           elseif (trim(op).eq.'FHO<') then
              if (obs.le.clm(1)) rp(icnt+1,2)=1.0
           else
              write(*,*) 'Wrong setting for FHO'
              stop 222
           end if

       ENDIF

      elseif (ib.eq.0) then

       do n = 1, m
        if (fst(n).gt.clm(1)) then
         icnt=icnt+1
        endif
       enddo
       rp(icnt+1,1)=1.0
       if (obs.gt.clm(1)) then
        rp(icnt+1,2)=1.0
       endif

      elseif (ib.eq.-1) then
       do n = 1, m
        if (fst(n).le.clm(1)) then
         icnt=icnt+1
        endif
       enddo
       rp(icnt+1,1)=1.0
       if (obs.gt.clm(1)) then
        rp(icnt+1,2)=1.0
       endif

      elseif (ib.ge.2) then
ccc for example: ib=2 2 equally-a-likely-bin
       icnt=0
       do n = 1, m
        if (fst(n).le.clm(2)) then
         icnt=icnt+1
        endif
       enddo
       rp(icnt+1,1)=rp(icnt+1,1)+1.0
       if (obs.le.clm(2)) then
        rp(icnt+1,2)=rp(icnt+1,2)+1.0
       endif

       icnt=0
       do n = 1, m
        if (fst(n).gt.clm(ib)) then
         icnt=icnt+1
        endif
       enddo
       rp(icnt+1,1)=rp(icnt+1,1)+1.0
       if (obs.gt.clm(ib)) then
        rp(icnt+1,2)=rp(icnt+1,2)+1.0
       endif

ccc FHO~ case
       if (ib.gt.2) then
       do i = 2, ib-1
        icnt=0
        do n = 1, m
         if (fst(n).gt.clm(i).and.fst(n).le.clm(i+1)) then
          icnt=icnt+1
         endif
        enddo
        rp(icnt+1,1)=rp(icnt+1,1)+1.0
        if (obs.gt.clm(i).and.obs.le.clm(i+1)) then
         rp(icnt+1,2)=rp(icnt+1,2)+1.0
        endif
       enddo
       endif
      else
       irt=99
      endif

      return   
      end


c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine      BRSC(fst,m,obs,clm,ib,bspec,sp,irt)
c  Prgmmr: Yuejian Zhu           Org: np23          Date: 2006-12-26
c
c  This is the subroutine to calculete Brier Score                       
c         for NAEFS project on IBM-SP
c
c   parameters:
c      fst   -- forecasts for m ensemble members
c      m     -- ensemble members
c      obs   -- analysis or observation, or climatology (truth)                
c      clm   -- climatological data
c      ib    -- climatological data dimension (equally-a-likely-bin)
c      bspec -- sample climatological frequence
c      sp    -- Brier Score
c      irt   -- 0-successful
c               10-need to review ensemble forecast data
c
c   Fortran 77 on IBMSP
c   Called subroutine: fapb   
c
C--------+---------+---------+---------+---------+----------+---------+--
      subroutine brsc(fst,m,obs,clm,ib,bspec,sp,irt)

      dimension fst(m),clm(ib+1) 
      dimension sp(ib,2),fcp(ib),anp(ib)

      call fapb(fst,m,obs,clm,ib,fcp,anp,irt)

c      print *, 'bspec=',bspec
c     write (*,'(20f3.0)') fcp,anp

      irt=0
      sp=0.0
      do nb = 1, ib

       if (anp(nb).ne.0.0) then
        ei = 1.0
       else
        ei = 0.0
       endif
      
       sf = (ei-fcp(nb)/float(m))**2
       if (ib.ge.2) then
        sc = (ei-1.0/float(ib))**2
       elseif (ib.eq.1) then
        sc = (ei-bspec)**2
       else
        sc = (ei-bspec)**2
       endif
       sp(nb,1) = sp(nb,1) + sf
       sp(nb,2) = sp(nb,2) + sc
      enddo     ! nb

      return   
      end


c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine      RRPS(fst,m,obs,clm,ib,bspec,rpsf,rpsc,irt)
c  Prgmmr: Yuejian Zhu           Org: np23          Date: 2006-12-26
c
c  This is the subroutine to calculete Ranked Probability Score          
c         for NAEFS project on IBM-SP
c
c   parameters:
c      fst   -- forecasts for m ensemble members
c      m     -- ensemble members
c      obs   -- analysis or observation, or climatology (truth)                
c      ib    -- climatological equally-a-likely-bin
c      bspec -- sample climatological frequence
c      rpsf  -- ranked probability scores for forecast
c      rpsc  -- ranked probability scores for climatology
c      irt   -- 0-successful
c               10-need to review ensemble forecast data
c
c   Fortran 77 on IBMSP
c   Called subroutine: fapb  
c
C--------+---------+---------+---------+---------+----------+---------+--
      subroutine rrps(fst,m,obs,clm,ib,bspec,rpsf,rpsc,irt)
      dimension fst(m),clm(ib+1) 
      dimension fcp(ib),anp(ib)

      call fapb(fst,m,obs,clm,ib,fcp,anp,irt)

      irt=0
      rpsf   = .0
      rpsc   = .0
      do nb = 1, ib
       sfcbs = .0
       sfcbc = .0
       sanbs = .0
       do ii = 1, nb
        fcbs  = fcp(ii)
        fcbc  = bspec
        anbs  = anp(ii)
        sfcbs = sfcbs+(fcbs/float(m))
        sfcbc = sfcbc+fcbc
        sanbs = sanbs+anbs
       enddo
       rpsf = rpsf+(sfcbs-sanbs)**2
       rpsc = rpsc+(sfcbc-sanbs)**2
      enddo
      if (ib.ge.2) then
       rpsf  = 1.0-(1.0/float(ib-1))*rpsf
       rpsc  = 1.0-(1.0/float(ib-1))*rpsc
      elseif (ib.eq.1) then
c --- try this way for one specification??? (no ranked)
       rpsf  = 1.0 - rpsf
       rpsc  = 1.0 - rpsc
      else
c --- try this way, (no ranked)
       rpsf  = 1.0 - rpsf
       rpsc  = 1.0 - rpsc
      endif

      return   
      end


c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine      CRPS(fst,m,obs,ccrps,ictl,irt)
c  Prgmmr: Yuejian Zhu           Org: np23          Date: 2006-12-26
c
c  This is the subroutine to calculete Continuous Ranked Probability Score
c         for NAEFS project on IBM-SP
c
c   parameters:
c      fst   -- forecasts for m ensemble members
c      m     -- ensemble size   
c      obs   -- analysis or observation, or climatology (truth)                
c      ccrps -- crp scores (local normalize score: 0 - 1.0)
c                          (standard score: 0 - infinite  )
c      ictl  -- =1 (make local normalize score)              
c      irt   -- 0-successful
c               10-need to review ensemble forecast data
c
c   Fortran 77 on IBMSP
c   Called subroutine: sortmm
c
C--------+---------+---------+---------+---------+----------+---------+--
      subroutine crps(fst,m,obs,ccrps,ictl,irt)
      dimension fst(m),ena(m,3)
      
      irt=0
      k=0
      ta=0.0
      ccrps=0.0
      if (m.le.0) goto 999
c     specific case if m=1
      if (m.eq.1) then
       ta=abs(fst(1)-obs)
       tds=ta
       goto 899
      endif
      do n = 1, m
       ena(n,1)=fst(n)
      enddo
ccc call sortmm to re-arrange the data from low to high
      call sortmm(ena,m,3,1)
c     print *, (ena(i,1),i=1,m)
c     print *, (ena(i,2),i=1,m)
c     print *, (ena(i,3),i=1,m)
      do n = 1, m
       if (obs.lt.ena(n,2)) goto 100
       k=n
      enddo
 100  continue
      if (m.gt.1) then
ccc left end point (approximately)
       elhf=(ena(2,2)-ena(1,2))/2.0
       elend=ena(1,2)-elhf                    
ccc right end point (approximately)
       erhf=(ena(m,2)-ena(m-1,2))/2.0
       erend=ena(m,2)+erhf                          
      else
       elend=ena(1,2)
       erend=ena(1,2)
      endif
      if (obs.gt.elend.and.obs.lt.erend) then
       tds=erend-elend                   
      else
       if (obs.lt.elend) then
        tds=erend-obs
       else
        tds=obs-elend
       endif
      endif

      if (k.le.0) then
       do n = 1, m
        fac=1.0-float(n-1)/float(m)
        if (n.eq.1) then
         ds=ena(1,2)-obs              
         ta=ta+fac*fac*ds
        else
         ds=ena(n,2)-ena(n-1,2)
         ta=ta+fac*fac*ds
        endif
c       print *, 'k=',k,' ds=',ds,' ta=',ta,' obs=',obs
       enddo
      elseif (k.ge.m) then
       do n = 1, m
        fac=float(n)/float(m)
        if (n.eq.m) then
         ds=obs-ena(m,2)
         ta=ta+fac*fac*ds
        else
         ds=ena(n+1,2)-ena(n,2)
         ta=ta+fac*fac*ds
        endif
c       print *, 'k=',k,' ds=',ds,' ta=',ta,' obs=',obs
       enddo
      else
       do n = 1, k
        fac=float(n)/float(m)
        if (n.ne.k) then
         ds=ena(n+1,2)-ena(n,2)
         ta=ta+fac*fac*ds
        else
         ds=obs-ena(n,2)
         ta=ta+fac*fac*ds
        endif
c       print *, 'k=',k,' ds=',ds,' ta=',ta,' obs=',obs
       enddo
       do n = k+1, m
        fac=1.0-float(n-1)/float(m)
        if (n.ne.k+1) then
         ds=ena(n,2)-ena(n-1,2)
         ta=ta+fac*fac*ds
        else
         ds=ena(n,2)-obs
         ta=ta+fac*fac*ds
        endif
c       print *, 'k=',k,' ds=',ds,' ta=',ta,' obs=',obs
       enddo
      endif
 899  continue
      if (tds.ne.0.0.and.ictl.eq.1) then
       ta=ta/tds
      endif
      ccrps=ta
c     print *, ccrps,obs,(ena(i,2),i=1,m)
      return   
 999  print *, "ensemble size is less than one, crps=0, quit"
      irt=10
      return
      end


c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine      CRPSD(fst,m,obs,af,bt,irt)
c  Prgmmr: Yuejian Zhu           Org: np23          Date: 2008-02-19
c
c  This is the subroutine to calculete Continuous Ranked Probability Score
c         and decomposition for NAEFS project on IBM-SP
c   Based on Hans Hersbach's paper 2000 Weather and Forecasting
c      "Decomposition of the Continuous Ranked Probability Score for
c       Ensemble Prediction System"
c
c   parameters:
c      fst   -- forecasts for m ensemble members
c      m     -- ensemble size (>0)
c      obs   -- analysis or observation, or climatology (truth)                
c      af    -- Alpha distance (maximum dimension m+1: left cdf)
c      bf    -- Beta distance (maximum dimension m+1: right cdf)
c      irt   -- 0-successful
c               10-need to review ensemble forecast data
c
c   Fortran 77 on IBMSP
c   Called subroutine: sortmm
c
C--------+---------+---------+---------+---------+----------+---------+--
      subroutine crpsd(fst,m,obs,af,bt,irt)
      dimension fst(m),ena(m,3)
      dimension af(0:m),bt(0:m)
      
      irt=0
      af = 0.0
      bt = 0.0
      if (m.lt.1) goto 999
      if (m.eq.1) then
       if (fst(1).le.obs) then
        af(1)=obs-fst(1)
       else
        bt(0)=fst(1)-obs
       endif
       goto 999
      endif
      do n = 1, m
       ena(n,1)=fst(n)
      enddo
ccc call sortmm to re-arrange the data from low to high
      call sortmm(ena,m,3,1)
c     print *, (ena(i,1),i=1,m)
c     print *, (ena(i,2),i=1,m)
c     print *, (ena(i,3),i=1,m)
      if (obs.le.ena(1,2)) then
ccc all forecasts on the right side
ccc alpha=0, beta(m)=0, but beta(0) is nonzero
       af = 0.0
       bt(0) = ena(1,2) - obs
       do n = 1, m-1
        bt(n) = ena(n+1,2) - ena(n,2)
       enddo
      elseif (obs.gt.ena(m,2)) then
ccc all forecasts on the left side
ccc beta = 0 alpha(0)=0, but alpha(m) is nonzero
       bt = 0.0
       af(m) = obs - ena(m,2)
       do n = 1, m-1
        af(n) = ena(n+1,2) - ena(n,2)
       enddo
      else
ccc forecasts are distributed on both left and right side
ccc obs is falling in bin n and n+1
ccc alpha(0)=0, alpha(i)=0 for i>n
ccc beta(m)=0, beta(i)=0 for i<n
ccc alpha(n) and beta(n) are nonzero
       do n = 1, m-1
        if (obs.gt.ena(n,2).and.obs.le.ena(n+1,2)) then           
         if (n.eq.1) then
          af(1) = obs - ena(1,2)
          bt(1) = ena(2,2) - obs
          do k = 2, m-1
           af(k) = 0.0
           bt(k) = ena(k+1,2) - ena(k,2)
          enddo
         elseif (n.eq.m-1) then
          af(m-1) = obs - ena(m-1,2)
          bt(m-1) = ena(m,2) - obs
          do k = 1, m-2
           af(k) = ena(k+1,2) - ena(k,2)
           bt(k) = 0.0
          enddo
         else
          af(n) = obs - ena(n,2)
          bt(n) = ena(n+1,2) - obs
          do k = 1, n-1
           af(k) = ena(k+1,2) - ena(k,2)
           bt(k) = 0.0
          enddo
          do k = n+1, m-1
           af(k) = 0.0                   
           bt(k) = ena(k+1,2) - ena(k,2)
          enddo
         endif
        endif
       enddo
      endif

      return   
 999  print *, "ensemble size is less than one, no decomposition, quit"
      irt=10
 998  return
      end


c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine      HRFA(fst,m,obs,clm,ib,fyy,fyn,irt)
c  Prgmmr: Yuejian Zhu           Org: np23          Date: 2006-12-26
c
c  This is the subroutine to calculete Brier Score                       
c         for NAEFS project on IBM-SP
c
c   parameters:
c      fst   -- forecasts for m ensemble members
c      m     -- ensemble members
c      obs   -- analysis or observation, or climatology (truth)                
c      clm   -- climatological data                                            
c      ib    -- climatological data dimension (equally-a-likely-bin)   
c      fyy   -- hit rate
c      fyn   -- false alarm rate
c      irt   -- 0-successful
c               10-need to review ensemble forecast data
c
c   Fortran 77 on IBMSP
c   Called subroutine: fapb  
c
C--------+---------+---------+---------+---------+----------+---------+--
      subroutine hrfa(fst,m,obs,clm,ib,yy,yn,irt)

      dimension fst(m),clm(ib+1) 
      dimension yy(m+1),yn(m+1)
      dimension fcp(ib),anp(ib)

      call fapb(fst,m,obs,clm,ib,fcp,anp,irt)

      irt=0
      yy=0.0
      yn=0.0
Ccccc question???
ccccc No question: fyy(1) is representing zero probability
ccccc if fyy(1) = 1  means miss
ccccc if fyn(1) = 1  means correct rejection
ccccc if fyy(2) = 1  means hit for 10% probability ( 1 of 10 members )
ccccc if fyn(2) = 1  means false alarm for 10% probability
ccccc if fyy(3) = 1  means hit for 20% probability ( 2 of 10 members )
ccccc if fyn(3) = 1  means false alarm for 20% probability
ccccc ......

      do nb = 1, ib
       do np = 1, m+1  
        if (fcp(nb).eq.float(np-1)) then
         if (anp(nb).eq.1.) then
          yy(np) = yy(np)+1.0
         else
          yn(np) = yn(np)+1.0
         endif
        endif
       enddo
      enddo          ! nb

      return   
      end


c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  Subroutine      FAPB(fst,m,obs,clm,ib,fcp,anp,irt)
c  Prgmmr: Yuejian Zhu           Org: np23          Date: 2006-12-26
c       
c
c  This is the subroutine to calculete forecast and analysis probability 
c  of special valus, or equally-a-likely-bin for NAEFS project on IBM-SP
c
c   parameters:
c      fst   -- forecasts for m ensemble members
c      m     -- ensemble members
c      obs   -- analysis or observation, or climatology (truth)                
c      clm   -- special values, or equally-a-likely-bin (ib+1)
c      rp    -- probabilities of each special value, or equally-a-likely-bin
c      irt   -- 0-successful
c               10-need to review ensemble forecast data
c 
c   Fortran 77 on IBMSP
c
C--------+---------+---------+---------+---------+----------+---------+--
      subroutine fapb(fst,m,obs,clm,ib,fcp,anp,irt)

      dimension fst(m),clm(ib+1),fcp(ib),anp(ib)
      
c     write (*,'(26f6.0)') (fst(i),i=1,m),obs,(clm(i),i=1,ib+1)

      fcp=0.0
      anp=0.0
      irt=0
      icnt=0


      if (ib.eq.1) then

         do n = 1, m
          if (fst(n).gt.clm(1)) then
           icnt=icnt+1          
          endif
         enddo
         fcp(1)=float(icnt)
         if (obs.gt.clm(1)) then
          anp(1)=1.0
         endif


c     elseif (ib.eq.-1) then
c      do n = 1, m
c       if (fst(n).le.clm(1)) then
c        icnt=icnt+1
c       endif
c      enddo
c      rp(1,1)=icnt
c      if (obs.gt.clm(1)) then
c       rp(1,2)=1.0
c      endif
c     elseif (ib.eq.0) then
ccc if ib=0, clm(1) is the mean of climatology
c      do n = 1, m
c       if (fst(n).gt.clm(1)) then
c        icnt=icnt+1
c       endif
c      enddo
c      rp(1,1)=icnt
c      if (obs.gt.clm(1)) then
c       rp(1,2)=1.0
c      endif
      elseif (ib.ge.2) then
ccc for example: ib=2 2 equally-a-likely-bin
       icnt=0
       do n = 1, m
        if (fst(n).le.clm(2)) then
         icnt=icnt+1
        endif
       enddo
       fcp(1)=float(icnt)      
       if (obs.le.clm(2)) then
        anp(1)=1.0
       endif

       icnt=0
       do n = 1, m
        if (fst(n).gt.clm(ib)) then
         icnt=icnt+1
        endif
       enddo
       fcp(ib)=float(icnt)       
       if (obs.gt.clm(ib)) then
        anp(ib)=1.0
       endif

       if (ib.gt.2) then
       do i = 2, ib-1
        icnt=0
        do n = 1, m
         if (fst(n).gt.clm(i).and.fst(n).le.clm(i+1)) then
          icnt=icnt+1
         endif
        enddo
        fcp(i)=float(icnt)      
        if (obs.gt.clm(i).and.obs.le.clm(i+1)) then
         anp(i)=1.0
        endif
       enddo
       endif
      else
       irt=99
      endif

      return   
      end


      subroutine grange(n,ld,d,dmin,dmax)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                    C
C     USAGE: DETERMINE THE MAXIMUM AND MINIMUM VALUES OF ARRAY       C
C     CODE : F77 on IBMSP --- Yuejian Zhu (05/10/99)                 C
C                                                                    C
C     INPUT: one dimension array d and ld                            C
C                                                                    C
C     OUTPUT:maximum and minimum values                              C
C                                                                    C
C     Arguments:                                                     C
C               1. n ( int number of dimension of d and ld )         C
C               2. ld ( logical array of dimension n )               C
C               3. d  ( real array of dimension n )                  C
C                                                                    C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      logical*1   ld
      dimension ld(n),d(n)
      dmin=  1.e20
      dmax= -1.e20
      do i=1,n
        if(ld(i)) then
          dmin=min(dmin,d(i))
          dmax=max(dmax,d(i))
        endif
      enddo
      return
      end


       subroutine evalue(hr,far,im1,clfr,fv)
c
c      This program will calculate the potential economic value of forecasts
c      copied from ~wd20zt/value/value.f of sgi machine
c      modified by Yuejian Zhu    (02/09/2001)
c
c      Changed: old- clfr = 0.1 for 10 climatological equally-likely-bin
c               new- clfr = sample climatological relative frequency
c               using subroutine pass this value from reliability diagram
c      modified by Yuejian Zhu    (09/03/2004)
c
c      Explaination of calculation from BAMS publication, Zhu. and etc..
c       ----------------------------------------------------
c              |       Yes           |        No
c       ----------------------------------------------------
c              |  h(its)             |   m(isses)
c         YES  |  M(itigated) L(oss) |   L(oss)
c              |  ML = ( C + Lu )    |   L = Lp + Lu
c       ----------------------------------------------------
c         NO   |  f(alse alarms)     |   c(orrect rejections)
c              |  C(ost)             |   N(o cost)
c       ----------------------------------------------------
c
c      o         - climatological frequency
c      ML=C+Lu   - Mitigated Loss
c      C         - Cost
c      L=Lp+Lu   - Loss
c      h         - hites
c      m         - misses
c      f         - false alarms
c
c      V = (xmecl - xmefc)/(xmecl - xmepf)
c      Mean Expense (general forecast) = h*ML + f*C + m*L         --> eq. (1)
c      Mean Expense (climate forecast) = min[o*L, o*ML + (1-o)*C] --> eq. (2)
c      Mean Expense (perfect forecast) = o*ML                     --> eq. (3)
c      Value = (ME(cl) - ME(fc))/(ME(cl) - ME(perf))              --> eq. (4)
c
c      by applying h+m=o, and h+m=h+f (bias free), define r=C/Lp, get:
c      (note: the equation will be independent of Lu)
c
c      V = (Min[o,r] - (h+f)r - m )/( Min[o,y] - or )             --> eq. (7)
c

       parameter (icl=18)
       dimension hr(*),   far(*),     v(im1-1,2), xmefc(im1)
       dimension xml(icl),xc(icl),    xl(icl),    fv(icl)

ccc... data for assume xml, xc and xl for totally 18 levels
       data xml/1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,
     *          1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00/
       data xc /1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,
     *          1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00,1.00/
       data xl /1.05,1.10,1.25,1.50,2.00,3.00,5.00,8.00,10.0,
     *          18.0,27.0,40.0,60.0,90.0,140.,210.,350.,500./
c
c      The following calculation will base on equation (7)
c
c      xmecl - mean expense for climate forecast: Min[o,r]
c      xmepf - mean expense for perfect forecast: o.r
c      xmefc - mean expense for general forecast: (h+f)r-m
c
c      xme1  - loss from climatological frequency  [ o ]
c      xme2  - loss from cost and mitigated loss   [ r ]
c      if xme2.lt.xme1 means need protect ( always )
c      if xme2.ge.xme1 means need giveup  ( never  )
c
c      input: (accumulative)
c      hr (i) = h/(h+m)
c      far(i) = f/(f+c)
c      when i=1,  not include in this calculation
c      when i=2,  start from 10% and above probabilities
c           i=3,  ......
c      when i=11, includes only 100% probabilities ( last for 10 members )
c
c      xml    - mitigrated loss ( the same as C, exclude Lu )
c      xc     - cost ( the same as C  )
c      xl     - loss ( the same as Lp, exclude Lu )
c      clfr   - climate frequency of forecast
c
ccc... loop over cost/loss ratios (r=C/Lp)

        do j = 1, 18

         do i = 1, im1-1
          v(i,1) = i
         enddo 

         xme1 = clfr*xl(j)/xl(j)
         xme2 = clfr*xml(j)/xl(j)+(1-clfr)*xc(j)/xl(j)
         xme  = xme1
         if(xme2.lt.xme1) xme=xme2
        if(xme2.lt.xme1) print *,'always protect'
         if(xme2.lt.xme1) np=1
c        if(xme2.ge.xme1) print *,'never  protect'
         if(xme2.ge.xme1) np=0

         xmecl  = xme                                 ! min[o,r]
         xmepf  = clfr*xml(j)/xl(j)                   ! o.r 

c         write(*,12)'j,mxme1,xme2,clfr,xml,xl,xmecl,xmepf=',
c     +      j, xme1,xme2,clfr,xml(j),xl(j),xmecl,xmepf
c12       format(a30, i3,2x,7f6.3)    
c
c     loop over ensemble probabilities
c        if hr=1, far=0, perfect forecast, xmefc = xmepf, v = 1.0
c        if hr=0, far=1, all cost, xmefc = xmecl, v = 0.0
c        loop over all probabilities, will find maximum value 
c        do i = 1, im1

         do i = 2, im1
c note: h+m=o, h+m+f+c=1, clfr=o, hr=h/(h+m), far=f/(f+c), xml/xl=r, xc/xl=r
          xmefc(i) = clfr*hr(i)*xml(j)/xl(j)          ! h.r    
     *             + clfr*(1-hr(i))*xl(j)/xl(j)       ! m 
     *             + (1-clfr)*far(i)*xc(j)/xl(j)      ! f.r
          v(i-1,2)   = (xmecl-xmefc(i))/(xmecl-xmepf) 
c         print *, 'im=',i,'xmefc=', xmefc(i),' hr=',hr(i),
c     +      ' fa=',far(i),' value=',v(i-1,2)
         enddo     

         call sortm(v,im1-1,2,2)

         fv(j) = v(im1-1,2)
c         write(6,66)  j,1./xl(j),np,v(im1-1,2),v(im1-1,1)
        enddo               ! do j = 1, 18

c         write(6,67)
c 66    format(1x,i4,f7.4,i4,2f8.3)
c 67    format(1x)

       return
       end


      SUBROUTINE SORTM(A,N,NC,K)
C     generalized version of sort subroutine for multidim. arrays
C     a(n,nc) array of n rows, nc columns to be sorted by column k
      PARAMETER (M=50)
      INTEGER N,I,J,L,R,S,STACK(M,2)
      REAL A(n,nc), X, W
 
      S = 1
      STACK(1, 1) = 1
      STACK(1, 2) = N
  100 CONTINUE
c---  SIMULATE OUTER REPEAT LOOP ...
      L = STACK(S, 1)
      R = STACK(S, 2)
      S = S - 1
  200 CONTINUE
c---  SIMULATE MIDDLE REPEAT LOOP ...
      I = L
      J = R
c     change last # for column to be sorted *********
      X = A ((L+R)/2, K)
  300 CONTINUE
c---  SIMULATE INNER REPEAT LOOP
  400 CONTINUE
c---  SIMULATE WHILE LOOP
c     change #  **************************************
      IF (A(I, K).LT.X) THEN
       I = I + 1
       GOTO 400
      ENDIF
  500 CONTINUE
c---  SIMULATE WHILE LOOP
c     change #  **************************************
      IF (X.LT.A(J, K)) THEN
       J = J -1
       GOTO 500
      ENDIF
      IF (I.LE.J) THEN
c     2nd # is total # of columns **********************
      DO 1000 ICOL = 1, NC, 1
       W          = A(I, ICOL)
       A(I, ICOL) = A(J, ICOL)
       A(J, ICOL) = W
 1000 continue
      I = I + 1
      J = J - 1
      ENDIF
c---  END OF INNER REPEAT LOOP
      IF (I.LE.J) GOTO 300
      IF (I.LT.R) THEN
       S = S + 1
       IF (S.GT.M) THEN
        PRINT *, 'STACK OVERFLOW IN QSORT'
        STOP 'STACK OVF'
       ENDIF
       STACK(S, 1) = I
       STACK(S, 2) = R
      ENDIF
      R = J
c---  END OF MIDDLE REPEAT LOOP
      IF (L.LT.R) GOTO 200
c---  END OF OUTER REPEAT LOOP
      IF (S.GT.0) GOTO 100
      RETURN
      END

