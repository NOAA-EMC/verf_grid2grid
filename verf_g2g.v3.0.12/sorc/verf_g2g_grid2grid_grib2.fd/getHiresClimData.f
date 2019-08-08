       subroutine get_HiresClimData(meangribfile,sprdgribfile,
     +    kk4,kk5,kk6,kk7,ngrid,Nmodel,kb,clip,nodata)


      use grib_mod
      include 'parm.inc'
                                                                         
      type(gribfield) :: gfld                                                                                                                                
      real, allocatable, dimension(:)  :: mean,spread
      real clip(ngrid,kb)
                                                                                                                                           
      CHARACTER*80 meangribfile,sprdgribfile

      real opara(2), clim_values(11),prob(11)

      integer yy,mm,dd,cyc,ff,jpdtn

       data (prob(k),k=1,11)
     * /0.001,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.9999/


      nodata = -1
      allocate(mean(ngrid))
      allocate(spread(ngrid))

      write(*,*) ' In get_HiresClimData, kb=',kb  

       write(*,*) trim(meangribfile),' ',trim(sprdgribfile)


       write(*,*)' Get clim mean data ------------------------'
       call baopenr(51,meangribfile, ierr)
       if(ierr.ne.0) then
        write(*,*)'open ',trim(meangribfile), ' error=',ierr
        nodata=1
        return
       end if

       if(kb.eq.11) then 
          if (kk4.eq.19.and.kk5.eq.20) then
           jpdtn=1
          else
           jpdtn=8
           kk27=0
          end if
        else             ! for HREFv2
          if (kk4.eq.19.and.kk5.eq.20) then
           jpdtn=1
          else if (kk4.eq.1.and.kk5.eq.8) then
           jpdtn=8
           kk27=3   !set accumulated 3hr for NAMNEST APCP as ref
          else
           jpdtn=0    
          end if
        end if
         

         !if (kk4.eq.16.and.kk5.eq.196) then  !HRRR REFC uses different kpds6
         ! kk6=10
         !end if
         !if (kk4.eq.6.and.kk5.eq.1) then  !HRRR TCDC  uses different kpds6
         ! kk6=10
         !end if
 

          call readClimGB2(51,jpdtn,kk4,kk5,kk6,kk7,kk27,gfld,iret)

          if(iret.ne.0) then
            write(*,*)'read mean clim data file error=',iret  
            nodata = 1
          else
            mean=gfld%fld
            write(*,'(10f8.2)') (mean(k),k=10001,10010)
          end if
               
        call baclose(51, ierr)
 
       write(*,*)' Get clim spread data ----------------------'                                      

       call baopen(52,sprdgribfile, ierr)
       if(ierr.ne.0) then
        write(*,*)'open ',trim(sprdgribfile), ' error'
        nodata = 1 
       end if

         call readClimGB2(52,jpdtn,kk4,kk5,kk6,kk7,kk27,gfld,iret)


          if(iret.ne.0) then
            write(*,*)'read spread clim data error=',iret     
            nodata = 1
          else
            spread=gfld%fld
            write(*,'(10f8.2)') (spread(k),k=10001,10010)
          end if

        call baclose(52, ierr)

CCC Now compute climate data at 11 probability bin ----
         write(*,*)'Compute climate data at kb bin --'

         if(nodata .eq. -1 ) then

           if (kb.eq.2) then
             clip(:,1)=mean(:)
             clip(:,2)=mean(:)
           else
             do i = 1, ngrid
              opara(1)=mean(i)
!             opara(2)=sqrt(spread(i)*spread(i)*(Nmodel-1)/Nmodel)
              opara(2)=spread(i)     !as suggested by Yuejian, just use spread for opara 
               do k = 1, kb
                p=prob(k)
                clim_values(k)=quanor(p,opara)
               end do
               clip(i,:)=clim_values(:)
             end do
           end if

         else
     
           nodata = 1
           clip = -99999.9
 
         end if                            


          deallocate(mean)
          deallocate(spread)

          write(*,*) 'Get_HiresClimData done, nodata=',nodata
                
        return
        end

      subroutine readClimGB2(igrb2,jpdtn,jpd1,jpd2,jpd10,jpd12,
     +     jpd27, gfld,iret)

        use grib_mod

        type(gribfield),intent(IN) :: gfld
        integer jids(200), jpdt(200), jgdt(200)
        integer igrb2,jpdtn,jpd1,jpd2,jpd10,jpd12,jpd27
        logical :: unpack=.true.

        write(*,*) igrb2, jpdtn,jpd1,jpd2,jpd10,jpd12,jpd27

        jids=-9999  !array define center, master/local table, year,month,day, hour, etc, -9999 wildcard to accept any
        jpdt=-9999  !array define Product, to be determined
        jgdt=-9999  !array define Grid , -9999 wildcard to accept any

        jdisc=-1    !discipline#  -1 wildcard 
        jgdtn=-1
        jskp=0      !Number of fields to be skip, 0 search from beginning

        jpdt(1)=jpd1   !Category #     
        jpdt(2)=jpd2   !Product # under this category     
        jpdt(10)=jpd10 !Product vertical ID      

        if(jpd10.eq.100) then
           jpdt(12)=jpd12*100   !pressure level     
        else
           jpdt(12)=jpd12
        end if

        if(jpdtn.eq.8) jpdt(27)=jpd27

        call getgb2(igrb2,0,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,
     +     unpack, jskp1, gfld,iret)


        return
        end


C===================================================== QUAGEV.FOR
      DOUBLE PRECISION FUNCTION QUAGEV(F,PARA)
C***********************************************************************
C*                                                                     *
C*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
C*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
C*                                                                     *
C*  J. R. M. HOSKING                                                   *
C*  IBM RESEARCH DIVISION                                              *
C*  T. J. WATSON RESEARCH CENTER                                       *
C*  YORKTOWN HEIGHTS                                                   *
C*  NEW YORK 10598, U.S.A.                                             *
C*                                                                     *
C*  VERSION 3     AUGUST 1996                                          *
C*                                                                     *
C***********************************************************************
C
C  QUANTILE FUNCTION OF THE GENERALIZED EXTREME-VALUE DISTRIBUTION
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION PARA(3)
      DATA ZERO/0D0/,ONE/1D0/
c      write(*,*) 'In QUAGEV:',F,PARA
      U=PARA(1)
      A=PARA(2)
      G=PARA(3)
      IF(A.LE.ZERO)GOTO 1000
      IF(F.LE.ZERO.OR.F.GE.ONE)GOTO 10
      Y=-DLOG(-DLOG(F))
      IF(G.NE.ZERO)Y=(ONE-DEXP(-G*Y))/G
      QUAGEV=U+A*Y
      RETURN
C
   10 IF(F.EQ.ZERO.AND.G.LT.ZERO)GOTO 20
      IF(F.EQ.ONE .AND.G.GT.ZERO)GOTO 20
      WRITE(6,7000)
      QUAGEV=ZERO
      RETURN
   20 QUAGEV=U+A/G
      RETURN
C
 1000 WRITE(6,7010)
      QUAGEV=ZERO
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE QUAGEV :',
     *  ' ARGUMENT OF FUNCTION INVALID')
 7010 FORMAT(' *** ERROR *** ROUTINE QUAGEV : PARAMETERS INVALID')
      END


C===================================================== PELGEV.FOR
      SUBROUTINE PELGEV(XMOM,PARA)
C***********************************************************************
C*                                                                     *
C*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
C*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
C*                                                                     *
C*  J. R. M. HOSKING                                                   *
C*  IBM RESEARCH DIVISION                                              *
C*  T. J. WATSON RESEARCH CENTER                                       *
C*  YORKTOWN HEIGHTS                                                   *
C*  NEW YORK 10598, U.S.A.                                             *
C*                                                                     *
C*  VERSION 3     AUGUST 1996                                          *
C*                                                                     *
C***********************************************************************
C
C  PARAMETER ESTIMATION VIA L-MOMENTS FOR THE GENERALIZED EXTREME-VALUE
C  DISTRIBUTION
C
C  PARAMETERS OF ROUTINE:
C  XMOM   * INPUT* ARRAY OF LENGTH 3. CONTAINS THE L-MOMENTS LAMBDA-1,
C                  LAMBDA-2, TAU-3.
C  PARA   *OUTPUT* ARRAY OF LENGTH 3. ON EXIT, CONTAINS THE PARAMETERS
C                  IN THE ORDER XI, ALPHA, K (LOCATION, SCALE, SHAPE).
C
C  OTHER ROUTINES USED: DLGAMA
C
C  METHOD: FOR  -0.8 LE TAU3 LT 1,  K IS APPROXIMATED BY RATIONAL
C  FUNCTIONS AS IN DONALDSON (1996, COMMUN. STATIST. SIMUL. COMPUT.).
C  IF TAU3 IS OUTSIDE THIS RANGE, NEWTON-RAPHSON ITERATION IS USED.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DOUBLE PRECISION XMOM(3),PARA(3)
      DATA ZERO/0D0/,HALF/0.5D0/,ONE/1D0/,TWO/2D0/,THREE/3D0/
      DATA P8/0.8D0/,P97/0.97D0/
C
C         SMALL IS USED TO TEST WHETHER K IS EFFECTIVELY ZERO
C         EPS,MAXIT CONTROL THE TEST FOR CONVERGENCE OF N-R ITERATION
C
      DATA SMALL/1D-5/,EPS/1D-6/,MAXIT/20/
C
C         EU IS EULER'S CONSTANT
C         DL2 IS LOG(2), DL3 IS LOG(3)
C
      DATA EU/0.57721566D0/,DL2/0.69314718D0/,DL3/1.0986123D0/
C
C         COEFFICIENTS OF RATIONAL-FUNCTION APPROXIMATIONS FOR K
C
      DATA A0,A1,A2/ 0.28377530D0,-1.21096399D0,-2.50728214D0/
      DATA A3,A4   /-1.13455566D0,-0.07138022D0/
      DATA B1,B2,B3/ 2.06189696D0, 1.31912239D0, 0.25077104D0/
      DATA C1,C2,C3/ 1.59921491D0,-0.48832213D0, 0.01573152D0/
      DATA D1,D2   /-0.64363929D0, 0.08985247D0/
C
      T3=XMOM(3)
      IF(XMOM(2).LE.ZERO)GOTO 1000
      IF(DABS(T3).GE.ONE)GOTO 1000
      IF(T3.LE.ZERO)GOTO 10
C
C         RATIONAL-FUNCTION APPROXIMATION FOR TAU3 BETWEEN 0 AND 1
C
      Z=ONE-T3
      G=(-ONE+Z*(C1+Z*(C2+Z*C3)))/(ONE+Z*(D1+Z*D2))
      IF(DABS(G).LT.SMALL)GOTO 50
      GOTO 40
C
C         RATIONAL-FUNCTION APPROXIMATION FOR TAU3 BETWEEN -0.8 AND 0
C
   10 G=(A0+T3*(A1+T3*(A2+T3*(A3+T3*A4))))/(ONE+T3*(B1+T3*(B2+T3*B3)))
      IF(T3.GE.-P8)GOTO 40
C
C         NEWTON-RAPHSON ITERATION FOR TAU3 LESS THAN -0.8
C
      IF(T3.LE.-P97)G=ONE-DLOG(ONE+T3)/DL2
      T0=(T3+THREE)*HALF
      DO 20 IT=1,MAXIT
      X2=TWO**(-G)
      X3=THREE**(-G)
      XX2=ONE-X2
      XX3=ONE-X3
      T=XX3/XX2
      DERIV=(XX2*X3*DL3-XX3*X2*DL2)/(XX2*XX2)
      GOLD=G
      G=G-(T-T0)/DERIV
      IF(DABS(G-GOLD).LE.EPS*G)GOTO 30
   20 CONTINUE
      WRITE(6,7010)
   30 CONTINUE
C
C         ESTIMATE ALPHA,XI
C
   40 PARA(3)=G
      GAM=DEXP(DLGAMA(ONE+G))
      PARA(2)=XMOM(2)*G/(GAM*(ONE-TWO**(-G)))
      PARA(1)=XMOM(1)-PARA(2)*(ONE-GAM)/G
      RETURN
C
C         ESTIMATED K EFFECTIVELY ZERO
C
   50 PARA(3)=ZERO
      PARA(2)=XMOM(2)/DL2
      PARA(1)=XMOM(1)-EU*PARA(2)
      RETURN
C
 1000 WRITE(6,7000)
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE PELGEV : L-MOMENTS INVALID')
 7010 FORMAT(' ** WARNING ** ROUTINE PELGEV ')
      END

C===================================================== QUANOR.FOR
C     DOUBLE PRECISION FUNCTION QUANOR(F,PARA)
      FUNCTION QUANOR(F,PARA)
C***********************************************************************
C*                                                                     *
C*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
C*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
C*                                                                     *
C*  J. R. M. HOSKING                                                   *
C*  IBM RESEARCH DIVISION                                              *
C*  T. J. WATSON RESEARCH CENTER                                       *
C*  YORKTOWN HEIGHTS                                                   *
C*  NEW YORK 10598, U.S.A.                                             *
C*                                                                     *
C*  VERSION 3     AUGUST 1996                                          *
C*                                                                     *
C***********************************************************************
C
C  QUANTILE FUNCTION OF THE NORMAL DISTRIBUTION
C
C  OTHER ROUTINES USED: QUASTN
C
C     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C     DOUBLE PRECISION PARA(2)

      DIMENSION PARA(2)
      DATA ZERO/0D0/,ONE/1D0/

c      IF(PARA(2).LE.ZERO)GOTO 1000
      IF(PARA(2).LE.ZERO) PARA(2)=0.001       !Binbin modify to make sure no negative spread
      IF(F.LE.ZERO.OR.F.GE.ONE)GOTO 1010   
      QUANOR=PARA(1)+PARA(2)*QUASTN(F)
      RETURN
C
 1000 WRITE(6,7000)
      QUANOR=ZERO
      RETURN
 1010 WRITE(6,7010)
      QUANOR=ZERO
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE QUANOR : PARAMETERS INVALID')
 7010 FORMAT(' *** ERROR *** ROUTINE QUANOR :')
      END
C


C===================================================== QUASTN.FOR
C     DOUBLE PRECISION FUNCTION QUASTN(F)
      FUNCTION QUASTN(F)
C***********************************************************************
C*                                                                     *
C*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
C*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
C*                                                                     *
C*  J. R. M. HOSKING                                                   *
C*  IBM RESEARCH DIVISION                                              *
C*  T. J. WATSON RESEARCH CENTER                                       *
C*  YORKTOWN HEIGHTS                                                   *
C*  NEW YORK 10598, U.S.A.                                             *
C*                                                                     *
C*  VERSION 3     AUGUST 1996                                          *
C*                                                                     *
C***********************************************************************
C
C  QUANTILE FUNCTION OF THE STANDARD NORMAL DISTRIBUTION
C
C  BASED ON ALGORITHM AS241, APPL. STATIST. (1988) VOL.37 NO.3
C
C     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA ZERO/0D0/,HALF/0.5D0/,ONE/1D0/
      DATA SPLIT1/0.425D0/,SPLIT2/5D0/,CONST1/0.180625D0/,CONST2/1.6D0/
C
C         COEFFICIENTS OF RATIONAL-FUNCTION APPROXIMATIONS
C
      DATA A0,A1,A2,A3,A4,A5,A6,A7,B1,B2,B3,B4,B5,B6,B7/
     *                                0.33871 32872 79636 661D  1,
     *  0.13314 16678 91784 377D  3,  0.19715 90950 30655 144D  4,
     *  0.13731 69376 55094 611D  5,  0.45921 95393 15498 715D  5,
     *  0.67265 77092 70087 009D  5,  0.33430 57558 35881 281D  5,
     *  0.25090 80928 73012 267D  4,  0.42313 33070 16009 113D  2,
     *  0.68718 70074 92057 908D  3,  0.53941 96021 42475 111D  4,
     *  0.21213 79430 15865 959D  5,  0.39307 89580 00927 106D  5,
     *  0.28729 08573 57219 427D  5,  0.52264 95278 85285 456D  4/
      DATA C0,C1,C2,C3,C4,C5,C6,C7,D1,D2,D3,D4,D5,D6,D7/
     *                                0.14234 37110 74968 358D  1,
     *  0.46303 37846 15654 530D  1,  0.57694 97221 46069 141D  1,
     *  0.36478 48324 76320 461D  1,  0.12704 58252 45236 838D  1,
     *  0.24178 07251 77450 612D  0,  0.22723 84498 92691 846D -1,
     *  0.77454 50142 78341 408D -3,  0.20531 91626 63775 882D  1,
     *  0.16763 84830 18380 385D  1,  0.68976 73349 85100 005D  0,
     *  0.14810 39764 27480 075D  0,  0.15198 66656 36164 572D -1,
     *  0.54759 38084 99534 495D -3,  0.10507 50071 64441 684D -8/
      DATA E0,E1,E2,E3,E4,E5,E6,E7,F1,F2,F3,F4,F5,F6,F7/
     *                                0.66579 04643 50110 378D  1,
     *  0.54637 84911 16411 437D  1,  0.17848 26539 91729 133D  1,
     *  0.29656 05718 28504 891D  0,  0.26532 18952 65761 230D -1,
     *  0.12426 60947 38807 844D -2,  0.27115 55568 74348 758D -4,
     *  0.20103 34399 29228 813D -6,  0.59983 22065 55887 938D  0,
     *  0.13692 98809 22735 805D  0,  0.14875 36129 08506 149D -1,
     *  0.78686 91311 45613 259D -3,  0.18463 18317 51005 468D -4,
     *  0.14215 11758 31644 589D -6,  0.20442 63103 38993 979D-14/
C
      Q=F-HALF
C     IF(DABS(Q).GT.SPLIT1)GOTO 10
      IF(ABS(Q).GT.SPLIT1)GOTO 10
      R=CONST1-Q*Q
      QUASTN=Q*(((((((A7*R+A6)*R+A5)*R+A4)*R+A3)*R+A2)*R+A1)*R+A0)
     *        /(((((((B7*R+B6)*R+B5)*R+B4)*R+B3)*R+B2)*R+B1)*R+ONE)
      RETURN
   10 R=F
      IF(Q.GE.ZERO)R=ONE-F
      IF(R.LE.ZERO)GOTO 1000
C     R=DSQRT(-DLOG(R))
      R=SQRT(-LOG(R))
      IF(R.GT.SPLIT2)GOTO 20
      R=R-CONST2
      QUASTN=(((((((C7*R+C6)*R+C5)*R+C4)*R+C3)*R+C2)*R+C1)*R+C0)
     *      /(((((((D7*R+D6)*R+D5)*R+D4)*R+D3)*R+D2)*R+D1)*R+ONE)
      GOTO 30
   20 R=R-SPLIT2
      QUASTN=(((((((E7*R+E6)*R+E5)*R+E4)*R+E3)*R+E2)*R+E1)*R+E0)
     *      /(((((((F7*R+F6)*R+F5)*R+F4)*R+F3)*R+F2)*R+F1)*R+ONE)
   30 IF(Q.LT.ZERO)QUASTN=-QUASTN
      RETURN
C
 1000 WRITE(6,7000)F
      QUASTN=ZERO
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE QUASTN :',
     *  ' ARGUMENT OF FUNCTION INVALID')
      END





CC On Zeus/WCOSS/EDDY no function DLGAMA(), following function is copied
Cfrom http://cpansearch.perl.org/src/AJOLMA/Statistics-Lmoments-0.03/lmoments/dlgama.f
CC
CC
      DOUBLE PRECISION FUNCTION DLGAMA(X)
C***********************************************************************
C*                                                                     *
C*  FORTRAN CODE WRITTEN FOR INCLUSION IN IBM RESEARCH REPORT RC20525, *
C*  'FORTRAN ROUTINES FOR USE WITH THE METHOD OF L-MOMENTS, VERSION 3' *
C*                                                                     *
C*  J. R. M. HOSKING                                                   *
C*  IBM RESEARCH DIVISION                                              *
C*  T. J. WATSON RESEARCH CENTER                                       *
C*  YORKTOWN HEIGHTS                                                   *
C*  NEW YORK 10598, U.S.A.                                             *
C*                                                                     *
C*  VERSION 3     AUGUST 1996                                          *
C*                                                                     *
C***********************************************************************
C
C  LOGARITHM OF GAMMA FUNCTION
C
C  BASED ON ALGORITHM ACM291, COMMUN. ASSOC. COMPUT. MACH. (1966)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DATA SMALL,CRIT,BIG,TOOBIG/1D-7,13D0,1D9,2D36/
C
C         C0 IS 0.5*LOG(2*PI)
C         C1...C7 ARE THE COEFFTS OF THE ASYMPTOTIC EXPANSION OF DLGAMA
C
      DATA C0,C1,C2,C3,C4,C5,C6,C7/
     *   0.91893 85332 04672 742D 0,  0.83333 33333 33333 333D-1,
     *  -0.27777 77777 77777 778D-2,  0.79365 07936 50793 651D-3,
     *  -0.59523 80952 38095 238D-3,  0.84175 08417 50841 751D-3,
     *  -0.19175 26917 52691 753D-2,  0.64102 56410 25641 026D-2/
C
C         S1 IS -(EULER'S CONSTANT), S2 IS PI**2/12
C
      DATA S1/-0.57721 56649 01532 861D 0/
      DATA S2/ 0.82246 70334 24113 218D 0/
C
      DATA ZERO/0D0/,HALF/0.5D0/,ONE/1D0/,TWO/2D0/
      DLGAMA=ZERO
      IF(X.LE.ZERO)GOTO 1000
      IF(X.GT.TOOBIG)GOTO 1000
C
C         USE SMALL-X APPROXIMATION IF X IS NEAR 0, 1 OR 2
C
      IF(DABS(X-TWO).GT.SMALL)GOTO 10
      DLGAMA=DLOG(X-ONE)
      XX=X-TWO
      GOTO 20
   10 IF(DABS(X-ONE).GT.SMALL)GOTO 30
      XX=X-ONE
   20 DLGAMA=DLGAMA+XX*(S1+XX*S2)
      RETURN
   30 IF(X.GT.SMALL)GOTO 40
      DLGAMA=-DLOG(X)+S1*X
      RETURN
C
C         REDUCE TO DLGAMA(X+N) WHERE X+N.GE.CRIT
C
   40 SUM1=ZERO
      Y=X
      IF(Y.GE.CRIT)GOTO 60
      Z=ONE
   50 Z=Z*Y
      Y=Y+ONE
      IF(Y.LT.CRIT)GOTO 50
      SUM1=SUM1-DLOG(Z)
C
C         USE ASYMPTOTIC EXPANSION IF Y.GE.CRIT
C
   60 SUM1=SUM1+(Y-HALF)*DLOG(Y)-Y+C0
      SUM2=ZERO
      IF(Y.GE.BIG)GOTO 70
      Z=ONE/(Y*Y)
      SUM2=((((((C7*Z+C6)*Z+C5)*Z+C4)*Z+C3)*Z+C2)*Z+C1)/Y
   70 DLGAMA=SUM1+SUM2
      RETURN
C
 1000 WRITE(6,7000)X
      RETURN
C
 7000 FORMAT(' *** ERROR *** ROUTINE DLGAMA :',
     *  ' ARGUMENT OUT OF RANGE :',D24.16)
      END

