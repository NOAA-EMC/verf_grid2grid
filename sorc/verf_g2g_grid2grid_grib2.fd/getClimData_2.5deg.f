	subroutine get_ClimaData(jmm,jdd,var,level,
     +   ix,iy, ngrid, clim1,clim2,noclim)

        real clipm1(101),clipm2(101)
        real clim1(ngrid,101),clim2(ngrid,101)

        character*80 clima1,clima2
        character*3  cmon(12)
        character*4  var, level  
        logical*1    cindex(ix,iy)
        integer noclim

       data cmon/'JAN','FEB','MAR','APR','MAY','JUN',
     +          'JUL','AUG','SEP','OCT','NOV','DEC'/


       jm1 = jmm
       if (jdd.le.15) jm2=jmm - 1
       if (jm2.lt.1)  jm2=jm2 + 12
       if (jdd.gt.15) jm2=jmm + 1
       if (jm2.gt.12) jm2=jm2 - 12

        
       clima1=trim(level)//trim(var)//'.'//cmon(jm1)
       clima2=trim(level)//trim(var)//'.'//cmon(jm2)
       print *," MONTH-1 is ",clima1
       print *," MONTH-2 is ",clima2
       open(unit=13,file=clima1,form='unformatted',status='old',
     +      ERR=100)
       open(unit=14,file=clima2,form='unformatted',status='old',
     +      ERR=100)
       rewind (13)
       rewind (14)

        goto 200

100     clim1=0.
        clim2=0.
        noclim=1 
        close(13)
        close(14)
        write(*,*) 'No climd data '
        return
  
200    continue

       noclim=0
       nxy = 0
       do ny = 1, iy
        do nx = 1, ix
         read (13) clipm1
         read (14) clipm2
         nxy = nxy + 1
         do n = 1, 101
           clim1(nxy,n)=clipm1(n)
           clim2(nxy,n)=clipm2(n)
         end do
        enddo
       enddo
       close (13)
       close (14)

       write(*,*) 'Read clim1, clim2 done'
       return
       end

