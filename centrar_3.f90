      program centrar
      implicit real*8 (a-h,o-z)
      real :: a, b, c, promx, promy, promz, sumax, sumay, sumaz
      dimension x(1500,3)

      call  prom(promx,promy,promz)
      a = promx
      b = promy
      c = promz
      print *, 'Coordenadas Promedio:', a , b, c
      call  mov(a, b, c)

      end program centrar
      
          subroutine prom(promx, promy, promz)
          implicit real*8 (a-h,o-z)
          real :: promx, promy, promz, sumax, sumay, sumaz
          dimension x(1500,3)  !el progrma soporta hasta 1500 atomos
          integer i, k, n
          
          write (*,*) 'Numero de atomos:'
          read (*,*), n
          open(unit=3,file='coord.dat',status='old')
          
          do 1 i=1,n
          read(3,*)(x(i,k), k=1,3)
        1  continue
             sumax=0
             sumay=0
             sumaz=0
             do 3 i=1,n
              sumax=x(i,1)+sumax
              sumay=x(i,2)+sumay
              sumaz=x(i,3)+sumaz
 
        3  continue
             promx=sumax/n
             promy=sumay/n
             promz=sumaz/n
         close(3)
           return
           end

           subroutine mov(a, b, c)
!           implicit real*8 (a-h,o-z)
           dimension x(1500,3)
           integer i, k, n
           write (*,*) 'Numero de atomos a desplazar:'
           read (*,*), n
       
            open(unit=3,file='coord.dat',status='old')
            open(unit=6,file='salida.dat',status='new')
       
            do 1 i=1,n
            read(3,*)(x(i,k), k=1,3)

    1 continue
       
            do 3 i=1,n

             x(i,1)=x(i,1) - a
             x(i,2)=x(i,2) - b
             x(i,3)=x(i,3) - c

    2     write(6,9000) x(i,1),x(i,2),x(i,3),i
    3     continue

          close(6)
          close(3)
 9000   format(1x,3(f9.5,1x),i5)
 9001   format(1x,i5,6x,i5,6x,i5)

        
       return
       end
