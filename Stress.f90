module Stress
  contains
    subroutine StressTensor(L,v,inn,num, coefficients, insidebulk, TotalSigma)
      use Geometry
      integer*4 :: L, cellNo, nn, inndim1
      integer*4 :: j, jp, i
      integer*4, allocatable, dimension(:) :: num
      integer*4, allocatable, dimension(:,:) :: inn
      real*8, allocatable, dimension(:,:) :: v
      real*8, allocatable, dimension(:) :: coefficients

      integer*4, dimension(:), allocatable :: insidebulk
      real*8, dimension(:), allocatable :: vx, vy
      real*8 :: area, perimeter, term1, term2, totalarea
      real*8, allocatable, dimension(:,:) :: sigma, TotalSigma
      real*8 :: lambda, beta, gamm, Ao, Co, eta
      real*8 :: X, Y, R

      inndim1 = size(inn(:,1))
      
      allocate(vx(1:inndim1) , vy(1:inndim1))
      allocate(sigma(2,2))


      vx = 0.0d0
      vy = 0.0d0

      lambda = coefficients(1)
      beta = coefficients(2)
      gamm = coefficients(3)
      Ao = coefficients(4)
      Co = coefficients(5)
      eta = coefficients(6)

      beta = beta/(lambda*Ao)
      gamm = gamm/(lambda*(Ao)**1.5)

      !write(*,*)"coeffs=", coefficients

 !     write(*,*)"We are inside stress subroutine"
      !write(*,*)num
     
     
      term1 = 0.0d0
      term2 = 0.0d0

      sigma = 0
      totalarea = 0.0d0
      TotalSigma = 0.0d0


!      write(*,*)'size_inside', size(insidebulk)

      do i = 1,size(insidebulk) 
         cellNo = insidebulk(i)
         nn = num(cellNo)
         vx(1:nn) = v(1,inn(1:nn,cellNo))
         vy(1:nn) = v(2,inn(1:nn,cellNo))
         call CalculateArea(vx,vy,inndim1,area)
         area = abs(area)
         totalarea = totalarea + area
      end do


      do i = 1,size(insidebulk)

        vx = 0.0d0;
        vy = 0.0d0; 
        
        cellNo = insidebulk(i)

        nn = num(cellNo)

        vx(1:nn) = v(1,inn(1:nn,cellNo)) 
        vy(1:nn) = v(2,inn(1:nn,cellNo)) 
      
        
        call CalculateArea(vx,vy,nn,area)
        area = abs(area)
        
        if(area < 1e-5)then
          write(*,*)"Ooopsi", cellNo, area
          !stop
        end if

        call CalculatePerimeter(vx,vy,nn,perimeter)

        term1 =  2.0d0 * lambda * (area - Ao)
        term2 = (2.0d0 * beta*perimeter + gamm)/(2.0d0*area)

        do j = 1,nn
          jp = j+1
          if (j==nn)then
            jp = 1
          end if
          X = vx(jp) - vx(j)
          Y = vy(jp) - vy(j)
          R = sqrt(X*X + Y*Y)

  !        write(*,*) X*Y

          sigma(1,1) = sigma(1,1) + X*X/R
          sigma(1,2) = sigma(1,2) + X*Y/R
          sigma(2,1) = sigma(2,1) + Y*X/R
          sigma(2,2) = sigma(2,2) + Y*Y/R


        end do

        sigma(1,1) = term1  + term2 * sigma(1,1)
        sigma(1,2) = term2 * sigma(1,2)
        sigma(2,1) = term2 * sigma(2,1)
        sigma(2,2) = term1 + term2 * sigma(2,2)



  !      do i=1,2
  !        write(*,*) (sigma(i,j), j=1,2)
  !      end do
        
        TotalSigma = TotalSigma + sigma * area/totalarea


    end do
      

!        do i=1,2
!          write(*,*) (TotalSigma(i,j), j=1,2)
!        end do

    


    end subroutine StressTensor





end module Stress
