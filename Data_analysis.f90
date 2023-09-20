module Data_analysis
  contains

    subroutine meanvarience(arrayin, dimin, mean, var, std)
      integer*4 :: iik, jjk
      real*8, allocatable, dimension(:) :: arrayin
      integer*4 :: dimin
      real*8 :: sums, sumsq, mean, var, std

      sums = 0.0d0
      sumsq = 0.0d0

        do iik = 1, dimin 
          sums = sums + arrayin(iik)
          sumsq = sumsq + arrayin(iik)*arrayin(iik)
        end do
        
        mean = sums/dimin
        var = (sumsq-sums*sums/dimin)/(dimin-1)
        std = sqrt(var)

    end subroutine meanvarience


  







end module Data_analysis
