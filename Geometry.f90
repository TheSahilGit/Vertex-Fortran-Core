module Geometry 

  contains

   subroutine CalculateDistance(x1,y1,x2,y2,distance)
    implicit none
    real*8, intent(in) :: x1,y1,x2,y2
    real*8, intent(out) :: distance
    real*8 :: di


    di = (x1-x2)**2 + (y1-y2)**2
    distance = sqrt(di)

   end subroutine CalculateDistance

   subroutine CalculatePerimeter(x,y,dimn,perimeter)
       implicit none
       integer*4 :: i,j
       integer*4 :: dimn
       real*8, dimension(:), allocatable :: x,y
       real*8 :: perimeter
       real*8 :: d
   
   
   
   
       perimeter = 0.0d0
       d = 0.0d0
   
   
       do i=1,dimn
           j = i+1
           if(i==dimn)then
               j=1
           end if
           d = (x(i)-x(j))**2 + (y(i)-y(j))**2
           perimeter = perimeter + sqrt(d)
   
       end do

    end subroutine CalculatePerimeter

    subroutine CalculateArea(x,y,dimn,area)
      implicit none
      integer*4 :: i,j
      integer*4, intent(in) :: dimn
      real*8, dimension(:), allocatable :: x,y
      real*8 :: area
      real*8 :: sum1, sum2
      integer*4 :: n1,n2
  
  
  
  
  
      area = 0.0d0
      sum1 = 0.0d0
      sum2 = 0.0d0
  
  
      do i=1,dimn
          j = i+1
          if(i==dimn)then
              j=1
          end if
  
          sum1 = sum1 + x(i)*y(j)
          sum2 = sum2 + x(j)*y(i)
  
          area = (sum1-sum2)/2.0
      end do

    end subroutine CalculateArea

    
    subroutine PerpendicularBisector(x,y,d,xnew, ynew)
      
      real*8, dimension(:), allocatable :: x,y, xnew, ynew
      real*8 :: xm, ym
      real*8 :: d !The ratio of length and the minimum length. 

      xm = (x(1) + x(2))/2.0
      ym = (y(1) + y(2))/2.0

      xnew(1) = -d * y(1) + d * ym + xm
      xnew(2) = -d * y(2) + d * ym + xm

      ynew(1) = d * x(1) - d * xm + ym
      ynew(2) = d * x(2) - d * xm + ym


    end subroutine PerpendicularBisector





end module Geometry
