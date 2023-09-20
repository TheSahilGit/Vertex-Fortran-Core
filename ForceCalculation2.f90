program ForceCalculation
  implicit none
  integer*8 :: i,j,k,jp,jm,n,nn,L
  integer*8 :: ii,jj
  real*8, dimension(1:900) :: num
  real*8, dimension(1:2,1:1760) :: v 
  integer*8, dimension(1:10,1:900) :: inn
  real*8 :: eta, lambda, beta, gamm, Ao, Co
  real*8, dimension(1:1760) :: fxx, fyy, fxx_temp, fyy_temp
!  integer*8, dimension(:), allocatable :: mainarea,leftP,rightP,topP,bottomP,corners,boundary
!  integer*8, dimension(:), allocatable :: GleftP,GrightP,GtopP,GbottomP,Gcorners,Gboundary
  integer*8, dimension(:), allocatable :: workingzone
  real*8, dimension(:), allocatable :: vx,vy
  real*8, dimension(:),allocatable :: area, perimeter
  real*8 :: totArea, d, pi
  real*8 :: fx1,fy1,fx2,fy2,fx3,fy3
  real*8, dimension(:), allocatable :: fx, fy



  L = 10 ! Remember to keep this consistant with the matlab code.  
  pi = acos(-1.0)
  Ao = 1.0
  Co = 2 * sqrt(Ao*pi)
  eta = 1.0
  lambda = 5.5
  beta = 1.0
  gamm = 1.5



!  allocate(mainarea(1:L*L),leftP(1:L),rightP(1:L),topP(1:L),bottomP(1:L),corners(1:4))
!  allocate(boundary(1:4*L))
!  allocate(GleftP(1:L),GrightP(1:L),GtopP(1:L),GbottomP(1:L),Gcorners(1:4))
!  allocate(Gboundary(1:4*L+4))
  allocate(workingzone(1:L*L+4*L+4))
  



  call TwinsArray(L,workingzone)

  open(213, file='testdata.dat', status = 'unknown')

  
  do k = 1, L*L+4*L+4
    write(213,*)workingzone(k)
    !write(213,*)GleftP(j)
  end do
  close(213)





  open(11, file='num.dat',status='old')
  open(22, file='v.dat', status='old')
  open(33, file='inn.dat', status='old')

  read(11,*)num
  read(22,*)v
  read(33,*)inn
  

  close(11)
  close(22)
  close(33)
  close(44)

  open(44, file='force.dat', status='unknown')

! Okay, now I have num, v and inn. Which will eventually go as input arrays. 
  
  allocate(area(1:L*L),perimeter(1:L*L))
  
  fxx = 0
  fyy = 0
  fxx_temp = 0
  fyy_temp = 0
  ii = 0
  do ii = 1,L*L!+4*L+4
    i = workingzone(ii)
    nn = num(i)
  
    allocate(vx(1:nn), vy(1:nn))
    allocate(fx(1:nn), fy(1:nn))

    vx = v(1,inn(1:nn,i))
    vy = v(2,inn(1:nn,i))
     

    call CalculateArea(vx,vy,nn,area(i))
    call CalculatePerimeter(vx,vy,nn,perimeter(i))
    
    fx1 = 0.0
    fx2 = 0.0
    fy1 = 0.0
    fy2 = 0.0
    fx3 = 0.0
    fy3 = 0.0

    do j=1,nn
      jp = j+1
      jm = j-1
      if(j == nn)then
        jp = 1
      elseif(j==1)then
        jm = nn
      end if 
      
      call CalculateDistance(vx(j),vy(j),vx(jm),vy(jm),d)
      
      fx1 = lambda * (area(i)- Ao) * (vy(jp)-vy(jm))
      fx2 = - 2 * beta * (perimeter(i) - Co) * (vx(j)-vx(jp))/d
      fx3 = - gamm * (vx(j)-vx(jp))
      
      fy1 = lambda * (area(i)- Ao) * (vx(jp)-vx(jm))
      fy2 = - 2 * beta * (perimeter(i) - Co) * (vy(j)-vy(jp))/d
      fy3 = - gamm * (vy(j)-vy(jp))
      
      fx(j) = (fx1 + fx2 + fx3)/eta
      fy(j) = (fy1 + fy2 + fy3)/eta

    end do ! ends j loop
    
    fxx_temp(inn(1:nn,i)) = fx(1:nn)
    fyy_temp(inn(1:nn,i)) = fy(1:nn)
    fxx(inn(1:nn,i)) = fxx(inn(1:nn,i)) + fxx_temp(inn(1:nn,i))
    fyy(inn(1:nn,i)) = fyy(inn(1:nn,i)) + fyy_temp(inn(1:nn,i))
    

!   area = abs(area)
    deallocate(vx,vy,fx,fy)

  end do

  totArea = sum(abs(area))
  
  do k=1,1760
   write(44,*)fxx(k), fyy(k)
  end do 
  
  write(*,*)totArea




end program
