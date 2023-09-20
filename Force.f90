module Force
  contains

  subroutine ForceCalculation
    
    use allocation
    use Geometry

    real*8, allocatable, dimension(:) :: ra1, ra2
!    real*8 :: etas
    
    
    pi = dacos(-1.0d0)
    

    call read_input

  !  Ao = 1.5d0
  !  Co = 2.0d0 * sqrt(Ao*pi)
  !  eta = 1.0d0
  !  lambda = 0.5d0 !1.50d0
  !  beta = 0.0d0
  !  gamm = 0.7d0


    beta = beta/(lambda*Ao)
    gamm = gamm/(lambda*(Ao)**1.5)
    

  !  write(*,*)'beta, gamma= ', beta, gamm
    

    
    allocate(ra1(inn_dim1), ra2(inn_dim2))

    ra1 = 0.0d0
    ra2 = 0.0d0

  !  write(*,*)'ra=', ra


    call TwinsArray

  !  open(213, file='testdata.dat', status = 'unknown')

    
  !  do k = 1, L*L+4*L+4
  !    write(213,*)workingzone(k)
  !    !write(213,*)GleftP(j)
  !  end do
  !  close(213)

    
  !  allocate(area(1:9*L*L),perimeter(1:9*L*L))
    
    fxx = 0.0d0
    fyy = 0.0d0
    fxx_temp = 0.0d0
    fyy_temp = 0.0d0
    totArea = 0.0d0
    ii = 0
    kke = 1

    do ii = 1,L*L !+4*L+4 
      i = workingzone(ii) ! i is the cell no. 
      nn = num(i)

    
      allocate(vx(1:nn), vy(1:nn))
      allocate(fx(1:nn), fy(1:nn))

      vx = v(1,inn(1:nn,i))
      vy = v(2,inn(1:nn,i))
      

     call CalculateArea(vx,vy,nn,area)
     call CalculatePerimeter(vx,vy,nn,perimeter)
     
  !   area = 0
     area = abs(area)
     

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
       
       call CalculateDistance(vx(j),vy(j),vx(jp),vy(jp),d)
       
  !     write(213,*)d
       
  !    write(*,*)area
       
       fx1 = - 2.0d0 * lambda * (area- Ao) * (vy(jm)-vy(jp)) ! Here jp is clockwise and jm is counterclockwise. So, that adjusts the -ve sign. 
       fx2 = - 2.0d0 *beta * (perimeter - Co) * (vx(j)-vx(jp))/d
       fx3 = - gamm * (vx(j)-vx(jp))/d
       
       fy1 = - 2.0d0 * lambda * (area- Ao) * (vx(jp)-vx(jm))
       fy2 = - 2.0d0 * beta * (perimeter - Co) * (vy(j)-vy(jp))/d
       fy3 = - gamm * (vy(j)-vy(jp))/d
       
       fx(j) = (fx1 + fx2 + fx3)/eta
       fy(j) = (fy1 + fy2 + fy3)/eta

       call random_number(ra1(j))
       call random_number(ra2(j))

       edgelength(kke) = d
       kke = kke + 1


     end do ! ends j loop
     
     fxx_temp(inn(1:nn,i)) = fx(1:nn)
     fyy_temp(inn(1:nn,i)) = fy(1:nn)
     



!     etas = 5.0/(lambda*Ao**(1.5)) ! Normalized. 

!      write(*,*)"Sizeof etas = ", size(etas)

!     etas = 0.0


!     write(*,*)"etas= ", etas(1,1)

     
     fxx(inn(1:nn,i)) = fxx(inn(1:nn,i)) + fxx_temp(inn(1:nn,i)) 

     fyy(inn(1:nn,i)) = fyy(inn(1:nn,i)) + fyy_temp(inn(1:nn,i))      

     
     if(ii<=L*L)then
       totArea = totArea + area
     end if

     deallocate(vx,vy,fx,fy)

    end do ! ends ii

    do i = 1,size(etas)
      call random_number(rann)
      fxx(i) = fxx(i) + etas(i)*(2.0d0 * rann-1)
      call random_number(rann)
      fyy(i) = fyy(i) + etas(i)*(2.0d0 * rann-1)
    end do



  !  totArea = sum(abs(area(1:L*L)))
    
    
  !  write(*,*)totArea
    
!  write(*,*)'borderver_dim= ', borderver_dim
!    ik = 3
!    write(*,*)'bv', borderver(2,ik)
!   do ik = 1,borderver_dim
!     write(*,*)borderver(2,ik),borderver(1,ik),inn(borderver(2,ik),borderver(1,ik))
!     fxx(inn(borderver(2,ik),borderver(1,ik))) = 0.0
!     fyy(inn(borderver(2,ik),borderver(1,ik))) = 0.0
!   end do
   

! Killing the forces at the boundary

    do ii = 1,size(boundary)
      i = boundary(ii)
      nn = num(i)
      fxx(inn(1:nn, i)) = 0.0d0
      fyy(inn(1:nn, i)) = 0.0d0
    end do

! ***********************************




  end subroutine

  subroutine ForcePeriodic(L_in,v_in,num_in,inn_in,fxx_in,fyy_in,fxx_out,fyy_out)

    integer*4 :: L_in
    integer*4, allocatable, dimension(:) :: num_in
    real*8, allocatable, dimension(:) :: fxx_in, fyy_in, fxx_out, fyy_out
    real*8, allocatable, dimension(:,:) :: v_in
    integer*4, allocatable, dimension(:,:) :: inn_in
    real*8, allocatable, dimension(:,:) :: temp_fx, temp_fy
    integer*4 :: inndim2, nnF

    inndim2 = size(inn_in,1)

!    write(*,*)inndim2

    allocate(temp_fx(inndim2, L_in), temp_fy(inndim2, L_in))



!                  ____ Right Ghost Panel _______
!             
!               L*L+1 : L*L+L  --> right ghost  panel. 
!               1:L            --> left real panel. 


    temp_fx = 0.0d0
    temp_fy = 0.0d0
   
     ii = 0
     do i = 1,L_in
       ii = ii + 1
       nnF = num_in(i)
       do j=1,nnF
         temp_fx(j,ii)=fxx_in(inn_in(j,i))
         temp_fy(j,ii)=fyy_in(inn_in(j,i))
       end do
     end do
   
   
     ii = 0
     do i = L_in*L_in+1,L_in*L_in+L_in
       ii = ii + 1
       nnF = num_in(i)
       do j=1,nnF
         fxx_in(inn_in(j,i))= temp_fx(j,ii)
         fyy_in(inn_in(j,i))= temp_fy(j,ii)
       end do
     end do



!                  ____ Left Ghost Panel _______
!             
!               3*L*L-L+1 : 3*L*L  --> Left ghost  panel. 
!               L*L-L+1 : L*L      --> Right real panel. 


    temp_fx = 0.0d0
    temp_fy = 0.0d0

     ii = 0
     do i = L_in*L_in-L_in+1,L_in*L_in
       ii = ii + 1
       nnF = num_in(i)
       do j=1,nnF
         temp_fx(j,ii)=fxx_in(inn_in(j,i))
         temp_fy(j,ii)=fyy_in(inn_in(j,i))
       end do
     end do



     ii = 0
     do i = 3*L_in*L_in-L_in+1,3*L_in*L_in
       ii = ii + 1
       nnF = num_in(i)
       do j=1,nnF
         fxx_in(inn_in(j,i))= temp_fx(j,ii)
         fyy_in(inn_in(j,i))= temp_fy(j,ii)
       end do
     end do



!                  ____ Top Ghost Panel _______
!             
!               3*L*L+1 : L : 4*L*L  --> Top ghost  panel. 
!               1 : L : L*L-L+1      --> Bottom real panel. 



    temp_fx = 0.0d0
    temp_fy = 0.0d0

     ii = 0
     do i = 1, L_in*L_in-L_in +1, L_in
       ii = ii + 1
       nnF = num_in(i)
       do j=1,nnF
         temp_fx(j,ii)=fxx_in(inn_in(j,i))
         temp_fy(j,ii)=fyy_in(inn_in(j,i))
       end do
     end do



     ii = 0
     do i = 3*L_in*L_in+1 , 4*L_in*L_in, L_in
       ii = ii + 1
       nnF = num_in(i)
       do j=1,nnF
         fxx_in(inn_in(j,i))= temp_fx(j,ii)
         fyy_in(inn_in(j,i))= temp_fy(j,ii)
       end do
     end do


!                  ____ Bottom Ghost Panel _______
!             
!               4*L*L + L : L : 5*L*L  --> Bottom ghost  panel. 
!               L : L : L*L        --> Top real panel. 



    temp_fx = 0.0d0
    temp_fy = 0.0d0

     ii = 0
     do i = L_in, L_in*L_in , L_in

       ii = ii + 1
       nnF = num_in(i)
       do j=1,nnF
         temp_fx(j,ii)=fxx_in(inn_in(j,i))
         temp_fy(j,ii)=fyy_in(inn_in(j,i))
       end do
     end do



     ii = 0
     do i = 4*L_in*L_in + L_in, 5*L_in*L_in, L_in
       ii = ii + 1
       nnF = num_in(i)
       do j=1,nnF
         fxx_in(inn_in(j,i))= temp_fx(j,ii)
         fyy_in(inn_in(j,i))= temp_fy(j,ii)
       end do
     end do

!               ______   Top right ghost corner. 
!             
!               5*L*L + 1   --> Top right ghost coner
!               1           --> Bottom left real corner

    temp_fx = 0.0d0
    temp_fy = 0.0d0

     ii = 0
     do i = 1,1

       ii = ii + 1
       nnF = num_in(i)
       do j=1,nnF
         temp_fx(j,ii)=fxx_in(inn_in(j,i))
         temp_fy(j,ii)=fyy_in(inn_in(j,i))
       end do
     end do


     ii = 0
     do i = 5*L_in*L_in + 1, 5*L_in*L_in + 1
       ii = ii + 1
       nnF = num_in(i)
       do j=1,nnF
         fxx_in(inn_in(j,i))= temp_fx(j,ii)
         fyy_in(inn_in(j,i))= temp_fy(j,ii)
       end do
     end do



!               ______   Top Left ghost corner. 
!             
!               9*L*L -L + 1   --> Top right ghost coner
!               L*L - L + 1    --> Bottom right real corner

    temp_fx = 0.0d0
    temp_fy = 0.0d0

     ii = 0
     do i = L_in*L_in - L_in + 1, L_in*L_in - L_in + 1

       ii = ii + 1
       nnF = num_in(i)
       do j=1,nnF
         temp_fx(j,ii)=fxx_in(inn_in(j,i))
         temp_fy(j,ii)=fyy_in(inn_in(j,i))
       end do
     end do


     ii = 0
     do i = 9*L_in*L_in - L_in + 1, 9*L_in*L_in - L_in + 1
       ii = ii + 1
       nnF = num_in(i)
       do j=1,nnF
         fxx_in(inn_in(j,i))= temp_fx(j,ii)
         fyy_in(inn_in(j,i))= temp_fy(j,ii)
       end do
     end do


!               ______   Bottom right ghost corner. 
!             
!               7*L*L + L   --> Bottom right ghost coner
!               L           --> Top left real corner

    temp_fx = 0.0d0
    temp_fy = 0.0d0

     ii = 0
     do i = L_in, L_in

       ii = ii + 1
       nnF = num_in(i)
       do j=1,nnF
         temp_fx(j,ii)=fxx_in(inn_in(j,i))
         temp_fy(j,ii)=fyy_in(inn_in(j,i))
       end do
     end do


     ii = 0
     do i = 7*L_in*L_in + L_in , 7*L_in*L_in + L_in 
       ii = ii + 1
       nnF = num_in(i)
       do j=1,nnF
         fxx_in(inn_in(j,i))= temp_fx(j,ii)
         fyy_in(inn_in(j,i))= temp_fy(j,ii)
       end do
     end do

!               ______   Bottom left ghost corner. 
!             
!               7*L*L    --> Bottom left ghost coner
!               L*L     --> Top right real corner

    temp_fx = 0.0d0
    temp_fy = 0.0d0

     ii = 0
     do i = L_in*L_in, L_in*L_in

       ii = ii + 1
       nnF = num_in(i)
       do j=1,nnF
         temp_fx(j,ii)=fxx_in(inn_in(j,i))
         temp_fy(j,ii)=fyy_in(inn_in(j,i))
       end do
     end do


     ii = 0
     do i = 7*L_in*L_in , 7*L_in*L_in 
       ii = ii + 1
       nnF = num_in(i)
       do j=1,nnF
         fxx_in(inn_in(j,i))= temp_fx(j,ii)
         fyy_in(inn_in(j,i))= temp_fy(j,ii)
       end do
     end do

     fxx_out = fxx_in
     fyy_out = fyy_in



  end subroutine ForcePeriodic




end module Force
