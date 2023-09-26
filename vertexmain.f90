! Here what I am not using any fortran library or subroutine to create the initial voronoi structure. 
! As, I need voronoi only to create the initial sturcture, I do it using matlab ( It is easier their) and write those
! data in a '*.dat' file, and then load those data in this fortran code. 
! It would not slow down the speed as we are gonna do it only once at the start and use the data file for the rest.
! (The time taken to create voronoi diagram with 100x100 points is approx 30 sec.) 
! REMEMBER : If we want to make changes like perfect honeycomb to randomly structured points, those we'll have to 
! do in the matlab code 'Main.m' . 



program vertexmain
  use allocation
  use array_info
  use System_Info
  use T1_Swap
  use Force
  use Data_analysis
  use Stress

!  integer*8 :: i,j,k, n, L
!  integer*8 :: t, totT
!  integer*8, dimension(1:900) :: num
!  real*8, dimension(1:2,1:1760) :: v 
!  integer*8, dimension(1:10,1:900) :: inn
!  real*8, dimension(1:1760) :: fxx, fyy
!!  integer*8, dimension(1:8100) :: num
!!  real*8, dimension(1:2,1:16096) :: v 
!!  integer*8, dimension(1:16,1:8100) :: inn
!!  real*8, dimension(1:16096) :: fxx, fyy
!  real*8 :: dt
!
!! Remember those dimensions mentioned above will change with changes in L, so check those and give accordingly. 
!! Also, for fixed L, dimension of v may come out different for different randomization. 
!! So, always check what the dimension is and then run the code, otherwise may face some runtime error. 
!! In future, I may automate the process where it will take the dimension from the matlab code  automatically (somehow). 
!! Till then let's do it this way. 
!! Also, I'v changed the first (Inf,Inf) point to (9999,9999). This changes nothing, just keep in mind. 
!
!
!
!
!  L = 10 ! Remember to keep this consistant with the matlab code.  
!
!  v = 0.0d0
!  inn = 0
!  num = 0
!  fxx = 0.0d0
!  fyy = 0.0d0

call cpu_time(startTime)

  call read_input
  call allocate_arrays


!  open(1, file='num.dat',status='old')
!  open(2, file='v.dat', status='old')
!  open(3, file='inn.dat', status='old')
!  
!  read(1,*)num
!  read(2,*)v
!  read(3,*)inn 
!  
!
!  close(1)
!  close(2)
!  close(3)
  
  call read_data

!  write(*,*)'borderver= ', borderver(:,10)

!  write(*,*)'coeffs= ', coefficients

  open(11, file='numOut.dat',status='unknown')
  open(21, file='vOut.dat', status='unknown')
  open(31, file='innOut.dat', status='unknown')


  open(221, file='vn.dat', status='unknown') 
  
  open(44, file='force.dat', status='unknown')
  open(47, file='Energy.dat', status='unknown')

  open(908, file ='final_data.dat', status='unknown')

  open(1421, file='StressTensor.dat', status='unknown')
  
!  write(*,*)shape(inn)
!  write(*,*)shape(num)
!  write(*,*)shape(v)

!************************************************************************************************
!  Format (or Psudocode):
!************************************************************************************************
!  do time=1,total time
!  **(DONE)**  1. ForceCalculation(L,v,inn,num,fx,fy) --> input- (L,v,inn,num); Output- (fx,fy)
!  **(DONE)**  2. ForcePeriodic(L,num,inn,fx,fy,newfx,newfy) --> input(L,num,inn,fx,fy); Output-(newfx,newfy)
!    3. vx = vx + dt*fx; vy = vy + dt*fy --> Something like this. (Or RK4)
!    4. Do T1, T2 etc. 
!  end do
!  
!  5. Write the final v,inn and num to data files which will be loaded in matlab to plot the final

   
!  totT = 1000
!  dt = 0.001d0

  ! For hard tissue dt = 0.001d0 and totT = 1000
  ! works fine. 


 
!  write(*,*)'inndim2_maincode= ', inn_dim2

  
  
!  call neighbor_data(inn,inn_dim1,inn_dim2,num,vn)
!  call find_T1(L,v,inn,num,0.2d0,is,js)
!  call find_T1_Affected(L,v,inn,num,vn,inn_dim2,is,js, T1_affected)
!  call T1_core(L,v,inn,num,is,js, T1_affected, v, inn,num)
  
!  write(*,*)'T_affected_in_main_code = ', (T1_affected(j), j=1,4)
!  write(*,*)'is, js=', is, js
!  write(*,*)'inn_dim2= ', inn_dim2

  
!  call Do_T1(L,v,inn,num, 0.2d0, v,inn,num)

  do i = 1, inn_dim2
    write(221,*)(vn(i,j), j=1,inn_dim2)
  end do
  

!  call find_T1(L,v,inn,num,0.1d0, isT1, jsT1)
  
!  write(*,*)'isT1, jsT1 = ', isT1, jsT1

  iunit_inn = 532
  iunit_num = 621
  iunit_v = 958

  data_interv = 1
  data_intervT1 = 50

! *********************************************
! ******** Dynamic Boundary Finding ***********
! *********************************************
!
!  do iki = 1,100
!    write(*,*)'iki', iki
!    call Do_T1(L,v,inn,num, 0.05d0, v,inn,num)
!  end do

!  call neighbor_data(inn,inn_dim1, inn_dim2, num, vn)
!
!  open(32323, file='vnTemp.dat', status = 'unknown')
!  open(3213, file='coordNum.dat', status = 'unknown')
!  
!  do i=1,inn_dim2
!    write(32323, *)(vn(i,j), j=1, inn_dim2)
!  end do
!  
!  do i = 1, L*L
!    couc = 0
!    do j = 1, L*L
!      if(vn(i,j).ne.0)then
!        couc = couc + 1
!      end if
!      coordNum(i) = couc
!    end do
!  end do
!
!
!  couc = 0
!  do i=1,L*L
!    if(coordNum(i)<5)then
!      couc = couc + 1
!      bound(couc) = i
!    end if
!  end do
!
!  write(3213,*) bound
!
!  close(32323)
!
!*********************************************

 call CellCentre(L,v,inn,num, cellCentInit)

! open(9932, file='cellCentInit.dat', status= 'unknown')
! write(9932,*)cellCentInit
! close(9932)

!write(*,*)L*L-8*L+16


  do t=1,totT

    call StressTensor(L,v,inn,num, coefficients, insidebulk, TotalSigma)

!    do i=1,2
!      write(*,*)"Total Sigma=", (TotalSigma(i,j), j=1,2)
!    end do



!!     call CellCentre(L,v,inn,num, cellCentInit)

!*********************************************************************
    !if(t.eq.1.or.modulo(t,10).eq.0.0d0)then
    if(modulo(t,data_interv).eq.0)then
!      write(fname_inn, '("data/inn/inn_", I6.6,".dat")')(t)
!      open(unit = iunit_inn, file=fname_inn, form = 'unformatted', status='unknown')
      
!      write(fname_num, '("data/num/num_", I6.6,".dat")')(t)
!      open(unit = iunit_num,file=fname_num, form ='unformatted', status='unknown')
      
!      write(fname_v, '("data/v/v_", I6.6,".dat")')(t)
!      open(unit = iunit_v, file=fname_v, form='unformatted', status='unknown')
 
 ! end if
 !if(t.eq.1.or.modulo(t,10).eq.0.0d0)then
 !  if(modulo(t,data_interv).eq.0)then

!       write(iunit_inn)((inn(i,j),i=1,inn_dim1),j=1,inn_dim2)
     
!       write(iunit_num)(num(i), i=1,num_dim)

!      write(iunit_v)((v(i,j), i=1,v_dim1),j=1,v_dim2)
   end if

!   close(iunit_inn)
!   close(iunit_num)
!   close(inuit_v)

!   iunit_inn = iunit_inn + 1
!   iunit_num = iunit_num + 1
!   iunit_v = iunit_v + 1

!******************************************************************
   
    if(t==1)then
      do iki = 1,100
        write(*,*)'iki', iki
         call Do_T1(L,v,inn,num, 0.1d0, v,inn,num)
      end do
    end if


    if(modulo(t,data_intervT1).eq.0.0d0)then
      do iki = 1,10
         call Do_T1(L,v,inn,num, 0.1d0, v,inn,num)
      end do
    end if




    call ForceCalculation
!    call ForcePeriodic(L,v,num,inn,fxx,fyy,fxx,fyy)

    v(1,:) = v(1,:) + dt * fxx(:)
    v(2,:) = v(2,:) + dt * fyy(:)

    
   write(*,*)'t= ', t
   write(*,*)'tttttttttttttttttttttttttttttttt'

   if(t.eq.1)then
     write(*,*)'dd'
     edgelengthIn = edgelength
   end if

    
    call Calculate_Energy(L,v,num,inn,coefficients,Energy(t))
!    call MeanSqDisp(L,v,inn,num, cellCentInit, msdt(t))
    
!    write(*,*)cellCent

!-----------------------------------------------------------
!    if(Energy(t).le.0.0001d0)then
!      exit
!    endif
!----------------------------------



    close(345)






!   close(iunit_inn)
!   close(iunit_num)
!   close(inuit_v)


    do i=1,2
      write(1421,*)(TotalSigma(i,j), j=1,2)
    end do

!    do i=1,2
!      do j=1,2
!        if(abs(TotalSigma(i,j)).gt.1.0e3)then
!          goto 922
!        end if 
!      end do
!    end do



    

  end do ! ends t

  close(1421)


 
 open(8989, file='msd.dat', status='unknown')
 do t=1,totT
   write(8989, *)msdt(t)
 end do
 close(8989)

!  write(*,*)'dt= ', dt

  call meanvarience(edgelength, kke, meanEd, varEd, stdEd)
  call meanvarience(edgelengthIn, kke, meanEdIn, varEdIn, stdEdIn)

  write(*,*)'meanIn, varIn, stdIn= ', meanEdIn, varEdIn, stdEdIn
  write(*,*)'mean, var, std= ', meanEd, varEd, stdEd


  do k=1,v_dim2
   write(44,*)fxx(k), fyy(k)
  end do 

        

  do i=1,num_dim
    write(11,*)num(i)
  end do

  do j= 1,v_dim2
    write(21,*)(v(i,j), i=1,v_dim1)
  end do

  do j = 1,inn_dim2
    write(31,*)(inn(i,j),i=1,inn_dim1)
  end do


  do t = 1, totT
    write(47,*)Energy(t)
  end do

  write(908,*)beta, gamm, varEd

!  write(*,*)'kke = ',kke
!  write(*,*)'edglelenth=', edgelength 
!  write(*,*)'inndim2 = ', L*L*inn_dim1


  close(11)
  close(21)
  close(31)
  close(221)
  close(47)
  close(908)
  close(44)

  call cpu_time(endTime)

  open(2632, file='cputime.dat', status='unknown')
  write(2632,*) '("Time = ",f6.4," seconds.")',endTime-startTime
  close(2632)


end program 
