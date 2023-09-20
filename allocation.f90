module allocation

   implicit none
!   save

   integer*4 :: i,j,k,t,jp,jm,n,nn,totT
   real*8 :: totTr
   integer*4 :: L
   integer*4 :: ii,jj, iki, isT1, jsT1, ik, jk, kke
   integer*4, dimension(:), allocatable :: num
   real*8, dimension(:,:), allocatable:: v
  integer*4, dimension(:,:), allocatable :: inn
  integer*4, dimension(:,:), allocatable :: borderver
  integer*4 :: couc
  integer*4, allocatable, dimension(:) :: coordNum, bound
   
!   integer*8, dimension(:) :: num_in
!   real*8, dimension(:,:):: v_in
!   integer*8, dimension(:,:) :: inn_in
   
   real*8 :: eta, lambda, beta, gamm, Ao, Co,  dt

   real*8, allocatable, dimension(:) :: coefficients
   real*8, dimension(:),  allocatable ::  fxx_temp, fyy_temp
   real*8, dimension(:), allocatable:: fxx, fyy
 !!  integer*8, dimension(:), allocatable :: mainarea,leftP,rightP,topP,bottomP,corners,boundary
 !!  integer*8, dimension(:), allocatable :: GleftP,GrightP,GtopP,GbottomP,Gcorners,Gboundary
   integer*4, dimension(:), allocatable :: workingzone
   real*8, dimension(:), allocatable :: vx,vy
   real*8, dimension(:), allocatable :: fx, fy
   real*8, dimension(:), allocatable :: edgelength, edgelengthIn
   real*8 :: area, perimeter
   real*8 :: totArea, d
   real*8 ::  pi, rann
   real*8 :: fx1,fy1,fx2,fy2,fx3,fy3
   integer*4 :: num_dim, inn_dim1,inn_dim2, v_dim1, v_dim2, borderver_dim
   integer*4, dimension(:), allocatable :: mainarea,insidebulk,leftP,rightP,topP,bottomP,corners,boundary
   integer*4, dimension(:), allocatable :: GleftP,GrightP,GtopP,GbottomP,Gcorners,Gboundary
   integer*4, dimension(:,:), allocatable ::  vn
!   integer*4, allocatable, dimension(:) :: T1_affected
   real*8, allocatable, dimension(:) :: Energy
   character(100) :: fname_inn, fname_num, fname_v
   integer*4 :: iunit_inn, iunit_num, iunit_v
   integer*4 :: data_interv, data_intervT1
   real*8 :: meanEd, varEd, stdEd
   real*8 :: meanEdIn, varEdIn, stdEdIn
   real*8, allocatable, dimension(:) :: etas
   real*8, allocatable, dimension(:) :: msdt, cellCentInit
   real*8 :: startTime, endTime






!   integer*4, dimension(:), allocatable :: intar
    



   contains 

   subroutine read_input

     implicit none
  
!     open(unit=121, file='para.in', status='old'); 
     open(112, file='para1_in.dat', status='old') 
     open(unit=121, file='para2_in.dat', status='old'); 
  
     read(121,*) L
     read(121,*) num_dim
     read(121,*) v_dim1
     read(121,*) v_dim2
     read(121,*) inn_dim1
     read(121,*) inn_dim2
     read(121,*) borderver_dim

     read(112,*)Ao
     read(112,*)Co
     read(112,*)lambda
     read(112,*)beta
     read(112,*)gamm
     read(112,*)eta
     read(112,*)totTr
     read(112,*)dt

     totT = int(totTr)

     close(121)
     close(112)
  
   end subroutine read_input

   subroutine allocate_arrays
     implicit none
     call read_input

     allocate(num(num_dim))
     allocate(v(v_dim1,v_dim2))
     allocate(inn(inn_dim1,inn_dim2))
     allocate(fxx(v_dim2), fyy(v_dim2))
     allocate(fxx_temp(v_dim2), fyy_temp(v_dim2))
     allocate(edgelength(L*L*inn_dim1))
     allocate(edgelengthIn(L*L*inn_dim1))
 
     allocate(workingzone(L*L+4*L+4))
     allocate(mainarea(L*L),leftP(L),rightP(L),topP(L),bottomP(L),corners(4))
     allocate(boundary(4*L))
     allocate(insidebulk(L*L - 8*L + 16)) 
     allocate(GleftP(L),GrightP(L),GtopP(L),GbottomP(L),Gcorners(4))
     allocate(Gboundary(4*L+4))
     allocate(vn(inn_dim2, inn_dim2))
!     allocate(T1_affected(4))
     allocate(coefficients(6))
     allocate(Energy(totT))
     allocate(msdt(totT))
     allocate(cellCentInit(L*L-8*L+16))
     allocate(borderver(2,borderver_dim)) 
     !allocate(etas(inn_dim1, L*L))
     allocate(etas(v_dim2))

     allocate(coordNum(L*L))
     allocate(bound(4*L))

    end subroutine allocate_arrays

    subroutine read_data
       open(1, file='num_in.dat',status='old')
       open(2, file='v_in.dat', status='old')
       open(3, file='inn_in.dat', status='old')
!       open(1, file='numNew.dat',status='old')
!       open(2, file='vNew.dat', status='old')
!       open(3, file='innNew.dat', status='old')
       open(2121, file='Borderver_in.dat', status='old')
       open(unit=282, file='motility_in.dat', status='old')
       open(1901, file='inside_in.dat', status = 'old')

       
       
    
       read(1,*)num
       read(2,*)v
       read(3,*)inn
       read(2121, *)borderver
       read(282,*)etas
       read(1901,*)insidebulk

!       write(*,*)'sizeof etas ', size(etas,1)

!       write(*,*)'etas11', etas(1,1)



    
    
       close(1)
       close(2)
       close(3)
       close(2121)
       close(282)

       coefficients = [lambda, beta, gamm, Ao, Co, eta]



    end subroutine read_data





end module allocation
