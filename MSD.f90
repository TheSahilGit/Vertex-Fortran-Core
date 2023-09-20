subroutine MSD(L,v,inn,num,msdt)
  integer*4 :: ic, jc
  integer*4 :: L
  real*8, allocatable, dimension(:) :: num
  real*8, allocatable, dimension(:,:) :: inn, v
  integer*4 :: inndim1, inndim2
  real*8, allocatable, dimension(:) :: vx, vy
  real*8 :: msdt

  inndim1 = size(inn, 1)
  inndim2 = size(inn, 2)

  allocate(vx(inndim2), vy(inndim2))


  write(*,*)'inndim2= ', inndim2



end subroutine MSD
