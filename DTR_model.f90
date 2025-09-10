program DTRmodel

!Dicision Tree Regressor
!1.PARAMETER SETTING
!!!!!!!!!!

implicit none

!data & variable number(last variable must be objective)
integer, parameter :: nmax = 15
integer, parameter :: mmax = 6

!exit criteria(data number by leaf)
integer, parameter :: min = 2

!max layer number
integer, parameter :: layermax = 5

!!!KOKOMADE

real, allocatable :: y(:)
real, allocatable :: x(:,:)
real, allocatable :: x_1(:,:)
integer, allocatable :: b(:)
real, allocatable :: c(:)
real, allocatable :: d(:)
integer, allocatable :: e(:)

integer i, j, k, l, lmax, minnumber, sign
integer buf, m, n, p, q, r, s, pmax, layer
real score, threshold, scoremin

lmax = 2**layermax

allocate (y(1:nmax))
allocate (x(1:nmax,1:mmax))
allocate (b(1:lmax))
allocate (c(1:lmax))
allocate (d(1:lmax))
allocate (e(1:lmax))

!2.DATA READING
!!!!!!!!!!
open(80,file = 'BostonHousing_test.csv')
read(80, '()')
do n = 1, nmax
	read(80,*) x(n,:)
end do
close(80)

!3.MODEL LEARNING
!!!!!!!!!!
open(90,file = 'model.txt')
read(90, '()')
do l = 1, lmax -1
	read(90,*) buf, b(l), c(l), d(l), e(l)
end do
close(90)

!4.PREDICT (MAIN2)
!initialize
do i = 1, nmax
l = 1
do layer = 1, layermax
sign = 0
if(b(l)/=0 .or. l == 1)then 
	s = b(l)
	if(x(i, s)<c(l))then
		l = 2*l
	else
		l = 2*l +1
	end if
elseif(sign == 0)then
	y(i) = d(l)
	sign = 1
end if
end do
end do



!5.OUTPUT
!!!!!!!!!!

open(100,file = 'result.csv')
do i = 1, nmax
	write(100,*) x(i, mmax),", ", y(i)
end do
close(100)

deallocate (y)
deallocate (x)
deallocate (b)
deallocate (c)
deallocate (d)
deallocate (e)



end program DTRmodel
