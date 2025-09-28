program DTR_predict

!Dicision Tree Regressor
!1.PARAMETER SETTING
!!!!!!!!!!
implicit none

!data & variable number(explanatory variable)
integer, parameter :: nmax = 5
integer, parameter :: mmax = 5

!exit criteria(data number by leaf)
integer, parameter :: min = 2

!max layer number
integer, parameter :: layermax = 5

!!!KOKOMADE
integer n
real, allocatable :: x(:,:)

allocate (x(1:nmax,1:mmax))

!2.DATA READING
!!!!!!!!!!
open(80,file = 'test_data.csv')
read(80, '()')
do n = 1, nmax
	read(80,*) x(n,:)
end do
close(80)

call DTR_MODEL(x, nmax, mmax, min, layermax)

deallocate (x)
end program DTR_predict



!3.PREDICT (MAIN2)
!!!!!!!!!!
subroutine DTR_MODEL(x, nmax, mmax, min, layermax)

implicit none
real x(1:nmax,1:mmax)
integer nmax, mmax, min, layermax
integer i, l, s, lmax, sign, layer
real buf

real, allocatable :: y(:)
integer, allocatable :: b(:)
real, allocatable :: c(:)
real, allocatable :: d(:)
integer, allocatable :: e(:)

lmax = 2**layermax

allocate (b(1:lmax))
allocate (c(1:lmax))
allocate (d(1:lmax))
allocate (e(1:lmax))
allocate (y(1:nmax))

open(90,file = 'model.txt')
read(90, '()')
do l = 1, lmax -1
	read(90,*) buf, b(l), c(l), d(l), e(l)
end do
close(90)

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

open(100,file = 'result.csv')
do i = 1, nmax
	write(100,*) "predicted"
	write(100,*) y(i)
end do
close(100)

deallocate (y)
deallocate (b)
deallocate (c)
deallocate (d)
deallocate (e)

end subroutine DTR_MODEL
