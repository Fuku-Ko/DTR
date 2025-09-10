program DTR

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
integer n
real, allocatable :: x(:,:)

allocate (x(1:nmax,1:mmax))

!2.DATA READING
!!!!!!!!!!
open(80,file = 'train_data.csv')
read(80, '()')
do n = 1, nmax
	read(80,*) x(n,:)
end do
close(80)

call DTR_TRAIN(x, nmax, mmax, min, layermax)

deallocate (x)
end program DTR

!3.MACHINE LEARNING (MAIN1)
!!!!!!!!!!
subroutine DTR_TRAIN(x, nmax, mmax, min, layermax)

implicit none
real x(1:nmax,1:mmax)
integer nmax, mmax, min, layermax
integer rightnum, leftnum
real right_div, left_div, right_ave, left_ave
real right, left
integer i, j, k, l, lmax, minnumber, sign, sign_obj
integer m, p, q, r, pmax, layer
real buf, score, threshold, scoremin

real, allocatable :: y(:)
real, allocatable :: z(:,:,:)
real, allocatable :: x_1(:,:)
integer, allocatable :: b(:)
real, allocatable :: c(:)
real, allocatable :: d(:)
integer, allocatable :: e(:)

lmax = 2**layermax

allocate (z(1:lmax,1:nmax,1:mmax))
allocate (b(1:lmax))
allocate (c(1:lmax))
allocate (d(1:lmax))
allocate (e(1:lmax))

!initialize
b(1) = 0
c(1) = 0
d(1) = 0.0
e(1) = nmax
do i = 1, nmax
do j = 1, mmax
	z(1,i,j) = x(i,j)
end do
end do
do l = 2, lmax
	b(l) = 0
	c(l) = 0
	d(l) = 0.0
	e(l) = 0
	do i = 1, nmax
	do j = 1, mmax
		z(l,i,j) =0.0
	end do
	end do
end do

!by leaf (l)
do layer = 1, layermax
do l = 2**(layer -1), 2**layer -1
pmax = e(l)
sign = 0

!leaf categorize
if (layer == layermax) then
	b(l) = 0
	c(l) = 0
else

!matrix prepare
allocate (x_1(1:pmax,1:mmax))
allocate (y(1:pmax))
do i = 1, pmax
do j = 1, mmax
		x_1(i,j) = z(l,i,j)
end do
end do

!by variable (search best threshold)
left = 0.0
right = 0.0
do m = 1, mmax-1

!sort
do i = 1, pmax-1
do j = i+1, pmax
	if (x_1(i,m)>x_1(j,m)) then
		do k = 1, mmax
			buf = x_1(i,k)
			x_1(i,k) = x_1(j,k)
			x_1(j,k) = buf
		end do
	end if
end do
end do

do i = 1, pmax
	y(i) = x_1(i, mmax)
end do


!score calculate and threshold dicision
do i = 1, pmax-1
	
	if (x_1(i,m) == x_1(i+1,m)) then
	else
		left = 0.0
		left_div = 0.0
		right = 0.0
		right_div = 0.0
		score = 0.0
		
		do j = 1, i
			left = left +y(j)
		end do
		left = left*1.0/(i*1.0)
		do j = 1, i
			left_div = left_div +(y(j)-left)*(y(j)-left)
		end do
		
		do j = i +1, pmax
			right = right +y(j)
		end do
		right = right*1.0/((pmax -i)*1.0)
		do j = i +1, pmax
			right_div = right_div +(y(j)-right)*(y(j)-right)
		end do
		
		score = sqrt((left_div +right_div)/(pmax*1.0))
		
		if(score<scoremin .or. sign == 0)then
			sign = 1
			scoremin = score
			minnumber = m
			left_ave = left
			right_ave = right
			threshold = (x_1(i,m) + x_1(i+1,m))*0.5
		end if
	end if
end do

end do

!not make leaf(all same value or category)
sign_obj = 0
do i = 1, pmax-1
	if(y(i) == y(i+1))then
	else
	sign_obj = 1
	end if
end do
if (sign == 0 .or. sign_obj == 0) then
	b(l) = 0
	c(l) = 0
else

!make leaf by threshold
q = 1
r = 1
do i = 1, pmax
	if (x_1(i,minnumber)<threshold) then
		do j = 1, mmax
			z(2*l,q,j) = x_1(i,j)
		end do
		e(2*l) = e(2*l) +1
		q = q +1
	else
		do j = 1, mmax
			z(2*l +1,r,j) = x_1(i,j)
		end do
		e(2*l + 1) = e(2*l + 1) +1
		r = r +1
	end if
end do

!memorize leaf
if(e(2*l)<min .or. e(2*l + 1)<min)then
	b(l) = 0
	c(l) = 0
	e(2*l) = 0
	e(2*l +1) = 0
else
	b(l) = minnumber
	c(l) = threshold
	d(2*l) = left_ave
	d(2*l + 1) = right_ave
end if
end if

deallocate (x_1)
deallocate (y)
end if

end do
end do

!4.MODEL
!!!!!!!!!!

open(100,file = 'model.txt')
open(110,file = 'check.txt')
write(100,*) 'leaf, ','category(0 means stop), ','threshold, ','value(predict), ','data_number'
do layer = 1, layermax
	write(110,*)'layer=', layer
	write(110,*) 'leaf, ','category(0 means stop), ','threshold, ','value(predict), ','data_number'
	do l = 2**(layer -1), 2**layer -1
		write(100,*) l, b(l), c(l), d(l), e(l)
		if(e(l)>0)then
			write(110,*) l, b(l), c(l), d(l), e(l)
		end if
	end do
end do
close(100)
close(110)

deallocate (z)
deallocate (b)
deallocate (c)
deallocate (d)
deallocate (e)

end subroutine DTR_TRAIN
