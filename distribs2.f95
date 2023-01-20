program wealth_distrib

use random	
implicit none

integer :: values(8), i, j, t, N, m, Tt, p1, p2 
double precision,allocatable :: guys(:), wdist(:)
double precision :: a, b, xi, dx, totw

call date_and_time(values=values)
write(*,*) values
call random_seed(put=values)

Tt = 10
p1 = 7
p2 = 3
N = 10**p1
m = 10**p2
allocate(guys(1:N))
allocate(wdist(1:m+1))
guys = 100.
wdist = 0.
do i = 1, N
	guys(i) = guys(i) +random_normal()
end do
totw = sum(guys)
write(*,*) "total wealth =", totw

!open(69,file="guys.dat")
!do i = 1, N
!	write(69,*) guys(i)
!end do
!close (69)

a = minval(guys)
b = maxval(guys)
dx = (b-a)/m
write(*,*) "a=", a, "b=", b

do i = 1, N
	j = int((guys(i)-a)/dx) +1
	wdist(j) = wdist(j) +1
end do

open(58,file="wdist.dat")
do j = 1, m+1
    if(wdist(j) .gt. 0.)then
		write(58,*) log(1.*j), log(wdist(j))
!		write(58,*) j, wdist(j)
	end if
!	write(58,*) j, wdist(j)
end do
close(58)

open(47,file="total_wealth.dat")
do t = 1, Tt
	do i = 1, N
        xi = random_normal()
		xi = erf(xi)
		guys(i) = (1.+xi)*guys(i)
		
	end do
	totw = sum(guys)
	write(47,*) t, totw
end do
close(47)

!open(692,file="guysb.dat")
!do i = 1, N
!	write(692,*) guys(i)
!end do
!close (692)

a = minval(guys)
b = maxval(guys)
dx = (b-a)/m
write(*,*) "a'=", a, "b'=", b

wdist = 0.
do i = 1, N
	j = int((guys(i)-a)/dx) +1
	wdist(j) = wdist(j) +1
end do

open(582,file="wdistb.dat")
do j = 1, 50 !m+1
	if(wdist(j) .gt. 0.)then
		write(582,*) log(1.*j), log(wdist(j))
!		write(582,*) j, wdist(j)
	end if
end do
close (582)

end program