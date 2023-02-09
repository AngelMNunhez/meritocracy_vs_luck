program wealth_distrib

use random	
implicit none

integer :: values(8), i, j, t, N, m, Tt, p1, p2 
double precision,allocatable :: wealth(:), wdist(:), talent(:)
double precision :: a, b, xi, dx, totw

call date_and_time(values=values)
write(*,*) values
call random_seed(put=values)

Tt = 20
p1 = 7
p2 = 3
N = 10**p1
m = 10**p2
allocate(wealth(1:N))
allocate(talent(1:N))
allocate(wdist(1:m+1))
wealth = 10.
talent = 0.
wdist = 0.
do i = 1, N
	wealth(i) = wealth(i) +random_normal()
    talent(i) = random_normal()
end do

open(69,file="wealth.dat")
do i = 1, N
	write(69,*) talent(i), wealth(i)
end do
close (69)

a = minval(wealth)
b = maxval(wealth)
dx = (b-a)/m
write(*,*) "a=", a, "b=", b

do i = 1, N
	j = int((wealth(i)-a)/dx) +1
	wdist(j) = wdist(j) +1
end do

open(58,file="wdist.dat")
do j = 1, m+1
    if(wdist(j) .gt. 0.)then
!		write(58,*) log(1.*j), log(wdist(j))
		write(58,*) j, wdist(j)
	end if
end do
close(58)

open(47,file="total_wealth.dat")
do t = 1, Tt
	do i = 1, N
        xi = random_normal()
		xi = erf(xi)
		wealth(i) = (1.+xi)*wealth(i)
		
	end do
	totw = sum(wealth)
	write(47,*) t, totw
end do
!close(47)

open(692,file="wealthb.dat")
do i = 1, N
!	write(692,*) talent(i), log(wealth(i))
	write(692,*) talent(i), wealth(i)
end do
close (692)

a = minval(wealth)
b = maxval(wealth)
dx = (b-a)/m
write(*,*) "a'=", a, "b'=", b

wdist = 0.
do i = 1, N
	j = int((wealth(i)-a)/dx) +1
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

!open(47,file="total_wealth.dat")
do t = Tt, 2*Tt
	do i = 1, N
!        xi = erf(talent(i))
		xi = 0.1*talent(i)
		wealth(i) = (1.+xi)*wealth(i)
		
	end do
	totw = sum(wealth)
	write(47,*) t, totw
end do
close(47)

open(693,file="wealthc.dat")
do i = 1, N
!    write(693,*) talent(i), log(wealth(i))
    if(talent(i) .le. 0.)then
	    write(693,*) talent(i), log(wealth(i))
    endif
end do
close (693)

end program