integer, parameter :: n=11964,nbin=1000
real, dimension(3,n) :: x
real, dimension(8) :: tmp8
integer, dimension(nbin) :: icount

open(10,file='/cita/home/pen/scr/CS095841.det.webarchive',status='old')
do i=1,n
	read(10,*) tmp8,x(:,i)
enddo
write(*,*) sqrt(sum(x**2,2)/n)
icount=0
!$omp parallel do default(none) private(j,idist,dist) shared(x) reduction(+:icount)
do i=1,n
  do j=i+1,n
    dist=sqrt(sum((x(:,i)-x(:,j))**2))
    idist=dist*100
    if (idist>0 .and. idist.le.nbin) icount(idist)=icount(idist)+1
  enddo
enddo
open(10,file='count.dat')
do i=1,nbin
write(10,*) i/100.,sum(icount(:i)*1./n)
enddo
end
