module erwartungswert
implicit none
contains

subroutine EW(i,j,N, E,k,dE)
integer::i,j,k
real(8):: N(4,4), E(4), dE(4), sumN
sumN=(N(i,j)+N(i+1,j+1)+N(i,j+1)+N(i+1,j))
E(k)=(N(i,j)+N(i+1,j+1)-(N(i,j+1)+N(i+1,j)))/sumN
dE(k)=sqrt((1.-E(k)**2)/sumN)
end subroutine

subroutine mittelwert3(N,N2,N3,dN)
integer::i,j,ks=0, kp=0
real(8):: N(4,4), N2(4,4), N3(4,4), dN(4,4)

  do i=1,4
    do j=1,4
      N(i,j)=(N(i,j)+N2(i,j)+N3(i,j))/3.
      dN(i,j)=sqrt((N(i,j)**2+N2(i,j)**2+N3(i,j)**2)/3.-(N(i,j))**2) !Standardabweichung des Mittelwertes
      if (dN(i,j)>=sqrt(N(i,j)))then
        !write(*,*) 'SIGMA'
        ks=ks+1
      else
        !write(*,*) 'POISSON'
        kp=kp+1
        !i.e. standard deviation should not be used for dS caluclation
        !because error is smaller
      end if
    end do
  end do

if (kp>ks) then
write(*,*) 'use ds=sqrt(sum dp)**2'
else
write(*,*) 'use standard deviation for calculating dS!'
end if


end subroutine

end module


program bellineq
use erwartungswert
implicit none
integer:: i,j,k
integer, parameter:: u=22
real(8):: E(4)=0,dE(4)=0,N(4,4), S,dS, N2(4,4), N3(4,4), dN(4,4)=0

open(unit=u, file='bell2.txt')
read(u,*) N
close(u)

open(unit=u+1, file='bell3min.txt')
read(u+1,*) N2
close(u+1)

open(unit=u+2, file='bell3min.txt')
read(u+2,*) N3
close(u+2)

!call mittelwert3(N,N2,N3,dN)

k=1
  do i=1,3,2
    do j=1,3,2
      call ew(i,j,N,E,k,dE)
      !write(*,*) E(k),i,j,k
      k=k+1
    end do
  end do

  S=abs(E(1)-E(2))+abs(E(3)+E(4))
  dS=sqrt(sum(dE**2))

write(*,'(i2,a4,f8.3,a4,f5.3,a4,f8.3)') 2,'< ',S,'pm ',dS,'< ', 2*sqrt(2.0)

write(*,'(a19,f8.2,a40)') 'Parameter S ist um ', (S-2.)/dS, 'Standardabweichungen groesser als 2.'

end program

