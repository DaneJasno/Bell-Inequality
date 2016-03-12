program error
implicit none
integer, parameter:: u=22 
real(8):: A, B, dA, dB, V, dV

open(u, file='visibilityHV3.txt')
read(u,*) A, dA, B, dB
close(u)


V=(A-B)/(A+B)

dV=sqrt(((1-V)/(A+B))**2*dA**2+((1+V)/(A+B))**2*dB**2)

write(*,'(f6.4,a4,f6.4,a3, f6.4)') V,'pm ', dV,'< ', 1./sqrt(2.)

end program

