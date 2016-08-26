!*********************************************************************
!
! This benchmark test program is measuring a cpu performance
! of floating point operation by a Poisson equation solver.
!!
! If you have any question, please ask me via email.
! written by Ryutaro HIMENO, November 26, 2001.
! Version 3.0
! ----------------------------------------------
! Ryutaro Himeno, Dr. of Eng.
! Head of Computer Information Division,
! RIKEN (The Institute of Pysical and Chemical Research)
! Email : himeno@postman.riken.go.jp
! ---------------------------------------------------------------
! You can adjust the size of this benchmark code to fit your target
! computer. In that case, please chose following sets of
! (mimax,mjmax,mkmax):
! small : 65,33,33
! small : 129,65,65
! midium: 257,129,129
! large : 513,257,257
! ext.large: 1025,513,513
! This program is to measure a computer performance in MFLOPS
! by using a kernel which appears in a linear solver of pressure
! Poisson eq. which appears in an incompressible Navier-Stokes solver.
! A point-Jacobi method is employed in this solver as this method can 
! be easyly vectrized and be parallelized.
! ------------------
! Finite-difference method, curvilinear coodinate system
! Vectorizable and parallelizable on each grid point
! No. of grid points : imax x jmax x kmax including boundaries
! ------------------
! A,B,C:coefficient matrix, wrk1: source term of Poisson equation
! wrk2 : working area, OMEGA : relaxation parameter
! BND:control variable for boundaries and objects ( = 0 or 1)
! P: pressure
! -------------------
! -------------------
! "use portlib" statement on the next line is for Visual fortran
! to use UNIX libraries. Please remove it if your system is UNIX.
! -------------------
! use portlib
!
MODULE pres
REAL(kind=4), DIMENSION(:,:,:), ALLOCATABLE :: p
END MODULE pres

!
MODULE mtrx
REAL(kind=4), ALLOCATABLE :: a(:,:,:,:), b(:,:,:,:), c(:,:,:,:)
END MODULE mtrx

!
MODULE bound
REAL(kind=4), DIMENSION(:,:,:), ALLOCATABLE :: bnd
END MODULE bound

!
MODULE work
REAL(kind=4), ALLOCATABLE :: wrk1(:,:,:), wrk2(:,:,:)
END MODULE work

!
MODULE others
INTEGER :: mimax, mjmax, mkmax
INTEGER :: imax, jmax, kmax
REAL(kind=4), PARAMETER :: omega = 0.8
END MODULE others

!
!
PROGRAM HimenoBMTxp_F90
USE others
!
IMPLICIT NONE
!
INTEGER :: nn
INTEGER :: ic, icr, icm
REAL(kind=4) :: flop, xmflops2, score, gosa
REAL(kind=8) :: cpu0, cpu1, cpu, dt
  ! ttarget specifys the measuring period in sec
REAL(kind=4), PARAMETER :: ttarget = 60.0
REAL(kind=8), EXTERNAL :: second
!
CALL readparam()
!
!! Initializing matrixes
CALL initmem()
CALL initmt()
PRINT *, ' mimax=',mimax,' mjmax=',mjmax,' mkmax=',mkmax
PRINT *, ' imax=',imax,' jmax=',jmax,' kmax=',kmax
!
CALL system_clock(ic,icr,icm)
dt = 1.0 / real(icr,8)
PRINT '(2x,a,e10.5)', 'Time measurement accuracy : ',dt
!! Start measuring
!
nn = 3
PRINT *, ' Start rehearsal measurement process.'
PRINT *, ' Measure the performance in 3 times.'
!
cpu0 = second()
!! Jacobi iteration
CALL jacobi(nn,gosa)
!
cpu1 = second()
cpu = (cpu1 - cpu0) * dt
flop = real(kmax - 2,4) * real(jmax - 2,4) * real(imax - 2,4) * 34.0
IF (cpu .NE. 0.0) xmflops2 = flop / real(cpu,4) * 1.0e-6 * real(nn,4)
PRINT *, '  MFLOPS:',xmflops2,'  time(s):',cpu,gosa
!
!! end the test loop
nn = int(ttarget / (cpu / 3.0))
PRINT *, 'Now, start the actual measurement process.'
PRINT *, 'The loop will be excuted in',nn,' times.'
PRINT *, 'This will take about one minute.'
PRINT *, 'Wait for a while.'
!
!! Jacobi iteration
cpu0 = second()
!! Jacobi iteration
CALL jacobi(nn,gosa)
!
cpu1 = second()
cpu = (cpu1 - cpu0) * dt
IF (cpu .NE. 0.0) xmflops2 = flop * 1.0e-6 / real(cpu,4) * real(nn,4)
!
!!!      xmflops2=nflop/cpu*1.0e-6*float(nn)
!
PRINT *, ' Loop executed for ',nn,' times'
PRINT *, ' Gosa :',gosa
PRINT *, ' MFLOPS:',xmflops2,'  time(s):',cpu
score = xmflops2 / 82.84
PRINT *, ' Score based on Pentium III 600MHz :',score
!
!  pause
!   stop
END PROGRAM HimenoBMTxp_F90

!
!
SUBROUTINE readparam()
!
USE others
!
IMPLICIT NONE
!
CHARACTER(len=10) :: size
!
PRINT *, 'Select Grid-size:'
!  print *,'For example:'
PRINT *, 'Grid-size= '
PRINT *, '           XS (64x32x32)'
PRINT *, '           S  (128x64x64)'
PRINT *, '           M  (256x128x128)'
PRINT *, '           L  (512x256x256)'
PRINT *, '           XL (1024x512x512)'
!  print *,' Grid-size = '
!  read(*,*) size
size = "M"
CALL grid_set(size)
!
imax = mimax - 1
jmax = mjmax - 1
kmax = mkmax - 1
!
RETURN
END SUBROUTINE readparam

!
!
SUBROUTINE grid_set(size)
USE others
IMPLICIT NONE
CHARACTER(len=10), INTENT(IN) :: size
!
SELECT CASE(size)
CASE ("xs")
mimax = 65
mjmax = 33
mkmax = 33
CASE ("XS")
mimax = 65
mjmax = 33
mkmax = 33
CASE ("s")
mimax = 129
mjmax = 65
mkmax = 65
CASE ("S")
mimax = 129
mjmax = 65
mkmax = 65
CASE ("m")
mimax = 257
mjmax = 129
mkmax = 129
CASE ("M")
mimax = 257
mjmax = 129
mkmax = 129
CASE ("l")
mimax = 513
mjmax = 257
mkmax = 257
CASE ("L")
mimax = 513
mjmax = 257
mkmax = 257
CASE ("xl")
mimax = 1025
mjmax = 513
mkmax = 513
CASE ("XL")
mimax = 1025
mjmax = 513
mkmax = 513
CASE DEFAULT
PRINT *, 'Invalid input character !!'
STOP 
END SELECT
!
RETURN
END SUBROUTINE grid_set

!
!
!**************************************************************
SUBROUTINE initmt()
!**************************************************************
USE pres
USE mtrx
USE bound
USE work
USE others
!
IMPLICIT NONE
!
INTEGER :: i, j, k
!
!  a=0.0
!  b=0.0
!  c=0.0
!  p=0.0
!  wrk1=0.0   
!  wrk2=0.0   
!  bnd=0.0 
!_KZ start
!$omp parallel
!$omp do
!_KZ end
DO k = 1, mkmax
DO j = 1, mjmax
DO i = 1, mimax
a(i,j,k,1) = 0.0
a(i,j,k,2) = 0.0
a(i,j,k,3) = 0.0
a(i,j,k,4) = 0.0
b(i,j,k,1) = 0.0
b(i,j,k,2) = 0.0
b(i,j,k,3) = 0.0
c(i,j,k,1) = 0.0
c(i,j,k,2) = 0.0
c(i,j,k,3) = 0.0
p(i,j,k) = 0.0
wrk1(i,j,k) = 0.0
wrk2(i,j,k) = 0.0
bnd(i,j,k) = 0.0
END DO
END DO
END DO
!_KZ start
!$omp end do
!$omp end parallel
!_KZ end
!
!  a(1:imax,1:jmax,1:kmax,1:3)=1.0
!  a(1:imax,1:jmax,1:kmax,4)=1.0/6.0
!  c(1:imax,1:jmax,1:kmax,:)=1.0
!  bnd(1:imax,1:jmax,1:kmax)=1.0 
!  do k=1,kmax
!     p(:,:,k)=real((k-1)*(k-1),4)/real((kmax-1)*(kmax-1),4)
!  enddo
!_KZ start
!$omp parallel
!$omp do
!_KZ end
DO k = 1, kmax
DO j = 1, jmax
DO i = 1, imax
a(i,j,k,1) = 1.0
a(i,j,k,2) = 1.0
a(i,j,k,3) = 1.0
a(i,j,k,4) = 1.0 / 6.0
b(i,j,k,1) = 0.0
b(i,j,k,2) = 0.0
b(i,j,k,3) = 0.0
c(i,j,k,1) = 1.0
c(i,j,k,2) = 1.0
c(i,j,k,3) = 1.0
p(i,j,k) = real((k - 1) * (k - 1),4) / real((kmax - 1) * (kmax - 1),4)
wrk1(i,j,k) = 0.0
wrk2(i,j,k) = 0.0
bnd(i,j,k) = 1.0
END DO
END DO
END DO
!_KZ start
!$omp end do
!$omp end parallel
!_KZ end
!
RETURN
END SUBROUTINE initmt

!
!*************************************************************
SUBROUTINE initmem()
!*************************************************************
USE pres
USE mtrx
USE bound
USE work
USE others
!
IMPLICIT NONE
!
allocate( p(mimax,mjmax,mkmax) )
allocate( a(mimax,mjmax,mkmax,4),b(mimax,mjmax,mkmax,3),c(mimax,mjmax,mkmax,3) )
allocate( bnd(mimax,mjmax,mkmax) )
allocate( wrk1(mimax,mjmax,mkmax),wrk2(mimax,mjmax,mkmax) )
!
RETURN
END SUBROUTINE initmem

!
!*************************************************************
SUBROUTINE jacobi(nn,gosa)
USE pres
USE mtrx
USE bound
USE work
USE others
IMPLICIT NONE
INTEGER, INTENT(IN) :: nn
REAL(kind=4), INTENT(INOUT) :: gosa
INTEGER :: i, j, k, loop
REAL(kind=4) :: s0, ss
!
!
DO loop = 1, nn
gosa = 0.0
!_KZ start
!$omp parallel
!$omp do
!_KZ end
DO k = 2, kmax - 1
DO j = 2, jmax - 1
DO i = 2, imax - 1
s0 = a(i,j,k,1) * p(i + 1,j,k) + a(i,j,k,2) * p(i,j + 1,k) + a(i,j,k,3) * p(i,j,k + 1) + b(i,j,k,1) * (p(i + 1,j + 1,k) - p(i + 1,j - 1,k) - p(i - 1,j + 1,k) + p(i - 1,j - 1,k)) + b(i,j,k,2) * (p(i,j + 1,k + 1) - p(i,j - 1,k + 1) - p(i,j + 1,k - 1) + p(i,j - 1,k - 1)) + b(i,j,k,3) * (p(i + 1,j,k + 1) - p(i - 1,j,k + 1) - p(i + 1,j,k - 1) + p(i - 1,j,k - 1)) + c(i,j,k,1) * p(i - 1,j,k) + c(i,j,k,2) * p(i,j - 1,k) + c(i,j,k,3) * p(i,j,k - 1) + wrk1(i,j,k)
ss = (s0 * a(i,j,k,4) - p(i,j,k)) * bnd(i,j,k)
gosa = gosa + ss * ss
wrk2(i,j,k) = p(i,j,k) + omega * ss
END DO
END DO
END DO
!_KZ start
!$omp end do
!$omp end parallel
!_KZ end
!     
!     p(2:imax-1,2:jmax-1,2:kmax-1)= &
!          wrk2(2:imax-1,2:jmax-1,2:kmax-1)
!_KZ start
!$omp parallel
!$omp do
!_KZ end
DO k = 2, kmax - 1
DO j = 2, jmax - 1
DO i = 2, imax - 1
p(i,j,k) = wrk2(i,j,k)
END DO
END DO
END DO
!_KZ start
!$omp end do
!$omp end parallel
!_KZ end
!
END DO
!! End of iteration
RETURN
END SUBROUTINE jacobi

!
!
!
 FUNCTION second() result(rtime)
!
IMPLICIT NONE
!
INTEGER :: ic, ir, im
REAL(kind=8) :: rtime
!
CALL system_clock(ic,ir,im)
!
rtime = real(ic,8)
!
END  FUNCTION second

