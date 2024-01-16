program calculitos

!USO DE MODULOS 
use math, only : Lu_system,det,inverse
use NumericalRecipes





IMPLICIT NONE


!DECLARACION DE VARIABLES
real(8),dimension(:,:),allocatable :: A
real(8),dimension(:),allocatable :: B
real(8) :: Toler
real(8) :: deter
real(8) :: x
real(8),dimension(:,:),allocatable :: inv
integer :: N
integer :: i
integer :: j
integer :: ier
integer :: resp


!INTERFACES
interface
 function det33_d(a) RESULT(x)
  real(8), dimension(3,3), intent(in) :: a
  real(8)                             :: x
 end function det33_d
end interface

interface
 function inverseM_d(A,det,ier) RESULT(AINV)
  Use NumericalRecipes, ONLY: ludcmp,lubksb
  real(8), dimension(:,:)                         :: A
  real(8), optional                               :: det
  integer, optional                               :: ier
  real(8), dimension(size(A,1),size(A,1))         :: AINV
  integer                                         :: i,j,n
  integer, dimension(size(A,1))                   :: indx  
  integer                                         :: jer
  real(8)                                         :: d
  real(8), parameter                              :: TOLER = 1.d-44
  real(8), dimension(:,:), allocatable            :: AUX
  real(8)                                         :: factor
 end function inverseM_d
end interface


!PANTALLA
write(*,*)"=========================================================="
print*,"Hola, este programa permite resolver un sistema de ecuaciones"
print*,"del tipo AX=B"
write(*,*)"=========================================================="
print*,"indique el orden de la Matriz A"
read(*,*) N

!ALOCAR MATRICES
allocate(A(N,N),B(N),inv(N,N))

!ingresar por teclado la matriz A
print*, "ingrese los elementos por columna de una matriz A"
do i=1,N
  do j=1,N
   read(*,*) A(j,i)
  end do
end do
write(*,*)

!ingresar por teclado una matriz B
print*, "ingrese los elementos por columna del vector solucion B"
do i=1,N
read(*,*) B(i)
end do
write(*,*)


!calculo del determinante de A
if (N==3) then
deter=det33_d(A)
print*,"el determinante de la matriz A es:",deter 
end if
write(*,*)


!imprimir por pantalla las matrices A y B
print*,"la matriz A ingresada es"
do i=1,N
print*, A(i,:)
end do
write(*,*)



print*, "la matriz B ingresada es"
do i=1,N
print*,B(i)
end do
write(*,*)


!pedir tolerancia
print*, "escriba la tolerancia"
read(*,*),Toler
write(*,*)


!resolver AX=B
call Lu_system(A,B,ier,Toler,deter)
print*, "La solucion del sistema de ecuaciones AX=B es:"
do i=1,N
print*,B(i)
end do

!Preguntar por pantalla por la matriz inversa de A
write(*,*)
write(*,*)
print*, "Desea conocer la Matriz inversa de A?"
print*, "Si=1"
print*, "No=0"
read(*,*) resp

if(resp .eq. 1)then
inv=inverseM_d(A,deter,ier)
write(*,*)
print*,"La matriz inversa de A es"
do i=1,N
 print*, inv(i,:)
end do
else
print*, "gracias"
end if


end program


