subroutine bond_orders(pah)
  use types_module
  
  implicit none
  integer(kint) :: atom1,atom2,atom3,level,nelim,path
  integer(kint),dimension(2) :: atoms
  type(structure),intent(inout) :: pah
  type(structure) :: pahtmp

  integer i,j

  do i=1,pah%nat
    do j=1,pah%neighbornumber(i)
!    atom1 = pah%initiallabel(i)
!    atom2 = pah%initiallabel(pah%neighborlist(j,i) 
     atom1 = i
     atom2 = pah%neighborlist(j,i)
     if (atom2 > atom1 ) then
!       write(*,*)pah%initiallabel(i),pah%initiallabel(pah%neighborlist(j,i))
        write(*,*)atom1,atom2
        call create_nobond_daughter(pah,pahtmp,atom1,atom2)
        call find_ZZ_polynomial(pahtmp,0,0)
        call print_ZZ_polynomial(pahtmp)
        call destroypah(pahtmp)

        atoms(1)=atom1
        atoms(2)=atom2
        call create_noatoms_daughter(pah,pahtmp,2,atoms)
        call find_ZZ_polynomial(pahtmp,0,0)
        call print_ZZ_polynomial(pahtmp)
        call destroypah(pahtmp)

     end if
   end do
 end do


end subroutine bond_orders
