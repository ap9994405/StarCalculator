subroutine bond_orders(pah)
  use types_module
  
  implicit none
  integer(kint) :: atom1,atom2,atom3,level,nelim,path
  integer(kint),dimension(2) :: atoms
  type(structure),intent(inout) :: pah
  type(structure) :: pahtmp
  type(vlonginteger) :: clarsingle,kekulesingle,clardouble,kekuledouble
  real(kreal) :: kekulepahreal,clarpahreal,kekuledoubleratio,clardoubleratio,clarsingleratio
  real(kreal) :: dist,distance
  integer i,j

  kekulepahreal=vli2real(pah%polynomial(1))
  clarpahreal=vli2real(clartotal(pah))

  write(*,'(X,A)')'Pi bond orders'
  write(*,'(X,A5,X,A5,X,A8,X,A8,3X,A12)')'atom1', 'atom2','Kekule','Clar','Input_length'
  do i=1,pah%nat
    do j=1,pah%neighbornumber(i)
!    atom1 = pah%initiallabel(i)
!    atom2 = pah%initiallabel(pah%neighborlist(j,i) 
     atom1 = i
     atom2 = pah%neighborlist(j,i)
     if (atom2 > atom1 ) then
!       write(*,*)pah%initiallabel(i),pah%initiallabel(pah%neighborlist(j,i))
        call create_nobond_daughter(pah,pahtmp,atom1,atom2)
        call find_ZZ_polynomial(pahtmp,0,0)
!        call print_ZZ_polynomial(pahtmp)
        kekulesingle=pahtmp%polynomial(1)
        clarsingle=clartotal(pahtmp)
        call destroypah(pahtmp)

        atoms(1)=atom1
        atoms(2)=atom2
        call create_noatoms_daughter(pah,pahtmp,2,atoms)
        call find_ZZ_polynomial(pahtmp,0,0)
!        call print_ZZ_polynomial(pahtmp)
                
        kekuledouble=pahtmp%polynomial(1)
        clardouble=clartotal(pahtmp)

        call destroypah(pahtmp)

        kekuledoubleratio=vli2real(kekuledouble)/kekulepahreal
        clardoubleratio=vli2real(clardouble)/clarpahreal
        clarsingleratio=vli2real(clarsingle)/clarpahreal        
        distance=dist(pah%nat,atom1,atom2,globalgeom)

        write(*,'(X,I5,I5,2F10.4,F12.6)')atom1,atom2,kekuledoubleratio,&
                        clardoubleratio + (1.0_kreal-clarsingleratio-clardoubleratio)*0.5_kreal,distance
        

     end if
   end do
 end do


end subroutine bond_orders
