subroutine bond_orders(pah)
  use types_module
  use options_module
  
  implicit none
  integer(kint) :: atom1,atom2
  integer(kint),dimension(2) :: atoms
  type(structure),intent(inout) :: pah
  type(structure) :: pahtmp
  type(vlonginteger) :: clarsingle,kekulesingle,clardouble,kekuledouble
  real(kreal) :: kekulepahreal,clarpahreal,kekuledoubleratio,clardoubleratio,clarsingleratio
  real(kreal) :: dist,distance
  integer, allocatable :: pahoriginalmap(:)
  integer i,j

  allocate(pahoriginalmap(pah%nat))

! sort creates reverse label map in initiallabel. Now we have to reverse it again
  call makereversemap(pah%initiallabel,pahoriginalmap,pah%nat)
  
  kekulepahreal=vli2real(pah%polynomial(1))
  clarpahreal=vli2real(clartotal(pah))

  write(*,'(1X,A)')'Pi bond orders'
  if (.not. is_adjacencyfile) then
    write(*,'(1X,A5,1X,A5,1X,A8,1X,A8,3X,A12)')'atom1', 'atom2','Kekule','Clar','Input_length'
  else
    write(*,'(1X,A5,1X,A5,1X,A8,1X,A8,3X,A12)')'atom1', 'atom2','Kekule','Clar'
  end if

  do i=1,pah%nat
    do j=1,pah%neighbornumber(i)
     atom1 = i
     atom2 = pah%neighborlist(j,i)
     if (pahoriginalmap(atom2) > pahoriginalmap(atom1) ) then
        call create_nobond_daughter(pah,pahtmp,atom1,atom2)
        call find_ZZ_polynomial(pahtmp,0,0)
        kekulesingle=pahtmp%polynomial(1)
        clarsingle=clartotal(pahtmp)
        call destroypah(pahtmp)

        atoms(1)=atom1
        atoms(2)=atom2
        call create_noatoms_daughter(pah,pahtmp,2,atoms)
        call find_ZZ_polynomial(pahtmp,0,0)

        kekuledouble=pahtmp%polynomial(1)
        clardouble=clartotal(pahtmp)

        call destroypah(pahtmp)

        kekuledoubleratio=vli2real(kekuledouble)/kekulepahreal
        clardoubleratio=vli2real(clardouble)/clarpahreal
        clarsingleratio=vli2real(clarsingle)/clarpahreal
        if (.not. is_adjacencyfile) then
          
          distance=dist(pah%nat,atom1,atom2,globalgeom)
! only labels have to be remapped to original labels. Distance is calculated from geometry that is reordered
          atom1 = pahoriginalmap(i)
          atom2 = pahoriginalmap(pah%neighborlist(j,i))
          
          
          write(*,'(1X,I5,I5,2F10.4,F12.6)')atom1,atom2,kekuledoubleratio,&
                        clardoubleratio + (1.0_kreal-clarsingleratio-clardoubleratio)*0.5_kreal,distance
        else
          write(*,'(1X,I5,I5,2F10.4)')atom1,atom2,kekuledoubleratio,&
                        clardoubleratio + (1.0_kreal-clarsingleratio-clardoubleratio)*0.5_kreal 
        end if
     end if
   end do
 end do

 deallocate(pahoriginalmap)

end subroutine bond_orders
