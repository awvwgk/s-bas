* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 1711
*     Copyright (C) 2017 awvwgk
*
*     This program is free software: you can redistribute it and/or 
*     modify it under the terms of the GNU General Public License as 
*     published by the Free Software Foundation, either version 3 of 
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be useful,
*     but WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program.  If not, see www.gnu.org/licenses/.
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 1711
      program soul
      use global
      implicit none
C - - all interfaces  - - - - - - - - - - - - - - - - - - - - - - - 1710
      interface
         subroutine rdargv(fname,bname,chrg)
         character(len=:), allocatable,intent(out) :: fname
         character(len=:), allocatable,intent(out) :: bname
         integer,intent(out) :: chrg
         end subroutine rdargv
         subroutine readbas(basdir,bname)
         use global
         character(len=*),intent(in) :: basdir
         character(len=*),intent(in) :: bname
         end subroutine readbas
         subroutine calcnao(nat,nao)
         use global, only : atom, nshell, lshell, mtbl
         integer, intent(in)  :: nat
         integer, intent(out) :: nao
         end subroutine calcnao
         subroutine readxyz(fname,nat)
         use global
         character(len=*),intent(in) :: fname
         integer,intent(out) :: nat
         end subroutine
         subroutine electro(nat,chrg,nel)
         use global, only : atom
         integer,intent(in)  :: chrg
         integer,intent(out) :: nel
         integer,intent(in)  :: nat
         end subroutine electro
         subroutine ovlp(nat,nao)
         use global
         integer,intent(in)  :: nat,nao
         end subroutine ovlp
      end interface
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1710
      integer :: i,j,k
C I/O - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1710
      integer :: nargs
      character(len=:),allocatable :: fname
      character(len=:),allocatable :: bname
      character(len=99)  :: basdir
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1710
      integer :: nat,nel,nao,chrg

C I/O - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1710
      call rdargv(fname,bname,chrg)
      call banner
      call options(basdir)

C GEO read coordinates  - - - - - - - - - - - - - - - - - - - - - - 1709
      print'(''* read coordinates from'',x,a)', trim(fname)
      call readxyz(trim(fname),nat)
      write(*,'(2x''number of atoms: '',i26)') nat
      write(*,'(2x''molecular charge: '',i25)') chrg
      call electro(nat,chrg,nel)
      write(*,'(2x''number of electrons: '',i22)') nel
      write(*,*)

C BAS read basis functions  - - - - - - - - - - - - - - - - - - - - 1709
      print'(''* using a'',x,a,x,''basis'')', trim(bname)
      call readbas(trim(basdir),trim(bname))
      call calcnao(nat,nao)

      deallocate( xyz, atom )
      call terminate(0)
      end program soul
