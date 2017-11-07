* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 1708
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 1708
      module global
C this sections contains all kind of weird parameters
      character(len=*),parameter :: pse= " "//
     1"H "//                            "He"//
     2"LiBe"//                "B C N O F Ne"//
     3"NaMg"//                "AlSiP S ClAr"//
     4"K CaScTiV CrMnFeCoNiCuZnGaGeAsSeBrKr"//
     5"RbSrY ZrNbMoTcRuRhPdAgCdInSnSbTeI Xe"//
     6"CsBa"//
     L      "LaCePrNdPmSmEuGdTbDyHoErTmYbLu"//
     6      "HfTaW ReOsIrPtAuHgTlPbBiPoAtRn"
C - - conversion factors of all kinds - - - - - - - - - - - - - - - 1710
      real*8,parameter :: a2kc=627.50947428              ! Eh → kcal/mol
      real*8,parameter :: kc2a=1/a2kc
      real*8,parameter :: a2ev=27.21138602               ! Eh → eV
      real*8,parameter :: ev2a=1/a2ev
      real*8,parameter :: a2kj=2625.499638               ! Eh → kJ/mol
      real*8,parameter :: kj2a=1/a2kj
      real*8,parameter :: a2cm=219474.6313702            ! Eh → cm⁻¹
      real*8,parameter :: cm2a=1/a2cm
      real*8,parameter :: a2aa=0.52917721067             ! α → Å
      real*8,parameter :: aa2a=1/a2aa
      real*8,parameter :: pi=4*atan(1.0d0)
C this section contains variables I don’t want to initalize every time
      integer :: nshell(86)
      integer :: lshell(86,25)
      integer :: nprim(86,25)
      integer :: aonum(86)
      integer :: mtbl(86,25)
      real*8  :: alpha(86,25,16), coeff(86,25,16)
      real*8,allocatable :: xyz(:,:) 
      integer,allocatable :: atom(:)
      end module global
C this section contains stuff that doesn’t fit anywhere else
      pure function capitalize (str) result(final)
      implicit none
      integer :: i,il
      character(len=*),intent(in) :: str
      character(len=len(str))     :: final
      character(len=26),parameter :: cap = 
     .  'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
      character(len=26),parameter :: low = 
     .  'abcdefghijklmnopqrstuvwxyz'
      final = str
      il = INDEX(low, str(1:1))
      if (il > 0) final(1:1) = cap(il:il)
      do i = 2, len_trim(str)
        il = INDEX(cap, str(i:i))
        if (il > 0) final(i:i) = low(il:il)
      enddo
      end function capitalize
