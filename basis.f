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
      subroutine readbas(basdir,bname)
      use global
      implicit none
      real*8  :: alpha_, coeff_, fact, expo
      real*8, parameter :: norm = (0.5d0/atan(1.0d0))**(0.75d0)
      integer :: id=42
      character(len=*),intent(in) :: basdir
      character(len=*),intent(in) :: bname
      character(len=50):: line
      character :: sl(0:5)
      data sl / 's','p','d','f','g','h' /
      character :: cht
      character(len=2) :: elem
      character(len=2) :: capitalize
      integer :: itype
      integer :: prime
      integer :: c
      integer :: i,ii
      integer :: iat
      integer :: info
      logical :: exist
      inquire(file=basdir//bname//'.bas',exist=exist)
      print*,basdir//bname//'.bas'
      if(.not.exist) stop 'basis not found'

      open(id,file=basdir//bname//'.bas')
      do
        read(id,'(a)') line
        if (index(line,'$end').ne.0) exit
        if (index(line,bname).ne.0) then
          read(line,*) elem
          elem = capitalize(elem)
          iat  = index(pse,elem)/2
          c = 0
c         write(*,'(''→ '',a,'' in '',a,''-basis'')') trim(elem),bname
          read(id,*)
          do
            read(id,'(a)') line
            if (index(line,'*').ne.0) exit
            read(line,*) prime, cht
            do i = 0, 5
              ii = index(sl(i),cht)
              if (ii.ne.0) exit
            enddo
            if (ii.eq.0) stop 'ao type unknown!'
            c = c + 1
            nprim(iat,c) = prime
            lshell(iat,c) = i
            fact = 2**i
            expo = 0.75d0 + dble(i)/2.0d0
c           aonum(iat) = aonum(iat) + itype
c           write(*,'(''→ → '',i3,x,i3,''/'',a)') prime,i,sl(i)
            do i = 1, prime
              read(id,*) alpha_, coeff_
              alpha(iat,c,i) = alpha_
              coeff(iat,c,i) = fact*norm*coeff_*alpha_**expo
c             write(*,'(''→ → → '',f11.3,X,f7.3)') alpha_, coeff_
            enddo
          enddo
          nshell(iat) = c
        endif
      enddo
      close(id)
      end subroutine readbas
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1709
      subroutine calcnao(nat,nao)
      use global, only : atom, nshell, lshell, mtbl
      implicit none
      integer :: i,ii,iii,ia,ishell
      integer :: lmax
      integer,intent(in)  :: nat
      integer,intent(out) :: nao
      character(len=24)   :: chaos
      character(len=9)    :: chdum
      character,parameter :: sl(0:5) =
     .(/ 's','p','d','f','g','h' /)
      integer,parameter   :: ll(0:5) =
     .(/  1,  3,  6, 10, 15, 21  /)
c    .(/  1,  3,  5,  7,  9, 11  /)
      mtbl = 0
      nao  = 0
      lmax = 0
      do i = 1, nat
        ia = atom(i)
        do ii = 1, nshell(ia)
          ishell = lshell(ia,ii)
          lmax = max(lmax,ishell)
          mtbl(i,ii) = nao
          do iii = 1, ll(ishell)
            nao = nao + 1
          enddo
        enddo
      enddo
      write(*,'(2x,''with'',x,$)')
      chaos = '['
      do i = 0, lmax-1
        write(chdum,'(i2,a)') ll(i),sl(i)
        chaos = trim(chaos)//trim(adjustl(chdum))//','
      enddo
      write(chdum,'(i2,a)') ll(lmax),sl(lmax)
      write(*,'(x,a,x,''gaussian functions'')')
     .  trim(chaos)//trim(adjustl(chdum))//']'
      write(*,'(2x''number of gaussian functions:'',i14)') nao
      write(*,*)

      end subroutine calcnao
