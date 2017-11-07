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
      subroutine readxyz(fname,c)
      use global
      implicit real*8(x-z)
      integer :: id=42
      character(len=*),intent(in) :: fname
      character(len=100):: line
      logical :: exist
      integer,intent(out) :: c
      character(len=2) :: elem
      interface
         pure function capitalize (str)
         character(len=*),intent(in) :: str
         character(len=len(str))     :: capitalize
         end function capitalize
      end interface
      c = 0
        
      inquire(file=fname,exist=exist)
      if(.not.exist) stop 'coordinates not found'
      open(id,file=fname)
      if(index(fname,'.xyz').ne.0) then
        read(id,*) c
        read(id,*)
        allocate( xyz(3,c) )
        allocate( atom(c) )
        do i = 1, c
          read(id,*) elem,x,y,z
          elem = capitalize(elem)
          if(index(pse,elem).eq.0) stop 'unknown element!'
          xyz(1,i) = x*aa2a
          xyz(2,i) = y*aa2a
          xyz(3,i) = z*aa2a
          atom(i)  = index(pse,elem)/2
c         write(*,*)  xyz(:,i),atom(i)
        enddo
      else
        do
          read(id,'(a)') line
          if(index(line,'$coord').ne.0) then
            c = 0
            do
              read(id,'(a)') line
              if(index(line,'$').ne.0) exit
              c = c+1
            enddo
          endif
          if(index(line,'$end').ne.0) exit
        enddo
        rewind(id)
        do
          read(id,'(a)') line
          if(index(line,'$coord').ne.0) then
            allocate( xyz(3,c) )
            allocate( atom(c) )
            do i = 1, c
              read(id,'(a)') line
              read(line,*) x,y,z,elem
              elem = capitalize(elem)
              if(index(pse,elem).eq.0) stop 'unkown element!'
              xyz(1,i) = x
              xyz(2,i) = y
              xyz(3,i) = z
              atom(i)  = index(pse,elem)/2
c             write(*,*) xyz(:,i),atom(i)
            enddo
          endif
          if(index(line,'$end').ne.0) exit
        enddo
      endif
      close(id)
      if(c.eq.0) stop 'coordinates not found!'
      end subroutine readxyz
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1709
      subroutine electro(nat,chrg,nel)
      use global, only : atom
      implicit none
      integer,intent(in)  :: chrg
      integer,intent(out) :: nel
      integer,intent(in)  :: nat
      integer :: i
      integer :: id=42
      integer :: info
      logical :: exist
      nel = 0
      do i = 1, nat
        nel = nel + atom(i)
      enddo
      nel = nel - chrg
      end subroutine electro

