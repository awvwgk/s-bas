* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 1709
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 1709
      subroutine banner
      write(*,*)
      write(*,'(7xa)')"┌────────"//
     ."─────────"//
     ."─────────"//
     ."──────────┐"
      write(*,'(7xa)')"│             soul-basis             │"
      write(*,'(7xa)')"│          ================          │"
      write(*,'(7xa)')"│               awvwgk               │"
      write(*,'(7xa)')"│        11/2017, Version 1.0        │"
      write(*,'(7xa)')"└─────────"//
     ."────────"//
     ."────────"//
     ."───────────┘"
      write(*,*)
      end subroutine banner
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1709
      subroutine options (basdir)
      implicit none
      integer :: id = 42
      integer :: c
      character(len=99) :: home,rcfile,line,basdir
      logical :: exist
      call getenv('HOME',home)
      print*, trim(home)
      rcfile = trim(home)//'/.soulrc'
      inquire (file=rcfile,exist=exist)
      if (exist) then
         open(id,file=rcfile,status='old')
         do
            read(id,'(a)') line
            c = index(line,'#')
            if (c.ne.0) then
               if (c.eq.1) cycle
                  line = line(1:c-1)
               endif
            if (index(line,'$basdir').ne.0) read(line,'(8x,a)') basdir
            if (index(line,'$end').ne.0) exit
         enddo
         close(id)
      else
      endif
      if (index(basdir,'~').ne.0) basdir=trim(home)//trim(basdir(2:99))
      basdir = trim(basdir)//'/'
      end subroutine options
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1710
      subroutine help
      print'(''Usage: shf [options] <geometry> [options]'',/)'

      print'(''<geometry> may be provided as'','// 
     &   'x,''valid TM coordinate file (*coord in Bohr) or'','// 
     &   '/,''in xmol format (*xyz in Ångström).'',/)'

      print'(3x,''-b, --basis <name>'','// 
     &     'x,''Specify basis set'')'

      print'(3x,''-h, --help        '','// 
     &     'x,''Show this message'')'

      end subroutine help
