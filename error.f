* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 1710
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
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 1710
      subroutine raise(mode,message)
      character,       intent(in) :: mode
      character(len=*),intent(in) :: message
      select case(mode)
      case('W','w')
      print'(''#WARNING!'',x,a)',message
      case('E','e')
      print'(''#ERROR!'',x,a)',  message
      call terminate(1)
      end select
      end subroutine raise

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1710
c     old version using FORTRAN error_unit
!     subroutine terminate(signal)
!     use iso_fortran_env, only : error_unit
!     integer,intent(in) :: signal
!     select case(signal)
!     case(0)
!     write(error_unit,'(''normal termination of soul-basis'')')
!     case default
!     write(error_unit,'(''abnormal termination of soul-basis'')')
!     end select
!     stop
!     end subroutine terminate

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1710
c     new version employing FORTRAN 2008 error stop
      subroutine terminate(signal)
      integer,intent(in) :: signal
      select case(signal)
      case(0)
      stop         'normal termination of soul-basis'
      case default
      error stop 'abnormal termination of soul-basis'
      end select
      end subroutine terminate
