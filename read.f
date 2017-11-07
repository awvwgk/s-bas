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
      subroutine rdargv(fname,bname,chrg)
      implicit none
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1710
      character(len=:), allocatable,intent(out) :: fname
      character(len=:), allocatable,intent(out) :: bname
      integer,intent(out) :: chrg
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1710
      integer :: i,j,k,l
      integer :: err
      logical :: exist
      logical :: getopts
      logical :: inchrg
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1710
      character(len=:), allocatable :: arg
      character(len=:), allocatable :: sec
      interface
         subroutine rdarg(i,arg)
         integer,intent(in) :: i
         character(len=:),allocatable,intent(out) :: arg
         end subroutine rdarg
         subroutine rdchrg(chrg)
         integer,intent(out) :: chrg
         end subroutine rdchrg
         subroutine raise(mode,message)
         character,intent(in) :: mode
         character(len=*),intent(in) :: message
         end subroutine raise
      end interface
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1710
c     get number of argument and loop over all of them
      inchrg = .false.
      getopts = .true.
      j = 0
      do i = 1, command_argument_count()
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1710
c        cycle if some command already have read arguments
         if (j.gt.0) then
            j = j-1
            cycle
         endif
         call rdarg(i,arg)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1710
c        check if there is a '--' present, forbid to read flags
         if (arg.eq.'--') then
            getopts = .false.
            cycle
         endif
         if (getopts) then
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1710
c           check arguments against select case statement
            select case(arg)
            case('-h','-help', '--help')
               call help
            case('-b','-bas',  '--bas')
               call rdarg(i+1,bname)
               j = 1
            case('-c','-chrg', '--chrg')
               call rdarg(i+1,sec); read(sec,*,iostat=err) chrg
               if (err.ne.0) call raise('E','charge corrupted')
               j = 1
               inchrg = .true.
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1710
c           if no flag does fit, assume as file name
            case default
               inquire(file=arg,exist=exist)
               if (exist) then
                  fname = arg
               else
                  if (arg(1:1).eq.'-') then
                     call raise('W','Flag unknown: '//arg)
                  else
                     call raise('E','File not found: '//arg)
                  endif
               endif
            end select
         else
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1710
c           read all arguments as file names if '--' was encountered
            inquire(file=arg,exist=exist)
            if (exist) then
               fname = arg
            else
               call raise('E','File not found: '//arg)
            endif
         endif
      enddo

      if (.not.inchrg) call rdchrg(chrg)
      if (.not.allocated(bname)) call raise('E','No basis set given')
      if (.not.allocated(fname)) call raise('E','No file name given')

      if (allocated(arg)) deallocate(arg)
      if (allocated(sec)) deallocate(sec)
      end subroutine rdargv     

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1710
      subroutine rdarg(i,arg)
      integer,intent(in) :: i
      character(len=:),allocatable,intent(out) :: arg
      integer :: l,err
      if (allocated(arg)) deallocate(arg)
      call get_command_argument(i,length=l,status=err)
      if (err.gt.0) call raise('E','Command argument corrupted')
      allocate( character(len=l) :: arg, stat=err )
      if (err.ne.0) call raise('E','could not be allocated')
      call get_command_argument(i,arg,status=err)
      if (err.gt.0) call raise('E','Command argument corrupted')
      end subroutine rdarg

C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 1710
      subroutine rdchrg(chrg)
      integer,intent(out) :: chrg
      logical :: exist
      integer :: i,id=42
      inquire(file='.CHRG',exist=exist)
      if (exist) then
         open(id,file='.CHRG')
         read(id,*,iostat=i) chrg
         close(id)
         if (i.ne.0) then
            call raise('W','Problem in .CHRG file. Set charge to zero!')
            chrg=0
         endif
      else
         chrg=0
      endif

      end subroutine rdchrg


