!FORTRAN program to extract coordinates from reax traj file
program trj2gro
    implicit none
    integer :: ios
    integer, parameter :: read_unit = 99
    character(len=200), allocatable :: command(:)
    character(len=200) :: line
    character(len=200) :: temp
    integer :: n, i, n_atoms,count,j,ID
    real x,y,z !x y z coordinates
    logical :: exist  ! if file exists
    character(len=30)::n_string !string of n_atoms
    
    !if production.trj doesn't exist
    open(unit=read_unit, file='production.trj', iostat=ios)
    if ( ios /= 0 ) stop "Error opening file production.trj"

!!!!!!!! get the total number of lines
    n = 0
    do
        read(read_unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        n = n + 1
    end do

    print*, "File contains ", n, "commands"
!!!!!!!!

!!!!!!!! read all the lines
    allocate(command(n))

    rewind(read_unit)

    do i = 1, n
        read(read_unit, '(A)') command(i)
    end do

    close(read_unit)
!!!!!!!!!

!!!!!!!!! get the number of atoms
    do i = 1, 4
        !print*, command(i)(12:17)
	if (i==4) then
	read(command(i)(13:17) ,'(I4)') n_atoms
        end if  
    end do

    !write(*,*) n_atoms
!!!!!!!!!    

!!!!!!!!! if production.gro exists, delete it and make a new one
    inquire(file="production.gro", exist=exist)
    if (exist) then
    	open(12, file="production.gro", status="old", position="append", action="write")
        close(12,status='delete')
    	open(12, file="production.gro", status="new", action="write")
    end if
!!!!!!!!! output data to gro file     
    write(n_string,'(I4)') n_atoms 
    count=0
    do i = 1,n
	if ((command(i)(1:9)=="        1") .AND. ( len_trim(command(i))>80 ) ) then
        count=count+1
	write(12,*) "MD of 2 waters, t="
	write(12,*) n_string
 	do j =i,i+n_atoms-1
		!print *,command(j)(15:46)
		read(command(j)(5:10) ,'(I5)') ID
	        read(command(j)(15:26) ,'(F6.3)') x
		read(command(j)(26:37) ,'(F6.3)') y
	 	read(command(j)(37:48) ,'(F6.3)') z
		!print *,ID, x,y,z
		write(12, '(i5,2a5,i5,3f8.3)') 1,'SIL','H',ID,x*0.1,y*0.1,z*0.1		
        end do
	write(12,*) "   3.40000   3.40000   3.40000"
	print *, "frame ",count," dumped"
        end if
    end do

    close(12)
end program trj2gro
