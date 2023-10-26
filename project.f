
      Program main

      

      implicit none
      character(50)::filename1, filename2
      integer :: size1, size2, size3
      real, allocatable :: array1(:), array2(:), array3(:)
      
      
      
      !ask for 1st file
      print *, "First Array"
      call read_file(filename1,array1,size1)
      call write_array(array1,size1)
     
      
      !ask for 2nd file
      print *, "Second Array"
      call read_file(filename2,array2,size2)
      call write_array(array2,size2)
  
  
      !merge 2 arrays and output
      print *, "Merged Array"
      call merge_array(array1,size1,array2,size2,array3,size3)
      call write_array(array3,size3)
   
      
      contains
      !subroutine to read file
      Subroutine read_file(filename, array, size)
          
          implicit none
          !declare variables
          real, allocatable :: array(:)
          integer :: size, i, iostat
          character(50)::filename
          
          !ask for file name
          print *, "What is the name of the data file? "
          read *, filename
          
          !open filename on unit 10
          open (unit=10, file = filename,iostat = iostat)
          
          !check if the input output is nonzero 
          if(iostat /= 0) stop 'Error opening file'
          
          size = 0
          
          !read file, check error, increase size
          do
              read (10, *, iostat = iostat)
              if (iostat /= 0) exit
              size = size + 1
              
          end do
          
          rewind(10)
          
          !read data into array
          allocate (array(size))
          do i = 1, size
              read(10,*)array(i)
          end do
          
          close(10)
          
         
      
      end subroutine   
      
      ! subroutine to write contents of array
      subroutine write_array (array, array_size)
      
          implicit none
          integer :: array_size, i
          real, allocatable :: array(:)
      
      
          print *, array_size, " Elements in this array: "
          !print *, array
          do i =1, array_size
              write(*,'(F7.3,$)') array(i)
          end do
          
          !give some space
          
          print *," "
          print *," "
          print *," "
      
      
      
      end subroutine write_array
      
      !subroutine to merge 2 arrays
      subroutine merge_array(array1,size1,array2,size2,array3,size3)
      implicit none
      integer :: size1, size2, size3, i1,i2,i3
      real, allocatable :: array1(:), array2(:), array3(:)
      i1 = 1
      i2 = 1
      i3 = 1
       
      size3 = size1+ size2
      allocate(array3(size3))
      
      !compare each element in both array and move on until reach the end
      do while (i1 <= size 1 .and. i2<= size2)
          if (array1(i1) <= array2(i2)) then
              array3(i3) = array1(i1)
              i1 = i1+1
          else
              array3(i3) = array2(i2)
              i2 = i2 +1
          end if
          i3= i3 +1
      end do
      
      !copy the rest of the other array to third array
      
      if (i1 > size1) then
          do i2 = i2, size2
              array3(i3) = array2(i2)
              i3 = i3 + 1
          end do
      else
          do i1 = i1, size1
              array3(i3) = array1(i1)
              i3 = i3 + 1
          end do
      end if
      
     
     
      
      end subroutine    
    
         
         
    
      
      
    
      End program main
      
      