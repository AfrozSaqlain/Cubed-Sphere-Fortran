program main

    implicit none
  
    call sphere_cubed_grid_point_count_test ( )
    call sphere_cubed_grid_points_test ( )
    call sphere_cubed_grid_points_face_test ( )
    call sphere_cubed_grid_points_display_test ( )
  
    call sphere_cubed_grid_ijk_to_xyz_test ( )
    call sphere_cubed_grid_line_count_test ( )
    call sphere_cubed_grid_lines_test ( )
    call sphere_cubed_grid_lines_display_test ( )
  !
  !  Terminate.
  !
  
    stop 0
  end
  subroutine sphere_cubed_grid_point_count_test ( )
 
    implicit none
  
    integer ( kind = 4 ) n
    integer ( kind = 4 ) point_num
  
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'SPHERE_CUBED_GRID_POINT_COUNT_TEST'
    write ( *, '(a)' ) '  SPHERE_CUBED_GRID_POINT_COUNT counts points on a cubed sphere grid.'
    write ( *, '(a)' ) '  Each square face is divided into NxN subfaces,'
    write ( *, '(a)' ) '  and there are 6 faces.'
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '        N      POINT_COUNT'
    write ( *, '(a)' ) ''
    do n = 1, 10
      call sphere_cubed_grid_point_count ( n, point_num )
      write ( *, '(2x,i8,2x,i8)' ) n, point_num
    end do
  
    return
  end
  subroutine sphere_cubed_grid_points_test ( )
  
    implicit none
  
    integer ( kind = 4 ) n
    integer ( kind = 4 ) ns
    real ( kind = 8 ), allocatable :: xyz(:,:)
  
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'SPHERE_CUBED_GRID_POINTS_TEST'
    write ( *, '(a)' ) '  SPHERE_CUBED_GRID_POINTS computes points on a cubed sphere grid.'
  
    n = 10
    write ( *, '(a)' ) ''
    write ( *, '(a,i4)' ) '  Number of divisions on each face = ', n
  
    call sphere_cubed_grid_point_count ( n, ns )
    write ( *, '(a,i4)' ) '  Total number of points = ', ns
  
    allocate ( xyz(1:3,1:ns) )
    call sphere_cubed_grid_points ( n, ns, xyz )
  
    call r8mat_transpose_print_some ( 3, ns, xyz, 1, 1, 3, 20, &
      '  Initial part of XYZ array:' )
  
    deallocate ( xyz )
  
    return
  end
  subroutine sphere_cubed_grid_points_face_test ( )
  
    implicit none
  
    integer ( kind = 4 ) i1
    integer ( kind = 4 ) i2
    integer ( kind = 4 ) j1
    integer ( kind = 4 ) j2
    integer ( kind = 4 ) k1
    integer ( kind = 4 ) k2
    integer ( kind = 4 ) n
    integer ( kind = 4 ) ns
    integer ( kind = 4 ) ns2
    real ( kind = 8 ), allocatable :: xyz(:,:)
  
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'SPHERE_CUBED_GRID_POINTS_FACE_TEST'
    write ( *, '(a)' ) '  SPHERE_CUBED_GRID_POINTS_FACE computes points associated'
    write ( *, '(a)' ) '  with one face of a cubed sphere grid.'
  
    n = 3
    write ( *, '(a)' ) ''
    write ( *, '(a,i4)' ) '  Number of divisions on each face = ', n
  
    call sphere_cubed_grid_point_count ( n, ns )
    write ( *, '(a,i4)' ) '  Total number of points = ', ns
  
    allocate ( xyz(1:3,1:ns) )
  
    ns2 = 0
    i1 = 0
    j1 = 0
    k1 = 0
    i2 = n
    j2 = n
    k2 = 0
  !
  !  Bottom face.
  !
    call sphere_cubed_grid_points_face ( n, i1, j1, k1, i2, j2, k2, ns2, xyz ) 
  
    write ( *, '(a)' ) ''
    write ( *, '(a,i4)' ) '  Current number of points = ', ns2
  
    call r8mat_transpose_print ( ns2, 3, xyz,'  XYZ array after call for bottom face:' )
  !
  !  Compute one more face, but skip points already generated.
  !
    i1 = 0
    j1 = 0
    k1 = 1
    i2 = 0
    j2 = n - 1
    k2 = n - 1
  
    call sphere_cubed_grid_points_face ( n, i1, j1, k1, i2, j2, k2, ns2, xyz )
  
    write ( *, '(a)' ) ''
    write ( *, '(a,i4)' ) '  Current number of points = ', ns2
  
    call r8mat_transpose_print ( ns2, 3, xyz,'  XYZ array after call for a side face face:' )
  
    deallocate ( xyz )
  
    return
  end
  subroutine sphere_cubed_grid_points_display_test ( )

    implicit none
  
    integer ( kind = 4 ) n
    integer ( kind = 4 ) ns
    character ( len = 255 ) prefix
    real ( kind = 8 ), allocatable :: xyz(:,:)
  
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'SPHERE_CUBED_GRID_POINTS_DISPLAY_TEST'
    write ( *, '(a)' ) '  SPHERE_CUBED_GRID_POINTS_DISPLAY_TEST displays points'
    write ( *, '(a)' ) '  on a cubed sphere grid.'
  
    n = 10
    write ( *, '(a)' ) ''
    write ( *, '(a,i4)' ) '  Number of divisions on each face = ', n
  
    call sphere_cubed_grid_point_count ( n, ns )
    write ( *, '(a,i4)' ) '  Total number of points = ', ns
  
    allocate ( xyz(1:3,1:ns) )
  
    call sphere_cubed_grid_points ( n, ns, xyz )
  
    prefix = 'sphere_cubed_grid_points'
  
    call sphere_cubed_grid_points_display ( ns, xyz, prefix )
  
    deallocate ( xyz )
  
    return
  end
  subroutine sphere_cubed_grid_ijk_to_xyz_test ( )

    
    implicit none
    
    integer ( kind = 4 ) i
    integer ( kind = 4 ) j
    integer ( kind = 4 ) k
    integer ( kind = 4 ) n
    real ( kind = 8 ) xyz(3)
  
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'SPHERE_CUBED_GRID_IJK_TO_XYZ_TEST:'
    write ( *, '(a)' ) '  SPHERE_CUBED_GRID_IJK_TO_XYZ returns the XYZ coordinates'
    write ( *, '(a)' ) '  of a point on the surface of the cubed sphere,'
    write ( *, '(a)' ) '  given its (I,J,K) indices.' 
  
    n = 3
  
    write ( *, '(a)' ) ''
    write ( *, '(a,i4)' ) '  Using grid parameter N = ', n
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '     I     J     K        X           Y           Z'
    write ( *, '(a)' ) ''
  
    do i = 0, n
      do j = 0, n
        do k = 0, n
          if ( i == 0 .or. i == n .or. &
               j == 0 .or. j == n .or. &
               k == 0 .or. k == n ) then
            call sphere_cubed_grid_ijk_to_xyz ( n, i, j, k, xyz )
            write ( *, '(2x,i4,2x,i4,2x,i4,2x,f10.4,2x,f10.4,2x,f10.4)' ) &
              i, j, k, xyz(1), xyz(2), xyz(3)
          end if
        end do
      end do
    end do
  
    return
  end
  subroutine sphere_cubed_grid_line_count_test ( )

    implicit none
  
    integer ( kind = 4 ) line_count
    integer ( kind = 4 ) n
  
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'SPHERE_CUBED_GRID_LINE_COUNT_TEST'
    write ( *, '(a)' ) '  SPHERE_CUBED_GRID_LINE_COUNT counts lines on a cubed sphere grid.'
    write ( *, '(a)' ) '  Each square face is divided into NxN subfaces,'
    write ( *, '(a)' ) '  and there are 6 faces.'
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '        N      LINE_COUNT'
    write ( *, '(a)' ) ''
    do n = 1, 10
      call sphere_cubed_grid_line_count ( n, line_count )
      write ( *, '(2x,i8,2x,i8)' ) n, line_count
    end do
  
    return
  end
  subroutine sphere_cubed_grid_lines_test ( )

    implicit none
  
    integer ( kind = 4 ) i
    real ( kind = 8 ), allocatable :: line_data(:,:,:)
    integer ( kind = 4 ) line_num
    integer ( kind = 4 ) n
    integer ( kind = 4 ) point_num
    real ( kind = 8 ), allocatable :: xyz(:,:)
  ! ----------------------------------------------------------------------------------
  n = 10
  ! ----------------------------------------------------------------------------------
  
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'SPHERE_CUBED_GRID_LINES_TEST'
    write ( *, '(a)' ) '  SPHERE_CUBED_GRID_LINES defines the lines'
    write ( *, '(a)' ) '  on a cubed sphere grid.'
    write ( *, '(a,i2,a,i2,a)' ) '  Each cube face is divided into ', n, ' by ', n, ' subfaces'
  
    call sphere_cubed_grid_point_count ( n, point_num )
  
    write ( *, '(a)' ) ''
    write ( *, '(a,i4)' ) '  The number of points is ', point_num
  
    allocate ( xyz(1:3,1:point_num) )
  
    call sphere_cubed_grid_points ( n, point_num, xyz )
  
    call sphere_cubed_grid_line_count ( n, line_num )
  
    write ( *, '(a,i4)' ) '  The number of grid lines is ', line_num
  
    allocate ( line_data(1:3,1:2,1:line_num) )
  
    call sphere_cubed_grid_lines ( n, line_num, line_data )
  
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) '  Line     Start              End'
    write ( *, '(a)' ) '  Index    X    Y   Z         X  Y   Z'
    write ( *, '(a)' ) ''
  
    do i = 1, min ( 10, line_num )
      write ( *, '(a)' ) ''
      write ( *, '(2x,i4,2x,f10.4,2x,f10.4,2x,f10.4,2x,f10.4,2x,f10.4,2x,f10.4)' ) &
        i, line_data(1:3,1,i), line_data(1:3,2,i)
    end do
  
    deallocate ( line_data )
    deallocate ( xyz )
  
    return
  end
  subroutine sphere_cubed_grid_lines_display_test ( )

    implicit none
  
    real ( kind = 8 ), allocatable :: line_data(:,:,:)
    integer ( kind = 4 ) line_num
    integer ( kind = 4 ) n
    character ( len = 255 ) prefix
  ! -------------------------------------------------------------------------------------
  n = 10
  ! -------------------------------------------------------------------------------------
  
    write ( *, '(a)' ) ''
    write ( *, '(a)' ) 'SPHERE_CUBED_GRID_LINES_DISPLAY_TEST'
    write ( *, '(a)' ) '  SPHERE_CUBED_GRID_LINES_DISPLAY displays the lines'
    write ( *, '(a)' ) '  on a cubed sphere grid.'
    write ( *, '(a,i2,a,i2,a)' ) '  Each cube face is divided into ', n, ' by ', n, ' subfaces'
  
    call sphere_cubed_grid_line_count ( n, line_num )
  
    write ( *, '(a,i4)' ) '  The number of grid lines is ', line_num
  
    allocate ( line_data(1:3,1:2,1:line_num) )
  
    call sphere_cubed_grid_lines ( n, line_num, line_data )
  
    prefix = 'sphere_cubed_grid_lines'
  
    call sphere_cubed_grid_lines_display ( line_num, line_data, prefix )
  
    deallocate ( line_data )
  
    return
  end














  subroutine get_unit ( iunit )

      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) ios
      integer ( kind = 4 ) iunit
      logical ( kind = 4 ) lopen
    
      iunit = 0
    
      do i = 1, 99
    
        if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then
    
          inquire ( unit = i, opened = lopen, iostat = ios )
    
          if ( ios == 0 ) then
            if ( .not. lopen ) then
              iunit = i
              return
            end if
          end if
    
        end if
    
      end do
    
      return
    end
    subroutine r8mat_transpose_print ( m, n, a, title )
  
      implicit none
    
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a(m,n)
      character ( len = * ) title
    
      call r8mat_transpose_print_some ( m, n, a, 1, 1, m, n, title )
    
      return
    end
    subroutine r8mat_transpose_print_some ( m, n, a, ilo, jlo, ihi, jhi, title )
  
      implicit none
    
      integer ( kind = 4 ), parameter :: incx = 5
      integer ( kind = 4 ) m
      integer ( kind = 4 ) n
    
      real ( kind = 8 ) a(m,n)
      character ( len = 14 ) ctemp(incx)
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i2
      integer ( kind = 4 ) i2hi
      integer ( kind = 4 ) i2lo
      integer ( kind = 4 ) ihi
      integer ( kind = 4 ) ilo
      integer ( kind = 4 ) inc
      integer ( kind = 4 ) j
      integer ( kind = 4 ) j2hi
      integer ( kind = 4 ) j2lo
      integer ( kind = 4 ) jhi
      integer ( kind = 4 ) jlo
      character ( len = * ) title
    
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) trim ( title )
    
      if ( m <= 0 .or. n <= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) '  (None)'
        return
      end if
    
      do i2lo = max ( ilo, 1 ), min ( ihi, m ), incx
    
        i2hi = i2lo + incx - 1
        i2hi = min ( i2hi, m )
        i2hi = min ( i2hi, ihi )
    
        inc = i2hi + 1 - i2lo
    
        write ( *, '(a)' ) ' '
    
        do i = i2lo, i2hi
          i2 = i + 1 - i2lo
          write ( ctemp(i2), '(i8,6x)' ) i
        end do
    
        write ( *, '(''  Row   '',5a14)' ) ctemp(1:inc)
        write ( *, '(a)' ) '  Col'
        write ( *, '(a)' ) ' '
    
        j2lo = max ( jlo, 1 )
        j2hi = min ( jhi, n )
    
        do j = j2lo, j2hi
    
          do i2 = 1, inc
            i = i2lo - 1 + i2
            write ( ctemp(i2), '(g14.6)' ) a(i,j)
          end do
    
          write ( *, '(i5,a,5a14)' ) j, ':', ( ctemp(i), i = 1, inc )
    
        end do
    
      end do
    
      return
    end

    ! -----------------------------------------------------------------------------------------------------------------------------
    subroutine sphere_cubed_grid_ijk_to_xyz ( n, i, j, k, xyz )
  
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      integer ( kind = 4 ) k
      integer ( kind = 4 ) n
      real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
      real ( kind = 8 ), parameter :: radius = 2.00D+00
      real ( kind = 8 ) xc
      real ( kind = 8 ) xyz(3)
      real ( kind = 8 ) xyzn
      real ( kind = 8 ) yc
      real ( kind = 8 ) zc
    
      if ( i == 0 ) then
        xc = -1.0D+00
      else if ( i == n ) then
        xc = +1.0D+00
      else
        xc = tan ( real ( 2 * i - n, kind = 8 ) * 0.25D+00 * r8_pi &
          / real ( n, kind = 8 ) )
      end if
    
      if ( j == 0 ) then
        yc = -1.0D+00
      else if ( j == n ) then
        yc = +1.0D+00
      else
        yc = tan ( real ( 2 * j - n, kind = 8 ) * 0.25D+00 * r8_pi &
          / real ( n, kind = 8 ) )
      end if
    
      if ( k == 0 ) then
        zc = -1.0D+00
      else if ( k == n ) then
        zc = +1.0D+00
      else
        zc = tan ( real ( 2 * k - n, kind = 8 ) * 0.25D+00 * r8_pi &
          / real ( n, kind = 8 ) )
      end if
    ! -----------------------------------------------------------------------------------------
      xyzn = sqrt ( xc ** 2 + yc ** 2 + zc ** 2 )
    
      xyz(1) = xc / xyzn
      xyz(2) = yc / xyzn
      xyz(3) = zc / xyzn
      
      ! After computing xyz using your subroutine:
      xyz(1) = xyz(1) * radius
      xyz(2) = xyz(2) * radius
      xyz(3) = xyz(3) * radius

      return
    end
    subroutine sphere_cubed_grid_line_count ( n, line_num )
    
      implicit none
    
      integer ( kind = 4 ) line_num
      integer ( kind = 4 ) n
    
      line_num = 0
  
      if ( n == 1 ) then
        line_num = 12
        return
   
      else
        line_num = line_num + 8 * 3
      end if
  
      if ( 2 < n ) then
        line_num = line_num + 12 * ( n - 2 )
      end if
   
      if ( 1 < n ) then
        line_num = line_num + 6 * 2 * n * ( n - 1 )
      end if
    
      return
    end
    subroutine sphere_cubed_grid_lines ( n, line_num, line_data )
  
      implicit none
    
      integer ( kind = 4 ) line_num
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) j
      integer ( kind = 4 ) l
      real ( kind = 8 ) line_data(3,2,line_num)
      integer ( kind = 4 ) n
    
      l = 0
   
      if ( n == 1 ) then
    
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, 0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, 0, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, 0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, n, n, 0, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n, n, 0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, 0, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, 0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, 0, line_data(1:3,2,l) )
    
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, n, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, n, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, n, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, n, n, n, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n, n, n, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, n, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, n, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, n, line_data(1:3,2,l) )
    
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, 0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, n, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, 0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, n, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n, n, 0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, n, n, n, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, 0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, n, line_data(1:3,2,l) )
        return
  
      else
    
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, 0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 1, 0, 0, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, 0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, 1, 0, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, 0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, 1, line_data(1:3,2,l) )
    
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n,   0, 0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, n-1, 0, 0, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, 0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, 1, 0, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, 0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, 1, line_data(1:3,2,l) )
    
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n,   n, 0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, n-1, n, 0, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n, n,   0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, n, n-1, 0, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n, n, 0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, n, n, 1, line_data(1:3,2,l) )
    
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, 0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 1, n, 0, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, n,   0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, n-1, 0, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, 0, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, 1, line_data(1:3,2,l) )
    
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, n, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 1, 0, n, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, n, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, 1, n, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, n,   line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, n-1, line_data(1:3,2,l) )
    
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n,   0, n, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, n-1, 0, n, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, n, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, n, 1, n, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, n,   line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, n-1, line_data(1:3,2,l) )
    
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n,   n, n, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, n-1, n, n, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n, n,   n, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, n, n-1, n, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, n, n, n,   line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, n, n, n-1, line_data(1:3,2,l) )
    
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, n, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 1, n, n, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, n,   n, line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, n-1, n, line_data(1:3,2,l) )
        l = l + 1
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, n,   line_data(1:3,1,l) )
        call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, n-1, line_data(1:3,2,l) )
    
      end if
    
      if ( 2 < n ) then
    
        do i = 1, n - 2
          l = l + 1
          call sphere_cubed_grid_ijk_to_xyz ( n, i,   0, 0, line_data(1:3,1,l) )
          call sphere_cubed_grid_ijk_to_xyz ( n, i+1, 0, 0, line_data(1:3,2,l) )
        end do
        do i = 1, n - 2
          l = l + 1
          call sphere_cubed_grid_ijk_to_xyz ( n, n,   i, 0, line_data(1:3,1,l) )
          call sphere_cubed_grid_ijk_to_xyz ( n, n, i+1, 0, line_data(1:3,2,l) )
        end do
        do i = 1, n - 2
          l = l + 1
          call sphere_cubed_grid_ijk_to_xyz ( n, n-i,   n, 0, line_data(1:3,1,l) )
          call sphere_cubed_grid_ijk_to_xyz ( n, n-i-1, n, 0, line_data(1:3,2,l) )
        end do
        do i = 1, n - 2
          l = l + 1
          call sphere_cubed_grid_ijk_to_xyz ( n, 0, n-i,   0, line_data(1:3,1,l) )
          call sphere_cubed_grid_ijk_to_xyz ( n, 0, n-i-1, 0, line_data(1:3,2,l) )
        end do
    
        do i = 1, n - 2
          l = l + 1
          call sphere_cubed_grid_ijk_to_xyz ( n, i,   0, n, line_data(1:3,1,l) )
          call sphere_cubed_grid_ijk_to_xyz ( n, i+1, 0, n, line_data(1:3,2,l) )
        end do
        do i = 1, n - 2
          l = l + 1
          call sphere_cubed_grid_ijk_to_xyz ( n, n,   i, n, line_data(1:3,1,l) )
          call sphere_cubed_grid_ijk_to_xyz ( n, n, i+1, n, line_data(1:3,2,l) )
        end do
        do i = 1, n - 2
          l = l + 1
          call sphere_cubed_grid_ijk_to_xyz ( n, n-i,   n, n, line_data(1:3,1,l) )
          call sphere_cubed_grid_ijk_to_xyz ( n, n-i-1, n, n, line_data(1:3,2,l) )
        end do
        do i = 1, n - 2
          l = l + 1
          call sphere_cubed_grid_ijk_to_xyz ( n, 0, n-i,   n, line_data(1:3,1,l) )
          call sphere_cubed_grid_ijk_to_xyz ( n, 0, n-i-1, n, line_data(1:3,2,l) )
        end do
    
        do i = 1, n - 2
          l = l + 1
          call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, i,   line_data(1:3,1,l) )
          call sphere_cubed_grid_ijk_to_xyz ( n, 0, 0, i+1, line_data(1:3,2,l) )
        end do
        do i = 1, n - 2
          l = l + 1
          call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, i,   line_data(1:3,1,l) )
          call sphere_cubed_grid_ijk_to_xyz ( n, n, 0, i+1, line_data(1:3,2,l) )
        end do
        do i = 1, n - 2
          l = l + 1
          call sphere_cubed_grid_ijk_to_xyz ( n, n, n, i,   line_data(1:3,1,l) )
          call sphere_cubed_grid_ijk_to_xyz ( n, n, n, i+1, line_data(1:3,2,l) )
        end do
        do i = 1, n - 2
          l = l + 1
          call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, i,   line_data(1:3,1,l) )
          call sphere_cubed_grid_ijk_to_xyz ( n, 0, n, i+1, line_data(1:3,2,l) )
        end do
    
      end if
   
      if ( 1 < n ) then
  
        do i = 1, n - 1
          do j = 0, n - 1
            l = l + 1
            call sphere_cubed_grid_ijk_to_xyz ( n, i, j,   0, line_data(1:3,1,l) )
            call sphere_cubed_grid_ijk_to_xyz ( n, i, j+1, 0, line_data(1:3,2,l) )
          end do
        end do
        do j = 1, n - 1
          do i = 0, n - 1
            l = l + 1
            call sphere_cubed_grid_ijk_to_xyz ( n, i,   j, 0, line_data(1:3,1,l) )
            call sphere_cubed_grid_ijk_to_xyz ( n, i+1, j, 0, line_data(1:3,2,l) )
          end do
        end do
    
        do i = 1, n - 1
          do j = 0, n - 1
            l = l + 1
            call sphere_cubed_grid_ijk_to_xyz ( n, i, j,   n, line_data(1:3,1,l) )
            call sphere_cubed_grid_ijk_to_xyz ( n, i, j+1, n, line_data(1:3,2,l) )
          end do
        end do
        do j = 1, n - 1
          do i = 0, n - 1
            l = l + 1
            call sphere_cubed_grid_ijk_to_xyz ( n, i,   j, n, line_data(1:3,1,l) )
            call sphere_cubed_grid_ijk_to_xyz ( n, i+1, j, n, line_data(1:3,2,l) )
          end do
        end do
  
        do i = 1, n - 1
          do j = 0, n - 1
            l = l + 1
            call sphere_cubed_grid_ijk_to_xyz ( n, i, 0, j,   line_data(1:3,1,l)   )
            call sphere_cubed_grid_ijk_to_xyz ( n, i, 0, j+1, line_data(1:3,2,l) )
          end do
        end do
        do j = 1, n - 1
          do i = 0, n - 1
            l = l + 1
            call sphere_cubed_grid_ijk_to_xyz ( n, i,   0, j, line_data(1:3,1,l) )
            call sphere_cubed_grid_ijk_to_xyz ( n, i+1, 0, j, line_data(1:3,2,l) )
          end do
        end do
  
        do i = 1, n - 1
          do j = 0, n - 1
            l = l + 1
            call sphere_cubed_grid_ijk_to_xyz ( n, i, n, j,   line_data(1:3,1,l)   )
            call sphere_cubed_grid_ijk_to_xyz ( n, i, n, j+1, line_data(1:3,2,l) )
          end do
        end do
        do j = 1, n - 1
          do i = 0, n - 1
            l = l + 1
            call sphere_cubed_grid_ijk_to_xyz ( n, i,   n, j, line_data(1:3,1,l) )
            call sphere_cubed_grid_ijk_to_xyz ( n, i+1, n, j, line_data(1:3,2,l) )
          end do
        end do
   
        do i = 1, n - 1
          do j = 0, n - 1
            l = l + 1
            call sphere_cubed_grid_ijk_to_xyz ( n, 0, i, j,   line_data(1:3,1,l)   )
            call sphere_cubed_grid_ijk_to_xyz ( n, 0, i, j+1, line_data(1:3,2,l) )
          end do
        end do
        do j = 1, n - 1
          do i = 0, n - 1
            l = l + 1
            call sphere_cubed_grid_ijk_to_xyz ( n, 0, i,   j, line_data(1:3,1,l) )
            call sphere_cubed_grid_ijk_to_xyz ( n, 0, i+1, j, line_data(1:3,2,l) )
          end do
        end do
  
        do i = 1, n - 1
          do j = 0, n - 1
            l = l + 1
            call sphere_cubed_grid_ijk_to_xyz ( n, n, i, j,   line_data(1:3,1,l)   )
            call sphere_cubed_grid_ijk_to_xyz ( n, n, i, j+1, line_data(1:3,2,l) )
          end do
        end do
        do j = 1, n - 1
          do i = 0, n - 1
            l = l + 1
            call sphere_cubed_grid_ijk_to_xyz ( n, n, i,   j, line_data(1:3,1,l) )
            call sphere_cubed_grid_ijk_to_xyz ( n, n, i+1, j, line_data(1:3,2,l) )
          end do
        end do
    
      end if
    
      if ( l /= line_num ) then
        write ( *, '(a)' ) ''
        write ( *, '(a)' ) 'SPHERE_CUBED_GRID_LINES - Fatal error!'
        write ( *, '(a,i6)' ) '  LINE_NUM = ', line_num
        write ( *, '(a,i6)' ) '  L = ', l
        stop 1
      end if
    
      return
    end
    subroutine sphere_cubed_grid_lines_display ( line_num, line_data, prefix )
  
      implicit none
    
      integer ( kind = 4 ) line_num
    
      character ( len = 255 ) command_filename
      integer ( kind = 4 ) command_unit
      integer ( kind = 4 ) j
      integer ( kind = 4 ) l
      real ( kind = 8 ) line_data(3,2,line_num)
      character ( len = 255 ) line_filename
      integer ( kind = 4 ) line_unit
      character ( len = 255 ) plot_filename
      character ( len = * ) prefix
   
      call get_unit ( line_unit )
      line_filename = trim ( prefix ) // '_lines.txt'
      open ( unit = line_unit, file = line_filename, status = 'replace' )
      do l = 1, line_num
        if ( 1 < l ) then
          write ( line_unit, '(a)' ) ''
          write ( line_unit, '(a)' ) ''
        end if
        write ( line_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) line_data(1:3,1,l)
        write ( line_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) line_data(1:3,2,l)
      end do
      close ( unit = line_unit )
      write ( *, '(a)' ) '  Created line file "' // trim ( line_filename ) // '".'
   
      call get_unit ( command_unit )
      command_filename = trim ( prefix ) // '_commands.txt'
      open ( unit = command_unit, file = command_filename, status = 'replace' )
      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      plot_filename = trim ( prefix ) // '.png'
      write ( command_unit, '(a)' ) 'set output "' // trim ( plot_filename ) // '"'
      write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
      write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
      write ( command_unit, '(a)' ) 'set title "' // trim ( prefix ) // '"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set key off'
      write ( command_unit, '(a)' ) 'set style data points'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'set view equal xyz'
      write ( command_unit, '(a)' ) 'splot "' // &
        trim ( line_filename ) // &
        '" with lines lw 3'
      write ( command_unit, '(a)' ) 'quit'
      close ( unit = command_unit )
    
      write ( *, '(a)' ) &
        '  Created command file "' // trim ( command_filename ) // '".'
    
      return
    end
    subroutine sphere_cubed_grid_point_count ( n, ns )
  
      implicit none
    
      integer ( kind = 4 ) n
      integer ( kind = 4 ) ns
    
      ns = ( n + 1 ) ** 3 - ( n - 1 ) ** 3
    
      return
    end
    subroutine sphere_cubed_grid_points ( n, ns, xyz )
   
      implicit none
    
      integer ( kind = 4 ) ns
    
      integer ( kind = 4 ) n
      integer ( kind = 4 ) ns2
      real ( kind = 8 ) xyz(3,ns)
    
      ns2 = 0
   
      call sphere_cubed_grid_points_face ( n, 0, 0, 0, n, n, 0, ns2, xyz )
   
      call sphere_cubed_grid_points_face ( n, 0, 0, 1, 0,   n-1, n-1, ns2, xyz )
      call sphere_cubed_grid_points_face ( n, 0, n, 1, n-1, n,   n-1, ns2, xyz )
      call sphere_cubed_grid_points_face ( n, n, 1, 1, n,   n,   n-1, ns2, xyz )
      call sphere_cubed_grid_points_face ( n, 1, 0, 1, n,   0,   n-1, ns2, xyz )
   
      call sphere_cubed_grid_points_face ( n, 0, 0, n, n, n, n, ns2, xyz )
      
      if ( ns2 /= ns ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'SPHERE_CUBED_GRID_POINTS - Fatal error!'
        write ( *, '(a,i8,a)' ) '  Expected to generated NS = ', ns, ' points.'
        write ( *, '(a,i8,a)' ) '  Generated ', ns2, ' points.'
        stop
      end if
    
      return
    end
    subroutine sphere_cubed_grid_points_display ( ng, xg, prefix )
  
      implicit none
    
      integer ( kind = 4 ) line_num
      integer ( kind = 4 ) ng
    
      character ( len = 255 ) command_filename
      integer ( kind = 4 ) command_unit
      integer ( kind = 4 ) j
      character ( len = 255 ) node_filename
      integer ( kind = 4 ) node_unit
      character ( len = 255 ) plot_filename
      character ( len = * ) prefix
      real ( kind = 8 ) xg(3,ng)
  
      call get_unit ( node_unit )
      node_filename = trim ( prefix ) // '_nodes.txt'
      open ( unit = node_unit, file = node_filename, status = 'replace' )
      do j = 1, ng
        write ( node_unit, '(2x,g14.6,2x,g14.6,2x,g14.6)' ) xg(1:3,j)
      end do
      close ( unit = node_unit )
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) '  Created node file "' // trim ( node_filename ) // '".'
  
      call get_unit ( command_unit )
      command_filename = trim ( prefix ) // '_commands.txt'
      open ( unit = command_unit, file = command_filename, status = 'replace' )
      write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) '# Usage:'
      write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
      write ( command_unit, '(a)' ) '#'
      write ( command_unit, '(a)' ) 'set term png'
      plot_filename = trim ( prefix ) // '.png'
      write ( command_unit, '(a)' ) 'set output "' // trim ( plot_filename ) // '"'
      write ( command_unit, '(a)' ) 'set xlabel "<--- X --->"'
      write ( command_unit, '(a)' ) 'set ylabel "<--- Y --->"'
      write ( command_unit, '(a)' ) 'set zlabel "<--- Z --->"'
      write ( command_unit, '(a)' ) 'set title "' // trim ( prefix ) // '"'
      write ( command_unit, '(a)' ) 'set grid'
      write ( command_unit, '(a)' ) 'set key off'
      write ( command_unit, '(a)' ) 'set style data points'
      write ( command_unit, '(a)' ) 'set timestamp'
      write ( command_unit, '(a)' ) 'set view equal xyz'
      write ( command_unit, '(a)' ) 'splot "' // &
        trim ( node_filename ) // '" with points pt 7 lt 0'
      write ( command_unit, '(a)' ) 'quit'
      close ( unit = command_unit )
    
      write ( *, '(a)' ) &
        '  Created command file "' // trim ( command_filename ) // '".'
    
      return
    end
    subroutine sphere_cubed_grid_points_face ( n, i1, j1, k1, i2, j2, k2, ns, xyz )
   
      implicit none
    
      integer ( kind = 4 ) i
      integer ( kind = 4 ) i1
      integer ( kind = 4 ) i2
      integer ( kind = 4 ) j
      integer ( kind = 4 ) j1
      integer ( kind = 4 ) j2
      integer ( kind = 4 ) k
      integer ( kind = 4 ) k1
      integer ( kind = 4 ) k2
      integer ( kind = 4 ) n
      integer ( kind = 4 ) ns
      real ( kind = 8 ), parameter :: r8_pi = 3.141592653589793D+00
      real ( kind = 8 ) xyz(3,*)
      real ( kind = 8 ) xyzn
      real ( kind = 8 ) xc
      real ( kind = 8 ) yc
      real ( kind = 8 ) zc
    
      do i = i1, i2
    
        if ( i1 < i2 ) then
          xc = tan ( real ( 2 * i - n, kind = 8 ) * 0.25D+00 * r8_pi &
            / real ( n, kind = 8 ) )
        else if ( i1 == 0 ) then
          xc = -1.0D+00
        else if ( i1 == n ) then
          xc = +1.0D+00
        else
          xc = 0.0D+00
        end if
    
        do j = j1, j2
    
          if ( j1 < j2 ) then
            yc = tan ( real ( 2 * j - n, kind = 8 ) * 0.25D+00 * r8_pi &
              / real ( n, kind = 8 ) )
          else if ( j1 == 0 ) then
            yc = -1.0D+00
          else if ( j1 == n ) then
            yc = +1.0D+00
          else
            yc = 0.0D+00
          end if
    
          do k = k1, k2
    
            if ( k1 < k2 ) then
              zc = tan ( real ( 2 * k - n, kind = 8 ) * 0.25D+00 * r8_pi &
                / real ( n, kind = 8 ) )
            else if ( k1 == 0 ) then
              zc = -1.0D+00
            else if ( k1 == n ) then
              zc = +1.0D+00
            else
              zc = 0.0D+00
            end if
    
            xyzn = sqrt ( xc ** 2 + yc ** 2 + zc ** 2 )
    
            ns = ns + 1
            xyz(1,ns) = xc / xyzn
            xyz(2,ns) = yc / xyzn
            xyz(3,ns) = zc / xyzn
    
          end do
        end do
      end do
    
      return
    end
    subroutine timestamp ( )
  
      implicit none
    
      character ( len = 8 ) ampm
      integer ( kind = 4 ) d
      integer ( kind = 4 ) h
      integer ( kind = 4 ) m
      integer ( kind = 4 ) mm
      character ( len = 9 ), parameter, dimension(12) :: month = (/ &
        'January  ', 'February ', 'March    ', 'April    ', &
        'May      ', 'June     ', 'July     ', 'August   ', &
        'September', 'October  ', 'November ', 'December ' /)
      integer ( kind = 4 ) n
      integer ( kind = 4 ) s
      integer ( kind = 4 ) values(8)
      integer ( kind = 4 ) y
    
      call date_and_time ( values = values )
    
      y = values(1)
      m = values(2)
      d = values(3)
      h = values(5)
      n = values(6)
      s = values(7)
      mm = values(8)
    
      if ( h < 12 ) then
        ampm = 'AM'
      else if ( h == 12 ) then
        if ( n == 0 .and. s == 0 ) then
          ampm = 'Noon'
        else
          ampm = 'PM'
        end if
      else
        h = h - 12
        if ( h < 12 ) then
          ampm = 'PM'
        else if ( h == 12 ) then
          if ( n == 0 .and. s == 0 ) then
            ampm = 'Midnight'
          else
            ampm = 'AM'
          end if
        end if
      end if
    
      write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
        d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )
    
      return
    end