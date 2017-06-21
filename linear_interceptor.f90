program linear_interceptor
    implicit none
    integer, parameter :: ikind = selected_real_kind(p = 18)
    real (kind = ikind) :: rad_to_deg, deg_to_rad                               ! function variables
    real (kind = ikind) :: dist_1, dist_2, ang_1, ang_2, time, x1, x2, y1, y2, b, m1, m2   ! Variables for calculation of point position section of program
    real (kind = ikind) :: proj_speed, proj_angle, proj_dist                               ! Variables for calculation of velocity components
    real (kind = ikind) :: xi, yi, interceptor_speed, interceptor_angle, e_time, dummy       ! Variables for calculation of interception angle
    real (kind = ikind) :: a, d, c, dx1, dy1, sub_1
    integer :: known_type, file_read, io
    character (LEN = 20) :: file_name

    1 format(1A6, 1f7.3, 1A21, 1f9.3, 1A21, 1f10.3)
    2 format(1A18, 1f7.3, 1A21, 1f9.3)

    write(*,*) "Would you like to read from a file? (0 for no, 1 for yes): "
    read *, file_read

    if (file_read == 0) then
        do
            ! input parameters
            write (*,*) "Enter first distance and angle (deg) seperated by a space."
            read *, dist_1, ang_1

            write (*,*) "Enter second distance and angle (deg) seperated by a space."
            read *, dist_2, ang_2

            write (*,*) "Enter time between measurements."
            read *, time

            do
                print *, "Enter the known interceptor property."
                print *, "(0 for intercept time, 1 for intercept angle, 2 for interceptor speed"
                print *, "3 to enter new incoming projectile, 4 to exit): "
                read *, known_type

                ! convert given angles to radians
                ang_1 = deg_to_rad(ang_1)
                ang_2 = deg_to_rad(ang_2)

                call calc_incoming(ang_1, ang_2, x1, y1, dist_1, x2, y2, time, dist_2, m1, b, proj_dist, proj_speed, proj_angle)

                if (known_type == 0) then
                ! branch for known time to intercept
                    print *, "Enter intercept time: "
                    read *, e_time
                    call calc_int_time(xi, yi, x1, y1, x2, y2, time, e_time, interceptor_angle, interceptor_speed)
                    interceptor_angle = rad_to_deg(interceptor_angle)

                else if (known_type == 1) then
                ! branch for known interception angle
                    print *, "Enter intercept angle: "
                    read *, interceptor_angle
                    interceptor_angle = deg_to_rad(interceptor_angle)
                    call calc_int_angle(interceptor_angle, xi, yi, m1, m2, b, y1, y2, time, e_time, interceptor_speed)

                else if (known_type == 2) then
                ! branch for known interceptor speed
                    print *, "Enter interceptor speed: "
                    read *, interceptor_speed
                    call calc_int_speed(x1, x2, xi, y1, y2, yi, dx1, dy1,&
                        interceptor_speed, a, d, c, sub_1, e_time, time, interceptor_angle)
                    interceptor_angle = rad_to_deg(interceptor_angle)

                else if (known_type == 3) then
                    exit
                else if (known_type == 4) then
                    exit
                end if

                write(*,2) "Projectile Speed: ", proj_speed, "   Projectile Angle: ", rad_to_deg(proj_angle)
                write(*,1) "Time: ", e_time, "   Interceptor Speed: ", interceptor_speed,&
                     "    Interceptor Angle: ", interceptor_angle

            end do
            if (known_type == 4) then
                exit
            end if
        end do

    else if (file_read == 1) then
        write(*,*) "Please enter input file name/path: "
        read *, file_name
        open(10, file = file_name)

        write(*,*) "Please enter output file name/path: "
        read *, file_name
        open(11, file = file_name)
        write(11,*) " PS     PA      TTI    IS     IA"

        do
            read(10,*, IOSTAT = io) dist_1, ang_1, dist_2, ang_2, time, known_type, dummy
            if(io < 0) then
                exit
            end if
            ang_1 = deg_to_rad(ang_1)
            ang_2 = deg_to_rad(ang_2)
            call calc_incoming(ang_1, ang_2, x1, y1, dist_1, x2, y2, time, dist_2, m1, b, proj_dist, proj_speed, proj_angle)

            if (known_type == 0) then
            ! branch for known time to intercept
                e_time = dummy
                call calc_int_time(xi, yi, x1, y1, x2, y2, time, e_time, interceptor_angle, interceptor_speed)
                interceptor_angle = rad_to_deg(interceptor_angle)

            else if (known_type == 1) then
            ! branch for known interception angle
                interceptor_angle = dummy
                interceptor_angle = deg_to_rad(interceptor_angle)
                call calc_int_angle(interceptor_angle, xi, yi, m1, m2, b, y1, y2, time, e_time, interceptor_speed)

            else if (known_type == 2) then
            ! branch for known interceptor speed
                interceptor_speed = dummy
                call calc_int_speed(x1, x2, xi, y1, y2, yi, dx1, dy1,&
                    interceptor_speed, a, d, c, sub_1, e_time, time, interceptor_angle)
                interceptor_angle = rad_to_deg(interceptor_angle)
            end if
            3 format(1f7.3, 1f8.3, 1f7.3, 1f7.3, 1f7.3)
            write(11,3) proj_speed, rad_to_deg(proj_angle), e_time, interceptor_speed, interceptor_angle

        end do
        write(11,*) ""
        write(11,*) " Key:"
        write(11,*) " PS = Incoming Projectile Speed"
        write(11,*) " PA = Incoming Projectile Angle"
        write(11,*) " TTI = Time To Intercept"
        write(11,*) " IS = Interceptor Speed"
        write(11,*) " IA = Interception Angle"
        close(10)
        close(11)
    end if
end program linear_interceptor

subroutine calc_incoming(ang_1, ang_2, x1, y1, dist_1, x2, y2, time, dist_2, m1, b, proj_dist, proj_speed, proj_angle)
    implicit none
    integer, parameter :: ikind = selected_real_kind(p = 18)
    real (kind = ikind) :: dist_1, dist_2, ang_1, ang_2, time, x1, x2, y1, y2, b, m1   ! Variables for calculation of point position section of program
    real (kind = ikind) :: proj_speed, proj_angle, proj_dist                               ! Variables for calculation of velocity components

    ! Calculate first point coordinates
    x1 = cos(ang_1)*dist_1
    y1 = sin(ang_1)*dist_1

    ! Calculate second point coordinates
    x2 = cos(ang_2)*dist_2
    y2 = sin(ang_2)*dist_2

    ! Calculate slope and y-intercept
    m1 = (y2 - y1)/(x2 - x1)
    b = y1 - x1*m1

    ! Calculate projectile distance traveled, speed, and angle
    proj_dist =  abs(sqrt((x2 - x1)**2 + (y2 - y1)**2))
    proj_speed = proj_dist/time
    proj_angle = atan((x2 - x1)/(y2 - y1))
end subroutine calc_incoming

subroutine calc_int_time(xi, yi, x1, y1, x2, y2, time, e_time, interceptor_angle, interceptor_speed)
    implicit none
    integer, parameter :: ikind = selected_real_kind(p = 18)
    real (kind = ikind) :: xi, yi, x1, y1, x2, y2, time, e_time, interceptor_angle, interceptor_speed

    ! Calculate intercept coordinates
    xi = (x2 - x1)/time*e_time + x2
    yi = (y2 - y1)/time*e_time + y2

    ! Calculate interceptor angle and speed
    interceptor_angle = atan(yi /xi)
    interceptor_speed = sqrt(xi**2 + yi**2)/e_time
end subroutine calc_int_time

subroutine calc_int_angle(interceptor_angle, xi, yi, m1, m2, b, y1, y2, time, e_time, interceptor_speed)
    implicit none
    integer, parameter :: ikind = selected_real_kind(p = 18)
    real (kind = ikind) :: xi, yi, y1, m1, m2, b, y2, time, e_time, interceptor_angle, interceptor_speed
! Calculate slope of interception
    m2 = sin(interceptor_angle)/cos(interceptor_angle)

    ! Calculate interception point
    xi = b/(m2-m1)
    yi = b*m2/(m2-m1)

    ! Calculate elapsed time
    e_time = (yi - y2)*(time/(y2 - y1))

    ! Calculate interception speed
    interceptor_speed = abs(sqrt(xi**2 + yi**2)/e_time)
end subroutine calc_int_angle

subroutine calc_int_speed(x1, x2, xi, y1, y2, yi, dx1, dy1, interceptor_speed, a, d, c, sub_1, e_time, time, interceptor_angle)
    implicit none
    integer, parameter :: ikind = selected_real_kind(p = 18)
    real (kind = ikind) :: x1, x2, xi, y1, y2, yi, dx1, dy1, interceptor_speed, a, d, c, sub_1, e_time, time, interceptor_angle
    ! calculate constants
    dx1 = (x2 - x1)/time
    dy1 = (y2 - y1)/time
    a = dx1**2 + dy1**2 - interceptor_speed**2
    d = 2*dx1 + 2*dy1
    c = x2**2 + y2**2

    sub_1 = d/(2*a)
    e_time = sqrt(sub_1**2 - (c/a)) - sub_1

    ! Calculate intercept coordinates
    xi = (x2 - x1)/time*e_time + x2
    yi = (y2 - y1)/time*e_time + y2

    ! Calculate intercept angle
    interceptor_angle = atan(yi /xi)
end subroutine calc_int_speed


function deg_to_rad(deg)
    implicit none
    integer, parameter :: ikind = selected_real_kind(p = 18)
    real (kind = ikind) :: pi, deg, deg_to_rad
    pi = 4.0_ikind*atan(1.0_ikind)
    deg_to_rad = (deg*pi/180.0_ikind)
end function deg_to_rad

function rad_to_deg(rad)
    implicit none
    integer, parameter :: ikind = selected_real_kind(p = 18)
    real (kind = ikind) :: pi, rad, rad_to_deg
    pi = 4.0_ikind*atan(1.0_ikind)
    rad_to_deg = (rad*(180.0_ikind)/pi)
end function rad_to_deg

