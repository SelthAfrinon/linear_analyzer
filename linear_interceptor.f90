program linear_interceptor
    implicit none
    integer, parameter :: ikind = selected_real_kind(p = 18)
    real (kind = ikind) :: rad_to_deg, deg_to_rad                               ! function variables
    real (kind = ikind) :: dist_1, dist_2, ang_1, ang_2, time, x1, x2, y1, y2, b, m1, m2   ! Variables for calculation of point position section of program
    real (kind = ikind) :: proj_speed, proj_angle, proj_dist                               ! Variables for calculation of velocity components
    real (kind = ikind) :: xi, yi, interceptor_speed, interceptor_angle, e_time       ! Variables for calculation of interception angle
    real (kind = ikind) :: a, d, c, dx1, dy1, sub_1
    integer :: known_type

    do
        ! input parameters
        write (*,*) "Enter first distance and angle (deg) seperated by a space."
        read *, dist_1, ang_1

        write (*,*) "Enter second distance and angle (deg) seperated by a space."
        read *, dist_2, ang_2

        write (*,*) "Enter time between measurements."
        read *, time

        ! convert given angles to radians
        ang_1 = deg_to_rad(ang_1)
        ang_2 = deg_to_rad(ang_2)

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

        ! Choose which interceptor property is known: 0 = Time to intercept (e_time), 1 = Intercept Angle, 2 = Interceptor velocity

        ! Start do loop for multiple inputs entered from screen
        do
            print *, "Enter the known interceptor property."
            print *, "(0 for intercept time, 1 for intercept angle, 2 for interceptor speed"
            print *, "3 to enter new incoming projectile, 4 to exit): "
            read *, known_type

            if (known_type == 0) then
            ! branch for known time to intercept
                print *, "Enter intercept time: "
                read *, e_time

                ! Calculate intercept coordinates
                xi = (x2 - x1)/time*e_time + x2
                yi = (y2 - y1)/time*e_time + y2

                ! Calculate interceptor angle and speed
                interceptor_angle = rad_to_deg(atan(yi /xi))
                interceptor_speed = sqrt(xi**2 + yi**2)/e_time

            else if (known_type == 1) then
            ! branch for known interception angle
                print *, "Enter intercept angle: "
                read *, interceptor_angle

                ! Calculate slope of interception
                m2 = sin(deg_to_rad(interceptor_angle))/cos(deg_to_rad(interceptor_angle))

                ! Calculate interception point
                xi = b/(m2-m1)
                yi = b*m2/(m2-m1)

                ! Calculate elapsed time
                e_time = (yi - y2)*(time/(y2 - y1))

                ! Calculate interception speed
                interceptor_speed = abs(sqrt(xi**2 + yi**2)/e_time)


            else if (known_type == 2) then
            ! branch for known interceptor speed
                print *, "Enter interceptor speed: "
                read *, interceptor_speed

                ! calculate constants

                dx1 = (x2 - x1)/time
                dy1 = (y2 - y1)/time
                a = dx1**2 + dy1**2 - interceptor_speed**2
                d = 2*dx1 + 2*dy1
                c = x2**2 + y2**2

                sub_1 = d/(2*a)
                !if(sub_1 > 0) then
                    e_time = sqrt(sub_1**2 - (c/a)) - sub_1

                    ! Calculate intercept coordinates
                    xi = (x2 - x1)/time*e_time + x2
                    yi = (y2 - y1)/time*e_time + y2

                    ! Calculate intercept angle
                    interceptor_angle = rad_to_deg(atan(yi /xi))
                !else
                    !e_time = 0
                    !print *, "The interceptor is not fast enough to catch the projectile."
                !end if

            else if (known_type == 3) then
                exit
            else if (known_type == 4) then
                exit
            end if
            write(*,2) "Projectile Speed: ", proj_speed, "   Projectile Angle: ", rad_to_deg(proj_angle)
            write(*,1) "Time: ", e_time, "   Interceptor Speed: ", interceptor_speed, "    Interceptor Angle: ", interceptor_angle
            1 format(1A6, 1f7.3, 1A21, 1f9.3, 1A21, 1f10.3)
            2 format(1A18, 1f7.3, 1A21, 1f9.3, 1A21, 1f10.3)
        end do

        ! Testing output area
        !1 format(1A20, f10.5)
        !write(*,1) "Projectile Speed is ", proj_speed
        !write(*,1) "Projectile Angle is ", rad_to_deg(proj_angle)

        !write(*,*) dx, dy

        !write(*,*) rad_to_deg(interceptor_angle_1), rad_to_deg(interceptor_angle_2)\
        if (known_type == 4) then
            exit
        end if
    end do
end program linear_interceptor

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
    rad_to_deg = (rad*180.0_ikind/pi)
end function rad_to_deg

