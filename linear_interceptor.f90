program linear_interceptor
    implicit none
    integer, parameter :: ikind = selected_real_kind(p = 18)
    real (kind = ikind) :: rad_to_deg, deg_to_rad                               ! function variables
    real (kind = ikind) :: dist_1, dist_2, ang_1, ang_2, time, x1, x2, y1, y2, b, m1, m2   ! Variables for calculation of point position section of program
    real (kind = ikind) :: proj_speed, proj_angle, proj_dist                               ! Variables for calculation of velocity components
    real (kind = ikind) :: xi, yi, interceptor_speed, interceptor_angle, e_time       ! Variables for calculation of interception angle
    integer :: known_type

    ! input parameters
    dist_1 = 50.0
    dist_2 = 40.0
    ang_1 = 70 ! in degrees
    ang_2 = 26 ! in degrees
    time = 9  ! Minutes

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
    known_type = 1

    if (known_type == 0) then
    ! branch for known time to intercept
        e_time = .5

        ! Calculate intercept coordinates
        xi = (x2 - x1)/time*e_time + x2
        yi = (y2 - y1)/time*e_time + y2

        ! Calculate interceptor angle and speed
        interceptor_angle = rad_to_deg(atan(yi /xi))
        interceptor_speed = sqrt(xi**2 + yi**2)/e_time

    else if (known_type == 1) then
    ! branch for known interception angle
        interceptor_angle = 23.254

        ! Calculate slope of interception
        m2 = sin(deg_to_rad(interceptor_angle))/cos(deg_to_rad(interceptor_angle))

        ! Calculate interception point
        xi = b/(m2-m1)
        yi = b*m2/(m2-m1)

        ! Calculate elapsed time
        e_time = (yi - y2)*(time/(y2 - y1))

        ! Calculate interception speed
        interceptor_speed = abs(sqrt(xi**2 + yi**2)/e_time)


    else if (known_type == 3) then
    ! branch for known interceptor speed


    end if

    ! Testing output area
    !1 format(1A20, f10.5)
    !write(*,1) "Projectile Speed is ", proj_speed
    !write(*,1) "Projectile Angle is ", rad_to_deg(proj_angle)

    !write(*,*) dx, dy

    !write(*,*) rad_to_deg(interceptor_angle_1), rad_to_deg(interceptor_angle_2)\
    write(*,1) "Time: ", e_time, "   Interceptor Speed: ", interceptor_speed, "    Interceptor Angle: ", interceptor_angle
    1 format(1A6, 1f7.3, 1A21, 1f9.3, 1A21, 1f10.3)

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

