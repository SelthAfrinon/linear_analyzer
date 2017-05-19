program linear_interceptor
    implicit none
    integer, parameter :: ikind = selected_real_kind(p = 18)
    real (kind = ikind) :: rad_to_deg, deg_to_rad                               ! function variables
    real (kind = ikind) :: dist_1, dist_2, ang_1, ang_2, time, x1, x2, y1, y2   ! Variables for calculation of point position section of program
    real (kind = ikind) :: proj_speed, proj_angle                       ! Variables for calculation of velocity components
    real (kind = ikind) :: xi, yi, interceptor_speed, interceptor_angle, e_time       ! Variables for calculation of interception angle

    dist_1 = 50.0
    dist_2 = 13.0
    ang_1 = 70 ! in degrees
    ang_2 = 26 ! in degrees
    time = 9  ! Minutes

    ang_1 = deg_to_rad(ang_1)
    ang_2 = deg_to_rad(ang_2)

    x1 = sin(ang_1)*dist_1
    x2 = sin(ang_2)*dist_2

    y1 = cos(ang_1)*dist_1
    y2 = cos(ang_2)*dist_2

    proj_speed = sqrt((x2-x1)**2 + (y2 - y1)**2)/time
    proj_angle = atan((y2 - y1)/(x2-x1))


    e_time = 6
    xi = proj_speed*cos(proj_angle)*e_time + x2
    yi = proj_speed*sin(proj_angle)*e_time + y2


    !interceptor_speed = 14.634

    interceptor_angle = atan(xi /yi)
    interceptor_speed = sqrt(xi**2 + yi**2)/e_time

    ! Testing output area
    !1 format(1A20, f10.5)
    !write(*,1) "Projectile Speed is ", proj_speed
    !write(*,1) "Projectile Angle is ", rad_to_deg(proj_angle)

    !write(*,*) dx, dy

    !write(*,*) rad_to_deg(interceptor_angle_1), rad_to_deg(interceptor_angle_2)\

    write(*,*) interceptor_speed, rad_to_deg(interceptor_angle)


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

