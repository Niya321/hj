program runge_kutta
2  implicit none
3  real :: x, y, h, k1, k2, k3, k4
4  integer :: i, n
5
6  ! Initial conditions
7  x = 0.0
8  y = 1.0
9  h = 0.1
10  n = 10
11
12  ! Print header
13  write (*,*) '   x          y'
14  write (*,*) '-----------------'
15
16  ! Runge-Kutta method
17  do i = 1, n
18    k1 = h * f(x, y)
19    k2 = h * f(x + 0.5 * h, y + 0.5 * k1)
20    k3 = h * f(x + 0.5 * h, y + 0.5 * k2)
21    k4 = h * f(x + h, y + k3)
22    y = y + (k1 + 2.0 * k2 + 2.0 * k3 + k4) / 6.0
23    x = x + h
24    write (*,*) x, y
25  end do
26
27contains
28
29  function f(x, y) result(res)
30    real :: x, y, res
31    res = -32.0 * (4.0 * x**4 + 1.0)**(-1) * x**3
32  end function f
33
34end program runge_kutta
