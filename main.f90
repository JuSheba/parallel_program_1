program main
  use :: omp_lib
  use :: Task
  implicit none
  integer :: x1, y1, x2, y2
  real(8), allocatable :: A(:,:)
  real(8) :: time1, time2

  allocate(A(1337, 1337))

  call random_number(A)
  A = 2d0 * A - 1d0

  call omp_set_num_threads(2)
  time1 = omp_get_wtime()
  call GetMaxCoordinates(A, x1, y1, x2, y2)
  time2 = omp_get_wtime()

  write(*,*) 'Coordinates: ', x1, y1, x2, y2
  write(*,*) 'Time: ', time2 - time1

end program
