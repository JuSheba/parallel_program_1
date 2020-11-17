program main
  use :: omp_lib
  use :: Task
  implicit none
  integer(4) :: i
  integer(4) :: x1, y1, x2, y2
  real(8), allocatable :: A(:,:)
  real(8) :: time1, time2
  integer, dimension(33) :: seed = (/ 73, 48, 53, 56, 75, 34, 96, 28, 17,  6,  2,&
                                      59, 26, 60, 40, 57, 69,  6, 60, 22,  2, 83,&
                                      89, 84,  9, 15, 32, 65, 20, 59, 61, 24, 73 /)
  allocate(A(1337, 1337))

  call random_seed(put=seed)

  call random_number(A)
  A = 2d0 * A - 1d0

  call omp_set_num_threads(1)
  time1 = omp_get_wtime()
  call GetMaxCoordinates(A, x1, y1, x2, y2)
  time2 = omp_get_wtime()

  write(*,*) 'Coordinates: ', x1, y1, x2, y2
  write(*,*) 'Time: ', time2 - time1

end program
