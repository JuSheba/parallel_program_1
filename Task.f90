module Task
  use omp_lib
  implicit none
  contains

  subroutine GetMaxCoordinates(A, x1, y1, x2, y2)
    implicit none
    real(8), intent(in), dimension(:,:) :: A
    integer(4), intent(out) :: x1, y1, x2, y2
    integer(4) :: n, L, R, Up, Down, m, tmp, num_all_threads, thread_num
    real(8), allocatable :: current_column(:)
    real(8), allocatable :: max_sum_threads(:)
    real(8) :: current_sum, max_sum
    integer(4), allocatable :: coords_threads(:), num_max(:)

    m = size(A, dim=1)
    n = size(A, dim=2)

    x1=1
    y1=1
    x2=1
    y2=1
    max_sum = A(1,1)

    allocate(current_column(m))

    !$omp  parallel shared(m, x1, y1, x2, y2, A, n, max_sum, coords_threads,&
    !$omp& num_all_threads, max_sum_threads, num_max)&
    !$omp& private(L, R, Up, Down, current_column, current_sum, thread_num)

    !$omp single
    num_all_threads = omp_get_num_threads()

    allocate(max_sum_threads(0:num_all_threads-1), &
             coords_threads(0:4*num_all_threads-1))

    coords_threads = 1
    max_sum_threads = 1
    !$omp end single

    thread_num = omp_get_thread_num()
    !$omp do schedule(dynamic)
    do L = 1, n
      current_column = A(:, L)

      do R = L, n
        if (R > L) then
          current_column = current_column + A(:, R)
        endif

        call FindMaxInArray(current_column, current_sum, Up, Down)

        if (current_sum > max_sum_threads(thread_num)) then
          max_sum_threads(thread_num) = current_sum
          coords_threads(thread_num)  = Up
          coords_threads(thread_num + 1*num_all_threads) = Down
          coords_threads(thread_num + 2*num_all_threads) = L
          coords_threads(thread_num + 3*num_all_threads) = R
        endif
      end do
    end do
    !$omp end do
    !$omp end parallel

    num_max = maxloc(max_sum_threads)
    max_sum = max_sum_threads(num_max(1))
    x1 = coords_threads(num_max(1) + 0*num_all_threads)
    x2 = coords_threads(num_max(1) + 1*num_all_threads)
    y1 = coords_threads(num_max(1) + 2*num_all_threads)
    y2 = coords_threads(num_max(1) + 3*num_all_threads)

    deallocate(current_column, max_sum_threads, coords_threads)

  end subroutine

  subroutine FindMaxInArray(A, Summ, Up, Down)
    implicit none
    real(8), intent(in), dimension(:) :: A
    integer(4), intent(out) :: Up, Down
    real(8), intent(out) :: Summ
    real(8) :: cur_sum
    integer(4) :: minus_pos, i

    Summ = A(1)
    Up = 1
    Down = 1
    cur_sum = 0
    minus_pos = 0

    do i=1, size(A)
      cur_sum = cur_sum + A(i)
      if (cur_sum > Summ) then
        Summ = cur_sum
        Up = minus_pos + 1
        Down = i
      endif

      if (cur_sum < 0) then
        cur_sum = 0
        minus_pos = i
      endif
    enddo

  end subroutine
end module
