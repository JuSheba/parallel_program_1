module Task
  use omp_lib
  implicit none
  contains

  subroutine GetMaxCoordinates(A, x1, y1, x2, y2)
    implicit none
    real(8), intent(in), dimension(:,:) :: A
    integer(4), intent(out) :: x1, y1, x2, y2
    integer(4) :: n, L, R, Up, Down, m, tmp
    real(8), allocatable :: current_column(:)
    real(8) :: current_sum, max_sum

    m = size(A, dim=1)
    n = size(A, dim=2)

    !$omp parallel shared(A, x1 y1, x2, y2, n, max_sum) private(L, R, Up, Down, current_column, current_sum)
    allocate(current_column(m))

    x1=1
    y1=1
    x2=1
    y2=1
    max_sum = A(1,1)

    !$omp do schedule(dynamic)
    do L = 1, n
      current_column = A(:, L)

      do R = L, n
        if (R > L) then
          current_column = current_column + A(:, R)
        endif

        call FindMaxInArray(current_column, current_sum, Up, Down)

        !$omp critical
        if (current_sum > max_sum) then
          max_sum = current_sum
          x1 = Up
          x2 = Down
          y1 = L
          y2 = R
        endif
        !$omp end critical
      end do
    end do
    !$omp end do
    deallocate(current_column)
    !$omp end parallel
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
