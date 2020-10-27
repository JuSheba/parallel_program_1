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

    !ПАРАЛЛЕЛЬНЫЙ РЕГИОН
    ! здесь нужно указать, какие переменные будут разделяться между потоками
    ! (доступ к одной области памяти), а для каких поток будет создавать копию.
    ! (shared и private, соответстсвенно)
    ! Shared  - A, x1 y1, x2, y2, n, max_sum
    ! Private - L, R, Up, Down, current_column, current_sum
    ! Значение private переменных требует инициализации, поэтому начало до allocate(current_column(m))
    ! Значение массива А, например, сохраняется при выходе из пар.региона.

    !$omp parallel shared(A, x1 y1, x2, y2, n, max_sum) private(L, R, Up, Down, current_column, current_sum)
    allocate(current_column(m))

    x1=1
    y1=1
    x2=1
    y2=1
    max_sum = A(1,1)

    !По умолчанию барьером для потоков является конец цикла. Все потоки достигнув конца
    !цикла дожидаются тех, кто еще не завершился, после чего основная нить продолжает
    !выполняться дальше.
    ! Условие schedule контролирует то, как работа распред. по потокам.
    ! Здесь подходит тип static (равном.распр. итераций). Но если итерации не равны по
    ! времени, то лучше использовать dynamic. Поток выполняет часть итераций и запрашивает
    ! новую и т.д., пока они не закончатся. Нет порядка, в котором распред. части итер-ий.
    ! Размер части по умолчанию (chunk-size): 1.

    !$omp do schedule(dynamic)
    do L = 1, n   ! НАЧАЛО: распределение итераций цикла do по потокам
      current_column = A(:, L)

      do R = L, n
        if (R &gt; L) then
          current_column = current_column + A(:, R)
        endif

        call FindMaxInArray(current_column, current_sum, Up, Down)

        if (current_sum &gt; max_sum) then
          max_sum = current_sum
          x1 = Up
          x2 = Down
          y1 = L
          y2 = R
        endif
      end do
    end do          ! КОНЕЦ: все потоки завершаются, кроме основного
    !$omp end do
    deallocate(current_column)
    !$omp end parallel
    ! КОНЕЦ ПАРАЛЛЕЛЬНОГО РЕГИОНА
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
      if (cur_sum &gt; Summ) then
        Summ = cur_sum
        Up = minus_pos + 1
        Down = i
      endif

      if (cur_sum &lt; 0) then
        cur_sum = 0
        minus_pos = i
      endif
    enddo

  end subroutine
end module
