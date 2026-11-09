! ============================================================
!  sum_1_to_n.f90
!  計算 1 + 2 + 3 + ... + N
!  編譯： gfortran sum_1_to_n.f90 -o sum_1_to_n
!  執行： ./sum_1_to_n        (Windows: sum_1_to_n.exe)
! ============================================================
program sum_1_to_n
    implicit none
    integer, parameter :: ik = selected_int_kind(18)   ! 64-bit 整數，避免溢位
    integer(kind=ik) :: n, i, total, formula
    integer :: ios

    do
        write(*, '(A)', advance='no') '請輸入正整數 N： '
        read(*, *, iostat=ios) n
        if (ios /= 0) then
            write(*, *) '輸入錯誤，請輸入整數。'
        else if (n < 1) then
            write(*, *) 'N 必須 >= 1，請重新輸入。'
        else
            exit
        end if
    end do

    ! 用迴圈逐項相加
    total = 0_ik
    do i = 1_ik, n
        total = total + i
    end do

    ! 用公式 N(N+1)/2 驗算
    formula = n * (n + 1_ik) / 2_ik

    write(*, '(A, I0, A, I0)') '1 加到 ', n, ' 的總和 = ', total
    write(*, '(A, I0)')        '公式 N(N+1)/2 驗算   = ', formula

    if (total == formula) then
        write(*, '(A)') '驗算結果：正確'
    else
        write(*, '(A)') '驗算結果：不一致'
    end if
end program sum_1_to_n
