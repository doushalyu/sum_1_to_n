! ============================================================
!  sum_1_to_n.f90
!  計算 1 + 2 + 3 + ... + N
!  編譯： gfortran sum_1_to_n.f90 -o sum_1_to_n
!  執行： ./sum_1_to_n        (Windows: sum_1_to_n.exe)
! ============================================================

! 主程式開始，程式名稱為 sum_1_to_n
program sum_1_to_n
    ! 關閉隱式宣告：所有變數都要明確宣告型別，避免打錯字時默默產生新變數
    implicit none
    ! 定義常數 ik：能表示到 10^18 的整數種類，也就是 64 位元整數
    integer, parameter :: ik = selected_int_kind(18)
    ! 宣告 64 位元整數：n=輸入值、i=迴圈計數、total=迴圈總和、formula=公式結果
    integer(kind=ik) :: n, i, total, formula
    ! 宣告一般整數 ios，接收 read 的狀態碼（0 代表讀取成功）
    integer :: ios

    ! 無限迴圈開始：一直要求輸入，直到輸入正確才跳出
    do
        ! 印出提示文字；advance='no' 表示不換行，游標停在提示後面
        write(*, '(A)', advance='no') '請輸入正整數 N： '
        ! 從鍵盤讀入 n，讀取狀態存到 ios（輸入非整數時 ios 不為 0）
        read(*, *, iostat=ios) n
        ! 若 ios 不等於 0，表示讀取失敗（例如輸入了文字）
        if (ios /= 0) then
            ! 印出錯誤訊息，之後回到迴圈開頭重新輸入
            write(*, *) '輸入錯誤，請輸入整數。'
        ! 若讀取成功，但 n 小於 1
        else if (n < 1) then
            ! 印出範圍錯誤訊息，之後回到迴圈開頭重新輸入
            write(*, *) 'N 必須 >= 1，請重新輸入。'
        ! 其餘情況：讀取成功且 n >= 1
        else
            ! 跳出 do 迴圈，繼續往下執行
            exit
        ! if 判斷結束
        end if
    ! 輸入迴圈結束
    end do

    ! ---------- 方法一：用迴圈逐項相加 ----------
    ! 總和歸零；0_ik 表示這個 0 是 ik 種類（64 位元）的整數
    total = 0_ik
    ! 計數迴圈：i 從 1 跑到 n，每次加 1
    do i = 1_ik, n
        ! 把目前的 i 累加到 total
        total = total + i
    ! 累加迴圈結束，此時 total = 1 + 2 + ... + n
    end do

    ! ---------- 方法二：用公式 N(N+1)/2 驗算 ----------
    ! 高斯公式；n(n+1) 一定是偶數，所以整數除以 2 不會有誤差
    formula = n * (n + 1_ik) / 2_ik

    ! 輸出迴圈結果；格式 A=字串、I0=整數以最少位數顯示
    write(*, '(A, I0, A, I0)') '1 加到 ', n, ' 的總和 = ', total
    ! 輸出公式計算的結果
    write(*, '(A, I0)')        '公式 N(N+1)/2 驗算   = ', formula

    ! 比較兩種算法的結果是否相同
    if (total == formula) then
        ! 相同：印出「正確」
        write(*, '(A)') '驗算結果：正確'
    ! 不相同
    else
        ! 印出「不一致」，代表程式或計算有問題
        write(*, '(A)') '驗算結果：不一致'
    ! if 判斷結束
    end if
! 主程式結束
end program sum_1_to_n
