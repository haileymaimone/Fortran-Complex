! CSC 306 Assignment 6 - The Complex Solver
! Hailey Maimone

      
      MODULE cmplxsolver
        CONTAINS 
        SUBROUTINE READCMPLX(a, b, soln, n)
            USE iso_Fortran_env
            IMPLICIT NONE
            COMPLEX(KIND=REAL64), INTENT(OUT), ALLOCATABLE, DIMENSION(:, :) :: a
            ! complex array of coefficients (n x n)
            REAL(KIND=REAL64), ALLOCATABLE, DIMENSION(:, :) :: ra
            ! matrix a real part
            REAL(KIND=REAL64), ALLOCATABLE, DIMENSION(:, :) :: rb
            ! matrix a imaginary part
            COMPLEX(KIND=REAL64), INTENT(OUT), ALLOCATABLE, DIMENSION(:) :: b
            ! complex array for constant vector
            REAL(KIND=REAL64), ALLOCATABLE, DIMENSION(:) :: ba
            ! constant vector b real part
            REAL(KIND=REAL64), ALLOCATABLE, DIMENSION(:) :: bb
            ! constant vector b imaginary part
            COMPLEX(KIND=REAL64), INTENT(OUT), ALLOCATABLE, DIMENSION(:) :: soln
            ! solution vector
            INTEGER, INTENT(OUT) :: n
            ! number of equations to solve
            INTEGER :: i, j, istat

10          FORMAT(' ', A, X, I2, X, A, X, I2, X, A)
20          FORMAT(' ', A, I2, X, I2, A)
30          FORMAT(' ', A, I2, A)

            WRITE(*,*) ' '
            WRITE(*,*) 'Enter the number of equations:'
            READ(*, *) n
            ! make sure there are at least 2 equations, but no more than 10
            DO WHILE( n .LT. 2 .OR. n .GT. 10)
                WRITE(*,*) ' '
                WRITE(*,*) '-WARNING:  INVALID INPUT.-'  
                WRITE(*,*) 'The number of equations must be between 2 and 10.'
                WRITE(*,*) 'Please enter a new number of equations:'
                READ(*,*) n
            END DO
            ! allocate arrays
            ALLOCATE(a(n, n), b(n), soln(n), &
                    ra(n, n), rb(n, n), ba(n), bb(n), STAT=istat)
            WRITE(*,*) ' '
            ! enter a real part and imaginary part for each coefficent as separate real's
            ! then convert to complex number
            WRITE(*, 10) 'Enter the ', n, 'equations with ', n, 'coefficients each for the coefficient matrix'
            DO i = 1, n
                DO j = 1, n
                    WRITE(*, 20) 'A(', i, j, ') as real imaginary'
                    READ(*, *) ra(i, j), rb(i, j)
                    a(i, j) = CMPLX(ra(i, j), rb(i, j), KIND=REAL64)
                END DO
            END DO
            ! enter a real part and imaginary part for each constant vector value as separate real's
            ! then convert to complex number
            WRITE(*, '(A, X, I2, X, A)') 'Enter the', n, 'constant vector values'
            DO i = 1, n
                WRITE(*, 30) 'Constant(', i, ') as real imaginary'
                READ(*, *) ba(i), bb(i)
                b(i) = CMPLX(ba(i), bb(i), KIND=REAL64)
            END DO
            ! deallocate real arrays since we are done with them
            DEALLOCATE(ra, rb, ba, bb)
            ! call printcmplx subroutine for printing
            CALL PRINTCMPLX(a, b, soln, n)
            RETURN
        END SUBROUTINE READCMPLX

        SUBROUTINE PRINTCMPLX(a, b, soln, n)
            USE iso_Fortran_env
            IMPLICIT NONE
            COMPLEX(KIND=REAL64), INTENT(INOUT), ALLOCATABLE, DIMENSION(:, :) :: a
            ! complex array of coefficients (n x n)
            COMPLEX(KIND=REAL64), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: b
            ! complex array for constant vector
            COMPLEX(KIND=REAL64), INTENT(INOUT), ALLOCATABLE, DIMENSION(:) :: soln
            INTEGER, INTENT(IN) :: n
            ! number of equations to solve
            INTEGER :: error_flag
            ! error flag
            INTEGER :: i, j
            ! loop counters
            CHARACTER(LEN=20) :: resp
            ! character used for reading a value
40          FORMAT(' ', '(', F8.4, ' + i ', F8.4, ')', A, I1, A)
45          FORMAT(' ', '(', F8.4, ' + i ', F8.4, ')', A, I2, A)
50          FORMAT(' ', A, I1, A, '(', F8.4, ' + i ', F8.4, ')')
51          FORMAT(' ', A, I2, A, '(', F8.4, ' + i ', F8.4, ')')
55          FORMAT(' ', A, I1, ' = ', '(', F8.4, ' + i ', F8.4, ')')
60          FORMAT(' ', A, I2, ' = ', '(', F8.4, ' + i ', F8.4, ')')
            ! print data points
            WRITE(*,*) ' '
            WRITE(*,*) '*****************'
            WRITE(*,*) '   DATA POINTS   '
            WRITE(*,*) '*****************'
            DO i = 1, n
                DO j = 1, n
                    ! print equations
                IF (j .NE. n) THEN
                    WRITE(*, 40, ADVANCE='no') a(i, j), 'X', j, ' + '
                ELSE
                    IF (j .eq. 10) THEN
                        WRITE(*, 45, ADVANCE='no') a(i, j), 'X', j, ''
                        PRINT '(/)'
                    ELSE
                        WRITE(*, 40, ADVANCE='no') a(i, j), 'X', j, ''
                        PRINT '(/)'
                    END IF
                END IF
                END DO
            END DO
            DO i = 1, n
                ! print constants
                WRITE(*,*) ' '
                IF (j .eq. 10) THEN
                    WRITE(*, 51) 'Constant(', i, ') = ', b(i)
                ELSE
                    WRITE(*, 50) 'Constant(', i, ') = ', b(i)
                END IF
            END DO
			
            WRITE(*,*) ' '
            PRINT*, 'Are these data points correct? (Y/N)'
            ! confirm if data points are correct
                    READ(*, *) resp
                    IF (resp .EQ. 'Y' .OR. resp .EQ. 'y') THEN
                        ! if yes, then call csimul subroutine to solve the equations
                        CALL csimul(a, b, soln, n, n, error_flag)
                        ! if there is no unique solution
                        IF (error_flag /= 0) THEN
                            WRITE(*, *) 'Zero pivot encountered. No unique Solution.'
                        ELSE
                            ! else print solution(s)
                            WRITE(*,*) ' '
                            WRITE(*,*) '**********'
                            WRITE(*,*) ' SOLUTION '
                            WRITE(*,*) '**********'
                            DO i = 1, n
                                WRITE(*,*) 'The solution is:'
                                IF (j .eq. 10) THEN
                                    WRITE(*, 60) 'X', i, soln(i)
                                ELSE
                                    WRITE(*, 55) 'X', i, soln(i)
                                END IF
                            END DO
                        END IF
                    END IF
                    ! deallocate arrays since we are done with them
                    DEALLOCATE(a, b, soln)

            RETURN
        END SUBROUTINE PRINTCMPLX
      ! gauss-jordan subroutine taken from text
      SUBROUTINE csimul(a, b, soln, ndim, n, error)
        USE iso_Fortran_env
        IMPLICIT NONE

        REAL(KIND=REAL64), PARAMETER :: EPSILON = 1.0E-12
        ! "small" number for comparison when determining singular eqns
        INTEGER, INTENT(IN) :: ndim
        ! Dimension of arrays a and b
        COMPLEX(KIND=REAL64), INTENT(IN), DIMENSION(ndim, ndim) :: a
        ! Array of coefficients (N x N)
        ! This array is of size ndim x
        ! ndim, but only N x N of the
        ! coefficients are being used
        COMPLEX(KIND=REAL64), INTENT(IN), DIMENSION(ndim) :: b
        ! Input: Right-hand side of eqns
        COMPLEX(KIND=REAL64), INTENT(OUT), DIMENSION(ndim) :: soln
        ! Output: Solution vector
        INTEGER, INTENT(IN) :: n
        ! Number of equations to solve
        INTEGER, INTENT(OUT) :: error
        ! Error flag:
        ! 0 -- No error
        ! 1 -- Singular equations

        COMPLEX(KIND=REAL64), DIMENSION(n, n) :: a1
        ! Copy of "a" which will be destroyed during the solution
        COMPLEX(KIND=REAL64) :: factor
        ! Factor to multiply eqn irow by before adding to eqn jrow
        INTEGER :: irow
        ! Number of the equation currently being processed
        INTEGER :: ipeak
        ! Pointer to equation containing maximum pivot value
        INTEGER :: jrow
        ! Number of the equation compared to the current equation
        COMPLEX(KIND=REAL64) :: temp
        ! Scratch value
        COMPLEX(KIND=REAL64), DIMENSION(n) :: temp1
        ! Scratch array

        ! Make copies of arrays "a" and "b" for local use
        a1 = a(1:n, 1:n)
        soln = b(1:n)

        ! Process N times to get all equations...
        DO irow = 1, n
            ! Find peak pivot for column irow in rows irow to N
            ipeak = irow
            DO jrow = irow+1, n
                IF (ABS(a1(jrow,irow)) > ABS(a1(ipeak,irow))) THEN
                    ipeak = jrow
                END IF
            END DO

            ! Check for singular equations
            IF (ABS(a1(ipeak,irow)) < EPSILON) THEN
                error = 1
                RETURN
            END IF

            ! Otherwise, if ipeak /= irow, swap equations irow and ipeak
            IF (ipeak /= irow) THEN
                temp1 = a1(ipeak, 1:n)
                a1(ipeak, 1:n) = a1(irow, 1:n)
                a1(irow, 1:n) = temp1
                temp = soln(ipeak)
                soln(ipeak) = soln(irow)
                soln(irow) = temp
            END IF

            ! Multiply equation irow by -a1(jrow, irow) / a1(irow, irow)
            ! add add it to Eqn jrow (for all eqns except irow itself)
            DO jrow = 1, n
                IF (jrow /= irow) THEN
                    factor = -a1(jrow, irow)/a1(irow, irow)
                    a1(jrow, 1:n) = a1(irow, 1:n)*factor + a1(jrow, 1:n)
                    soln(jrow) = soln(irow)*factor + soln(jrow)
                END IF
            END DO
        END DO

        ! End of main loop over all equations.
        ! All off-diagonal terms are now zero.
        ! To get the final answer, we must divide each equation
        ! by the coefficient of its on-diagonal term
        DO irow = 1, n
            soln(irow) = soln(irow) / a1(irow,irow)
        END DO

        ! Set error flag to 0 and return
        error = 0
        RETURN
      END SUBROUTINE csimul
    END MODULE cmplxsolver


      PROGRAM main
        USE iso_Fortran_env
        USE cmplxsolver
        IMPLICIT NONE
        COMPLEX(KIND=REAL64), ALLOCATABLE, DIMENSION(:, :) :: a
        ! coefficients
        COMPLEX(KIND=REAL64), ALLOCATABLE, DIMENSION(:) :: b
        ! constant values
        COMPLEX(KIND=REAL64), ALLOCATABLE, DIMENSION(:) :: soln
        ! solution
        INTEGER :: n
        ! Size of system of eqns to solve
        LOGICAL :: switch = .TRUE.
        CHARACTER(LEN=20) :: resp = ' '
        WRITE(*,*) ' '
        WRITE(*,*) '********************************'
        WRITE(*,*) '*Welcome to The Complex Solver!*'
        WRITE(*,*) '********************************'
        WRITE(*,*) ' '
        WRITE(*,*) 'We will be solving complex linear equations.'
        PRINT*, ' '
        ! menu
        DO WHILE (switch)
            IF (resp .EQ. ' ') THEN
                ! on first entry display this message
                PRINT*, 'Do you wish to solve complex linear equations?'
            ELSE
                ! otherwise, ask if they want to solve more
                PRINT*, ' '
                PRINT*, ' '
                PRINT*, 'Do you wish to solve more?'
            END IF
            READ(*, *) resp
            SELECT CASE (resp)
            CASE ('Y', 'y', 'yes', 'YES', 'Yes')
                ! if respond yes, call readcmplx subroutine
                CALL READCMPLX(a, b, soln, n)
            CASE DEFAULT
                ! exit program
                switch = .FALSE.
                PRINT*, 'Exiting program'
            END SELECT
        END DO
      END PROGRAM main
        

    

                

