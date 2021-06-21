module core
contains
  SUBROUTINE qcheck(ret, q)
    LOGICAL, INTENT(OUT) :: ret
    REAL, DIMENSION(4), INTENT(IN) :: q
    REAL :: qout
    qout = q(1)**2 + q(2)**2 + q(3)**2 + q(4)**2
    IF (qout > 1.0) THEN
       ret = .FALSE.
    ELSE
       ret = .TRUE.
    END IF
  END SUBROUTINE qcheck
  subroutine mul(qproduct, q1, q2)
    ! q = [x, y, z, w]
    REAL, DIMENSION(4), INTENT(OUT) :: qout
    REAL, DIMENSION(4), INTENT(IN) :: q1, q2
    qproduct = &
         [q1(4)*q2(1) + q1(1)*q2(4) + q1(2)*q2(3) - q1(3)*q2(2), &
         q1(4)*q2(2) - q1(1)*q2(3) + q1(2)*q2(3) + q1(3)*q2(1), &
         q1(4)*q2(3) + q1(1)*q2(2) - q1(2)*q2(1) + q1(3)*q2(4), &
         q1(4)*q2(4) - q1(1)*q2(1) - q1(2)*q2(2) - q1(3)*q2(3)]
  END SUBROUTINE mul
  subroutine norm(rotmat, euler)
    real, dimension :: rotmat
  end subroutine norm
  subroutine conj(quat)
    real, dimension :: quat
  end subroutine conj
end module core
