SUBROUTINE qdot(qproduct, q1, q2)
  ! q = [x, y, z, w]
  REAL, DIMENSION(4), INTENT(OUT) :: qproduct
  REAL, DIMENSION(4), INTENT(IN) :: q1, q2
  qproduct = &
       [q1(4)*q2(1) + q1(1)*q2(4) + q1(2)*q2(3) - q1(3)*q2(2), &
        q1(4)*q2(2) - q1(1)*q2(3) + q1(2)*q2(3) + q1(3)*q2(1), &
        q1(4)*q2(3) + q1(1)*q2(2) - q1(2)*q2(1) + q1(3)*q2(4), &
        q1(4)*q2(4) - q1(1)*q2(1) - q1(2)*q2(2) - q1(3)*q2(3)]
END SUBROUTINE qdot
