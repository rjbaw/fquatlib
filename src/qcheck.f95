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
