module conversion
contains
  subroutine checkrotmatrix(rotmat)
    real, dimension(:) :: rotmat
    identity
  end subroutine checkrotmatrix
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

  SUBROUTINE euler2rotmat(rotmat, euler)
    REAL, DIMENSION(3), INTENT(IN) :: euler
    REAL, DIMENSION(3,3), INTENT(OUT) :: rotmat
    REAL, DIMENSION(3,3) :: r_x, r_y, r_z
    r_z = &
         [[COS(euler(3)), -SIN(euler(3)), 0], &
         [SIN(euler(3)),  COS(euler(3)), 0], &
         [0, 0, 1]]
    r_y = &
         [[COS(euler(2)), 0, SIN(euler(2))],  &
         [0, 1, 0], &
         [-SIN(euler(2)), 0, COS(euler(2))]]
    r_z = &
         [[1, 0, 0], &
         [0, COS(euler(1)), -SIN(euler(1))], &
         [0, SIN(euler(1)), COS(euler(1))]]
    rotmat = r_z * r_y * r_x
  END SUBROUTINE euler2rotmat
  subroutine quat2rotmat(quat)
    real, dimension(4) :: quat
  end subroutine quat2rotmat
  subroutine rotmat2euler(rotmat)
    real, dimension(:) :: rotmat
    logical :: singular 
    real, dimension(:) :: euler
    sy = sqrt(rotmat(1,1) * rot
  end subroutine rotmat2euler
end module conversion
