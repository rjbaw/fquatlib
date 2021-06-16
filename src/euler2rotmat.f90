SUBROUTINE EULER2ROTMAT(rotmat, euler)
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
