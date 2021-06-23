MODULE conversion
CONTAINS
  !checks
  SUBROUTINE checkrotmat(rotmat)
    REAL, DIMENSION(:) :: rotmat
    identity
  END SUBROUTINE checkrotmat
  SUBROUTINE qcheck(ret, q)
    LOGICAL, INTENT(OUT) :: ret
    REAL, DIMENSION(4), INTENT(IN) :: q
    REAL :: out
    out = q(1)**2 + q(2)**2 + q(3)**2 + q(4)**2
    IF (out > 1.0) THEN
       ret = .FALSE.
    ELSE
       ret = .TRUE.
    END IF
  END SUBROUTINE qcheck

  SUBROUTINE euler2rotmat(rotmat, euler)
    REAL, DIMENSION(3), INTENT(IN) :: euler
    REAL, DIMENSION(3,3), INTENT(OUT) :: rotmat
    REAL, DIMENSION(4) :: q
    !REAL, DIMENSION(3,3) :: r_x, r_y, r_z
    !r_z(1,:) = (/ COS(euler(3)), -SIN(euler(3)), 0 /)
    !r_z(2,:) = (/ SIN(euler(3)),  COS(euler(3)), 0 /)
    !r_z(3,:) = (/ 0, 0, 1 /)
    !r_y(1,:) = (/ COS(euler(2)), 0, SIN(euler(2)) /)
    !r_y(2,:) = (/ 0, 1, 0 /)
    !r_y(3,:) = (/ -SIN(euler(2)), 0, COS(euler(2)) /)
    !r_z(1,:) = (/ 1, 0, 0 /)
    !r_z(2,:) = (/ 0, COS(euler(1)), -SIN(euler(1)) /)
    !r_z(3,:) = (/ 0, SIN(euler(1)), COS(euler(1)) /)
    !rotmat = r_z * r_y * r_x
    call euler2quat(q, euler)
    call quat2rotmat(rotmat, q)
  END SUBROUTINE euler2rotmat
  SUBROUTINE rotmat2euler(euler, rotmat)
    REAL, DIMENSION(3,3), INTENT(IN) :: rotmat
    REAL, DIMENSION(4), INTENT(OUT) :: euler
    REAL, DIMENSION(4) :: q
    !LOGICAL :: singular 
    !sy = sqrt( rotmat(1,1)**2 + rotmat(2,1)**2 )
    !singular = sy < 1e-6
    !if (singular == .true.) then
    !   euler(1) = atan2(rotmat(3,2), rotmat(3,3))
    !   euler(2) = atan2(-rotmat(3,1), sy)
    !   euler(3) = atan2(rotmat(2,1), rotmat(1,1))
    !else
    !   euler(1) = atan2(-rotmat(2,3), rotmat(2,2))
    !   euler(2) = atan2(-rotmat(3,1), sy)
    !   euler(3) = 0
    !endif
    call rotmat2quat(q, rotmat)
    call quat2euler(euler, q)
  end subroutine rotmat2euler

  subroutine rotmat2quat(quat, rotmat)
    REAL, DIMENSION(4), INTENT(IN) :: quat
    REAL, DIMENSION(3,3), INTENT(OUT) :: rotmat
  end subroutine rotmat2quat
  subroutine euler2quat(quat, euler)
    REAL, DIMENSION(4), INTENT(IN) :: euler
    REAL, DIMENSION(4), INTENT(OUT) :: quat
  end subroutine rotmat2quat

  subroutine quat2rotmat(rotmat, quat)
    ! r, i, j, k
    ! i am sure there is a better way to implement this
    REAL, DIMENSION(4), INTENT(IN) :: quat
    REAL, DIMENSION(3,3), INTENT(OUT) :: rotmat
    !rotmat(1,:) = (/ 1 - 2*s*(quat(3)**2 + quat(4)**2), 2*s*(q_i
  contains
    subroutine qmul(p,q)
      real, dimension(4) :: p,q
    end subroutine qmul
  end subroutine quat2rotmat
  subroutine quat2euler(euler, quat)
    REAL, DIMENSION(4), INTENT(IN) :: quat
    REAL, DIMENSION(4), INTENT(OUT) :: euler
  end subroutine quat2rotmat

end module conversion
