module extensions
  interface rodrigues
     subroutine rodrigues_rotvec(rotmat, rotvec)
       real, dimension(4), intent(in) :: rotvec
       real, dimension(3,3), intent(out) :: rotmat
     end subroutine rodrigues_rotvec
     subroutine rodrigues_rotmat(rotvec, rotmat)
       real, dimension(4), intent(out) :: rotvec
       real, dimension(3,3), intent(out) :: rotmat
     end subroutine rodrigues_rotmat
  end interface rodrigues
contains
  subroutine avgquat(quat)
    real, dimension(4) :: quat
  end subroutine avgquat
  subroutine avgwquat(quat)
    real, dimension(4) :: quat
  end subroutine avgwquat
  subroutine rodrigues_rotvec(rotmat, rotvec)
    real, dimension(4), intent(in) :: rotvec
    real, dimension(3,3), intent(out) :: rotmat
  end subroutine rodrigues_rotvec
  subroutine rodrigues_rotmat(rotvec, rotmat)
    real, dimension(4), intent(out) :: rotvec
    real, dimension(3,3), intent(out) :: rotmat
  end subroutine rodrigues_rotmat
end module extensions


