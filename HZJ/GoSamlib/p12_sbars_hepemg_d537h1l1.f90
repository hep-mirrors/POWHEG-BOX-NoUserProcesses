module     p12_sbars_hepemg_d537h1l1
   ! file: /home/gionata/Documenti/Lavoro/GoSamPowheg/POWHEG-BOX/HZJ_tmp/GoSam_ &
   ! &POWHEG/Virtual/p12_sbars_hepemg/helicity1d537h1l1.f90
   ! generator: buildfortran.py
   use p12_sbars_hepemg_config, only: ki
   use p12_sbars_hepemg_util, only: cond
   implicit none
   private
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
   public :: numerator_samurai
   public :: numerator_golem95
contains
!---#[ function brack_1:
   pure function brack_1(Q,mu2) result(brack)
      use p12_sbars_hepemg_model
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_color
      use p12_sbars_hepemg_abbrevd537h1
      implicit none
      complex(ki), dimension(4), intent(in) :: Q
      complex(ki), intent(in) :: mu2
      complex(ki) :: brack
      complex(ki) :: acc537(34)
      complex(ki) :: Qspvak6k1
      complex(ki) :: Qspvak2k6
      complex(ki) :: QspQ
      complex(ki) :: Qspvak2e6
      complex(ki) :: Qspk2
      complex(ki) :: Qspvak5k4
      complex(ki) :: Qspvak2k1
      complex(ki) :: Qspk1
      complex(ki) :: Qspvak2k4
      complex(ki) :: Qspvak5k1
      complex(ki) :: Qspe6
      complex(ki) :: Qspk6
      Qspvak6k1 = dotproduct(Q,spvak6k1)
      Qspvak2k6 = dotproduct(Q,spvak2k6)
      QspQ = dotproduct(Q,Q)
      Qspvak2e6 = dotproduct(Q,spvak2e6)
      Qspk2 = dotproduct(Q,k2)
      Qspvak5k4 = dotproduct(Q,spvak5k4)
      Qspvak2k1 = dotproduct(Q,spvak2k1)
      Qspk1 = dotproduct(Q,k1)
      Qspvak2k4 = dotproduct(Q,spvak2k4)
      Qspvak5k1 = dotproduct(Q,spvak5k1)
      Qspe6 = dotproduct(Q,e6)
      Qspk6 = dotproduct(Q,k6)
      acc537(1)=abb537(4)
      acc537(2)=abb537(5)
      acc537(3)=abb537(6)
      acc537(4)=abb537(7)
      acc537(5)=abb537(8)
      acc537(6)=abb537(9)
      acc537(7)=abb537(10)
      acc537(8)=abb537(11)
      acc537(9)=abb537(12)
      acc537(10)=abb537(13)
      acc537(11)=abb537(14)
      acc537(12)=abb537(15)
      acc537(13)=abb537(16)
      acc537(14)=abb537(17)
      acc537(15)=abb537(18)
      acc537(16)=abb537(19)
      acc537(17)=abb537(20)
      acc537(18)=abb537(21)
      acc537(19)=abb537(22)
      acc537(20)=abb537(23)
      acc537(21)=abb537(24)
      acc537(22)=abb537(25)
      acc537(23)=abb537(26)
      acc537(24)=abb537(27)
      acc537(25)=abb537(28)
      acc537(26)=abb537(29)
      acc537(27)=-acc537(4)*Qspvak6k1
      acc537(28)=Qspvak2k6*acc537(12)
      acc537(29)=QspQ*acc537(24)
      acc537(30)=Qspvak2e6*acc537(25)
      acc537(31)=Qspk2*acc537(22)
      acc537(27)=acc537(31)+acc537(30)+acc537(29)+acc537(28)+acc537(20)+acc537(&
      &27)
      acc537(27)=Qspvak5k4*acc537(27)
      acc537(28)=-Qspvak5k4*acc537(11)
      acc537(28)=acc537(28)+acc537(10)
      acc537(28)=Qspvak2k1*acc537(28)
      acc537(29)=Qspk2+Qspk1
      acc537(29)=acc537(2)*acc537(29)
      acc537(30)=Qspvak2k6*acc537(16)
      acc537(31)=Qspvak2k4*acc537(19)
      acc537(32)=Qspvak5k1*acc537(26)
      acc537(28)=acc537(32)+acc537(31)+acc537(30)+acc537(9)+acc537(29)+acc537(2&
      &8)
      acc537(28)=Qspe6*acc537(28)
      acc537(29)=Qspvak2k4*acc537(21)
      acc537(30)=Qspvak5k1*acc537(13)
      acc537(29)=acc537(29)-acc537(30)
      acc537(30)=acc537(14)+acc537(29)
      acc537(30)=Qspk6*acc537(30)
      acc537(31)=Qspvak2k4*acc537(23)
      acc537(32)=Qspvak5k1*acc537(3)
      acc537(31)=acc537(32)+acc537(17)+acc537(31)
      acc537(31)=QspQ*acc537(31)
      acc537(32)=-Qspk6*acc537(8)
      acc537(33)=QspQ*acc537(18)
      acc537(32)=acc537(33)+acc537(7)+acc537(32)
      acc537(32)=Qspvak2e6*acc537(32)
      acc537(33)=Qspvak2e6*acc537(8)
      acc537(29)=acc537(33)+acc537(15)-acc537(29)
      acc537(29)=Qspk2*acc537(29)
      acc537(33)=Qspk1*acc537(1)
      acc537(34)=Qspvak2k6*acc537(5)
      brack=acc537(6)+acc537(27)+acc537(28)+acc537(29)+acc537(30)+acc537(31)+ac&
      &c537(32)+acc537(33)+acc537(34)
   end  function brack_1
!---#] function brack_1:
!---#[ numerator interfaces:
   !------#[ function numerator_samurai:
   function numerator_samurai(ncut,Q_ext, mu2_ext) result(numerator)
      use precision, only: ki_sam => ki
!      use p12_sbars_hepemg_groups, only: &
!           & sign => diagram537_sign, shift => diagram537_shift
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd537h1
      implicit none
      integer, intent(in) :: ncut
      complex(ki_sam), dimension(4), intent(in) :: Q_ext
      complex(ki_sam), intent(in) :: mu2_ext
      complex(ki_sam) :: numerator
      complex(ki) :: d537
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(0:3) :: qshift
      qshift = -k2
      Q(1)  =cmplx(real(-Q_ext(4)  -qshift(0),  ki_sam),aimag(-Q_ext(4)),  ki)
      Q(2:4)=cmplx(real(-Q_ext(1:3)-qshift(1:3),ki_sam),aimag(-Q_ext(1:3)),ki)
      d537 = 0.0_ki
      d537 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d537, ki), aimag(d537), ki_sam)
   end function numerator_samurai
   !------#] function numerator_samurai:
   !------#[ function numerator_golem95:
   function numerator_golem95(Q_ext, mu2_ext) result(numerator)
      use precision_golem, only: ki_gol => ki
      use p12_sbars_hepemg_globalsl1, only: epspow
      use p12_sbars_hepemg_kinematics
      use p12_sbars_hepemg_abbrevd537h1
      implicit none
      real(ki_gol), dimension(0:3), intent(in) :: Q_ext
      real(ki_gol), intent(in) :: mu2_ext
      complex(ki_gol) :: numerator
      complex(ki) :: d537
      ! The Q that goes into the diagram
      complex(ki), dimension(4) :: Q
      complex(ki) :: mu2
      real(ki), dimension(4) :: qshift
      qshift = -k2
      Q(:)  =cmplx(real(-Q_ext(:)  -qshift(:),  ki_gol), 0.0_ki_gol, ki)
      d537 = 0.0_ki
      d537 = (cond(epspow.eq.0,brack_1,Q,mu2))
      numerator = cmplx(real(d537, ki), aimag(d537), ki_gol)
   end function numerator_golem95
   !------#] function numerator_golem95:
!---#] numerator interfaces:
end module p12_sbars_hepemg_d537h1l1
