module     p4_ubaru_hepemg_abbrevd45h0
   use p4_ubaru_hepemg_config, only: ki
   use p4_ubaru_hepemg_globalsh0
   implicit none
   private
   complex(ki), dimension(81), public :: abb45
   complex(ki), public :: R2d45
   public :: init_abbrev
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
contains
   subroutine     init_abbrev()
      use p4_ubaru_hepemg_config, only: deltaOS, &
     &    logfile, debug_nlo_diagrams
      use p4_ubaru_hepemg_kinematics
      use p4_ubaru_hepemg_model
      use p4_ubaru_hepemg_color, only: TR
      use p4_ubaru_hepemg_globalsl1, only: epspow
      implicit none
      abb45(1)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb45(2)=sqrt(mT**2)
      abb45(3)=es12**(-1)
      abb45(4)=spak2l3**(-1)
      abb45(5)=spbl3k2**(-1)
      abb45(6)=abb45(3)*abb45(2)**3
      abb45(7)=i_*TR*c1*gHT*gel*abb45(1)
      abb45(8)=abb45(7)*gTl
      abb45(9)=abb45(6)*abb45(8)
      abb45(10)=abb45(7)*gTr
      abb45(11)=abb45(6)*abb45(10)
      abb45(12)=abb45(9)-abb45(11)
      abb45(13)=spak5e6*spbe6k2
      abb45(14)=abb45(12)*abb45(13)
      abb45(15)=abb45(2)*abb45(7)*abb45(3)
      abb45(16)=abb45(15)*gTr
      abb45(15)=abb45(15)*gTl
      abb45(17)=abb45(16)-abb45(15)
      abb45(18)=abb45(17)*spak1e6
      abb45(19)=abb45(18)*spbk2k1
      abb45(20)=spak5k6*spbk6e6
      abb45(21)=abb45(20)*abb45(19)
      abb45(22)=spak1k5*spak1e6
      abb45(23)=abb45(22)*spbk2k1
      abb45(24)=abb45(23)*spbe6k1
      abb45(25)=-abb45(17)*abb45(24)
      abb45(26)=abb45(17)*spbe6k2
      abb45(27)=abb45(26)*spak2k5
      abb45(28)=spbk2k1*spak1e6
      abb45(29)=-abb45(27)*abb45(28)
      abb45(14)=abb45(29)+abb45(25)+abb45(14)+abb45(21)
      abb45(21)=spak1l3*spbk4l3
      abb45(14)=abb45(14)*abb45(21)
      abb45(25)=spak1k5*spbe6k2
      abb45(29)=spbk6k4*spae6k6
      abb45(30)=abb45(25)*abb45(29)
      abb45(31)=spak1k2*spbk4k2
      abb45(32)=abb45(31)*abb45(13)
      abb45(33)=mH**2*abb45(5)*abb45(4)
      abb45(34)=-1.0_ki+abb45(33)
      abb45(34)=abb45(32)*abb45(34)
      abb45(35)=abb45(23)*spbe6k4
      abb45(34)=-abb45(30)-abb45(35)+abb45(34)
      abb45(34)=abb45(12)*abb45(34)
      abb45(36)=spbe6k4*spak1e6
      abb45(37)=-abb45(12)*abb45(36)
      abb45(38)=spak1k2*spbe6k2
      abb45(39)=abb45(17)*abb45(38)*abb45(29)
      abb45(37)=abb45(37)+abb45(39)
      abb45(39)=spbl3k2*spal3k5
      abb45(37)=abb45(37)*abb45(39)
      abb45(8)=abb45(8)-abb45(10)
      abb45(8)=abb45(33)*abb45(2)*abb45(8)
      abb45(10)=abb45(8)-abb45(12)
      abb45(40)=spbk4k2*spak1e6
      abb45(41)=abb45(20)*abb45(40)
      abb45(42)=spbe6k1*abb45(40)*spak1k5
      abb45(41)=abb45(41)-abb45(42)
      abb45(42)=abb45(40)*spbe6k2
      abb45(43)=spak2k5*abb45(42)
      abb45(43)=abb45(43)-abb45(41)
      abb45(10)=abb45(10)*abb45(43)
      abb45(43)=spbe6k2*abb45(12)*abb45(22)
      abb45(44)=abb45(17)*spbl3k2
      abb45(45)=abb45(44)*spal3k5
      abb45(46)=-spak1e6*abb45(38)*abb45(45)
      abb45(43)=abb45(43)+abb45(46)
      abb45(43)=spbk4k1*abb45(43)
      abb45(10)=abb45(43)+abb45(37)+abb45(14)+abb45(34)+abb45(10)
      abb45(10)=2.0_ki*abb45(10)
      abb45(6)=abb45(2)-abb45(6)
      abb45(14)=-gTr-gTl
      abb45(6)=abb45(14)*abb45(7)*abb45(6)
      abb45(6)=2.0_ki*abb45(6)+abb45(8)
      abb45(7)=spak1k5*spbk4k2
      abb45(6)=abb45(7)*abb45(6)
      abb45(8)=abb45(17)*spak1k5
      abb45(14)=-spbk2k1*abb45(8)*abb45(21)
      abb45(21)=-abb45(31)*abb45(45)
      abb45(6)=abb45(21)+abb45(14)+abb45(6)
      abb45(14)=abb45(16)+abb45(15)
      abb45(21)=2.0_ki*abb45(14)
      abb45(33)=abb45(33)*abb45(17)
      abb45(34)=abb45(21)+abb45(33)
      abb45(37)=abb45(31)*abb45(34)
      abb45(43)=abb45(17)*spak1l3
      abb45(46)=abb45(43)*spbk4l3
      abb45(37)=abb45(37)+abb45(46)
      abb45(37)=spbk6k2*spak5k6*abb45(37)
      abb45(47)=spbk2k1*spak1k5
      abb45(48)=-abb45(21)*abb45(47)
      abb45(48)=abb45(48)-abb45(45)
      abb45(48)=spak1k6*spbk6k4*abb45(48)
      abb45(6)=abb45(48)+2.0_ki*abb45(6)+abb45(37)
      abb45(6)=2.0_ki*abb45(6)
      abb45(37)=abb45(33)*abb45(32)
      abb45(48)=abb45(45)*abb45(36)
      abb45(49)=abb45(13)*spbk4l3
      abb45(43)=abb45(49)*abb45(43)
      abb45(43)=-abb45(37)+abb45(48)-abb45(43)
      abb45(48)=abb45(35)-abb45(32)
      abb45(48)=abb45(21)*abb45(48)
      abb45(48)=abb45(48)+abb45(43)
      abb45(48)=2.0_ki*abb45(48)
      abb45(50)=abb45(8)*spbe6k1
      abb45(51)=abb45(20)*abb45(17)
      abb45(27)=abb45(27)+abb45(50)-abb45(51)
      abb45(50)=abb45(27)*abb45(40)
      abb45(51)=abb45(32)+abb45(35)
      abb45(51)=abb45(17)*abb45(51)
      abb45(52)=abb45(22)*spbk4k1
      abb45(53)=abb45(29)*spak1k5
      abb45(54)=abb45(53)-abb45(52)
      abb45(54)=abb45(26)*abb45(54)
      abb45(50)=abb45(50)+abb45(54)+abb45(51)
      abb45(51)=2.0_ki*abb45(50)
      abb45(34)=-abb45(34)*abb45(41)
      abb45(41)=2.0_ki*abb45(17)
      abb45(54)=spak1l3*abb45(49)*abb45(41)
      abb45(55)=abb45(41)*abb45(36)
      abb45(39)=abb45(39)*abb45(55)
      abb45(39)=abb45(54)-abb45(39)
      abb45(54)=abb45(32)*abb45(21)
      abb45(56)=abb45(21)-abb45(33)
      abb45(35)=-abb45(56)*abb45(35)
      abb45(57)=3.0_ki*abb45(16)
      abb45(58)=abb45(57)+abb45(15)
      abb45(59)=abb45(25)*abb45(58)
      abb45(60)=abb45(33)*abb45(25)
      abb45(60)=-abb45(59)+abb45(60)
      abb45(60)=abb45(60)*abb45(29)
      abb45(42)=abb45(21)*abb45(42)
      abb45(61)=abb45(33)*spbe6k2
      abb45(62)=abb45(40)*abb45(61)
      abb45(42)=abb45(42)+abb45(62)
      abb45(42)=spak2k5*abb45(42)
      abb45(62)=abb45(58)*spbe6k2
      abb45(63)=abb45(62)-abb45(61)
      abb45(63)=abb45(63)*abb45(52)
      abb45(34)=abb45(63)+abb45(42)+abb45(60)+abb45(35)+abb45(54)+3.0_ki*abb45(&
      &37)+abb45(39)+abb45(34)
      abb45(34)=2.0_ki*abb45(34)
      abb45(35)=abb45(8)*spbk4k2
      abb45(35)=4.0_ki*abb45(35)
      abb45(32)=abb45(14)*abb45(32)
      abb45(42)=abb45(14)*spbe6k4
      abb45(54)=-abb45(23)*abb45(42)
      abb45(32)=abb45(54)+abb45(32)+abb45(37)
      abb45(37)=spbe6k2*abb45(52)
      abb45(30)=abb45(37)-abb45(30)
      abb45(30)=abb45(21)*abb45(30)
      abb45(37)=spak2k5*spbe6k2
      abb45(52)=spbe6k1*spak1k5
      abb45(52)=abb45(37)+abb45(52)-abb45(20)
      abb45(54)=3.0_ki*abb45(15)
      abb45(60)=abb45(54)+abb45(16)
      abb45(63)=abb45(60)*abb45(40)
      abb45(52)=abb45(63)*abb45(52)
      abb45(30)=2.0_ki*abb45(32)+abb45(52)+abb45(30)+abb45(39)
      abb45(30)=2.0_ki*abb45(30)
      abb45(32)=-abb45(43)+abb45(50)
      abb45(32)=2.0_ki*abb45(32)
      abb45(39)=-8.0_ki*abb45(14)*abb45(7)
      abb45(43)=-abb45(11)-3.0_ki*abb45(9)
      abb45(40)=abb45(43)*abb45(40)
      abb45(43)=2.0_ki*abb45(63)
      abb45(50)=-3.0_ki*abb45(11)-abb45(9)
      abb45(25)=abb45(50)*abb45(25)
      abb45(50)=2.0_ki*abb45(59)
      abb45(52)=spbe6k4*spak1k5*abb45(12)
      abb45(64)=abb45(8)*spbe6k4
      abb45(65)=2.0_ki*abb45(64)
      abb45(66)=spak5e6*spbk4k2
      abb45(12)=-abb45(12)*abb45(66)
      abb45(67)=abb45(41)*abb45(66)
      abb45(66)=-abb45(17)*abb45(66)
      abb45(68)=abb45(58)*spbk6e6
      abb45(69)=abb45(23)*abb45(68)
      abb45(8)=2.0_ki*spbk6k4*abb45(8)
      abb45(70)=abb45(60)*spae6k6
      abb45(71)=-spbk4k2*abb45(38)*abb45(70)
      abb45(72)=spbl3k1*spak1e6
      abb45(73)=-spbk6l3*spae6k6
      abb45(72)=abb45(73)+abb45(72)
      abb45(72)=abb45(26)*abb45(72)
      abb45(73)=-spbe6k1*spak1e6*abb45(44)
      abb45(74)=-spbe6l3*abb45(19)
      abb45(72)=abb45(74)+abb45(73)+abb45(72)
      abb45(72)=spak1l3*abb45(72)
      abb45(73)=-spal3e6*abb45(38)
      abb45(74)=spal3k6*spbk6e6*spak1e6
      abb45(73)=abb45(74)+abb45(73)
      abb45(44)=abb45(44)*abb45(73)
      abb45(73)=-abb45(17)-abb45(33)
      abb45(73)=spbk6k2*spae6k6*abb45(38)*abb45(73)
      abb45(19)=-abb45(19)*spak1k6*spbk6e6
      abb45(74)=-spak2l3*spbl3k2*spak1e6*abb45(26)
      abb45(19)=abb45(74)+abb45(19)+abb45(73)+abb45(72)+abb45(44)
      abb45(44)=abb45(17)*spak1k6
      abb45(72)=-spbk6e6*abb45(44)
      abb45(73)=abb45(17)*spbk6k2
      abb45(74)=spae6k6*abb45(73)
      abb45(18)=-spbk6e6*abb45(18)
      abb45(75)=spbk4k1*spak1e6
      abb45(75)=abb45(75)-abb45(29)
      abb45(76)=-abb45(38)*abb45(15)*abb45(75)
      abb45(77)=abb45(9)*abb45(36)
      abb45(76)=abb45(77)+abb45(76)
      abb45(76)=4.0_ki*abb45(76)
      abb45(77)=-abb45(16)-7.0_ki*abb45(15)
      abb45(77)=abb45(77)*abb45(31)
      abb45(44)=spbk6k4*abb45(44)
      abb45(44)=abb45(77)+abb45(44)
      abb45(44)=2.0_ki*abb45(44)
      abb45(77)=abb45(36)*abb45(15)
      abb45(78)=8.0_ki*abb45(77)
      abb45(77)=-4.0_ki*abb45(77)
      abb45(38)=-abb45(38)*abb45(63)
      abb45(23)=abb45(23)*abb45(62)
      abb45(79)=-spak5k6*spbk4k2*abb45(41)
      abb45(26)=spae6k6*abb45(26)
      abb45(80)=abb45(13)*abb45(11)
      abb45(28)=abb45(28)*abb45(16)
      abb45(81)=-abb45(20)*abb45(28)
      abb45(28)=abb45(28)*abb45(37)
      abb45(28)=abb45(28)+abb45(80)+abb45(81)
      abb45(37)=abb45(15)+7.0_ki*abb45(16)
      abb45(24)=abb45(37)*abb45(24)
      abb45(24)=abb45(24)+4.0_ki*abb45(28)
      abb45(28)=abb45(37)*abb45(47)
      abb45(37)=-spak5k6*abb45(73)
      abb45(28)=abb45(28)+abb45(37)
      abb45(28)=2.0_ki*abb45(28)
      abb45(37)=abb45(13)*abb45(41)
      abb45(13)=abb45(13)*abb45(16)
      abb45(41)=8.0_ki*abb45(13)
      abb45(13)=-4.0_ki*abb45(13)
      abb45(47)=abb45(20)*spbk4k2*abb45(56)
      abb45(14)=4.0_ki*abb45(14)
      abb45(56)=-abb45(14)+abb45(33)
      abb45(7)=spbe6k1*abb45(7)*abb45(56)
      abb45(45)=spbe6k4*abb45(45)
      abb45(7)=abb45(45)+abb45(47)+abb45(7)
      abb45(7)=spak2e6*abb45(7)
      abb45(27)=spal3e6*spbk4l3*abb45(27)
      abb45(45)=-spak2e6*spbk4k2
      abb45(45)=abb45(45)-abb45(75)
      abb45(45)=spbe6l3*abb45(45)
      abb45(47)=spbl3k1*abb45(36)
      abb45(45)=abb45(47)+abb45(45)
      abb45(45)=abb45(45)*abb45(17)*spal3k5
      abb45(31)=-abb45(31)*abb45(33)
      abb45(31)=-abb45(46)+abb45(31)
      abb45(31)=spak5e6*abb45(31)
      abb45(21)=abb45(21)*abb45(53)
      abb45(21)=abb45(21)+abb45(31)
      abb45(21)=spbe6k1*abb45(21)
      abb45(9)=-abb45(11)-abb45(9)
      abb45(9)=spbe6k4*spak5e6*abb45(9)
      abb45(11)=abb45(62)+abb45(61)
      abb45(11)=abb45(11)*abb45(29)
      abb45(31)=spbk2k1*abb45(36)*abb45(33)
      abb45(11)=abb45(31)+abb45(11)
      abb45(11)=spak2k5*abb45(11)
      abb45(31)=-spbe6k2*abb45(14)
      abb45(31)=abb45(31)-abb45(61)
      abb45(31)=spak2k5*abb45(31)
      abb45(33)=abb45(60)*abb45(20)
      abb45(31)=abb45(31)+abb45(33)
      abb45(31)=spak1e6*abb45(31)
      abb45(15)=abb45(57)+5.0_ki*abb45(15)
      abb45(22)=-spbe6k1*abb45(15)*abb45(22)
      abb45(22)=abb45(22)+abb45(31)
      abb45(22)=spbk4k1*abb45(22)
      abb45(17)=-spak2l3*abb45(17)*abb45(49)
      abb45(7)=abb45(17)+abb45(27)+abb45(7)+abb45(22)+abb45(11)+4.0_ki*abb45(9)&
      &+abb45(21)+abb45(45)
      abb45(9)=spbk6k4*spak5k6*abb45(14)
      abb45(11)=-5.0_ki*abb45(16)-abb45(54)
      abb45(11)=spak2k5*spbk4k2*abb45(11)
      abb45(14)=-spbk4k1*spak1k5*abb45(15)
      abb45(9)=abb45(14)+abb45(9)+abb45(11)
      abb45(9)=2.0_ki*abb45(9)
      abb45(11)=abb45(42)*spak5e6
      abb45(14)=8.0_ki*abb45(11)
      abb45(11)=4.0_ki*abb45(11)
      abb45(15)=-abb45(60)*abb45(29)
      abb45(16)=-abb45(58)*abb45(20)
      abb45(17)=-spak5e6*abb45(68)
      abb45(20)=-spbe6k4*abb45(70)
      R2d45=0.0_ki
      rat2 = rat2 + R2d45
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='45' value='", &
          & R2d45, "'/>"
      end if
   end subroutine
end module p4_ubaru_hepemg_abbrevd45h0
