module     p8_cbarc_hepemg_abbrevd77h2
   use p8_cbarc_hepemg_config, only: ki
   use p8_cbarc_hepemg_globalsh2
   implicit none
   private
   complex(ki), dimension(90), public :: abb77
   complex(ki), public :: R2d77
   public :: init_abbrev
   complex(ki), parameter :: i_ = (0.0_ki, 1.0_ki)
contains
   subroutine     init_abbrev()
      use p8_cbarc_hepemg_config, only: deltaOS, &
     &    logfile, debug_nlo_diagrams
      use p8_cbarc_hepemg_kinematics
      use p8_cbarc_hepemg_model
      use p8_cbarc_hepemg_color, only: TR
      use p8_cbarc_hepemg_globalsl1, only: epspow
      implicit none
      abb77(1)=1.0_ki/(-mZ**2+es45+i_*mZ*wZ)
      abb77(2)=sqrt(mT**2)
      abb77(3)=es12**(-1)
      abb77(4)=spak2l3**(-1)
      abb77(5)=spbl3k2**(-1)
      abb77(6)=spbe6l3*spak1l3
      abb77(7)=spak4e6*spbk5k2
      abb77(8)=abb77(6)*abb77(7)
      abb77(9)=spal3e6*spbl3k2
      abb77(10)=spbe6k5*spak1k4
      abb77(11)=abb77(9)*abb77(10)
      abb77(12)=abb77(8)-abb77(11)
      abb77(13)=gTl-gTr
      abb77(14)=i_*TR*c1*gHT*ger*abb77(1)
      abb77(15)=abb77(3)*abb77(14)
      abb77(16)=-abb77(13)*abb77(15)*abb77(2)**3
      abb77(12)=abb77(16)*abb77(12)
      abb77(17)=mH**2
      abb77(18)=abb77(17)*abb77(4)
      abb77(19)=abb77(18)*abb77(5)
      abb77(13)=abb77(19)*abb77(13)*abb77(2)*abb77(14)
      abb77(14)=spbe6k2*abb77(13)
      abb77(20)=abb77(16)*spbe6k2
      abb77(21)=abb77(20)-abb77(14)
      abb77(22)=spbk6k5*spae6k6
      abb77(23)=abb77(22)*spak1k4
      abb77(21)=abb77(21)*abb77(23)
      abb77(15)=abb77(15)*abb77(2)
      abb77(24)=gTl*abb77(15)
      abb77(15)=gTr*abb77(15)
      abb77(25)=abb77(24)-abb77(15)
      abb77(26)=abb77(25)*spbk5k2
      abb77(27)=abb77(18)*spak1k2
      abb77(28)=abb77(26)*abb77(27)
      abb77(29)=abb77(25)*spbl3k2
      abb77(30)=abb77(29)*spak1k2
      abb77(31)=-spbk5k2*abb77(30)
      abb77(28)=abb77(28)+abb77(31)
      abb77(28)=spal3e6*abb77(28)
      abb77(31)=abb77(16)*spak1e6
      abb77(32)=abb77(31)*spbk5k2
      abb77(28)=-abb77(32)+abb77(28)
      abb77(33)=spbk6e6*spak4k6
      abb77(28)=abb77(28)*abb77(33)
      abb77(34)=abb77(29)*abb77(6)
      abb77(35)=spbe6k2*abb77(27)
      abb77(36)=-abb77(25)*abb77(35)
      abb77(36)=-abb77(34)+abb77(36)
      abb77(36)=abb77(22)*abb77(36)
      abb77(37)=abb77(31)*spbe6k5
      abb77(38)=spbl3k2*abb77(37)
      abb77(36)=abb77(38)+abb77(36)
      abb77(36)=spal3k4*abb77(36)
      abb77(38)=-spak4e6*abb77(20)
      abb77(39)=abb77(29)*spal3e6
      abb77(40)=abb77(33)*abb77(39)
      abb77(38)=abb77(38)+abb77(40)
      abb77(40)=spbk5l3*spak1l3
      abb77(38)=abb77(38)*abb77(40)
      abb77(31)=abb77(10)*abb77(31)
      abb77(41)=abb77(6)*abb77(25)
      abb77(42)=-abb77(23)*abb77(41)
      abb77(31)=abb77(31)+abb77(42)
      abb77(31)=spbk2k1*abb77(31)
      abb77(42)=abb77(7)*spak1k2
      abb77(43)=abb77(20)*abb77(42)
      abb77(12)=abb77(31)+abb77(38)+abb77(36)+abb77(28)+abb77(43)+abb77(21)+abb&
      &77(12)
      abb77(12)=2.0_ki*abb77(12)
      abb77(21)=spbk5k2*abb77(13)
      abb77(28)=abb77(15)*spbk6k5
      abb77(31)=abb77(28)*spbl3k2
      abb77(36)=spal3k6*abb77(31)
      abb77(21)=2.0_ki*abb77(36)+abb77(21)
      abb77(21)=spak1k4*abb77(21)
      abb77(36)=spbk2k1*spak1k4
      abb77(38)=abb77(25)*abb77(40)*abb77(36)
      abb77(43)=spak1k2*spbk5k2
      abb77(44)=abb77(43)-abb77(40)
      abb77(44)=spbk6k2*abb77(44)
      abb77(45)=2.0_ki*spbk5k2
      abb77(46)=spbk6l3*spak1l3*abb77(45)
      abb77(44)=abb77(46)+2.0_ki*abb77(44)
      abb77(44)=abb77(44)*abb77(24)*spak4k6
      abb77(46)=abb77(29)*spal3k4
      abb77(47)=abb77(46)*abb77(43)
      abb77(28)=-abb77(36)*abb77(28)
      abb77(31)=-spal3k4*abb77(31)
      abb77(28)=abb77(31)+abb77(28)
      abb77(28)=spak1k6*abb77(28)
      abb77(21)=2.0_ki*abb77(28)+abb77(38)+abb77(47)+abb77(21)+abb77(44)
      abb77(21)=4.0_ki*abb77(21)
      abb77(28)=3.0_ki*abb77(15)
      abb77(31)=abb77(28)+abb77(24)
      abb77(11)=abb77(31)*abb77(11)
      abb77(38)=3.0_ki*abb77(24)
      abb77(44)=abb77(38)+abb77(15)
      abb77(8)=abb77(44)*abb77(8)
      abb77(8)=abb77(11)+abb77(8)
      abb77(11)=abb77(10)*spbk2k1
      abb77(47)=spal3k4*spbe6k5
      abb77(48)=spbl3k2*abb77(47)
      abb77(48)=abb77(11)+abb77(48)
      abb77(49)=7.0_ki*abb77(15)
      abb77(50)=abb77(49)+abb77(24)
      abb77(48)=abb77(50)*spak1e6*abb77(48)
      abb77(51)=7.0_ki*abb77(24)
      abb77(52)=abb77(51)+abb77(15)
      abb77(53)=abb77(52)*spbe6k2
      abb77(54)=abb77(25)*spbe6k2
      abb77(55)=abb77(54)*abb77(19)
      abb77(56)=abb77(53)-abb77(55)
      abb77(56)=abb77(56)*spak1k2
      abb77(57)=-abb77(7)*abb77(56)
      abb77(58)=abb77(53)*abb77(40)
      abb77(59)=spak4e6*abb77(58)
      abb77(8)=abb77(59)+abb77(57)+abb77(48)-2.0_ki*abb77(8)
      abb77(48)=abb77(54)*spak1k4
      abb77(57)=abb77(48)*abb77(22)
      abb77(59)=abb77(25)*spak1e6
      abb77(60)=abb77(59)*spbk5k2
      abb77(61)=abb77(60)*abb77(33)
      abb77(57)=abb77(57)-abb77(61)
      abb77(61)=abb77(25)*abb77(7)
      abb77(62)=abb77(61)*abb77(6)
      abb77(63)=abb77(39)*abb77(10)
      abb77(64)=abb77(62)-abb77(63)
      abb77(42)=-abb77(54)*abb77(42)
      abb77(65)=abb77(47)*abb77(29)
      abb77(66)=-spak1e6*abb77(65)
      abb77(67)=abb77(54)*spak4e6
      abb77(68)=abb77(67)*abb77(40)
      abb77(69)=-abb77(59)*abb77(11)
      abb77(42)=abb77(69)+abb77(68)+abb77(66)+abb77(42)-abb77(64)+3.0_ki*abb77(&
      &57)
      abb77(66)=-abb77(53)+3.0_ki*abb77(55)
      abb77(68)=abb77(66)*abb77(23)
      abb77(69)=abb77(25)*abb77(10)
      abb77(70)=abb77(18)*abb77(69)
      abb77(71)=-abb77(10)*abb77(29)
      abb77(70)=abb77(70)+abb77(71)
      abb77(70)=spal3e6*abb77(70)
      abb77(19)=abb77(19)*abb77(25)
      abb77(71)=abb77(19)*abb77(7)
      abb77(71)=abb77(71)-abb77(61)
      abb77(72)=2.0_ki*abb77(6)
      abb77(72)=-abb77(71)*abb77(72)
      abb77(73)=abb77(45)*abb77(15)
      abb77(74)=abb77(19)*spbk5k2
      abb77(73)=abb77(73)+abb77(74)
      abb77(75)=abb77(33)*spak1e6
      abb77(76)=abb77(73)*abb77(75)
      abb77(77)=-abb77(59)*abb77(47)*abb77(18)
      abb77(78)=abb77(55)*spak4e6
      abb77(79)=abb77(40)*abb77(78)
      abb77(11)=abb77(11)*abb77(19)
      abb77(80)=-spak1e6*abb77(11)
      abb77(68)=abb77(80)+2.0_ki*abb77(79)+abb77(77)-4.0_ki*abb77(76)+abb77(72)&
      &+abb77(68)+abb77(70)
      abb77(51)=abb77(51)+9.0_ki*abb77(15)
      abb77(45)=abb77(45)*spak1k4
      abb77(70)=abb77(51)*abb77(45)
      abb77(72)=abb77(55)*spak1k2
      abb77(76)=abb77(72)*abb77(7)
      abb77(23)=abb77(23)*abb77(24)
      abb77(77)=spbe6k2*abb77(23)
      abb77(75)=abb77(75)*abb77(50)
      abb77(79)=-spbk5k2*abb77(75)
      abb77(62)=abb77(79)+abb77(62)-2.0_ki*abb77(63)+abb77(76)-8.0_ki*abb77(77)
      abb77(49)=abb77(49)+9.0_ki*abb77(24)
      abb77(45)=abb77(49)*abb77(45)
      abb77(57)=abb77(76)+abb77(64)+abb77(57)
      abb77(63)=-abb77(54)-abb77(55)
      abb77(63)=abb77(63)*abb77(43)
      abb77(64)=-abb77(6)*abb77(26)
      abb77(76)=1.0_ki/2.0_ki*abb77(54)
      abb77(77)=abb77(40)*abb77(76)
      abb77(63)=abb77(77)+1.0_ki/2.0_ki*abb77(63)+abb77(64)
      abb77(63)=spak4k6*abb77(63)
      abb77(64)=1.0_ki/2.0_ki*abb77(25)
      abb77(77)=-abb77(64)*abb77(36)
      abb77(77)=abb77(77)-1.0_ki/2.0_ki*abb77(46)
      abb77(77)=spak1e6*abb77(77)
      abb77(79)=spak1k4*abb77(39)
      abb77(77)=abb77(79)+abb77(77)
      abb77(77)=spbk6k5*abb77(77)
      abb77(79)=-1.0_ki/2.0_ki*spak1k4*abb77(20)
      abb77(80)=-spak1k4*abb77(55)
      abb77(80)=abb77(48)+abb77(80)
      abb77(32)=1.0_ki/2.0_ki*abb77(32)
      abb77(26)=abb77(74)-abb77(26)
      abb77(81)=spak1e6*abb77(26)
      abb77(82)=abb77(69)*spbk2k1
      abb77(65)=abb77(65)+abb77(82)
      abb77(83)=1.0_ki/2.0_ki*spak1l3
      abb77(65)=abb77(65)*abb77(83)
      abb77(84)=spbl3k2*spak1k2
      abb77(84)=-abb77(27)+abb77(84)
      abb77(84)=abb77(61)*abb77(84)
      abb77(85)=-abb77(29)*abb77(40)*spak4e6
      abb77(84)=abb77(85)+abb77(84)
      abb77(84)=1.0_ki/2.0_ki*abb77(84)
      abb77(13)=abb77(16)+1.0_ki/2.0_ki*abb77(13)
      abb77(13)=abb77(10)*abb77(13)
      abb77(47)=abb77(47)*abb77(64)
      abb77(85)=abb77(27)*abb77(47)
      abb77(13)=abb77(85)+abb77(13)
      abb77(85)=-abb77(10)*abb77(19)
      abb77(85)=abb77(69)+abb77(85)
      abb77(7)=-abb77(16)*abb77(7)
      abb77(16)=spal3k4*spbl3k2
      abb77(16)=abb77(36)+abb77(16)
      abb77(86)=1.0_ki/2.0_ki*abb77(50)
      abb77(87)=spbk6e6*spak1e6
      abb77(16)=abb77(86)*abb77(87)*abb77(16)
      abb77(88)=spbk6e6*spak1k4*abb77(15)*abb77(9)
      abb77(16)=-4.0_ki*abb77(88)+abb77(16)
      abb77(88)=-abb77(53)-abb77(55)
      abb77(43)=abb77(88)*abb77(43)
      abb77(43)=abb77(58)+abb77(43)
      abb77(58)=1.0_ki/2.0_ki*spae6k6
      abb77(43)=abb77(58)*abb77(43)
      abb77(88)=4.0_ki*abb77(24)
      abb77(88)=-spae6k6*spbk5k2*abb77(6)*abb77(88)
      abb77(43)=abb77(88)+abb77(43)
      abb77(88)=abb77(72)+abb77(41)
      abb77(88)=spbk6k2*spae6k6*abb77(88)
      abb77(89)=-spak1k6*spbk6e6*abb77(39)
      abb77(88)=abb77(88)+abb77(89)
      abb77(89)=-spak1e6*abb77(55)
      abb77(55)=abb77(55)-abb77(54)
      abb77(90)=spak1k2*abb77(55)
      abb77(41)=abb77(90)+abb77(41)
      abb77(90)=-spbk2k1*abb77(59)
      abb77(90)=-abb77(39)+abb77(90)
      abb77(87)=abb77(64)*abb77(87)
      abb77(53)=-abb77(22)*abb77(53)
      abb77(39)=-spbe6k5*abb77(39)
      abb77(39)=abb77(53)+abb77(39)
      abb77(39)=abb77(39)*abb77(83)
      abb77(53)=abb77(22)*abb77(56)
      abb77(56)=abb77(25)*spbe6k5
      abb77(27)=-abb77(56)*abb77(27)
      abb77(30)=spbe6k5*abb77(30)
      abb77(27)=abb77(27)+abb77(30)
      abb77(27)=spal3e6*abb77(27)
      abb77(27)=abb77(27)-abb77(37)+abb77(53)
      abb77(30)=abb77(44)*abb77(6)*abb77(22)
      abb77(27)=1.0_ki/2.0_ki*abb77(27)+abb77(30)
      abb77(30)=-spak1k6*spbk6k5
      abb77(30)=abb77(30)-abb77(40)
      abb77(30)=abb77(25)*abb77(30)
      abb77(28)=-abb77(28)-13.0_ki*abb77(24)
      abb77(28)=spbk5k2*abb77(28)
      abb77(28)=abb77(28)-abb77(74)
      abb77(28)=spak1k2*abb77(28)
      abb77(28)=abb77(28)+abb77(30)
      abb77(30)=abb77(59)*spbe6k5
      abb77(19)=abb77(19)*spbe6k5
      abb77(37)=spak1e6*abb77(19)
      abb77(40)=abb77(25)*spak4e6
      abb77(35)=abb77(40)*abb77(35)
      abb77(34)=spak4e6*abb77(34)
      abb77(44)=-spbl3k2*abb77(75)
      abb77(34)=abb77(44)+abb77(35)+abb77(34)
      abb77(34)=1.0_ki/2.0_ki*abb77(34)
      abb77(35)=spbe6l3*spak1k2*abb77(71)
      abb77(44)=abb77(72)*spak4e6
      abb77(53)=-abb77(59)*abb77(33)
      abb77(53)=-abb77(44)+abb77(53)
      abb77(53)=spbk5l3*abb77(53)
      abb77(35)=abb77(35)+abb77(53)
      abb77(35)=1.0_ki/2.0_ki*abb77(35)
      abb77(53)=-spbe6l3*spak1e6*abb77(64)
      abb77(72)=-abb77(54)*abb77(58)
      abb77(14)=abb77(20)+abb77(14)
      abb77(14)=spak4e6*abb77(14)
      abb77(6)=abb77(40)*abb77(6)
      abb77(6)=abb77(6)-abb77(75)
      abb77(20)=spbk2k1*abb77(6)
      abb77(14)=abb77(14)+abb77(20)
      abb77(9)=abb77(31)*abb77(33)*abb77(9)
      abb77(9)=abb77(9)+1.0_ki/2.0_ki*abb77(14)
      abb77(14)=13.0_ki*abb77(15)+abb77(38)
      abb77(14)=abb77(14)*abb77(36)
      abb77(20)=spbk6k2*spak4k6*abb77(25)
      abb77(14)=abb77(20)+abb77(46)+abb77(14)
      abb77(20)=abb77(22)*spal3k4
      abb77(31)=abb77(54)*abb77(20)
      abb77(36)=-spal3e6*abb77(82)
      abb77(31)=abb77(31)+abb77(36)
      abb77(31)=1.0_ki/2.0_ki*abb77(31)
      abb77(36)=spal3e6*abb77(76)
      abb77(38)=abb77(66)*abb77(22)
      abb77(40)=-abb77(18)*abb77(56)
      abb77(29)=spbe6k5*abb77(29)
      abb77(29)=abb77(40)+abb77(29)
      abb77(29)=spal3e6*abb77(29)
      abb77(29)=-abb77(38)+abb77(29)
      abb77(38)=1.0_ki/2.0_ki*spak2k4
      abb77(29)=abb77(29)*abb77(38)
      abb77(40)=2.0_ki*abb77(33)
      abb77(46)=abb77(73)*abb77(40)
      abb77(18)=abb77(18)*abb77(47)
      abb77(11)=1.0_ki/2.0_ki*abb77(11)+abb77(46)+abb77(18)
      abb77(11)=spak2e6*abb77(11)
      abb77(17)=abb77(17)*abb77(5)
      abb77(18)=-spak2l3+abb77(17)
      abb77(18)=abb77(61)*abb77(18)
      abb77(46)=2.0_ki*abb77(25)
      abb77(20)=-abb77(20)*abb77(46)
      abb77(18)=abb77(20)+abb77(18)
      abb77(18)=spbe6l3*abb77(18)
      abb77(17)=-abb77(17)*abb77(67)
      abb77(20)=abb77(25)*spal3e6
      abb77(40)=abb77(20)*abb77(40)
      abb77(17)=abb77(17)+abb77(40)
      abb77(17)=spbk5l3*abb77(17)
      abb77(6)=-abb77(44)-abb77(6)
      abb77(6)=spbk5k1*abb77(6)
      abb77(23)=spbe6k1*abb77(23)
      abb77(40)=spbl3k1*abb77(69)*spal3e6
      abb77(6)=abb77(40)+4.0_ki*abb77(23)+1.0_ki/2.0_ki*abb77(6)+abb77(11)+abb7&
      &7(29)+abb77(17)+abb77(18)
      abb77(11)=abb77(24)+abb77(15)
      abb77(11)=8.0_ki*abb77(11)
      abb77(15)=spak4k6*spbk6k5*abb77(11)
      abb77(17)=-spak2k4*spbk5k2*abb77(51)
      abb77(18)=-spbk5k1*spak1k4*abb77(49)
      abb77(15)=abb77(18)+abb77(15)+abb77(17)
      abb77(11)=-spbe6k5*spak4e6*abb77(11)
      abb77(17)=abb77(64)*spbe6k5
      abb77(18)=-spak4k6*abb77(17)
      abb77(23)=abb77(64)*spak4e6
      abb77(24)=spbk6k5*abb77(23)
      abb77(29)=spal3k4*spbe6l3
      abb77(40)=-spbe6k1*spak1k4
      abb77(29)=abb77(40)+abb77(29)
      abb77(25)=abb77(25)*abb77(29)
      abb77(29)=spak2k4*abb77(55)
      abb77(33)=-abb77(50)*abb77(33)
      abb77(25)=abb77(29)+abb77(33)+abb77(25)
      abb77(25)=1.0_ki/2.0_ki*abb77(25)
      abb77(26)=-spak2e6*abb77(26)
      abb77(22)=-abb77(52)*abb77(22)
      abb77(20)=-spbk5l3*abb77(20)
      abb77(29)=spbk5k1*abb77(59)
      abb77(20)=abb77(29)+abb77(26)+abb77(22)+abb77(20)
      abb77(20)=1.0_ki/2.0_ki*abb77(20)
      abb77(22)=-spbk5l3*abb77(23)
      abb77(26)=-abb77(56)+abb77(19)
      abb77(26)=abb77(26)*abb77(38)
      abb77(29)=-1.0_ki/2.0_ki*abb77(71)
      abb77(10)=-abb77(10)*abb77(64)
      abb77(33)=spbk5k1*abb77(23)
      abb77(38)=-spbk6e6*spak4e6*abb77(86)
      abb77(40)=-spbe6k5*abb77(52)*abb77(58)
      abb77(17)=-spal3e6*abb77(17)
      abb77(19)=-1.0_ki/2.0_ki*spak2e6*abb77(19)
      abb77(23)=spbe6l3*abb77(23)
      abb77(44)=1.0_ki/2.0_ki*abb77(78)
      R2d77=0.0_ki
      rat2 = rat2 + R2d77
      if (debug_nlo_diagrams) then
          write (logfile,*) "<result name='r2' index='77' value='", &
          & R2d77, "'/>"
      end if
   end subroutine
end module p8_cbarc_hepemg_abbrevd77h2