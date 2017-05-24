!Units:
! mass - atomic mass unit (u = 1.6605402E-27 kg)
! charge - unit of proton charge (1.60217733E-19 C)
! time - picoseconds (10^-12 seconds)
! length - Angstroms (10^-10 meters)
! force - mass*length*time^-2 = 1.6605402E-13 N
! electric current - 1.60217733E-7 Ampere
! velocity - L*T^-1 = 1E-10 * 1E12 = 100 meters/second = 360 km/hr
! acceleration - L*T^-2 = 1E-10 * 1E24 = 1E14 meters/second^2
! energy - M*L^2*T^-2 = 1.6605402E-23 Joules
! electric potential - energy/charge = 1.03642722E-4 Volts(J/C)
MODULE MDParameters
  IMPLICIT NONE
  TYPE Particle
     REAL :: mass
     REAL :: charge
     REAL :: position
     REAL :: velocity
     REAL :: force
     REAL :: prev_position
     LOGICAL :: alive
     LOGICAL :: chargeCounted
  END TYPE Particle
  TYPE Gaussian
     REAL :: height, width, average !Gaussian(x) = height*exp( -(x-average)^2 / (2 * width^2) )
  END TYPE Gaussian

  TYPE BiasVoltage
     REAL :: startPos, endPos, voltage
  END TYPE BiasVoltage

  ! Conversion factors between SI units and custom MD units
  REAL, PARAMETER :: COULOMB_PER_CHARGE = 1.60217733E-19
  REAL, PARAMETER :: AMPERE_PER_CURRENT = 1.60217733E-7
  REAL, PARAMETER :: VOLTS_PER_VOLTAGE = 1.03642722E-4
  
  REAL, PARAMETER :: ELECTRIC_CONSTANT = 228.56386723044220146350
  REAL, PARAMETER :: COULOMB_FORCE_CONSTANT = 33781.6968627452800192811907 !(4*pi*Elec_Const)^-1

  INTEGER, PARAMETER :: INPUT_FILEID = 10
  CHARACTER(LEN=*), PARAMETER :: INPUT_FILENAME = "input"
  
  INTEGER, PARAMETER :: CONFIG_OUT_FILEID = 11
  CHARACTER(LEN=*), PARAMETER :: CONFIG_OUT_FILENAME = "config.out"

  CHARACTER(LEN=*), PARAMETER :: FORMAT_INT = "I10"
  CHARACTER(LEN=*), PARAMETER :: FORMAT_REAL = "F10.3"
  
  INTEGER, PARAMETER :: GP_FILEID = 12
  CHARACTER(LEN=*), PARAMETER :: GP_FILENAME = "mdsim.gp"
  CHARACTER(LEN=*), PARAMETER :: GP_GIF_FILENAME = "config.gif"

  INTEGER, PARAMETER :: STATISTIC_OUT_FILEID = 13
  CHARACTER(LEN=*), PARAMETER :: STATISTIC_FILENAME = "statistic.out"


  TYPE(Particle), ALLOCATABLE, SAVE :: atoms(:)
  TYPE(Gaussian), ALLOCATABLE, SAVE :: gaussian_sites(:) !Represent binding site potentials
  TYPE(BiasVoltage), SAVE :: bias_voltage
  
  INTEGER, SAVE :: timesteps
  INTEGER, SAVE :: delta_timestep
  INTEGER, SAVE:: num_atoms
  REAL, SAVE :: sim_size  
  REAL, SAVE :: poisson_rate
  INTEGER, SAVE :: poisson_lbound
  INTEGER, SAVE :: num_gaussian_sites

  REAL, SAVE :: generate_pos
  REAL, SAVE :: generate_cut    !minimum distance of two successive generating atoms

  REAL, SAVE :: current_probe_pos

  REAL, SAVE :: gp_lower_x, gp_lower_y, gp_upper_x, gp_upper_y

  REAL, SAVE :: viscosity
END MODULE MDParameters
