MODULE ParticleModule
  IMPLICIT NONE
  TYPE Particle
     REAL :: mass
     REAL :: charge
     REAL :: position
     REAL :: velocity
     REAL :: force
     REAL :: prev_position
  END TYPE Particle
END MODULE ParticleModule
