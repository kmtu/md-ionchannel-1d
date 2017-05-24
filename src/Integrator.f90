MODULE Integrator
  USE MDParameters
  IMPLICIT NONE
CONTAINS
  SUBROUTINE integrate()
    IMPLICIT NONE
    REAL :: nextpos
    REAL :: acc                 !acceleration
    INTEGER :: i

    DO i = 1, NUM_ATOMS
       acc = atoms(i)%force / atoms(i)%mass
       nextpos = 2*atoms(i)%position - atoms(i)%prev_position + acc*delta_timestep*delta_timestep
       atoms(i)%velocity = (nextpos - atoms(i)%prev_position) / (2*delta_timestep)
       atoms(i)%prev_position = atoms(i)%position
       atoms(i)%position = nextpos
    END DO
  END SUBROUTINE integrate  
END MODULE Integrator
