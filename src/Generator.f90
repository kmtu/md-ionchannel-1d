MODULE Generator
  USE MDParameters
  IMPLICIT NONE
  
CONTAINS
  SUBROUTINE generate_particle(time)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: time
    REAL, SAVE :: next_poisson_time !next event time of a poisson process
    INTEGER :: i
    REAL :: rand
    LOGICAL, SAVE :: waiting = .FALSE.

    IF (.NOT. waiting) THEN     
       !time interval between two poisson events has exp decay distribution
       CALL RANDOM_NUMBER(rand)
       next_poisson_time = time - LOG(rand) / poisson_rate + poisson_lbound
       waiting = .TRUE.
       WRITE(CONFIG_OUT_FILEID,*) "# Next poisson time = ", next_poisson_time
       WRITE(CONFIG_OUT_FILEID,*) "# Waiting time = ", next_poisson_time - time       
    END IF
    
    IF (time > next_poisson_time) THEN
       IF (isPositionClear()) THEN
          DO i = 1, NUM_ATOMS
             IF (.NOT. atoms(i)%alive) THEN
                waiting = .FALSE.          
                atoms(i)%alive = .TRUE.
                atoms(i)%mass = 39.0983
                atoms(i)%charge = 1.
                atoms(i)%position = generate_pos
                CALL RANDOM_NUMBER(rand)
                atoms(i)%velocity = (rand + 0.1)*10
                atoms(i)%prev_position = atoms(i)%position - atoms(i)%velocity * delta_timestep
                atoms(i)%chargeCounted = .FALSE.
                WRITE(CONFIG_OUT_FILEID, *) time, atoms(i)%position, atoms(i)%velocity
                EXIT
             END IF
          END DO
       END IF
    END IF
    WRITE(CONFIG_OUT_FILEID,*)      !blank line for dividing records with different times
  END SUBROUTINE generate_particle

  FUNCTION isPositionClear()
    IMPLICIT NONE
    LOGICAL :: isPositionClear
    INTEGER :: i
    isPositionClear = .TRUE.
    DO i = 1, NUM_ATOMS
       IF (atoms(i)%alive) THEN
          IF (ABS(atoms(i)%position - generate_pos) < generate_cut) THEN
             isPositionClear = .FALSE.
             RETURN
          END IF
       END IF
    END DO
    RETURN
  END FUNCTION isPositionClear
END MODULE Generator
