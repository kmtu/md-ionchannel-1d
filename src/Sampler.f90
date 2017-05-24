MODULE Sampler
  USE MDParameters
  IMPLICIT NONE
  REAL, SAVE :: charge_count = 0.
  INTEGER, SAVE :: first_charge_count_time = -1
  INTEGER, SAVE :: last_charge_count_time = -1
CONTAINS
  SUBROUTINE sample_position(time)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: time
    INTEGER :: i        
    DO i = 1, NUM_ATOMS
       IF (atoms(i)%alive) THEN
          WRITE(CONFIG_OUT_FILEID,*) time, atoms(i)%position, atoms(i)%velocity
       ELSE
          WRITE(CONFIG_OUT_FILEID,*) time
       END IF
    END DO
  END SUBROUTINE sample_position

  SUBROUTINE sample_current(time)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: time
    INTEGER :: i
    DO i = 1, NUM_ATOMS
       IF (atoms(i)%alive .AND. (.NOT. atoms(i)%chargeCounted)) THEN
          IF (atoms(i)%position > current_probe_pos) THEN
             IF (first_charge_count_time > 0) THEN !counter has been activated
                charge_count = charge_count + atoms(i)%charge
                atoms(i)%chargeCounted = .TRUE.
                last_charge_count_time = time
             ELSE
                first_charge_count_time = time
             END IF
          END IF
       END IF
    END DO    
  END SUBROUTINE sample_current
    
  SUBROUTINE output_statistics()
    IMPLICIT NONE
    REAL :: average_current
    average_current = charge_count / (last_charge_count_time - first_charge_count_time)
    WRITE(*,*)
    WRITE(*,*) "================= Output ======================"
    WRITE(*,*) "charge count:", charge_count
    WRITE(*,*) "first_charge_count_time:", first_charge_count_time    
    WRITE(*,*) "last_charge_count_time:", last_charge_count_time
    WRITE(*, *) "Average current: ", average_current*AMPERE_PER_CURRENT, "A"    
    WRITE(STATISTIC_OUT_FILEID, *) "Average current: ", average_current*AMPERE_PER_CURRENT, "A"
  END SUBROUTINE output_statistics
END MODULE Sampler
