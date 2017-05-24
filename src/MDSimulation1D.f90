PROGRAM MDSimulation1D
  USE MDParameters
  USE Initializer
  USE Interactor
  USE Integrator
  USE Generator
  USE Sampler
  IMPLICIT NONE
  INTEGER :: t

  CALL initialize()
  DO t = 1, timesteps, delta_timestep
     CALL interact()
     CALL integrate()
     CALL sample_current(t)
     CALL apply_boundary_condition()
     CALL sample_position(t)
     CALL generate_particle(t)     
  END DO
  CALL output_statistics()
  CALL finalize()
  
CONTAINS
  SUBROUTINE apply_boundary_condition()
    IMPLICIT NONE
    INTEGER :: i
!    DO i = 1, NUM_ATOMS
!       IF (atoms(i)%alive .AND. (atoms(i)%position < 0 .OR. atoms(i)%position > sim_size)) THEN
!          atoms(i)%alive = .FALSE.
!       END IF
!    END DO
    FORALL (i=1:NUM_ATOMS, (atoms(i)%alive .AND. (atoms(i)%position < 0 .OR. atoms(i)%position > sim_size)))
       atoms(i)%alive = .FALSE.
    END FORALL
  END SUBROUTINE apply_boundary_condition
  
  SUBROUTINE finalize()
    USE MDParameters
    IMPLICIT NONE
    CLOSE(CONFIG_OUT_FILEID)
    CALL make_gnuplot()
    CLOSE(GP_FILEID)
    CLOSE(STATISTIC_OUT_FILEID)
  END SUBROUTINE finalize

  SUBROUTINE make_gnuplot()
    USE MDParameters
    IMPLICIT NONE
    INTEGER :: i
    CHARACTER(LEN=*), PARAMETER :: fmt = "(A,I1,A,5(F10.5,A))"
    
    OPEN(UNIT=GP_FILEID, FILE=GP_FILENAME)
    WRITE(GP_FILEID, *) "set terminal gif animate"
    WRITE(GP_FILEID, *) "set output ", "'", GP_GIF_FILENAME, "'"
    WRITE(GP_FILEID, *) "set key off"
    WRITE(GP_FILEID, *) "set style line 1 linewidth 3 pointsize 2"
    WRITE(GP_FILEID, *) "set style line 2 linewidth 3 pointsize 2"
    WRITE(GP_FILEID, *) "set style line 3 linewidth 2 pointsize 2"
    WRITE(GP_FILEID, *) "set style increment user"
    WRITE(GP_FILEID, *) "COULOMB_FORCE_CONSTANT = 33781.696862745280019"
    WRITE(GP_FILEID, *) "count=0"
    WRITE(GP_FILEID, *) "lx =",gp_lower_x, "; ux =", gp_upper_x
    WRITE(GP_FILEID, *) "ly =",gp_lower_y, "; uy =", gp_upper_y
    
    DO i = 1, NUM_GAUSSIAN_SITES
       WRITE(GP_FILEID, fmt) "gaussian",i,"(x) = ",gaussian_sites(i)%height,&
            &"*exp( -(x-",gaussian_sites(i)%average,")*(x-",gaussian_sites(i)%average,&
            &") / (2*",gaussian_sites(i)%width,"*",gaussian_sites(i)%width,&
            &") )"
    END DO

    IF (NUM_GAUSSIAN_SITES > 0) THEN
       WRITE(GP_FILEID, "(A)", ADVANCE='NO') "gaussianTot(x) = "
       DO i = 1, NUM_GAUSSIAN_SITES
          IF (i == NUM_GAUSSIAN_SITES) THEN
             WRITE(GP_FILEID, "(A,I1,A)") "gaussian",i,"(x)"
          ELSE
             WRITE(GP_FILEID, "(A,I1,A)", ADVANCE='NO') "gaussian",i,"(x) + "
          END IF
       END DO
    END IF

    WRITE(GP_FILEID, "(A)", ADVANCE='NO') "totPotential(x) = 0"
    IF (NUM_GAUSSIAN_SITES > 0) THEN
       WRITE(GP_FILEID, *) "+ gaussianTot(x)"
    END IF

    WRITE(GP_FILEID, *) "coulomb(x, x0) = COULOMB_FORCE_CONSTANT / ((x-x0)*(x-x0))"
    WRITE(GP_FILEID, *) "call ", "'loop.gp' ", "200"    
  END SUBROUTINE make_gnuplot
END PROGRAM MDSimulation1D
