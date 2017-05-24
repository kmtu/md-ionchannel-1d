MODULE Interactor
  USE MDParameters
  IMPLICIT NONE
  PRIVATE
  PUBLIC interact
CONTAINS
  SUBROUTINE interact()
    IMPLICIT NONE
    REAL :: force
    INTEGER :: i, j
    FORALL (i = 1:NUM_ATOMS, atoms(i)%alive)
       atoms(i)%force = bias_force(bias_voltage, atoms(i)) - viscosity*atoms(i)%velocity
    END FORALL

    DO i = 1, NUM_ATOMS
       IF (atoms(i)%alive) THEN
          DO j = 1, 3
             atoms(i)%force = atoms(i)%force + &
                  &gaussian_force(gaussian_sites(j), atoms(i)%position)
          END DO
       END IF
    END DO

    DO i = 1, NUM_ATOMS-1
       IF (atoms(i)%alive) THEN
          DO j = i+1, NUM_ATOMS
             IF (atoms(j)%alive) THEN
                force = coulomb_force(atoms(i), atoms(j), 1)
                atoms(i)%force = atoms(i)%force + force
                atoms(j)%force = atoms(j)%force - force
             END IF
          END DO
       END IF
    END DO
  END SUBROUTINE interact

  PURE FUNCTION gaussian_force(gauss, x)
    IMPLICIT NONE
    TYPE(Gaussian), INTENT(IN) :: gauss
    REAL, INTENT(IN) :: x
    REAL :: gaussian_force
    REAL :: x_a, w2
    x_a = x - gauss%average
    w2 = gauss%width * gauss%width
    gaussian_force = gauss%height * x_a / w2 * EXP(-(x_a * x_a)/(2*w2))
    RETURN
  END FUNCTION gaussian_force

  PURE FUNCTION coulomb_force(atom1, atom2, index)
    IMPLICIT NONE
    TYPE(Particle), INTENT(IN) :: atom1, atom2
    INTEGER, INTENT(IN) :: index !return the force acting on atom(index)
    REAL :: coulomb_force
    REAL :: r2
    INTEGER :: direction
    r2 = atom1%position - atom2%position
!    IF (r2 == 0) THEN
!       WRITE(*,*) "Error: coulomb_force(): two atoms on the same position."
!    END IF
    IF (index == 1) THEN
       IF (r2 > 0) THEN
          direction = 1
       ELSE
          direction = -1
       END IF
    ELSE
       IF (r2 > 0) THEN
          direction = -1
       ELSE
          direction = 1
       END IF
    END IF
    r2 = r2*r2
    coulomb_force = direction * COULOMB_FORCE_CONSTANT * &
         &(atom1%charge * atom2%charge) / r2
    RETURN
  END FUNCTION coulomb_force

  PURE FUNCTION bias_force(bVoltage, atom)
    IMPLICIT NONE
    REAL :: bias_force
    TYPE(BiasVoltage), INTENT(IN) :: bVoltage
    TYPE(Particle), INTENT(IN) :: atom

    IF (atom%position > bVoltage%startPos .AND. atom%position < bVoltage%endPos) THEN
       bias_force = atom%charge * bVoltage%voltage / (bVoltage%endPos - bVoltage%startPos)
    ELSE
       bias_force = 0.
    END IF
  END FUNCTION bias_force
END MODULE Interactor
