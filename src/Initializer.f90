MODULE Initializer
  USE MDParameters
  USE Directives
  IMPLICIT NONE
  PRIVATE !let everything be private by default
  PUBLIC initialize

CONTAINS
  SUBROUTINE initialize()
    IMPLICIT NONE
    INTEGER :: stat, line_num
    CHARACTER(LEN=LINE_WIDTH) :: line
    CHARACTER(LEN=LINE_WIDTH) :: directive
    CHARACTER(LEN=LINE_WIDTH) :: potential_type, gp_dir
    CHARACTER(LEN=3) :: str_len, str2_len
    !string version of the integer "length of a string". used in format
    INTEGER :: i, j, seed(1000)

    CALL setDefaultValue()

    line_num = 0
    OPEN(UNIT=INPUT_FILEID, FILE=INPUT_FILENAME)
    DO WHILE(.TRUE.)
       READ(UNIT=INPUT_FILEID, FMT="(A)", IOSTAT=stat) line
       line_num = line_num + 1       
       IF (stat /= 0) EXIT      !unable to read more lines, exit
       CALL removeComment(line)
       IF (line == '') CYCLE
       CALL getDirective(line, directive)
       WRITE(str_len, "(I3)") LEN_TRIM(directive)

       SELECT CASE (directive)
       CASE (DIR_TIMESTEPS)
          CALL getValueInt(line, timesteps)
          WRITE(*,"(A"//str_len//",':',"//FORMAT_INT//")") directive, timesteps
       CASE (DIR_DELTA_TIMESTEPS)
          CALL getValueInt(line, delta_timestep)
          WRITE(*,"(A"//str_len//",':',"//FORMAT_INT//")") directive, delta_timestep
       CASE (DIR_SIM_SIZE)
          CALL getValueReal(line, sim_size)
          WRITE(*,"(A"//str_len//",':',"//FORMAT_REAL//")") directive, sim_size
       CASE (DIR_POSSION_RATE)
          CALL getValueReal(line, poisson_rate)
          WRITE(*,"(A"//str_len//",':',"//FORMAT_REAL//")") directive, poisson_rate
       CASE (DIR_POSSION_LBOUND)
          CALL getValueInt(line, poisson_lbound)
          WRITE(*,"(A"//str_len//",':',"//FORMAT_INT//")") directive, poisson_lbound
       CASE (DIR_GENERATE_POS)
          CALL getValueReal(line, generate_pos)
          WRITE(*,"(A"//str_len//",':',"//FORMAT_REAL//")") directive, generate_pos
       CASE (DIR_GENERATE_CUT)
          CALL getValueReal(line, generate_cut)
          WRITE(*,"(A"//str_len//",':',"//FORMAT_REAL//")") directive, generate_cut
       CASE (DIR_CURRENT_PROBE_POS)
          CALL getValueReal(line, current_probe_pos)
          WRITE(*,"(A"//str_len//",':',"//FORMAT_REAL//")") directive, current_probe_pos
       CASE (DIR_NUM_ATOMS)
          CALL getValueInt(line, num_atoms)
          WRITE(*,"(A"//str_len//",':',"//FORMAT_INT//")") directive, num_atoms
          ALLOCATE(atoms(num_atoms))
          atoms(:)%alive = .FALSE.
       CASE (DIR_VISCOSITY)
          CALL getValueReal(line, viscosity)
          WRITE(*,"(A"//str_len//",':',"//FORMAT_REAL//")") directive, viscosity
       CASE (DIR_FIXED_POTENTIAL)
          CALL getValueString(line, potential_type)
          CALL downcase(potential_type)
          WRITE(str2_len, "(I3)") LEN_TRIM(potential_type)
          SELECT CASE (potential_type)
          CASE (POTENTIAL_GAUSSIAN)
             WRITE(*,"(A"//str_len//",': ',A"//str2_len//")") directive, potential_type
             READ(UNIT=INPUT_FILEID, FMT=*, IOSTAT=stat) num_gaussian_sites
             line_num = line_num + 1
             IF (stat /= 0) THEN
                WRITE(*,*) "Error: insufficient data in file '", INPUT_FILENAME, "', line ",&
                     &line_num, ":"
                WRITE(*,*) "       Number of potentials must be given first!"
                STOP
             END IF
             WRITE(*,*) num_gaussian_sites
             ALLOCATE(gaussian_sites(num_gaussian_sites))
             DO i = 1, num_gaussian_sites
                READ(UNIT=INPUT_FILEID, FMT=*, IOSTAT=stat) &
                     &gaussian_sites(i)%height, gaussian_sites(i)%width, gaussian_sites(i)%average
                line_num = line_num + 1
                IF (stat /= 0) THEN
                   WRITE(*,*) "Error: insufficient data in file '", INPUT_FILENAME, "', line ",&
                        &line_num, ":"
                   WRITE(*,*) "       There should be ",num_gaussian_sites , "potentials"
                   STOP
                END IF
                WRITE(*,*) gaussian_sites(i)%height, gaussian_sites(i)%width,&
                     &gaussian_sites(i)%average
             END DO
          CASE (POTENTIAL_BIAS_VOLTAGE)
             WRITE(*,"(A"//str_len//",': ',A"//str2_len//")") directive, potential_type
             READ(UNIT=INPUT_FILEID, FMT=*, IOSTAT=stat) bias_voltage%startPos, bias_voltage%endPos, bias_voltage%voltage
             line_num = line_num + 1
             IF (stat /= 0) THEN
                WRITE(*,*) "Error: insufficient data in file '", INPUT_FILENAME, "', line ",&
                     &line_num, ":"
                WRITE(*,*) "       Usage: <startPos> <endPos> <voltage>"
                STOP
             END IF
             WRITE(*,*) bias_voltage%startPos, bias_voltage%endPos, bias_voltage%voltage
             bias_voltage%voltage = bias_voltage%voltage / (1000*VOLTS_PER_VOLTAGE)
          CASE DEFAULT
             WRITE(*,*) "Error: unable to understand the potential type in file '", INPUT_FILENAME,&
                  &"', line ", line_num, ":"
             WRITE(*,*) potential_type
             STOP
          END SELECT
       CASE (DIR_GNUPLOT)
          CALL getValueString(line, gp_dir)
          CALL downcase(gp_dir)
          WRITE(str2_len, "(I3)") LEN_TRIM(gp_dir)
          SELECT CASE (gp_dir)
          CASE (GP_BOUNDINGS)
             WRITE(*,"(A"//str_len//",': ',A"//str2_len//")") directive, gp_dir
             READ(UNIT=INPUT_FILEID, FMT=*, IOSTAT=stat) gp_lower_x, gp_upper_x, gp_lower_y, gp_upper_y
             line_num = line_num + 1
             IF (stat /= 0) THEN
                WRITE(*,*) "Error: insufficient data in file '", INPUT_FILENAME, "', line ",&
                     &line_num, ":"
                WRITE(*,*) "       Usage: <x lower bound>, <x upper bound> <y lower bound> <y upper bound>"
                STOP
             END IF
             WRITE(*,*) gp_lower_x, gp_upper_x, gp_lower_y, gp_upper_y
          CASE DEFAULT
             WRITE(*,*) "Error: unable to understand the gnuplot directives in file '", INPUT_FILENAME,&
                  &"', line ", line_num, ":"
             WRITE(*,*) gp_dir
             STOP
          END SELECT
       CASE DEFAULT
          WRITE(*,*) "Error: unable to understand the directive in file '", INPUT_FILENAME, "', line ", line_num, ":"
          WRITE(*,*) line
          STOP
       END SELECT
    END DO

    CALL SYSTEM_CLOCK(COUNT=j)
    seed = j + 37*(/(i-1, i=1,1000)/)
    CALL RANDOM_SEED(PUT=seed)
    
    OPEN(unit=CONFIG_OUT_FILEID, file=CONFIG_OUT_FILENAME)
    OPEN(unit=STATISTIC_OUT_FILEID, file=STATISTIC_FILENAME)
!     DO i = 1, NUM_ATOMS
!        atoms(i)%mass = 39.0983
!        atoms(i)%charge = 1.
!        atoms(i)%position = 85. + (i-1)*250
!        atoms(i)%velocity = 0.
!        atoms(i)%prev_position = atoms(i)%position - atoms(i)%velocity * timestep
!        WRITE(CONFIG_OUT_FILEID, *) 0, atoms(i)%position, atoms(i)%velocity
!     END DO
!    WRITE(CONFIG_OUT_FILEID,*)      !blank line for dividing records with different times
    CALL checkVariables()       !check if every necessary variable is set by user
  END SUBROUTINE initialize

  SUBROUTINE removeComment(line)
    IMPLICIT NONE
    CHARACTER(LEN=*) :: line
    INTEGER :: idx
    line = TRIM(ADJUSTL(line)) !delete the head and tail spaces
    idx = INDEX(line, COMMENT_SYMBOL)
    IF (idx > 0) THEN           !comment symbol appears
       line = line(1:idx-1)   !ignore char after comment symbol
    END IF
  END SUBROUTINE removeComment

  SUBROUTINE getDirective(line, dir)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: line
    CHARACTER(LEN=LINE_WIDTH), INTENT(OUT) :: dir
    INTEGER :: idx
    idx = INDEX(line, ASSIGN_SYMBOL)
    IF (idx == 0) THEN
       WRITE(*,*) "Error: no assign symbol '", ASSIGN_SYMBOL, "'"
       WRITE(*,*) "       Usage: <Directive> : <Value> ..."
    END IF
    dir = TRIM(ADJUSTL(line(1:idx-1)))
    CALL downcase(dir)
  END SUBROUTINE getDirective

  SUBROUTINE getValueInt(line, valueInt)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: line
    INTEGER, INTENT(OUT) :: valueInt
    INTEGER :: idx
    idx = INDEX(line, ASSIGN_SYMBOL)
    IF (idx == 1) THEN
       WRITE(*,*) "Error: a directive must follow by an assign symbol '", ASSIGN_SYMBOL, "'"
    ELSE
       READ(UNIT=line(idx+1:), FMT=*) valueInt
    END IF
  END SUBROUTINE getValueInt

  SUBROUTINE getValueReal(line, valueFloat)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: line
    REAL, INTENT(OUT) :: valueFloat
    INTEGER :: idx
    idx = INDEX(line, ASSIGN_SYMBOL)
    IF (idx == 1) THEN
       WRITE(*,*) "Error: a directive must follow by an assign symbol '", ASSIGN_SYMBOL, "'"
    ELSE
       READ(UNIT=line(idx+1:), FMT=*) valueFloat
    END IF
  END SUBROUTINE getValueReal

  SUBROUTINE getValueString(line, valueString)
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: line
    CHARACTER(LEN=*), INTENT(OUT) :: valueString
    INTEGER :: idx
    idx = INDEX(line, ASSIGN_SYMBOL)
    IF (idx == 1) THEN
       WRITE(*,*) "Error: a directive must follow by an assign symbol '", ASSIGN_SYMBOL, "'"
    ELSE
       READ(UNIT=line(idx+1:), FMT=*) valueString
    END IF
    valueString = TRIM(ADJUSTL(valueString))
  END SUBROUTINE getValueString  

  SUBROUTINE downcase(words)
    IMPLICIT NONE
    CHARACTER(LEN=*) :: words
    INTEGER :: i, ic
    DO i = 1, LEN_TRIM(words)
       ic = ICHAR(words(i:i))
       IF (ic > IAA .AND. ic < IZZ) THEN !it's a capital word
          words(i:i) = ACHAR(ic+IA-IAA)
       END IF
    END DO
  END SUBROUTINE downcase

  SUBROUTINE setDefaultValue()
    IMPLICIT NONE
    sim_size = 0.               !unacceptable, check later
    poisson_rate = 0.           !unacceptable, check later
    num_atoms = 0.              !unacceptable, check later

    poisson_lbound = 0.
    generate_pos = 0.
    generate_cut = 0.
    current_probe_pos = 0.
    viscosity = 0.
    
    bias_voltage%startPos = 0.
    bias_voltage%endPos = sim_size
    bias_voltage%voltage = 0.
  END SUBROUTINE setDefaultValue

  SUBROUTINE checkVariables()
    IMPLICIT NONE
    IF (sim_size <= 0) THEN
       WRITE(*,*) "Error: simulation size must be set to a positive number."
       WRITE(*,*) "     Usage: ", DIR_SIM_SIZE, ": <value>"
       STOP
    ELSE IF (poisson_rate <= 0) THEN
       WRITE(*,*) "Error: poisson waiting rate must be set to a positive number."
       WRITE(*,*) "     Usage: ", DIR_POSSION_RATE, ": <value>"
       STOP
    ELSE IF (num_atoms <= 0) THEN
       WRITE(*,*) "Error: number of atoms must be set to a positive number."
       WRITE(*,*) "     Usage: ", DIR_NUM_ATOMS, ": <value>"
       STOP       
    END IF
  END SUBROUTINE checkVariables
END MODULE Initializer
