   MODULE utilities_timer

   USE kinds
   USE parallel_utilities
   USE utilities_misc

   IMPLICIT NONE
   SAVE

!-----------------------------------------------------------------------
!  begin event_node type
!-----------------------------------------------------------------------
   TYPE event_node
      CHARACTER (LEN=72) :: &
         event_name
      LOGICAL :: &
         l_running
      REAL (KIND=dbl_kind) :: &
         wall_start,wall_stop,wall_total
      TYPE (event_node),POINTER :: &
         next
   END TYPE event_node
!-----------------------------------------------------------------------
!  end event_node type
!-----------------------------------------------------------------------
   LOGICAL :: &
      l_allocate_event_head    = .TRUE., &!
      l_allocate_squnc_head = .TRUE., &!
      l_sequence_running = .FALSE.
   TYPE (event_node),POINTER :: &
      event_head,squnc_head,squnc_tail

   REAL (KIND=dbl_kind) :: &
      wall_squnc_start = 0.0_dbl_kind

   INTEGER (KIND=int_kind) :: &
      wallclock0,wallclock1

   CONTAINS
!======================================================================
!  BEGIN timer
!======================================================================
   SUBROUTINE timer (event_name,action,communicator_name)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*),OPTIONAL :: &
      event_name,action,communicator_name
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL :: &
      l_event_added,l_event_found,l_total_time_found
   INTEGER (KIND=int_kind) :: &
      rnk,ierr
   REAL (KIND=dbl_kind) :: &
      wall_now,wall_save,wall_tmpry,wall_min,wall_ave,wall_max
   TYPE (event_node),POINTER :: &
      event,tmpry
   TYPE (comm_node),POINTER :: &
      comm
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!-----------------------------------------------------------------------
!  get the current time
!-----------------------------------------------------------------------
   wall_now = MPI_WTIME ()
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  NO ACTION SPECIFIED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (.NOT.PRESENT (action)) RETURN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  START AN EVENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (TRIM (action)=="start") THEN
      IF (.NOT.PRESENT (event_name)) RETURN

      l_event_added = .FALSE.
      l_event_found = .FALSE.
!-----------------------------------------------------------------------
!  the first time an event is added allocate the head node
!-----------------------------------------------------------------------
      IF (l_allocate_event_head) THEN
         l_allocate_event_head = .FALSE.

         ALLOCATE (event_head); event => event_head;
         event%event_name = " "

         ALLOCATE (event_head%next); event => event_head%next;
         event%event_name = TRIM (event_name)
         NULLIFY (event%next)

         l_event_added = .TRUE.
      ELSE
!-----------------------------------------------------------------------
!  scan the list of events to find events which already exist
!-----------------------------------------------------------------------
         event => event_head
         DO WHILE (ASSOCIATED (event))
            IF (TRIM (event%event_name)==TRIM (event_name)) THEN
               l_event_found = .TRUE.
               EXIT
            ENDIF
            event => event%next
         ENDDO
!-----------------------------------------------------------------------
!  if not found, then scan list to find position to add new event
!-----------------------------------------------------------------------
         IF (.NOT.l_event_found) THEN
            event => event_head
            DO WHILE (ASSOCIATED (event%next))
!-----------------------------------------------------------------------
!  add a new node in the middle of the list
!-----------------------------------------------------------------------
               IF (LLT (TRIM (event%event_name),TRIM (event_name)).AND.    &
                   LLT (TRIM (event_name),TRIM (event%next%event_name))) THEN
                  tmpry => event%next
                  NULLIFY (event%next); ALLOCATE (event%next)
                  event => event%next
                  event%next => tmpry
                  l_event_added = .TRUE.
                  EXIT
               ENDIF
               event => event%next
            ENDDO
!-----------------------------------------------------------------------
!  add a new node to the end of the list
!-----------------------------------------------------------------------
            IF (.NOT.l_event_added) THEN
               ALLOCATE (event%next)
               event => event%next
               NULLIFY (event%next)
               l_event_added = .TRUE.
            ENDIF
         ENDIF
      ENDIF
!-----------------------------------------------------------------------
!  if new event added, then initialize stuff
!-----------------------------------------------------------------------
      IF (l_event_added) THEN
         event%event_name = event_name
         event%l_running  = .TRUE.
         event%wall_start = wall_now
         event%wall_stop  = 0.0_dbl_kind
         event%wall_total = 0.0_dbl_kind
      ENDIF
!-----------------------------------------------------------------------
!  if (new event added) or (an existing event is not currently running)
!  then turn the timer on and get the beginning time
!-----------------------------------------------------------------------
      IF (l_event_found) THEN
         IF (.NOT.event%l_running) THEN
            event%l_running  = .TRUE.
            event%wall_start = wall_now
         ENDIF
      ENDIF
   ENDIF ! (TRIM (action)=="start")
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  STOP AN EVENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (TRIM (action)=="stop") THEN
      IF (.NOT.PRESENT (event_name)) RETURN

      l_event_found = .FALSE.

      IF (l_allocate_event_head) THEN
         PRINT *," timer :: action=stop called with no events initiated "
         STOP
      ELSE
         event => event_head
         DO WHILE (ASSOCIATED (event))
            IF (TRIM (event%event_name)==TRIM (event_name)) THEN
               l_event_found = .TRUE.
               EXIT
            ENDIF
            event => event%next
         ENDDO
!-----------------------------------------------------------------------
!  if the end of the list is reached and the event found then error
!-----------------------------------------------------------------------
         IF (.NOT.l_event_found) THEN
            PRINT *," timer :: action=stop called with event_name=",event_name
            PRINT *,"          this event has not been initiated "
            STOP
         ENDIF
      ENDIF
!-----------------------------------------------------------------------
!  if the timer is running, then turn the timer off, get the stopping time
!  and the total accumulated time
!-----------------------------------------------------------------------
      IF (event%l_running) THEN
         event%l_running  = .FALSE.
         event%wall_stop  = wall_now
         event%wall_total = event%wall_total+(event%wall_stop-event%wall_start)
      ENDIF
   ENDIF ! (TRIM (action)=="stop")
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  REPORT TIMINGS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (TRIM (action)=="report") THEN
!-----------------------------------------------------------------------
!  if the event list is empty then return
!-----------------------------------------------------------------------
      IF (l_allocate_event_head) RETURN
!-----------------------------------------------------------------------
!  scan for an event called "total time"
!-----------------------------------------------------------------------
      l_total_time_found = .FALSE.
      event => event_head
      DO WHILE (ASSOCIATED (event))
         IF (TRIM (event%event_name)=="total time") THEN
            l_total_time_found = .TRUE.
            IF (event%l_running) THEN
               wall_save = event%wall_total + (wall_now-event%wall_start)
            ELSE
               wall_save = event%wall_total
            ENDIF
            EXIT
         ENDIF
         event => event%next
      ENDDO

      IF (PRESENT (communicator_name)) THEN
         comm => parallel_get_communicator (communicator_name)
      ELSE
         comm => parallel_get_communicator ("world")
      ENDIF

      IF (comm%rnk_comm==0) THEN
         IF (l_total_time_found) THEN
            PRINT "(A9,A20,A15,A12,A9)"," process "," event name    ", &
                           " wall time (s) "," % of total "," running "
         ELSE
            PRINT "(A9,A20,A15,A9)"," process "," event name    ", &
                                          " wall time (s) "," running "
         ENDIF
      ENDIF

      CALL MPI_BARRIER (comm%comm,ierr)

      DO rnk = 0,comm%npe_comm-1
         IF (rnk==comm%rnk_comm) THEN
            event => event_head%next
            DO WHILE (ASSOCIATED (event))
               IF (event%l_running) THEN
                  wall_tmpry = event%wall_total + (wall_now-event%wall_start)
               ELSE
                  wall_tmpry = event%wall_total
               ENDIF
               IF (l_total_time_found) THEN
                  PRINT "(I5,A4,A20,F12.3,A3,F9.3,A7,L1)",rnk,"  : ", &
                           TRIM (event%event_name),wall_tmpry," ", &
                           100._8*wall_tmpry/wall_save," ", &
                           event%l_running
               ELSE
                  PRINT "(I5,A4,A20,F12.3,A7,L1)",rnk,"  : ", &
                           TRIM (event%event_name),wall_tmpry, " ", &
                           event%l_running
               ENDIF
               event => event%next
            ENDDO
         ENDIF
         CALL MPI_BARRIER (comm%comm,ierr)
      ENDDO

   ENDIF ! (TRIM (action)=="report")
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  REPORT PARALLEL TIMINGS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (TRIM (action)=="report_parallel") THEN

      IF (l_allocate_event_head) RETURN

      IF (PRESENT (communicator_name)) THEN
         comm => parallel_get_communicator (communicator_name)
      ELSE
         comm => parallel_get_communicator ("world")
      ENDIF

      IF (comm%rnk_comm==0) THEN
         PRINT "(A66)", &
          "%%%_REPORT_PARALLEL_TIMINGS_%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
         PRINT "(A32,A34)","           event name           ", &
                               "    wall_min   wall_ave   wall_max"
      ENDIF
      CALL MPI_BARRIER (comm%comm,ierr)

      event => event_head%next
      DO WHILE (ASSOCIATED (event))
         IF (event%l_running) THEN
             wall_tmpry = event%wall_total + (wall_now-event%wall_start)
         ELSE
             wall_tmpry = event%wall_total
         ENDIF

         wall_min = parallel_reduce (comm%comm_name,"min"    , &
                                                           flt_rk0=wall_tmpry)
         wall_ave = parallel_reduce (comm%comm_name,"average", &
                                                           flt_rk0=wall_tmpry)
         wall_max = parallel_reduce (comm%comm_name,"max"    , &
                                                           flt_rk0=wall_tmpry)
         IF (comm%rnk_comm==0) THEN
            PRINT "(A32,3F11.4)", &
                           TRIM (event%event_name),wall_min,wall_ave,wall_max
         ENDIF

         event => event%next
         CALL MPI_BARRIER (comm%comm,ierr)
      ENDDO

      IF (comm%rnk_comm==0) THEN
         PRINT "(A66)", &
          "%%%_REPORT_PARALLEL_TIMINGS_%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
      ENDIF
      CALL MPI_BARRIER (comm%comm,ierr)

   ENDIF ! (TRIM (action)=="report_parallel")
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  CLEAR TIMINGS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (TRIM (action)=="clear") THEN
      event => event_head
      DO WHILE (ASSOCIATED (event))
         event%wall_start = 0.0_dbl_kind
         event%wall_stop  = 0.0_dbl_kind
         event%wall_total = 0.0_dbl_kind
         event%l_running  = .FALSE.
         event => event%next
      ENDDO
   ENDIF ! (TRIM (action)=="clear")

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  RESET TIMER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (TRIM (action)=="reset") THEN
      l_allocate_event_head = .TRUE.
      DEALLOCATE (event_head)
      NULLIFY    (event_head)
   ENDIF ! (TRIM (action)=="reset")

   END SUBROUTINE timer
!======================================================================
!  END timer
!======================================================================

!======================================================================
!  BEGIN squnc
!======================================================================
   SUBROUTINE squnc (event_name,action,communicator_name,report_name)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*),OPTIONAL :: &
      event_name,action,communicator_name,report_name
!.......................................................................
!  LOCAL
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      wall_now
   INTEGER (KIND=int_kind) :: &
      rnk,ierr
   TYPE (event_node),POINTER :: &
      event
   TYPE (comm_node),POINTER :: &
      comm
   CHARACTER (LEN=72) :: &
      file_name
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!-----------------------------------------------------------------------
!  get the current time
!-----------------------------------------------------------------------
   wall_now = MPI_WTIME ()
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  NO ACTION SPECIFIED
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (.NOT.PRESENT (action)) RETURN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  START A SEQUENCE EVENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (TRIM (action)=="start") THEN
      IF (.NOT.PRESENT (event_name)) RETURN
      IF (l_sequence_running) THEN
         PRINT *," squnc :: action=start called with l_sequence_running=true "
         PRINT *," squnc :: event_name = ",TRIM (event_name)
         STOP
      ENDIF
      l_sequence_running = .TRUE.

      IF (l_allocate_squnc_head) THEN
         l_allocate_squnc_head = .FALSE.
         wall_squnc_start = wall_now
         ALLOCATE (squnc_head)
         squnc_tail => squnc_head
      ELSE
         ALLOCATE (squnc_tail%next)
         squnc_tail => squnc_tail%next
      ENDIF
      squnc_tail%event_name = TRIM (event_name)
      squnc_tail%wall_start = wall_now - wall_squnc_start
      NULLIFY (squnc_tail%next)
   ENDIF ! (TRIM (action)=="start")
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  STOP AN EVENT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (TRIM (action)=="stop") THEN
      IF (.NOT.PRESENT (event_name)) RETURN
      IF (l_allocate_squnc_head) THEN
         PRINT *," squnc :: action=stop called with no events initiated "
         STOP
      ENDIF

      IF (.NOT.l_sequence_running) THEN
         PRINT *," squnc :: action=stop called with l_sequence_running=false "
         PRINT *," squnc :: event_name = ",TRIM (event_name)
         STOP
      ENDIF
      l_sequence_running = .FALSE.

      squnc_tail%wall_stop  = wall_now - wall_squnc_start
   ENDIF ! (TRIM (action)=="stop")
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  REPORT TIMINGS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (TRIM (action)=="report") THEN

      IF (PRESENT (communicator_name)) THEN
         comm => parallel_get_communicator (communicator_name)
      ELSE
         comm => parallel_get_communicator ("world")
      ENDIF

      IF (PRESENT (report_name)) THEN
         file_name = "squnc_"//TRIM (report_name)
      ELSE
         file_name = "squnc"
      ENDIF

      DO rnk = 0,comm%npe_comm-1
         IF (rnk==comm%rnk_comm) THEN
            IF (rnk==0) THEN
               OPEN (UNIT=31,FILE=TRIM (file_name),FORM='FORMATTED')
            ELSE
               OPEN (UNIT=31,FILE=TRIM (file_name),FORM='FORMATTED', &
                                                            POSITION='APPEND')
            ENDIF
            WRITE (UNIT=31,FMT="(A1)") " "
            WRITE (UNIT=31,FMT="(A9,I6)") "_proc_ = ",rnk
            WRITE (UNIT=31,FMT="(A1)") " "
            event => squnc_head
            DO WHILE (ASSOCIATED (event))
               WRITE (UNIT=31,FMT="(A16,2F14.6)") &
                             event%event_name,event%wall_start,event%wall_stop
               event => event%next
            ENDDO
            CLOSE (UNIT=31)
         ENDIF
         CALL MPI_BARRIER (comm%comm,ierr)
      ENDDO

   ENDIF ! (TRIM (action)=="report")

   END SUBROUTINE squnc
!======================================================================
!  END squnc
!======================================================================

!======================================================================
!  BEGIN get_wall_time
!======================================================================
   FUNCTION get_wall_time () RESULT (wall_time)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      wall_time
!.......................................................................
!  LOCAL
!.......................................................................
!   INTEGER (KIND=4) :: &
!      current_values(8)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   wall_time = MPI_WTIME ()

!   CALL DATE_AND_TIME (VALUES=current_values)
!   wall_time = 86400000_8*current_values(3) + &
!                3600000_8*current_values(5) + &
!                  60000_8*current_values(6) + &
!                   1000_8*current_values(7) + &
!                          current_values(8)

   END FUNCTION get_wall_time
!======================================================================
!  END get_time
!======================================================================

!=======================================================================
!  BEGIN time_per_timestep
!=======================================================================
   FUNCTION time_per_timestep () RESULT (delta_time)
!.......................................................................
!  INTENT OUT
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      delta_time
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      current_time(8)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   CALL DATE_AND_TIME (VALUES=current_time)
   wallclock1 = 60000*current_time(6) + &
                 1000*current_time(7) + &
                      current_time(8)

   delta_time = wallclock1-wallclock0

   wallclock0 = wallclock1

   END FUNCTION time_per_timestep
!=======================================================================
!  END time_per_timestep
!=======================================================================

   END MODULE utilities_timer
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
