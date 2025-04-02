       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLI-ADMIN.
       AUTHOR. PAYROLL-SYSTEM.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SYSIN ASSIGN TO KEYBOARD.
           SELECT SYSOUT ASSIGN TO DISPLAY.
       
       DATA DIVISION.
       FILE SECTION.
       FD SYSIN.
       01 CLI-INPUT          PIC X(80).
       
       FD SYSOUT.
       01 CLI-OUTPUT         PIC X(80).
       
       WORKING-STORAGE SECTION.
       01 WS-COMMAND         PIC X(20).
       01 WS-ARGS            PIC X(60).
       01 WS-USER-ROLE       PIC X(10) VALUE "ADMIN".
       01 WS-AUTHENTICATED   PIC X VALUE 'N'.
           88 AUTHENTICATED  VALUE 'Y'.
           88 NOT-AUTHENTICATED VALUE 'N'.
       
       COPY payroll_processing_system.cpy.
       
       PROCEDURE DIVISION.
       MAIN-CLI.
           PERFORM SHOW-BANNER
           PERFORM AUTHENTICATE-USER
           
           IF AUTHENTICATED
               PERFORM COMMAND-LOOP
           ELSE
               DISPLAY "Authentication failed. Access denied."
           END-IF
           
           STOP RUN.
       
       SHOW-BANNER.
           DISPLAY "=========================================="
           DISPLAY "  PAYROLL PROCESSING SYSTEM - ADMIN CLI    "
           DISPLAY "  Version 1.0                              "
           DISPLAY "==========================================".
       
       AUTHENTICATE-USER.
           DISPLAY "Admin Authentication Required"
           DISPLAY "Enter password: " WITH NO ADVANCING
           ACCEPT WS-INPUT-PASSWORD SECURE
           
           *> In production, use proper password hashing
           IF WS-INPUT-PASSWORD = "admin123"  *> Temporary for development
               SET AUTHENTICATED TO TRUE
               DISPLAY "Authentication successful."
           ELSE
               SET NOT-AUTHENTICATED TO TRUE
           END-IF.
       
       COMMAND-LOOP.
           PERFORM UNTIL WS-COMMAND = "EXIT"
               DISPLAY "payroll> " WITH NO ADVANCING
               ACCEPT WS-INPUT-LINE
               UNSTRING WS-INPUT-LINE DELIMITED BY SPACE
                   INTO WS-COMMAND WS-ARGS
               END-UNSTRING
               
               EVALUATE WS-COMMAND
                   WHEN "HELP"      PERFORM SHOW-HELP
                   WHEN "ADD"       CALL "process_employee" USING WS-ARGS
                   WHEN "CALC"      CALL "process_payroll" USING WS-ARGS
                   WHEN "REPORT"    CALL "generate_report" USING WS-ARGS
                   WHEN "CONFIG"    CALL "system_config" USING WS-ARGS
                   WHEN "EXIT"      CONTINUE
                   WHEN OTHER       DISPLAY "Unknown command. Type HELP for options."
               END-EVALUATE
           END-PERFORM.
       
       SHOW-HELP.
           DISPLAY "Available commands:"
           DISPLAY "  HELP       - Show this help"
           DISPLAY "  ADD EMP    - Add new employee"
           DISPLAY "  CALC [ID]  - Calculate payroll for employee/all"
           DISPLAY "  REPORT     - Generate payroll reports"
           DISPLAY "  CONFIG     - System configuration"
           DISPLAY "  EXIT       - Exit the CLI".
       
       END PROGRAM CLI-ADMIN.