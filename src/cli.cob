       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLI-INTERFACE.
       AUTHOR.     BYTEBANK-DEV.
       DATE-WRITTEN. 2023-11-15.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS WS-CRT-STATUS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CRT-STATUS          PIC 9(4).
       01  WS-COMMAND             PIC X(20).
       01  WS-PARAMETERS          PIC X(100).
       01  WS-INPUT-BUFFER        PIC X(120).
       01  WS-COMMAND-VALID       PIC X VALUE 'N'.
       01  WS-CONTINUE-FLAG       PIC X VALUE 'Y'.

       LINKAGE SECTION.
       01  LS-USER-ROLE           PIC X(1).
       01  LS-EMP-ID              PIC X(10).

       PROCEDURE DIVISION USING LS-USER-ROLE, LS-EMP-ID.
       100-MAIN-CLI.
           PERFORM UNTIL WS-CONTINUE-FLAG = 'N'
               DISPLAY "ByteBank CLI > " WITH NO ADVANCING
               ACCEPT WS-INPUT-BUFFER
               UNSTRING WS-INPUT-BUFFER DELIMITED BY SPACE
                   INTO WS-COMMAND, WS-PARAMETERS
               END-UNSTRING
               
               PERFORM 200-PROCESS-COMMAND
               
               DISPLAY "Execute another command? (Y/N): " 
                       WITH NO ADVANCING
               ACCEPT WS-CONTINUE-FLAG
           END-PERFORM.
           GOBACK.

       200-PROCESS-COMMAND.
           MOVE 'N' TO WS-COMMAND-VALID
           EVALUATE FUNCTION LOWER-CASE(WS-COMMAND)
               WHEN "help"    PERFORM 300-SHOW-HELP
               WHEN "payroll" PERFORM 400-PROCESS-PAYROLL
               WHEN "report"  PERFORM 500-GENERATE-REPORT
               WHEN "config"  PERFORM 600-CONFIG-SYSTEM
               WHEN "exit"    MOVE 'N' TO WS-CONTINUE-FLAG
               WHEN OTHER     DISPLAY "Invalid command. Type 'help' for options"
           END-EVALUATE.

       300-SHOW-HELP.
           DISPLAY "Available commands:"
           DISPLAY "help     - Show this help message"
           DISPLAY "payroll  - Process payroll (admin/hr only)"
           DISPLAY "report   - Generate reports (admin/hr only)"
           DISPLAY "config   - System configuration (admin only)"
           DISPLAY "exit     - Exit the CLI".
           MOVE 'Y' TO WS-COMMAND-VALID.

       400-PROCESS-PAYROLL.
           IF LS-USER-ROLE = 'A' OR LS-USER-ROLE = 'H'
               DISPLAY "Processing payroll with parameters: " WS-PARAMETERS
               *> Call payroll processing module here
               CALL "PAYROLL" USING LS-USER-ROLE, LS-EMP-ID
               MOVE 'Y' TO WS-COMMAND-VALID
           ELSE
               DISPLAY "Error: Insufficient privileges"
           END-IF.

       500-GENERATE-REPORT.
           IF LS-USER-ROLE = 'A' OR LS-USER-ROLE = 'H'
               DISPLAY "Generating report with parameters: " WS-PARAMETERS
               *> Call report generation module here
               CALL "REPORTS" USING LS-USER-ROLE, LS-EMP-ID
               MOVE 'Y' TO WS-COMMAND-VALID
           ELSE
               DISPLAY "Error: Insufficient privileges"
           END-IF.

       600-CONFIG-SYSTEM.
           IF LS-USER-ROLE = 'A'
               DISPLAY "Configuring system with parameters: " WS-PARAMETERS
               *> Call configuration module here
               CALL "CONFIG" USING LS-USER-ROLE, LS-EMP-ID
               MOVE 'Y' TO WS-COMMAND-VALID
           ELSE
               DISPLAY "Error: Administrator privileges required"
           END-IF.
       END PROGRAM CLI-INTERFACE.