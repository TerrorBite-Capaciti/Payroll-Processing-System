       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL-MAIN.
       AUTHOR.     BYTEBANK-DEV.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS WS-CRT-STATUS.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CRT-STATUS          PIC 9(4).
       01  WS-AUTH-STATUS         PIC X(1).
       01  WS-USER-ROLE           PIC X(1).
       01  WS-EMP-ID              PIC X(10).
       01  WS-CHOICE              PIC 9(2).
       01  WS-EXIT-FLAG           PIC X(1) VALUE 'N'.

       PROCEDURE DIVISION.
       100-MAIN-PROCEDURE.
           PERFORM 200-AUTHENTICATE-USER
           
           IF WS-AUTH-STATUS = 'S'
               PERFORM UNTIL WS-EXIT-FLAG = 'Y'
                   PERFORM 300-DISPLAY-MENU
                   ACCEPT WS-CHOICE
                   
                   EVALUATE WS-CHOICE
                       WHEN 1 PERFORM 400-PROCESS-PAYROLL
                       WHEN 2 PERFORM 500-VIEW-PAYSLIPS
                       WHEN 3 PERFORM 600-MANAGE-EMPLOYEES
                       WHEN 4 PERFORM 700-SYSTEM-REPORTS
                       WHEN 5 PERFORM 800-SYSTEM-CONFIG
                       WHEN 0 MOVE 'Y' TO WS-EXIT-FLAG
                       WHEN OTHER 
                           DISPLAY "Invalid choice, please try again."
                   END-EVALUATE
               END-PERFORM
           END-IF.
           
           DISPLAY "Thank you for using ByteBank Payroll System".
           STOP RUN.

       200-AUTHENTICATE-USER.
           CALL "AUTHENTICATION" USING WS-AUTH-STATUS, WS-USER-ROLE, WS-EMP-ID.

       300-DISPLAY-MENU.
           DISPLAY "ByteBank Payroll System - Main Menu"
           DISPLAY "----------------------------------"
           DISPLAY "1. Process Payroll"
           DISPLAY "2. View Payslips"
           DISPLAY "3. Employee Management"
           DISPLAY "4. System Reports"
           DISPLAY "5. System Configuration"
           DISPLAY "0. Exit"
           DISPLAY "Enter your choice: " WITH NO ADVANCING.

       400-PROCESS-PAYROLL.
           CALL "PAYROLL" USING WS-USER-ROLE, WS-EMP-ID.

       500-VIEW-PAYSLIPS.
           CALL "REPORTS" USING WS-USER-ROLE, WS-EMP-ID.

       600-MANAGE-EMPLOYEES.
           IF WS-USER-ROLE = 'A' OR WS-USER-ROLE = 'H'
               CALL "EMPLOYEE-MGMT" USING WS-USER-ROLE, WS-EMP-ID
           ELSE
               DISPLAY "Access denied. You don't have permission."
           END-IF.

       700-SYSTEM-REPORTS.
           IF WS-USER-ROLE = 'A' OR WS-USER-ROLE = 'H'
               CALL "REPORTS" USING WS-USER-ROLE, WS-EMP-ID
           ELSE
               DISPLAY "Access denied. You don't have permission."
           END-IF.

       800-SYSTEM-CONFIG.
           IF WS-USER-ROLE = 'A'
               CALL "CONFIG" USING WS-USER-ROLE, WS-EMP-ID
           ELSE
               DISPLAY "Access denied. Administrator only."
           END-IF.
       END PROGRAM PAYROLL-MAIN.