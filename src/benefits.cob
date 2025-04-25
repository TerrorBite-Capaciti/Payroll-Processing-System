       IDENTIFICATION DIVISION.
       PROGRAM-ID. BENEFITS.
       AUTHOR.     BYTEBANK-DEV.
       DATE-WRITTEN. 2023-11-15.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-BENEFIT-CHOICE      PIC 9(1).
       01  WS-EMPLOYEE-ID         PIC X(10).
       01  WS-BENEFIT-DATA.
           05  WS-HEALTH-INS      PIC X(1) VALUE 'N'.
           05  WS-401K-PCT        PIC 9V999 VALUE 0.000.
           05  WS-LIFE-INS        PIC X(1) VALUE 'N'.
           05  WS-DENTAL-INS      PIC X(1) VALUE 'N'.
           05  WS-VISION-INS      PIC X(1) VALUE 'N'.
       01  WS-TEMP-VALUE          PIC 9(3)V999.
       01  WS-DISPLAY-PCT         PIC Z9.99.

       LINKAGE SECTION.
       01  LS-USER-ROLE           PIC X(1).
       01  LS-EMP-ID              PIC X(10).

       PROCEDURE DIVISION USING LS-USER-ROLE, LS-EMP-ID.
       100-MAIN-BENEFITS.
           IF LS-USER-ROLE NOT = 'A' AND LS-USER-ROLE NOT = 'H'
               DISPLAY "Access denied. Requires HR or Admin access."
               GOBACK
           END-IF.

           PERFORM UNTIL WS-BENEFIT-CHOICE = 0
               DISPLAY " "
               DISPLAY "ByteBank Benefits Management"
               DISPLAY "---------------------------"
               DISPLAY "1. View Employee Benefits"
               DISPLAY "2. Update Health Insurance"
               DISPLAY "3. Update 401K Contribution"
               DISPLAY "4. Update Other Benefits"
               DISPLAY "0. Return to Main Menu"
               DISPLAY "Enter choice: " WITH NO ADVANCING
               ACCEPT WS-BENEFIT-CHOICE

               EVALUATE WS-BENEFIT-CHOICE
                   WHEN 1 PERFORM 200-VIEW-BENEFITS
                   WHEN 2 PERFORM 300-UPDATE-HEALTH
                   WHEN 3 PERFORM 400-UPDATE-401K
                   WHEN 4 PERFORM 500-UPDATE-OTHER
                   WHEN 0 CONTINUE
                   WHEN OTHER DISPLAY "Invalid choice"
               END-EVALUATE
           END-PERFORM.
           GOBACK.

       200-VIEW-BENEFITS.
           DISPLAY "Enter Employee ID: " WITH NO ADVANCING
           ACCEPT WS-EMPLOYEE-ID
           *> Calculate percentage for display
           COMPUTE WS-DISPLAY-PCT = WS-401K-PCT * 100
           DISPLAY " "
           DISPLAY "Current Benefits for Employee: " WS-EMPLOYEE-ID
           DISPLAY "----------------------------------"
           DISPLAY "Health Insurance:  " WS-HEALTH-INS
           DISPLAY "401K Contribution: " WS-DISPLAY-PCT "%"
           DISPLAY "Life Insurance:    " WS-LIFE-INS
           DISPLAY "Dental Insurance:  " WS-DENTAL-INS
           DISPLAY "Vision Insurance:  " WS-VISION-INS.

       300-UPDATE-HEALTH.
           DISPLAY "Enter Employee ID: " WITH NO ADVANCING
           ACCEPT WS-EMPLOYEE-ID
           DISPLAY "Current Health Insurance Status: " WS-HEALTH-INS
           DISPLAY "Update to (Y/N): " WITH NO ADVANCING
           ACCEPT WS-HEALTH-INS
           DISPLAY "Health insurance updated for employee: " WS-EMPLOYEE-ID.

       400-UPDATE-401K.
           DISPLAY "Enter Employee ID: " WITH NO ADVANCING
           ACCEPT WS-EMPLOYEE-ID
           COMPUTE WS-DISPLAY-PCT = WS-401K-PCT * 100
           DISPLAY "Current 401K Contribution: " WS-DISPLAY-PCT "%"
           DISPLAY "Enter new percentage (0-15): " WITH NO ADVANCING
           ACCEPT WS-TEMP-VALUE
           COMPUTE WS-401K-PCT = WS-TEMP-VALUE / 100
           DISPLAY "401K contribution updated for employee: " WS-EMPLOYEE-ID.

       500-UPDATE-OTHER.
           DISPLAY "Enter Employee ID: " WITH NO ADVANCING
           ACCEPT WS-EMPLOYEE-ID
           DISPLAY "Current Other Benefits:"
           DISPLAY "Life Insurance:   " WS-LIFE-INS
           DISPLAY "Dental Insurance: " WS-DENTAL-INS
           DISPLAY "Vision Insurance: " WS-VISION-INS
           DISPLAY " "
           DISPLAY "Update Life Insurance (Y/N): " WITH NO ADVANCING
           ACCEPT WS-LIFE-INS
           DISPLAY "Update Dental Insurance (Y/N): " WITH NO ADVANCING
           ACCEPT WS-DENTAL-INS
           DISPLAY "Update Vision Insurance (Y/N): " WITH NO ADVANCING
           ACCEPT WS-VISION-INS
           DISPLAY "Other benefits updated for employee: " WS-EMPLOYEE-ID.
       END PROGRAM BENEFITS.