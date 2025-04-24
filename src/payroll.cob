       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL.
       AUTHOR.     BYTEBANK-DEV.
       DATE-WRITTEN. 23-APR-2025.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "../data/employees.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS EMP-ID
               FILE STATUS IS EMP-FILE-STATUS.
               
           SELECT PAYROLL-FILE ASSIGN TO "../data/payroll.dat"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS PAY-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-RECORD.
           05  EMP-ID               PIC X(10).
           05  EMP-NAME             PIC X(30).
           05  EMP-TYPE             PIC X(1).  *> 'F'=Full-time, 'P'=Part-time
           05  EMP-BASE-SALARY      PIC 9(7)V99.
           05  EMP-TAX-CODE         PIC X(3).  *> 'PAY'=PAYE, 'IND'=Independent
           05  FILLER               PIC X(108).

       FD  PAYROLL-FILE.
       01  PAYROLL-RECORD.
           05  PR-EMP-ID           PIC X(10).
           05  PR-PAY-PERIOD       PIC X(23).
           05  PR-GROSS-PAY        PIC 9(7)V99.
           05  PR-NET-PAY          PIC 9(7)V99.
           05  PR-TIMESTAMP        PIC X(19).  *> YYYY-MM-DD HH:MM:SS

       WORKING-STORAGE SECTION.
       01  FILE-STATUS.
           05  EMP-FILE-STATUS     PIC XX.
           05  PAY-FILE-STATUS     PIC XX.

       01  WS-CURRENT-DATE.
           05  WS-YEAR             PIC 9(4) VALUE 2025.
           05  WS-MONTH            PIC 9(2) VALUE 04.
           05  WS-DAY              PIC 9(2) VALUE 23.

       01  WS-PAYROLL-DATA.
           05  WS-PAY-PERIOD       PIC X(23) 
                                  VALUE "2025-04-01 to 2025-04-30".
           05  WS-PAY-DATE         PIC X(10) VALUE "2025-04-25".
           05  WS-BASE-SALARY      PIC 9(7)V99.
           05  WS-HOURLY-RATE      PIC 9(5)V99.
           05  WS-HOURS-WORKED     PIC 9(3)V99.
           05  WS-OVERTIME-HOURS   PIC 9(3)V99.
           05  WS-BONUS-AMT        PIC 9(7)V99.
           05  WS-LEAVE-DAYS       PIC 9(2).
           05  WS-GROSS-PAY        PIC 9(7)V99.
           05  WS-NET-PAY          PIC 9(7)V99.

       01  WS-DEDUCTIONS.
           05  WS-PAYE-TAX        PIC 9(7)V99.  *> South African PAYE
           05  WS-UIF             PIC 9(7)V99.  *> Unemployment Insurance Fund
           05  WS-SDL             PIC 9(7)V99.  *> Skills Development Levy
           05  WS-HEALTH-INS     PIC 9(7)V99.
           05  WS-PENSION        PIC 9(7)V99.
           05  WS-OTHER-DED      PIC 9(7)V99.
           05  WS-TOTAL-DED      PIC 9(7)V99.

       01  WS-TAX-RATES.
           05  SA-PAYE-RATE      PIC V999 VALUE .18.  *> Basic PAYE rate
           05  SA-UIF-RATE       PIC V999 VALUE .01.  *> 1% of gross
           05  SA-SDL-RATE       PIC V999 VALUE .01.  *> 1% of gross

       01  WS-EMPLOYEE-COUNT      PIC 9(5) VALUE 0.
       01  WS-TOTAL-GROSS         PIC 9(9)V99 VALUE 0.
       01  WS-TOTAL-NET           PIC 9(9)V99 VALUE 0.
       01  WS-CONFIRM             PIC X(1).

       01  WS-TIMESTAMP           PIC X(19).
       01  WS-FORMATTED-AMOUNT    PIC Z,ZZZ,ZZ9.99.

       LINKAGE SECTION.
       01  LS-USER-ROLE           PIC X(1).
       01  LS-EMP-ID              PIC X(10).

       PROCEDURE DIVISION USING LS-USER-ROLE, LS-EMP-ID.
       100-MAIN-PAYROLL.
           PERFORM 200-INITIALIZE-PAYROLL
           PERFORM 300-GET-PAY-PERIOD
           IF WS-CONFIRM = 'Y' OR 'y'
               PERFORM 400-PROCESS-EMPLOYEES
           END-IF
           PERFORM 900-FINALIZE-PAYROLL
           GOBACK.

       200-INITIALIZE-PAYROLL.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           STRING FUNCTION CURRENT-DATE(1:4) "-" FUNCTION CURRENT-DATE(5:2) "-"
                  FUNCTION CURRENT-DATE(7:2) " " FUNCTION CURRENT-TIME(1:2) ":"
                  FUNCTION CURRENT-TIME(3:2) ":" FUNCTION CURRENT-TIME(5:2)
           INTO WS-TIMESTAMP
           
           OPEN INPUT EMPLOYEE-FILE
           OPEN OUTPUT PAYROLL-FILE
           IF EMP-FILE-STATUS NOT = '00' OR PAY-FILE-STATUS NOT = '00'
               DISPLAY "ERROR: File access failure. Status: " 
                       EMP-FILE-STATUS " " PAY-FILE-STATUS
               STOP RUN
           END-IF
           DISPLAY "PAYROLL PROCESSING SYSTEM - SOUTH AFRICA (2025)".

       300-GET-PAY-PERIOD.
           DISPLAY "CURRENT DATE: " WS-TIMESTAMP
           DISPLAY "ENTER PAY PERIOD [YYYY-MM-DD to YYYY-MM-DD]: "
                   WITH NO ADVANCING
           ACCEPT WS-PAY-PERIOD
           
           DISPLAY "ENTER PAY DATE [YYYY-MM-DD]: " WITH NO ADVANCING
           ACCEPT WS-PAY-DATE
           
           DISPLAY "REVIEW PERIOD: " WS-PAY-PERIOD
           DISPLAY "PAY DATE:      " WS-PAY-DATE
           DISPLAY "CONFIRM PROCESSING? (Y/N): " WITH NO ADVANCING
           ACCEPT WS-CONFIRM.

       400-PROCESS-EMPLOYEES.
           MOVE LOW-VALUES TO EMP-ID
           START EMPLOYEE-FILE KEY IS NOT LESS THAN EMP-ID
               INVALID KEY 
                   DISPLAY "ERROR: Cannot access employee records"
               NOT INVALID KEY
                   PERFORM UNTIL EMP-FILE-STATUS = '10'
                       READ EMPLOYEE-FILE NEXT RECORD
                           AT END 
                               CONTINUE
                           NOT AT END
                               ADD 1 TO WS-EMPLOYEE-COUNT
                               PERFORM 420-CALCULATE-PAY
                               PERFORM 430-APPLY-SOUTH-AFRICAN-DEDUCTIONS
                               PERFORM 440-GENERATE-PAYSLIP
                               PERFORM 450-UPDATE-PAYROLL-FILE
                       END-READ
                   END-PERFORM
           END-START.

       420-CALCULATE-PAY.
           EVALUATE EMP-TYPE
               WHEN 'F'  *> Full-time employee
                   MOVE EMP-BASE-SALARY TO WS-BASE-SALARY
                   MOVE 0 TO WS-HOURS-WORKED, WS-OVERTIME-HOURS
               WHEN 'P'  *> Part-time employee
                   COMPUTE WS-HOURLY-RATE = EMP-BASE-SALARY / 160
                   MOVE 80.0 TO WS-HOURS-WORKED  *> Standard part-time hours
                   MOVE 5.0 TO WS-OVERTIME-HOURS
                   COMPUTE WS-BASE-SALARY = WS-HOURLY-RATE * WS-HOURS-WORKED
                   COMPUTE WS-BASE-SALARY = WS-BASE-SALARY + 
                                          (WS-HOURLY-RATE * 1.5 * WS-OVERTIME-HOURS)
           END-EVALUATE
           
           *> Annual bonus calculation (April is common bonus month in SA)
           IF WS-MONTH = 04
               COMPUTE WS-BONUS-AMT = EMP-BASE-SALARY * 0.1
           ELSE
               MOVE 0 TO WS-BONUS-AMT
           END-IF
           
           MOVE 1 TO WS-LEAVE-DAYS  *> Assuming 1 leave day this period
           COMPUTE WS-GROSS-PAY = WS-BASE-SALARY + WS-BONUS-AMT - 
                                 (WS-HOURLY-RATE * 8 * WS-LEAVE-DAYS)
           ADD WS-GROSS-PAY TO WS-TOTAL-GROSS.

       430-APPLY-SOUTH-AFRICAN-DEDUCTIONS.
           *> South African tax calculations
           COMPUTE WS-PAYE-TAX = WS-GROSS-PAY * SA-PAYE-RATE
           COMPUTE WS-UIF = WS-GROSS-PAY * SA-UIF-RATE
           COMPUTE WS-SDL = WS-GROSS-PAY * SA-SDL-RATE
           
           *> Benefits deductions
           MOVE 350.00 TO WS-HEALTH-INS  *> Average medical aid contribution
           COMPUTE WS-PENSION = WS-GROSS-PAY * 0.075  *> 7.5% pension
           MOVE 120.00 TO WS-OTHER-DED    *> Union fees/other
           
           COMPUTE WS-TOTAL-DED = WS-PAYE-TAX + WS-UIF + WS-SDL +
                                 WS-HEALTH-INS + WS-PENSION + WS-OTHER-DED
           COMPUTE WS-NET-PAY = WS-GROSS-PAY - WS-TOTAL-DED
           ADD WS-NET-PAY TO WS-TOTAL-NET.

       440-GENERATE-PAYSLIP.
           MOVE WS-GROSS-PAY TO WS-FORMATTED-AMOUNT
           DISPLAY " "
           DISPLAY "BYTEBANK PAYROLL SYSTEM - SOUTH AFRICA"
           DISPLAY "PAYSLIP FOR: " EMP-NAME " (" EMP-ID ")"
           DISPLAY "PAY PERIOD:  " WS-PAY-PERIOD
           DISPLAY "----------------------------------------"
           DISPLAY "GROSS PAY:        R " WS-FORMATTED-AMOUNT
           DISPLAY "DEDUCTIONS:"
           MOVE WS-PAYE-TAX TO WS-FORMATTED-AMOUNT
           DISPLAY "  PAYE TAX:      R " WS-FORMATTED-AMOUNT
           MOVE WS-UIF TO WS-FORMATTED-AMOUNT
           DISPLAY "  UIF:           R " WS-FORMATTED-AMOUNT
           MOVE WS-SDL TO WS-FORMATTED-AMOUNT
           DISPLAY "  SDL:           R " WS-FORMATTED-AMOUNT
           MOVE WS-HEALTH-INS TO WS-FORMATTED-AMOUNT
           DISPLAY "  HEALTH INS:     R " WS-FORMATTED-AMOUNT
           MOVE WS-PENSION TO WS-FORMATTED-AMOUNT
           DISPLAY "  PENSION:       R " WS-FORMATTED-AMOUNT
           MOVE WS-OTHER-DED TO WS-FORMATTED-AMOUNT
           DISPLAY "  OTHER:         R " WS-FORMATTED-AMOUNT
           MOVE WS-TOTAL-DED TO WS-FORMATTED-AMOUNT
           DISPLAY "TOTAL DEDUCTIONS: R " WS-FORMATTED-AMOUNT
           MOVE WS-NET-PAY TO WS-FORMATTED-AMOUNT
           DISPLAY "NET PAY:          R " WS-FORMATTED-AMOUNT
           DISPLAY "----------------------------------------"
           DISPLAY "PAYMENT DATE: " WS-PAY-DATE
           DISPLAY " ".

       450-UPDATE-PAYROLL-FILE.
           MOVE EMP-ID TO PR-EMP-ID
           MOVE WS-PAY-PERIOD TO PR-PAY-PERIOD
           MOVE WS-GROSS-PAY TO PR-GROSS-PAY
           MOVE WS-NET-PAY TO PR-NET-PAY
           MOVE WS-TIMESTAMP TO PR-TIMESTAMP
           WRITE PAYROLL-RECORD
           IF PAY-FILE-STATUS NOT = '00'
               DISPLAY "WARNING: Failed to record payroll for " EMP-ID
               DISPLAY "STATUS CODE: " PAY-FILE-STATUS
           END-IF.

       900-FINALIZE-PAYROLL.
           CLOSE EMPLOYEE-FILE PAYROLL-FILE
           MOVE WS-TOTAL-GROSS TO WS-FORMATTED-AMOUNT
           DISPLAY " "
           DISPLAY "PAYROLL PROCESSING COMPLETE"
           DISPLAY "--------------------------"
           DISPLAY "EMPLOYEES PROCESSED:  " WS-EMPLOYEE-COUNT
           DISPLAY "TOTAL GROSS PAY:     R " WS-FORMATTED-AMOUNT
           MOVE WS-TOTAL-NET TO WS-FORMATTED-AMOUNT
           DISPLAY "TOTAL NET PAY:       R " WS-FORMATTED-AMOUNT
           DISPLAY "PROCESS TIMESTAMP:   " WS-TIMESTAMP
           DISPLAY " ".
       END PROGRAM PAYROLL.