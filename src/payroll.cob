       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL.
       AUTHOR.     BYTEBANK-DEV.

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
           05  FILLER               PIC X(150).

       FD  PAYROLL-FILE.
       01  PAYROLL-RECORD.
           05  PR-EMP-ID           PIC X(10).
           05  PR-PAY-DATA         PIC X(100).

       WORKING-STORAGE SECTION.
       01  FILE-STATUS.
           05  EMP-FILE-STATUS     PIC XX.
           05  PAY-FILE-STATUS     PIC XX.

       01  WS-CURRENT-DATE.
           05  WS-YEAR             PIC 9(4).
           05  WS-MONTH            PIC 9(2).
           05  WS-DAY              PIC 9(2).

       01  WS-PAYROLL-DATA.
           05  WS-PAY-PERIOD       PIC X(20).
           05  WS-PAY-DATE         PIC X(10).
           05  WS-BASE-SALARY      PIC 9(7)V99.
           05  WS-HOURLY-RATE      PIC 9(5)V99.
           05  WS-HOURS-WORKED     PIC 9(3)V99.
           05  WS-OVERTIME-HOURS   PIC 9(3)V99.
           05  WS-BONUS-AMT        PIC 9(7)V99.
           05  WS-LEAVE-DAYS       PIC 9(2).
           05  WS-GROSS-PAY        PIC 9(7)V99.
           05  WS-NET-PAY          PIC 9(7)V99.

       01  WS-DEDUCTIONS.
           05  WS-FED-TAX         PIC 9(7)V99.
           05  WS-STATE-TAX       PIC 9(7)V99.
           05  WS-FICA           PIC 9(7)V99.
           05  WS-HEALTH-INS     PIC 9(7)V99.
           05  WS-401K           PIC 9(7)V99.
           05  WS-OTHER-DED      PIC 9(7)V99.
           05  WS-TOTAL-DED      PIC 9(7)V99.

       01  WS-EMPLOYEE-COUNT      PIC 9(5) VALUE 0.
       01  WS-TOTAL-GROSS         PIC 9(9)V99 VALUE 0.
       01  WS-TOTAL-NET           PIC 9(9)V99 VALUE 0.
       01  WS-CONFIRM             PIC X(1).

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
           OPEN INPUT EMPLOYEE-FILE
           OPEN OUTPUT PAYROLL-FILE
           IF EMP-FILE-STATUS NOT = '00' OR PAY-FILE-STATUS NOT = '00'
               DISPLAY "Error opening files. Status: " EMP-FILE-STATUS 
                       " " PAY-FILE-STATUS
               STOP RUN
           END-IF
           DISPLAY "Payroll Processing Initialized".

       300-GET-PAY-PERIOD.
           DISPLAY "Enter Pay Period (e.g., '2023-11-01 to 2023-11-15'): "
                   WITH NO ADVANCING
           ACCEPT WS-PAY-PERIOD
           
           DISPLAY "Enter Pay Date (YYYY-MM-DD): " WITH NO ADVANCING
           ACCEPT WS-PAY-DATE
           
           DISPLAY "About to process payroll for period: " WS-PAY-PERIOD
           DISPLAY "Pay date will be: " WS-PAY-DATE
           DISPLAY "Continue? (Y/N): " WITH NO ADVANCING
           ACCEPT WS-CONFIRM.

       400-PROCESS-EMPLOYEES.
           MOVE LOW-VALUES TO EMP-ID
           START EMPLOYEE-FILE KEY IS NOT LESS THAN EMP-ID
               INVALID KEY 
                   DISPLAY "Error starting employee file"
               NOT INVALID KEY
                   PERFORM UNTIL EMP-FILE-STATUS = '10'
                       READ EMPLOYEE-FILE NEXT RECORD
                           AT END 
                               CONTINUE
                           NOT AT END
                               ADD 1 TO WS-EMPLOYEE-COUNT
                               PERFORM 420-CALCULATE-PAY
                               PERFORM 430-APPLY-DEDUCTIONS
                               PERFORM 440-GENERATE-PAYSLIP
                               PERFORM 450-UPDATE-PAYROLL-FILE
                       END-READ
                   END-PERFORM
           END-START.

       420-CALCULATE-PAY.
           MOVE 40.00 TO WS-HOURLY-RATE
           MOVE 160.0 TO WS-HOURS-WORKED
           MOVE 10.0 TO WS-OVERTIME-HOURS
           MOVE 500.00 TO WS-BONUS-AMT
           MOVE 2 TO WS-LEAVE-DAYS
           
           COMPUTE WS-BASE-SALARY = WS-HOURLY-RATE * WS-HOURS-WORKED
           COMPUTE WS-BASE-SALARY = WS-BASE-SALARY + 
                                  (WS-HOURLY-RATE * 1.5 * WS-OVERTIME-HOURS)
           ADD WS-BONUS-AMT TO WS-BASE-SALARY
           COMPUTE WS-GROSS-PAY = WS-BASE-SALARY - 
                                 (WS-HOURLY-RATE * 8 * WS-LEAVE-DAYS)
           ADD WS-GROSS-PAY TO WS-TOTAL-GROSS.

       430-APPLY-DEDUCTIONS.
           COMPUTE WS-FED-TAX = WS-GROSS-PAY * 0.12
           COMPUTE WS-STATE-TAX = WS-GROSS-PAY * 0.05
           COMPUTE WS-FICA = WS-GROSS-PAY * 0.0765
           MOVE 200.00 TO WS-HEALTH-INS
           COMPUTE WS-401K = WS-GROSS-PAY * 0.03
           MOVE 50.00 TO WS-OTHER-DED
           COMPUTE WS-TOTAL-DED = WS-FED-TAX + WS-STATE-TAX + WS-FICA +
                                 WS-HEALTH-INS + WS-401K + WS-OTHER-DED
           COMPUTE WS-NET-PAY = WS-GROSS-PAY - WS-TOTAL-DED
           ADD WS-NET-PAY TO WS-TOTAL-NET.

       440-GENERATE-PAYSLIP.
           DISPLAY " "
           DISPLAY "Generated Payslip for Employee: " EMP-ID
           DISPLAY "Pay Period: " WS-PAY-PERIOD
           DISPLAY "----------------------------------------"
           DISPLAY "Gross Pay:       $" WS-GROSS-PAY
           DISPLAY "Deductions:"
           DISPLAY "  Federal Tax:   $" WS-FED-TAX
           DISPLAY "  State Tax:     $" WS-STATE-TAX
           DISPLAY "  FICA:          $" WS-FICA
           DISPLAY "  Health Ins:    $" WS-HEALTH-INS
           DISPLAY "  401K:          $" WS-401K
           DISPLAY "  Other:         $" WS-OTHER-DED
           DISPLAY "Total Deductions $" WS-TOTAL-DED
           DISPLAY "Net Pay:         $" WS-NET-PAY
           DISPLAY "----------------------------------------".

       450-UPDATE-PAYROLL-FILE.
           MOVE EMP-ID TO PR-EMP-ID
           *> Add other payroll data to PR-PAY-DATA here
           WRITE PAYROLL-RECORD
           IF PAY-FILE-STATUS NOT = '00'
               DISPLAY "Error writing payroll record for employee: " EMP-ID
               DISPLAY "File status: " PAY-FILE-STATUS
           END-IF.

       900-FINALIZE-PAYROLL.
           CLOSE EMPLOYEE-FILE PAYROLL-FILE
           DISPLAY " "
           DISPLAY "Payroll Processing Summary"
           DISPLAY "------------------------"
           DISPLAY "Employees processed: " WS-EMPLOYEE-COUNT
           DISPLAY "Total Gross Pay:    $" WS-TOTAL-GROSS
           DISPLAY "Total Net Pay:      $" WS-TOTAL-NET
           DISPLAY "Payroll processing completed.".
       END PROGRAM PAYROLL.