       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL.
       AUTHOR.     [Your Name].
       DATE-WRITTEN. [Date].

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO 'EMPFILE'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS EMP-ID
               FILE STATUS IS EMP-FILE-STATUS.

           SELECT PAYROLL-TRANS-FILE ASSIGN TO 'PAYTRAN'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS PAY-TRANS-STATUS.

           SELECT PAYSLIP-FILE ASSIGN TO 'PAYSLIP'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS PAYSLIP-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-RECORD.
           05  EMP-ID                PIC X(06).
           05  EMP-NAME              PIC X(40).
           05  EMP-DEPT              PIC X(15).
           05  EMP-POSITION          PIC X(20).
           05  EMP-BASE-SALARY       PIC 9(07)V99.
           05  EMP-TAX-CODE          PIC X(02).
           05  EMP-MARITAL-STATUS    PIC X(01).
           05  EMP-NUM-DEPENDENTS    PIC 9(02).
           05  EMP-LEAVE-BALANCE     PIC S9(03).
           05  EMP-ACCOUNT-NUM       PIC X(20).
           05  FILLER                PIC X(20).

       FD  PAYROLL-TRANS-FILE.
       01  PAYROLL-TRANS-RECORD.
           05  PT-EMP-ID            PIC X(06).
           05  PT-HOURS-WORKED      PIC 9(03)V99.
           05  PT-OVERTIME-HOURS    PIC 9(03)V99.
           05  PT-BONUS-AMOUNT      PIC 9(05)V99.
           05  PT-ALLOWANCES        PIC 9(05)V99.
           05  PT-UNPAID-LEAVE      PIC 9(02).
           05  PT-PERIOD-START      PIC 9(08).
           05  PT-PERIOD-END        PIC 9(08).
           05  FILLER               PIC X(20).

       FD  PAYSLIP-FILE.
       01  PAYSLIP-RECORD.
           05  PS-EMP-ID            PIC X(06).
           05  PS-EMP-NAME          PIC X(40).
           05  PS-PERIOD            PIC X(17).
           05  PS-GROSS-PAY         PIC Z,ZZZ,ZZ9.99.
           05  PS-TAX-DEDUCTIONS    PIC Z,ZZZ,ZZ9.99.
           05  PS-OTHER-DEDUCTIONS  PIC Z,ZZZ,ZZ9.99.
           05  PS-NET-PAY           PIC Z,ZZZ,ZZ9.99.
           05  PS-PAYMENT-DATE      PIC X(10).
           05  FILLER               PIC X(20).

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-VARS.
           05  EMP-FILE-STATUS      PIC XX.
           05  PAY-TRANS-STATUS     PIC XX.
           05  PAYSLIP-STATUS       PIC XX.

       01  PROGRAM-CONTROL.
           05  WS-END-OF-FILE       PIC X(01) VALUE 'N'.
               88  EOF              VALUE 'Y'.
               88  NOT-EOF          VALUE 'N'.
           05  WS-EMPLOYEE-FOUND    PIC X(01) VALUE 'N'.
               88  EMPLOYEE-FOUND   VALUE 'Y'.
               88  EMPLOYEE-NOT-FOUND VALUE 'N'.

       01  WS-CALCULATION-VARS.
           05  WS-GROSS-PAY         PIC 9(07)V99.
           05  WS-TAX-AMOUNT        PIC 9(07)V99.
           05  WS-NET-PAY           PIC 9(07)V99.
           05  WS-OVERTIME-PAY      PIC 9(07)V99.
           05  WS-LEAVE-DEDUCTION   PIC 9(07)V99.
           05  WS-TOTAL-DEDUCTIONS  PIC 9(07)V99.

       01  WS-CURRENT-DATE.
           05  WS-CURRENT-YEAR      PIC 9(04).
           05  WS-CURRENT-MONTH     PIC 9(02).
           05  WS-CURRENT-DAY       PIC 9(02).

       PROCEDURE DIVISION.
       000-MAIN-PAYROLL-PROCESSING.
           PERFORM 100-INITIALIZE-PAYROLL
           PERFORM 200-PROCESS-PAYROLL-TRANSACTIONS
           PERFORM 300-TERMINATE-PAYROLL
           GOBACK.

       100-INITIALIZE-PAYROLL.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           OPEN INPUT EMPLOYEE-FILE
                      PAYROLL-TRANS-FILE
               OUTPUT PAYSLIP-FILE
           
           IF EMP-FILE-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING EMPLOYEE FILE: ' EMP-FILE-STATUS
               MOVE 'Y' TO WS-END-OF-FILE
           END-IF
           
           IF PAY-TRANS-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING PAYROLL TRANS FILE: ' PAY-TRANS-STATUS
               MOVE 'Y' TO WS-END-OF-FILE
           END-IF
           
           IF PAYSLIP-STATUS NOT = '00'
               DISPLAY 'ERROR OPENING PAYSLIP FILE: ' PAYSLIP-STATUS
               MOVE 'Y' TO WS-END-OF-FILE
           END-IF.

       200-PROCESS-PAYROLL-TRANSACTIONS.
           PERFORM UNTIL EOF
               READ PAYROLL-TRANS-FILE
                   AT END
                       MOVE 'Y' TO WS-END-OF-FILE
                   NOT AT END
                       IF PAY-TRANS-STATUS = '00'
                           PERFORM 210-PROCESS-EMPLOYEE-PAYROLL
                       ELSE
                           DISPLAY 'ERROR READING PAYROLL TRANS FILE: '
                                   PAY-TRANS-STATUS
                       END-IF
               END-READ
           END-PERFORM.

       210-PROCESS-EMPLOYEE-PAYROLL.
           MOVE PT-EMP-ID TO EMP-ID
           READ EMPLOYEE-FILE
               INVALID KEY
                   DISPLAY 'EMPLOYEE NOT FOUND: ' PT-EMP-ID
                   MOVE 'N' TO WS-EMPLOYEE-FOUND
               NOT INVALID KEY
                   MOVE 'Y' TO WS-EMPLOYEE-FOUND
           END-READ
           
           IF EMPLOYEE-FOUND
               PERFORM 220-CALCULATE-GROSS-PAY
               PERFORM 230-CALCULATE-DEDUCTIONS
               PERFORM 240-CALCULATE-NET-PAY
               PERFORM 250-GENERATE-PAYSLIP
           END-IF.

       220-CALCULATE-GROSS-PAY.
           COMPUTE WS-GROSS-PAY = EMP-BASE-SALARY / 160 * PT-HOURS-WORKED
           COMPUTE WS-OVERTIME-PAY = (EMP-BASE-SALARY / 160 * 1.5) 
                                   * PT-OVERTIME-HOURS
           ADD WS-OVERTIME-PAY TO WS-GROSS-PAY
           ADD PT-BONUS-AMOUNT TO WS-GROSS-PAY
           ADD PT-ALLOWANCES TO WS-GROSS-PAY.

       230-CALCULATE-DEDUCTIONS.
           CALL 'LEAVEDEDUCTIONS' USING BY CONTENT EMP-LEAVE-BALANCE
                                        PT-UNPAID-LEAVE
                                        EMP-BASE-SALARY
                                        WS-LEAVE-DEDUCTION
           
           PERFORM 231-CALCULATE-TAX-DEDUCTIONS
           
           COMPUTE WS-TOTAL-DEDUCTIONS = WS-TAX-AMOUNT + WS-LEAVE-DEDUCTION.

       231-CALCULATE-TAX-DEDUCTIONS.
           EVALUATE EMP-TAX-CODE
               WHEN 'T1'  COMPUTE WS-TAX-AMOUNT = WS-GROSS-PAY * 0.20
               WHEN 'T2'  COMPUTE WS-TAX-AMOUNT = WS-GROSS-PAY * 0.25
               WHEN 'T3'  COMPUTE WS-TAX-AMOUNT = WS-GROSS-PAY * 0.30
               WHEN OTHER COMPUTE WS-TAX-AMOUNT = WS-GROSS-PAY * 0.15
           END-EVALUATE
           
           IF EMP-NUM-DEPENDENTS > 0
               COMPUTE WS-TAX-AMOUNT = WS-TAX-AMOUNT 
                                     - (EMP-NUM-DEPENDENTS * 500.00)
               IF WS-TAX-AMOUNT < 0
                   MOVE 0 TO WS-TAX-AMOUNT
               END-IF
           END-IF.

       240-CALCULATE-NET-PAY.
           COMPUTE WS-NET-PAY = WS-GROSS-PAY - WS-TOTAL-DEDUCTIONS.

       250-GENERATE-PAYSLIP.
           MOVE EMP-ID TO PS-EMP-ID
           MOVE EMP-NAME TO PS-EMP-NAME
           STRING PT-PERIOD-START ' - ' PT-PERIOD-END 
               DELIMITED BY SIZE INTO PS-PERIOD
           MOVE WS-GROSS-PAY TO PS-GROSS-PAY
           MOVE WS-TOTAL-DEDUCTIONS TO PS-TAX-DEDUCTIONS
           MOVE WS-LEAVE-DEDUCTION TO PS-OTHER-DEDUCTIONS
           MOVE WS-NET-PAY TO PS-NET-PAY
           STRING WS-CURRENT-DAY '/' WS-CURRENT-MONTH '/' WS-CURRENT-YEAR
               DELIMITED BY SIZE INTO PS-PAYMENT-DATE
           
           WRITE PAYSLIP-RECORD
           IF PAYSLIP-STATUS NOT = '00'
               DISPLAY 'ERROR WRITING PAYSLIP FOR: ' EMP-ID
           END-IF.

       300-TERMINATE-PAYROLL.
           CLOSE EMPLOYEE-FILE
                 PAYROLL-TRANS-FILE
                 PAYSLIP-FILE
           DISPLAY 'PAYROLL PROCESSING COMPLETED'.