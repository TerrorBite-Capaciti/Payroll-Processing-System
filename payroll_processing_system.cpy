       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYSLIP-GENERATOR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO 'data/employees.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PAYSLIP-FILE ASSIGN TO 'data/payslip.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 EMP-ID              PIC X(5).
           05 EMP-NAME            PIC X(20).
           05 HOURS-WORKED        PIC 9(3).
           05 OVERTIME-HOURS      PIC 9(2).
           05 HOURLY-RATE         PIC 9(3)V99.
           05 BONUS               PIC 9(4)V99.
           05 TAX-DEDUCTION       PIC 9(4)V99.
           05 LEAVE-DEDUCTION     PIC 9(4)V99.
           05 BENEFITS            PIC 9(4)V99.

       FD PAYSLIP-FILE.
       01 PAYSLIP-RECORD         PIC X(100).

       WORKING-STORAGE SECTION.
       01 EOF-FLAG               PIC X VALUE 'N'.
           88 END-OF-FILE        VALUE 'Y'.
           88 NOT-END-OF-FILE    VALUE 'N'.

       01 CALCULATIONS.
           05 GROSS-PAY          PIC 9(6)V99 VALUE 0.
           05 NET-PAY            PIC 9(6)V99 VALUE 0.

       01 LINE1    PIC X(100).
       01 LINE2    PIC X(100).
       01 LINE3    PIC X(100).
       01 LINE4    PIC X(100).
       01 LINE5    PIC X(100).
       01 LINE6    PIC X(100).
       01 LINE7    PIC X(100).
       01 LINE8    PIC X(100).
       01 LINE9    PIC X(100).
       01 LINE10   PIC X(100).
       01 LINE11   PIC X(100).
       01 LINE12   PIC X(100).
       01 LINE13   PIC X(100).

       PROCEDURE DIVISION.
       BEGIN.
           OPEN INPUT EMPLOYEE-FILE
                OUTPUT PAYSLIP-FILE

           PERFORM UNTIL END-OF-FILE
               READ EMPLOYEE-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM GENERATE-PAYSLIP
               END-READ
           END-PERFORM

           CLOSE EMPLOYEE-FILE
                 PAYSLIP-FILE

           DISPLAY "Payslips generated successfully."
           STOP RUN.

       GENERATE-PAYSLIP.
           COMPUTE GROSS-PAY = (HOURS-WORKED * HOURLY-RATE)
                             + (OVERTIME-HOURS * HOURLY-RATE * 1.5)
                             + BONUS

           COMPUTE NET-PAY = GROSS-PAY
                         - TAX-DEDUCTION
                         - LEAVE-DEDUCTION
                         + BENEFITS

           STRING "===============================" DELIMITED BY SIZE
                  INTO LINE1
           END-STRING

           STRING "Employee ID:      " EMP-ID DELIMITED BY SIZE
                  INTO LINE2
           END-STRING

           STRING "Name:             " EMP-NAME DELIMITED BY SIZE
                  INTO LINE3
           END-STRING

           STRING "Hours Worked:     " HOURS-WORKED DELIMITED BY SIZE
                  INTO LINE4
           END-STRING

           STRING "Overtime Hours:   " OVERTIME-HOURS DELIMITED BY SIZE
                  INTO LINE5
           END-STRING

           STRING "Hourly Rate:      R" HOURLY-RATE DELIMITED BY SIZE
                  INTO LINE6
           END-STRING

           STRING "Bonus:            R" BONUS DELIMITED BY SIZE
                  INTO LINE7
           END-STRING

           STRING "Tax Deduction:    R" TAX-DEDUCTION DELIMITED BY SIZE
                  INTO LINE8
           END-STRING

           STRING "Leave Deduction:  R" LEAVE-DEDUCTION DELIMITED BY SIZE
                  INTO LINE9
           END-STRING

           STRING "Benefits:         R" BENEFITS DELIMITED BY SIZE
                  INTO LINE10
           END-STRING

           STRING "Gross Pay:        R" GROSS-PAY DELIMITED BY SIZE
                  INTO LINE11
           END-STRING

           STRING "Net Pay:          R" NET-PAY DELIMITED BY SIZE
                  INTO LINE12
           END-STRING

           STRING "===============================" DELIMITED BY SIZE
                  INTO LINE13
           END-STRING

           WRITE PAYSLIP-RECORD FROM LINE1
           WRITE PAYSLIP-RECORD FROM LINE2
           WRITE PAYSLIP-RECORD FROM LINE3
           WRITE PAYSLIP-RECORD FROM LINE4
           WRITE PAYSLIP-RECORD FROM LINE5
           WRITE PAYSLIP-RECORD FROM LINE6
           WRITE PAYSLIP-RECORD FROM LINE7
           WRITE PAYSLIP-RECORD FROM LINE8
           WRITE PAYSLIP-RECORD FROM LINE9
           WRITE PAYSLIP-RECORD FROM LINE10
           WRITE PAYSLIP-RECORD FROM LINE11
           WRITE PAYSLIP-RECORD FROM LINE12
           WRITE PAYSLIP-RECORD FROM LINE13
