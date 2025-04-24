      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYSLIP-GENERATOR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "data/employees.dat"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EMP-ID
      *       FILE STATUS IS FILE-STATUS-CODE
              .
           SELECT PAYSLIP-FILE ASSIGN TO 'data/payslip.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05  EMP-ID                    PIC 9(5).
           05  EMP-NAME                  PIC X(50).
           05  EMP-SURNAME               PIC X(50).
           05  EMP-POSITION-TYPE         PIC X(20).
           05  EMP-BIRTH               .
              10 EMP-BIRTH-YEAR             PIC X(4).
              10 EMP-BIRTH-MONTH            PIC X(2).
              10 EMP-BIRTH-DATE             PIC X(2).
           05 HOURS-WORKED        PIC 9(3).
           05 OVERTIME-HOURS      PIC 9(2).
           05 HOURLY-RATE         PIC Z(6)9.99.
           05 BONUS               PIC Z(6)9.99.
           05 TAX-DEDUCTION       PIC Z(6)9.99.
           05 LEAVE-DEDUCTION     PIC Z(6)9.99.
           05 BENEFITS            PIC Z(6)9.99.
           05  EMP-AGE                   PIC 9(2).
      *****   THIS IS AN OPTIONAL DATA-ITEM    ****
           05 EMP-UNION-FEE              PIC 9(5).





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
       01 LINE14   PIC X(100).
       01 LINE15   PIC X(100).

       01 EMPLOYEE-INDEX         PIC 9(1) VALUE 1.

       PROCEDURE DIVISION.
       BEGIN.

           OPEN OUTPUT PAYSLIP-FILE
           PERFORM UNTIL EMPLOYEE-INDEX > 3
               EVALUATE EMPLOYEE-INDEX
                   WHEN 1

                       MOVE 12345         TO EMP-ID
                       MOVE "Silindile"             TO EMP-NAME
                       MOVE "Shabangu"              TO EMP-SURNAME
                       MOVE "Manager"          TO EMP-POSITION-TYPE
                       MOVE "1990"             TO EMP-BIRTH-YEAR
                       MOVE "05"               TO EMP-BIRTH-MONTH
                       MOVE "15"               TO EMP-BIRTH-DATE
                       MOVE 160                TO HOURS-WORKED
                       MOVE 10                 TO OVERTIME-HOURS
                       MOVE 0                  TO BONUS
                       MOVE 1500.00            TO TAX-DEDUCTION
                       MOVE 300.00             TO LEAVE-DEDUCTION
                       MOVE 1200.00            TO BENEFITS
                       MOVE 35                 TO EMP-AGE
                       MOVE 50                 TO EMP-UNION-FEE

                  WHEN 2

                       MOVE 23456 TO EMP-ID
                       MOVE "Thabo" TO EMP-NAME
                       MOVE "Mokoena" TO EMP-SURNAME
                       MOVE "Senior" TO EMP-POSITION-TYPE
                       MOVE "1985" TO EMP-BIRTH-YEAR
                       MOVE "08" TO EMP-BIRTH-MONTH
                       MOVE "22" TO EMP-BIRTH-DATE
                       MOVE 170 TO HOURS-WORKED
                       MOVE 5 TO OVERTIME-HOURS
                       MOVE 500.00 TO BONUS
                       MOVE 1200.00 TO TAX-DEDUCTION
                       MOVE 200.00 TO LEAVE-DEDUCTION
                       MOVE 1000.00 TO BENEFITS
                       MOVE 40 TO EMP-AGE
                       MOVE 60 TO EMP-UNION-FEE

                  WHEN 3

                       MOVE 34567 TO EMP-ID
                       MOVE "Nomvula" TO EMP-NAME
                       MOVE "Dlamini" TO EMP-SURNAME
                       MOVE "Intern" TO EMP-POSITION-TYPE
                       MOVE "1998" TO EMP-BIRTH-YEAR
                       MOVE "11" TO EMP-BIRTH-MONTH
                       MOVE "30" TO EMP-BIRTH-DATE
                       MOVE 150 TO HOURS-WORKED
                       MOVE 8 TO OVERTIME-HOURS
                       MOVE 0 TO BONUS
                       MOVE 800.00 TO TAX-DEDUCTION
                       MOVE 100.00 TO LEAVE-DEDUCTION
                       MOVE 900.00 TO BENEFITS
                       MOVE 27 TO EMP-AGE
                       MOVE 40 TO EMP-UNION-FEE
               END-EVALUATE

               PERFORM GENERATE-PAYSLIP

               ADD 1 TO EMPLOYEE-INDEX
           END-PERFORM

       PERFORM GENERATE-PAYSLIP

       CLOSE PAYSLIP-FILE

       DISPLAY "Dummy payslip written."
       STOP RUN.


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

           EVALUATE EMP-POSITION-TYPE
               WHEN "Manager"
                   MOVE 100 TO HOURLY-RATE
               WHEN "Senior"
                   MOVE 70 TO HOURLY-RATE
               WHEN "Intern"
                   MOVE 50 TO HOURLY-RATE
               WHEN OTHER
                   MOVE 60 TO HOURLY-RATE  *> Default rate
               END-EVALUATE
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

           STRING "Surname:          " EMP-SURNAME DELIMITED BY SIZE
                  INTO LINE4
           END-STRING

           STRING "Position Type:  " EMP-POSITION-TYPE DELIMITED BY SIZE
                  INTO LINE5
           END-STRING


           STRING "Hours Worked:     " HOURS-WORKED DELIMITED BY SIZE
                  INTO LINE6
           END-STRING

           STRING "Overtime Hours:   " OVERTIME-HOURS DELIMITED BY SIZE
                  INTO LINE7
           END-STRING

           STRING "Hourly Rate:      R" HOURLY-RATE DELIMITED BY SIZE
                  INTO LINE8
           END-STRING

           STRING "Bonus:            R" BONUS DELIMITED BY SIZE
                  INTO LINE9
           END-STRING

           STRING "Tax Deduction:    R" TAX-DEDUCTION DELIMITED BY SIZE
                  INTO LINE10
           END-STRING

           STRING "Leave Deduction: R" LEAVE-DEDUCTION DELIMITED BY SIZE
                  INTO LINE11
           END-STRING

           STRING "Benefits:         R" BENEFITS DELIMITED BY SIZE
                  INTO LINE12
           END-STRING

           STRING "Gross Pay:        R" GROSS-PAY DELIMITED BY SIZE
                  INTO LINE13
           END-STRING

           STRING "Net Pay:          R" NET-PAY DELIMITED BY SIZE
                  INTO LINE14
           END-STRING

           STRING "===============================" DELIMITED BY SIZE
                  INTO LINE15
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
           WRITE PAYSLIP-RECORD FROM LINE14
           WRITE PAYSLIP-RECORD FROM LINE15.
