IDENTIFICATION DIVISION.
PROGRAM-ID. PAYSLIP-GENERATOR.

 ENVIRONMENT DIVISION.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.
    SELECT EMPLOYEE-FILE ASSIGN TO 'data/employees.dat'
         ORGANIZATION IS LINE SEQUENTIAL.
    SELECT PAYSLIP-FILE ASSIGN TO 'data/payslip.txt'
         SELECT PAYSLIP-FILE ASSIGN TO 'data/payslip.txt'

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
                    
    WRITE PAYSLIP-RECORD FROM
               "===============================" & X"0A" &
               "Employee ID: " & EMP-ID & X"0A" &
               "Name       : " & EMP-NAME & X"0A" &
               "Hours Worked     : " & HOURS-WORKED & X"0A" &
               "Overtime Hours   : " & OVERTIME-HOURS & X"0A" &
               "Hourly Rate      : R" & HOURLY-RATE & X"0A" &
               "Bonus            : R" & BONUS & X"0A" &
               "Tax Deduction    : R" & TAX-DEDUCTION & X"0A" &
               "Leave Deduction  : R" & LEAVE-DEDUCTION & X"0A" &
               "Benefits         : R" & BENEFITS & X"0A" &
               "Gross Pay        : R" & GROSS-PAY & X"0A" &
               "Net Pay          : R" & NET-PAY & X"0A" &
               "===============================".            