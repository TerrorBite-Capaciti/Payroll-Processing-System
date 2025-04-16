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