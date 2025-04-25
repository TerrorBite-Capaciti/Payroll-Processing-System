       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL.
       AUTHOR. Sisamkele Vava.
       DATE-WRITTEN. 2025-04-25.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYROLL-FILE ASSIGN TO "payroll.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD PAYROLL-FILE.
       01 PAYROLL-RECORD.
           05 EMP-ID        PIC 9(5).
           05 EMP-NAME      PIC A(30).
           05 EMP-SURNAME   PIC A(30).
           05 EMP-POSITION  PIC A(20).
           05 EMP-BIRTH-DATE.
               10 BIRTH-YEAR   PIC 9(4).
               10 BIRTH-MONTH  PIC 99.
               10 BIRTH-DAY    PIC 99.
           05 HOURS-WORKED   PIC 9(5)V99.
           05 OVERTIME-HOURS PIC 9(5)V99.
           05 HOURLY-RATE    PIC 9(5)V99.
           05 BONUS          PIC 9(5)V99.
           05 GROSS-SALARY   PIC 9(6)V99.
           05 UIF-DEDUCTION  PIC 9(5)V99.
           05 PAYE-DEDUCTION PIC 9(5)V99.
           05 PENSION-DEDUCTION PIC 9(5)V99.
           05 GROSS-PAY      PIC 9(6)V99.
           05 NET-PAY        PIC 9(6)V99.

       WORKING-STORAGE SECTION.
       01 EOF-FLAG          PIC X VALUE "N".
       01 UIF-RATE          PIC 9(2)V99 VALUE 0.01.       *> UIF Rate (1% of salary)
       01 PAYE-TAXABLE-THRESHOLD PIC 9(6)V99 VALUE 225000. *> Example tax threshold per year
       01 PAYE-BRACKET-RATE    PIC 9(2)V99 VALUE 0.18.    *> PAYE tax rate (18% for income above R225k/year)
       01 PENSION-CONTRIBUTION  PIC 9(2)V99 VALUE 0.07.    *> Pension contribution (7%)

       PROCEDURE DIVISION.

       OPEN INPUT PAYROLL-FILE.

       PERFORM READ-RECORD UNTIL EOF-FLAG = "Y".

       CLOSE PAYROLL-FILE.

       STOP RUN.

       READ-RECORD.
           READ PAYROLL-FILE INTO PAYROLL-RECORD
               AT END
                   MOVE "Y" TO EOF-FLAG
               NOT AT END
                   PERFORM PROCESS-PAYROLL
           END-READ.

       PROCESS-PAYROLL.
           *> Calculate Gross Salary
           COMPUTE GROSS-SALARY = (HOURS-WORKED * HOURLY-RATE) + (OVERTIME-HOURS * HOURLY-RATE) + BONUS.
           
           *> Calculate UIF Deduction (1% of gross salary)
           COMPUTE UIF-DEDUCTION = GROSS-SALARY * UIF-RATE.
           
           *> Calculate PAYE Deduction (South African tax calculation based on income)
           IF GROSS-SALARY > PAYE-TAXABLE-THRESHOLD THEN
               COMPUTE PAYE-DEDUCTION = (GROSS-SALARY - PAYE-TAXABLE-THRESHOLD) * PAYE-BRACKET-RATE
           ELSE
               COMPUTE PAYE-DEDUCTION = 0
           END-IF.
           
           *> Calculate Pension Deduction (7% of gross salary)
           COMPUTE PENSION-DEDUCTION = GROSS-SALARY * PENSION-CONTRIBUTION.
           
           *> Calculate Gross Pay (including deductions)
           COMPUTE GROSS-PAY = GROSS-SALARY.
           
           *> Calculate Net Pay (after deductions)
           COMPUTE NET-PAY = GROSS-SALARY - UIF-DEDUCTION - PAYE-DEDUCTION - PENSION-DEDUCTION.
           
           *> Display Payroll Information
           DISPLAY "Employee ID: " EMP-ID
                   " Name: " EMP-NAME
                   " Gross Salary: " GROSS-SALARY
                   " UIF Deduction: " UIF-DEDUCTION
                   " PAYE Deduction: " PAYE-DEDUCTION
                   " Pension Deduction: " PENSION-DEDUCTION
                   " Net Pay: " NET-PAY.
           
           *> Move results back to file or process further as required
           MOVE NET-PAY TO PAYROLL-RECORD.
           *> Add additional logic to store or output results if necessary.

       END PROGRAM PAYROLL.
