       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PAYROLL.
       AUTHOR. YOUR-NAME.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TEST-EMPLOYEE-FILE ASSIGN TO "data/test_employees.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT TEST-SALARY-FILE ASSIGN TO "data/test_salary_records.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       
       FD TEST-EMPLOYEE-FILE.
       01 TEST-EMPLOYEE-RECORD.
           COPY employee_records FROM payroll_processing_system.cpy.
       
       FD TEST-SALARY-FILE.
       01 TEST-SALARY-RECORD.
           COPY salary_records FROM payroll_processing_system.cpy.

       WORKING-STORAGE SECTION.
       01 TEST-CASES.
          05 TEST-CASE-COUNT      PIC 9(2) VALUE 0.
          05 PASSED-COUNT         PIC 9(2) VALUE 0.
          05 FAILED-COUNT         PIC 9(2) VALUE 0.
          
       01 TEST-RESULTS.
          05 FILLER               PIC X(30) VALUE SPACES.
          05 TEST-RESULT          PIC X(5).
          05 FILLER               PIC X(30) VALUE SPACES.
          
       01 EXPECTED-RESULTS.
          05 EXP-GROSS-SALARY     PIC 9(7)V99.
          05 EXP-TAX-DEDUCTED     PIC 9(6)V99.
          05 EXP-NET-SALARY       PIC 9(7)V99.
          
       01 TEST-INPUTS.
          05 TEST-EMPLOYEE.
             10 TEST-EMP-ID       PIC X(5).
             10 TEST-EMP-NAME     PIC X(30).
             10 TEST-BASIC-SALARY PIC 9(7)V99.
             10 TEST-HOURS-WORKED PIC 9(3)V99.
             10 TEST-OVERTIME     PIC 9(3)V99.
             10 TEST-LEAVE-DED    PIC 9(5)V99.
             10 TEST-TAX-RATE     PIC 9(2)V99.
             10 TEST-BENEFITS     PIC 9(5)V99.

       PROCEDURE DIVISION.
       MAIN-TEST.
           DISPLAY "STARTING PAYROLL TEST SUITE".
           DISPLAY "==========================".
           
           PERFORM TEST-CASE-1.
           PERFORM TEST-CASE-2.
           PERFORM TEST-CASE-3.
           PERFORM TEST-CASE-4.
           
           DISPLAY " ".
           DISPLAY "TEST SUMMARY".
           DISPLAY "===========".
           DISPLAY "TOTAL TESTS: " TEST-CASE-COUNT.
           DISPLAY "PASSED:     " PASSED-COUNT.
           DISPLAY "FAILED:     " FAILED-COUNT.
           
           STOP RUN.

       TEST-CASE-1.
           MOVE "00001" TO TEST-EMP-ID.
           MOVE "Test Employee 1" TO TEST-EMP-NAME.
           MOVE 0050000.00 TO TEST-BASIC-SALARY.
           MOVE 040.00 TO TEST-HOURS-WORKED.
           MOVE 005.00 TO TEST-OVERTIME.
           MOVE 00050.00 TO TEST-LEAVE-DED.
           MOVE 15.00 TO TEST-TAX-RATE.
           MOVE 00500.00 TO TEST-BENEFITS.
           
           COMPUTE EXP-GROSS-SALARY = TEST-BASIC-SALARY + 
                                     (TEST-OVERTIME * 100).
           COMPUTE EXP-TAX-DEDUCTED = (EXP-GROSS-SALARY * TEST-TAX-RATE) / 100.
           COMPUTE EXP-NET-SALARY = EXP-GROSS-SALARY - EXP-TAX-DEDUCTED - 
                                   TEST-LEAVE-DED + TEST-BENEFITS.
           
           PERFORM RUN-TEST
               THRU VERIFY-RESULTS.
           
       TEST-CASE-2.
           MOVE "00002" TO TEST-EMP-ID.
           MOVE "Test Employee 2" TO TEST-EMP-NAME.
           MOVE 0060000.00 TO TEST-BASIC-SALARY.
           MOVE 045.00 TO TEST-HOURS-WORKED.
           MOVE 006.00 TO TEST-OVERTIME.
           MOVE 00060.00 TO TEST-LEAVE-DED.
           MOVE 20.00 TO TEST-TAX-RATE.
           MOVE 00600.00 TO TEST-BENEFITS.
           
           COMPUTE EXP-GROSS-SALARY = TEST-BASIC-SALARY + 
                                     (TEST-OVERTIME * 100).
           COMPUTE EXP-TAX-DEDUCTED = (EXP-GROSS-SALARY * TEST-TAX-RATE) / 100.
           COMPUTE EXP-NET-SALARY = EXP-GROSS-SALARY - EXP-TAX-DEDUCTED - 
                                   TEST-LEAVE-DED + TEST-BENEFITS.
           
           PERFORM RUN-TEST
               THRU VERIFY-RESULTS.
           
       RUN-TEST.
           ADD 1 TO TEST-CASE-COUNT.
           OPEN OUTPUT TEST-EMPLOYEE-FILE.
           WRITE TEST-EMPLOYEE-RECORD FROM TEST-EMPLOYEE.
           CLOSE TEST-EMPLOYEE-FILE.
           
           CALL "payroll" USING TEST-EMPLOYEE-FILE, TEST-SALARY-FILE.
           
           OPEN INPUT TEST-SALARY-FILE.
           READ TEST-SALARY-FILE INTO TEST-SALARY-RECORD.
           CLOSE TEST-SALARY-FILE.
           
       VERIFY-RESULTS.
           DISPLAY "TEST CASE " TEST-CASE-COUNT " - " TEST-EMP-ID.
           DISPLAY "----------------------------------".
           
           IF GROSS-SALARY IN TEST-SALARY-RECORD = EXP-GROSS-SALARY AND
              TAX-DEDUCTED IN TEST-SALARY-RECORD = EXP-TAX-DEDUCTED AND
              NET-SALARY IN TEST-SALARY-RECORD = EXP-NET-SALARY
               MOVE "PASS" TO TEST-RESULT
               ADD 1 TO PASSED-COUNT
           ELSE
               MOVE "FAIL" TO TEST-RESULT
               ADD 1 TO FAILED-COUNT
           END-IF.
           
           DISPLAY "Expected Gross: " EXP-GROSS-SALARY.
           DISPLAY "Actual Gross:   " GROSS-SALARY IN TEST-SALARY-RECORD.
           DISPLAY "Expected Tax:   " EXP-TAX-DEDUCTED.
           DISPLAY "Actual Tax:     " TAX-DEDUCTED IN TEST-SALARY-RECORD.
           DISPLAY "Expected Net:   " EXP-NET-SALARY.
           DISPLAY "Actual Net:     " NET-SALARY IN TEST-SALARY-RECORD.
           DISPLAY "RESULT:         " TEST-RESULT.
           DISPLAY " ".
           
       END PROGRAM TEST-PAYROLL.