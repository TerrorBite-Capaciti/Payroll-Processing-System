       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLI-MENU.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CHOICE           PIC 9(1).
       01  LOGGED-IN-ROLE      PIC X(10).
       01  WS-MENU-OPTION      PIC 9(2).
       01  EXIT-FLAG           PIC X VALUE 'N'.
           88  EXIT-YES        VALUE 'Y'.
           88  EXIT-NO         VALUE 'N'.

       PROCEDURE DIVISION.
       CLI-START.
           DISPLAY "======================================="
           DISPLAY "      WELCOME TO PAYROLL SYSTEM"
           DISPLAY "======================================="
           DISPLAY "Please select your role:"
           DISPLAY "1. Administrator"
           DISPLAY "2. Employee"
           ACCEPT WS-CHOICE

           EVALUATE WS-CHOICE
               WHEN 1
                   MOVE "ADMIN" TO LOGGED-IN-ROLE
                   PERFORM ADMIN-MENU
               WHEN 2
                   MOVE "EMPLOYEE" TO LOGGED-IN-ROLE
                   PERFORM EMPLOYEE-MENU
               WHEN OTHER
                   DISPLAY "Invalid choice. Exiting..."
           END-EVALUATE
           STOP RUN.

       ADMIN-MENU.
           MOVE 'N' TO EXIT-FLAG
           PERFORM UNTIL EXIT-YES
               DISPLAY "---------------------------------------"
               DISPLAY "ADMINISTRATOR MENU"
               DISPLAY "1. View Employee Records"
               DISPLAY "2. Add New Employee"
               DISPLAY "3. Update Employee Info"
               DISPLAY "4. Modify Salary Rates"
               DISPLAY "5. Generate Payroll"
               DISPLAY "6. Download Payroll Report"
               DISPLAY "7. View Payroll Summary by Department"
               DISPLAY "8. Logout"
               ACCEPT WS-MENU-OPTION

               EVALUATE WS-MENU-OPTION
                   WHEN 1
                       PERFORM VIEW-EMPLOYEE-RECORDS
                   WHEN 2
                       PERFORM ADD-NEW-EMPLOYEE
                   WHEN 3
                       PERFORM UPDATE-EMPLOYEE-INFO
                   WHEN 4
                       PERFORM MODIFY-SALARY-RATES
                   WHEN 5
                       PERFORM GENERATE-PAYROLL
                   WHEN 6
                       PERFORM DOWNLOAD-PAYROLL-REPORT
                   WHEN 7
                       PERFORM VIEW-DEPARTMENT-SUMMARY
                   WHEN 8
                       MOVE 'Y' TO EXIT-FLAG
                   WHEN OTHER
                       DISPLAY "Invalid selection. Try again."
               END-EVALUATE
           END-PERFORM.

       EMPLOYEE-MENU.
           MOVE 'N' TO EXIT-FLAG
           PERFORM UNTIL EXIT-YES
               DISPLAY "---------------------------------------"
               DISPLAY "EMPLOYEE MENU"
               DISPLAY "1. View My Payslip"
               DISPLAY "2. Submit Leave Request"
               DISPLAY "3. View Leave Balance"
               DISPLAY "4. Check Attendance History"
               DISPLAY "5. Download Payslip"
               DISPLAY "6. Change Password / Update Info"
               DISPLAY "7. Logout"
               ACCEPT WS-MENU-OPTION

               EVALUATE WS-MENU-OPTION
                   WHEN 1
                       PERFORM VIEW-PAYSLIP
                   WHEN 2
                       PERFORM SUBMIT-LEAVE-REQUEST
                   WHEN 3
                       PERFORM VIEW-LEAVE-BALANCE
                   WHEN 4
                       PERFORM CHECK-ATTENDANCE-HISTORY
                   WHEN 5
                       PERFORM DOWNLOAD-PAYSLIP
                   WHEN 6
                       PERFORM UPDATE-ACCOUNT-INFO
                   WHEN 7
                       MOVE 'Y' TO EXIT-FLAG
                   WHEN OTHER
                       DISPLAY "Invalid selection. Try again."
               END-EVALUATE
           END-PERFORM.

       * ------------------------
       * Admin Feature Stubs
       * ------------------------

       VIEW-EMPLOYEE-RECORDS.
           DISPLAY ">>> View Employee Records feature coming soon..."
           DISPLAY "Returning to Admin Menu..."

       ADD-NEW-EMPLOYEE.
           DISPLAY ">>> Add New Employee feature coming soon..."
           DISPLAY "Returning to Admin Menu..."

       UPDATE-EMPLOYEE-INFO.
           DISPLAY ">>> Update Employee Info feature coming soon..."
           DISPLAY "Returning to Admin Menu..."

       MODIFY-SALARY-RATES.
           DISPLAY ">>> Modify Salary Rates feature coming soon..."
           DISPLAY "Returning to Admin Menu..."

       GENERATE-PAYROLL.
           DISPLAY ">>> Running Payroll Calculation..."
           CALL 'PAYROLL'
           DISPLAY ">>> Payroll Generation Complete!"
           DISPLAY "Returning to Admin Menu..."

       DOWNLOAD-PAYROLL-REPORT.
           DISPLAY ">>> Download Payroll Report feature coming soon..."
           DISPLAY "Returning to Admin Menu..."

       VIEW-DEPARTMENT-SUMMARY.
           DISPLAY ">>> View Payroll Summary by Department coming soon..."
           DISPLAY "Returning to Admin Menu..."

       * ------------------------
       * Employee Feature Stubs
       * ------------------------

       VIEW-PAYSLIP.
           DISPLAY ">>> Payslip View feature coming soon..."
           DISPLAY "Returning to Employee Menu..."

       SUBMIT-LEAVE-REQUEST.
           DISPLAY ">>> Leave Submission feature coming soon..."
           DISPLAY "Returning to Employee Menu..."

       VIEW-LEAVE-BALANCE.
           DISPLAY ">>> Leave Balance feature coming soon..."
           DISPLAY "Returning to Employee Menu..."

       CHECK-ATTENDANCE-HISTORY.
           DISPLAY ">>> Attendance History feature coming soon..."
           DISPLAY "Returning to Employee Menu..."

       DOWNLOAD-PAYSLIP.
           DISPLAY ">>> Payslip Download feature coming soon..."
           DISPLAY "Returning to Employee Menu..."

       UPDATE-ACCOUNT-INFO.
           DISPLAY ">>> Change Password / Update Info feature coming soon..."
           DISPLAY "Returning to Employee Menu..."
