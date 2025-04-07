       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLI-MENU.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CHOICE           PIC 9(1).
       01  LOGGED-IN-ROLE      PIC X(10).

       PROCEDURE DIVISION.
       CLI-START.
           DISPLAY "Welcome to Payroll System"
           DISPLAY "--------------------------------"
           DISPLAY "Please select your role:"
           DISPLAY "1. Administrator"
           DISPLAY "2. Employee"
           ACCEPT WS-CHOICE

           IF WS-CHOICE = 1
               MOVE "ADMIN" TO LOGGED-IN-ROLE
               PERFORM ADMIN-MENU
           ELSE IF WS-CHOICE = 2
               MOVE "EMPLOYEE" TO LOGGED-IN-ROLE
               PERFORM EMPLOYEE-MENU
           ELSE
               DISPLAY "Invalid choice. Exiting..."
               STOP RUN
           END-IF

       ADMIN-MENU.
           DISPLAY "Admin Menu"
           DISPLAY "1. View Employees"
           DISPLAY "2. Modify Salary Rates"
           DISPLAY "3. Process Payroll"
           DISPLAY "Enter your choice: "
           ACCEPT WS-CHOICE

           EVALUATE WS-CHOICE
               WHEN 1
                   DISPLAY "Viewing Employees..."
                   * Insert logic for viewing employee data
               WHEN 2
                   DISPLAY "Modifying Salary Rates..."
                   * Insert logic for modifying salary data
               WHEN 3
                   DISPLAY "Processing Payroll..."
                   * Insert logic for processing payroll
               WHEN OTHER
                   DISPLAY "Invalid option"
           END-EVALUATE

       EMPLOYEE-MENU.
           DISPLAY "Employee Menu"
           DISPLAY "1. View Payslip"
           DISPLAY "2. View Salary History"
           DISPLAY "3. Request Leave"
           DISPLAY "Enter your choice: "
           ACCEPT WS-CHOICE

           EVALUATE WS-CHOICE
               WHEN 1
                   DISPLAY "Viewing Payslip..."
                   * Insert logic for displaying payslip
               WHEN 2
                   DISPLAY "Viewing Salary History..."
                   * Insert logic for viewing salary history
               WHEN 3
                   DISPLAY "Requesting Leave..."
                   * Insert logic for leave request
               WHEN OTHER
                   DISPLAY "Invalid option"
           END-EVALUATE

       STOP RUN.
