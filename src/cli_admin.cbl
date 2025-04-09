      ******************************************************************
      * Author: TerrorBite CAPACITI (BYTE BANK)
      * Date: 04 APRIL 2025
      * Purpose: PAYROLL SYSTEM
      * Compiler: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BYTEBANK-PAYROLL.

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 USER-CHOICE    PIC 9.

       PROCEDURE DIVISION.
       DISPLAY-INTRO.
           DISPLAY-INTRO.
           DISPLAY "--------------------------------------------------".
           DISPLAY "            WELCOME TO BYTE-BANK PAYROLL SYSTEM   ".
           DISPLAY "--------------------------------------------------".
           DISPLAY " ".
           DISPLAY "Please select your role:".
           DISPLAY "1. Admin Login".
           DISPLAY "2. Employee Login".
           DISPLAY " ".
           DISPLAY "Enter your choice: ".
           ACCEPT USER-CHOICE.

           EVALUATE USER-CHOICE
            WHEN 1
                PERFORM ADMIN-MENU
            WHEN 2
                PERFORM EMPLOYEE-MENU
            WHEN OTHER
              DISPLAY "Invalid selection. Please run the progarm again."
           END-EVALUATE.
           STOP RUN.

       ADMIN-MENU.
           DISPLAY"___________________________________________________".
           DISPLAY"            BYTE-BANK ADMIN MENU                   ".
           DISPLAY"___________________________________________________".
           DISPLAY"1. Add New Employee".
           DISPLAY"2. Process Payroll".
           DISPLAY"3. View Employee Payslip".
           DISPLAY"4. View All Employees Report".
           DISPLAY"5. Exit".
           DISPLAY"___________________________________________________".
           DISPLAY "Please select an option (1-5): ".
           ACCEPT USER-CHOICE.

           EVALUATE USER-CHOICE
            WHEN 1
                DISPLAY "Add New Employee ."
      * Code to add new employee goes here
            WHEN 2
                DISPLAY "Process Payroll Functionality."
      * Code to process payroll goes here
            WHEN 3
                DISPLAY "View Employee Payslip Functionality."
      * Code to view payslip goes here
            WHEN 4
                DISPLAY "View All Employees Report Functionality."
      * Code to view employee reports goes here
            WHEN 5
                DISPLAY "Exiting program."
            WHEN OTHER
                DISPLAY "Invalid selection. Please select between 1-5."
           END-EVALUATE.


       EMPLOYEE-MENU.
           DISPLAY"___________________________________________________".
           DISPLAY"            BYTE-BANK EMPLOYEE MENU                ".
           DISPLAY"___________________________________________________".
           DISPLAY"1. View Employee Payslip".
           DISPLAY"2. View All Employees Report".
           DISPLAY"3. Exit".
           DISPLAY"___________________________________________________".
           DISPLAY "Please select an option (1-3): ".
           ACCEPT USER-CHOICE.
           EVALUATE USER-CHOICE
            WHEN 1
                DISPLAY "View Employee Payslip Functionality."
      * Code to view payslip goes here
            WHEN 2
                DISPLAY "View All Employees Report Functionality."
      * Code to view reports goes here
            WHEN 3
                DISPLAY "Exiting program."
            WHEN OTHER
                DISPLAY "Invalid selection. Please select between 3-5."
           END-EVALUATE.
