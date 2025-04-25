IDENTIFICATION DIVISION.
       PROGRAM-ID. BYTEBANK-PAYROLL.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
 
       WORKING-STORAGE SECTION.
       01 USER-CHOICE PIC X.
 
       01 EMPLOYEE-DATA.
           05 EMP-NAME PIC X(30).
           05 EMP-SURNAME    PIC X(30).
           05 EMP-ID         PIC X(5).
           05 EMP-BIRTHDATE  PIC X(10). *>YYYY-MM-DD
           05 EMP-POSITION  PIC X(15). *>LOW/INTERMEDIATE/SENIOR
           05 EMP-HOURS      PIC 9(3).
           05 EMP-BASE-SALARY PIC 9(6).
           05 EMP-FINAL-SAL   PIC 9(7).
           05 EMP-BONUS      PIC 9(5).
 
        01 GROSS-SALARY        PIC 9(7)V99 VALUE 0.
       01 BONUS-AMOUNT        PIC 9(4) VALUE 0.
       01 NET-SALARY          PIC 9(7)V99 VALUE 0.    
 
       01  INPUT-EMP-ID  PIC X(5).
       01  AUTHENTICATED PIC X VALUE "N".
           88 IS-AUTHENTICATED VALUE "Y".
 
       01 CURRENT-DATE.
           05 CURRENT-YEAR  PIC 9(4).
           05 CURRENT-MONTH  PIC 9(2).
           05 CURRENT-DAY  PIC 9(2).
 
        01 UNION-FEE-ANSWER     PIC X.
 
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           MOVE SPACES TO EMPLOYEE-DATA
           MOVE ZEROS TO EMP-HOURS, EMP-BASE-SALARY, EMP-FINAL-SAL,
           EMP-BONUS
           MOVE "N" TO AUTHENTICATED
           ACCEPT CURRENT-DATE FROM DATE YYYYMMDD
            PERFORM DISPLAY-MAIN-MENU
           STOP RUN.
 
       DISPLAY-MAIN-MENU.
           DISPLAY"---------------------------------------------------".
           DISPLAY"BYTE-BANK PAYROLL SYSTEM MAIN MENU                 ".
           DISPLAY"---------------------------------------------------".
           DISPLAY"1. Admin Functions"
           DISPLAY"2. Employee Functions"
           DISPLAY"Q. Quit"
           DISPLAY"----------------------------------------------------"
           DISPLAY"Enter your choice:"
           ACCEPT USER-CHOICE
 
 
           EVALUATE USER-CHOICE
             WHEN "1" PERFORM ADMIN-MENU
             WHEN "2" PERFORM EMPLOYEE-MENU
             WHEN "Q" DISPLAY "Goodbye!"
             WHEN OTHER DISPLAY "Invalid choice, please try again."
             END-EVALUATE.
 
 
      *admin menu
       ADMIN-MENU.
           PERFORM UNTIL USER-CHOICE = "Q"
            DISPLAY"---------------------------------------------------"
            DISPLAY"ADMINISTARTOR FUNCTIONS                            "
            DISPLAY"---------------------------------------------------"
            DISPLAY"1. Add Employee"
            DISPLAY"2. Process Payroll"
            DISPLAY"3. View Payslip"
            DISPLAY"Q. Return to Main Menu"
            DISPLAY"---------------------------------------------------"
            DISPLAY"Enter your choice:"
            ACCEPT USER-CHOICE
 
            EVALUATE USER-CHOICE
            WHEN "1" PERFORM ADD-EMPLOYEE
            WHEN "2" PERFORM PROCESS-PAYROLL
            WHEN "3" PERFORM VIEW-PAYSLIP
            WHEN "Q" CONTINUE
            WHEN OTHER DISPLAY "Invalid choice, please try again!"
            END-EVALUATE
           END-PERFORM.
 
 
       ADD-EMPLOYEE.
            MOVE 'N' TO AUTHENTICATED
           DISPLAY"ADD NEW EMPLOYEE"
           DISPLAY"Enter first name:"
           ACCEPT EMP-NAME
           DISPLAY"Enter surname:"
           ACCEPT EMP-SURNAME
           
           MOVE SPACES TO EMP-ID
           PERFORM UNTIL EMP-ID NOT = SPACES
            DISPLAY "Enter employee ID (5 digits):"
            ACCEPT EMP-ID
           END-PERFORM.
 
           DISPLAY "Enter birthdate (YYYY-MM-DD) :"
           ACCEPT EMP-BIRTHDATE
 
 
           DISPLAY "Enter position (low/intermediate/senior) :"
           ACCEPT EMP-POSITION
 
           DISPLAY "Enter hours worked this month: "
           ACCEPT EMP-HOURS
 
           DISPLAY "Employee added successfully!".
 
      *salary calculation
       CALCULATE-SALARY.
           *> set base salary based on POSITION
           EVALUATE EMP-POSITION
            WHEN "Low"   MOVE 10000 TO EMP-BASE-SALARY
            WHEN "Intermediate"  MOVE 50000 TO EMP-BASE-SALARY
            WHEN "Senior" MOVE 100000 TO EMP-BASE-SALARY
           END-EVALUATE
 
           *> calculate final salary (base * hours/ standard work hours)
           COMPUTE GROSS-SALARY =EMP-BASE-SALARY * EMP-HOURS
 
           MOVE 0 TO BONUS-AMOUNT
           IF EMP-BIRTHDATE(6:2) = CURRENT-MONTH
           MOVE 500 TO BONUS-AMOUNT
           END-IF
 
           MOVE NET-SALARY TO EMP-FINAL-SAL
 
 
           DISPLAY"Process union fee (Y/N) :"
           ACCEPT UNION-FEE-ANSWER
 
           CALL "TAXCALCULATION" USING GROSS-SALARY BONUS-AMOUNT'
           UNION-FEE-ANSWER NET-SALARY.
 
 
 
           *> check for birthday bonus
           IF EMP-BIRTHDATE(6:2) = CURRENT-MONTH
                MOVE 500 TO EMP-BONUS
                ADD EMP-BONUS TO EMP-FINAL-SAL
           END-IF.
 
      *process payroll
       PROCESS-PAYROLL.
           DISPLAY" PROCESS PAYROLL"
           DISPLAY"Enter employee ID:"
           ACCEPT EMP-ID
 
           PERFORM CALCULATE-SALARY
 
           
           IF UNION-FEE-ANSWER = "Y" OR "y"
               SUBTRACT 150 FROM EMP-FINAL-SAL
               DISPLAY "Union fee of R150 deducted"
           END-IF
 
           DISPLAY"Final salary: R", EMP-FINAL-SAL.
 
      *view payslip
       VIEW-PAYSLIP.
            IF NOT IS-AUTHENTICATED
           DISPLAY "Access denied! Please authenticate first."
           ELSE
            IF EMP-NAME = SPACES
           DISPLAY "No payslip data available!"
            ELSE
           DISPLAY "---------------------------------------------------"
           DISPLAY "EMPLOYEE PAYSLIP (CONFIDENTIAL)"
           DISPLAY "---------------------------------------------------"
           DISPLAY "Name: ", FUNCTION TRIM(EMP-NAME), " ",
           FUNCTION TRIM(EMP-SURNAME)
           DISPLAY "ID: ", FUNCTION TRIM(EMP-ID)
           DISPLAY "Position: ", EMP-POSITION
           DISPLAY "Hours worked: ", EMP-HOURS
           DISPLAY "Base salary: R", EMP-BASE-SALARY
           DISPLAY "BONUS: R", EMP-BONUS
           DISPLAY "Final salary: R", EMP-FINAL-SAL
            END-IF
           END-IF.
 
           
           
 
       EMPLOYEE-MENU.
           PERFORM UNTIL USER-CHOICE = "Q"
            DISPLAY"---------------------------------------------------"
            DISPLAY"         EMPLOYEE SELF-SERVICE                     "
           DISPLAY"----------------------------------------------------"
           DISPLAY"1. View my payslip"
           DISPLAY"Q. Return to Main Menu"
           DISPLAY"----------------------------------------------------"
           DISPLAY"Enter your choice: "
           ACCEPT USER-CHOICE
 
           EVALUATE USER-CHOICE
            WHEN "1"
            PERFORM AUTHENTICATE-EMPLOYEE
            IF IS-AUTHENTICATED        
             PERFORM VIEW-PAYSLIP
             ELSE
                DISPLAY "Authentication failed. Please try again."
                END-IF
            WHEN "Q" CONTINUE
            WHEN OTHER
            DISPLAY "Invalid choice, please try again."
            END-EVALUATE
            END-PERFORM.
           
       AUTHENTICATE-EMPLOYEE.
            MOVE "N" TO AUTHENTICATED
            DISPLAY "Enter your Employee ID:"
            ACCEPT INPUT-EMP-ID
 
            *>simple validation for employee id
            IF INPUT-EMP-ID = EMP-ID
                MOVE "Y" TO AUTHENTICATED
                DISPLAY "Authentication successful."
            ELSE
                DISPLAY "Invalid Employee ID."
            END-IF.
            DISPLAY "--------------------------------------------------"
           
 
 
 
 
           DISPLAY "Thank you for using the Byte Bank Payroll System."
           DISPLAY "--------------------------------------------------"
           DISPLAY "Press any key to return to the main menu."
           ACCEPT USER-CHOICE.