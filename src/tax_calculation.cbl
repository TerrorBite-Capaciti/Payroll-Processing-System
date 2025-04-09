       IDENTIFICATION DIVISION.
       PROGRAM-ID. TaxCalculation.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Employee-Salary        PIC 9(5)V99 VALUE 0.
       01 Tax-Amount             PIC 9(5)V99 VALUE 0.
       01 Tax-Bracket            PIC 9(2)    VALUE 0.
       01 Tax-Rate              PIC 9(2)    VALUE 0.

       01 Birthday-Bonus         PIC 9(4)    VALUE 500.
       01 Union-Fee              PIC 9(3)    VALUE 150.
       01 Final-Salary           PIC 9(5)V99 VALUE 0.
       01 User-Input             PIC X VALUE SPACE.

       PROCEDURE DIVISION.

      *Step 1:  the Employee's Salary
           DISPLAY "Enter employee salary (Rands): ".
       ACCEPT Employee-Salary.

      *Step 2: Apply Tax Calculation Logic
       IF Employee-Salary <= 195850 THEN


           MOVE 18 TO Tax-Rate
       ELSE IF Employee-Salary <= 305850 THEN
           MOVE 26 TO Tax-Rate
       ELSE IF Employee-Salary <= 423300 THEN
           MOVE 31 TO Tax-Rate
       ELSE IF Employee-Salary <= 555600 THEN
           MOVE 36 TO Tax-Rate
       ELSE IF Employee-Salary <= 708310 THEN
           MOVE 39 TO Tax-Rate
       ELSE
           MOVE 41 TO Tax-Rate
       END-IF.

      *Step 3: Calculate Tax
       COMPUTE Tax-Amount = (Employee-Salary * Tax-Rate) / 100.

      *Step 4: Calculate Final Salary after Tax Deduction
       COMPUTE Final-Salary = Employee-Salary - Tax-Amount.

      *Step 5: Display Tax and Final Salary
       DISPLAY "Employee Salary: " Employee-Salary.
       DISPLAY "Tax Rate: " Tax-Rate "%".
       DISPLAY "Tax Amount: " Tax-Amount.
       DISPLAY "Final Salary after Tax: " Final-Salary.

      *Optional: Apply Union Fee or Birthday Bonus
       DISPLAY "Do you want to apply Union Fee of R150? (Y/N): ".
       ACCEPT User-Input.
       IF User-Input = "Y" OR User-Input = "y" THEN
           COMPUTE Final-Salary = Final-Salary - Union-Fee
       END-IF.

       DISPLAY "Do you want to apply Birthday Bonus? (Y/N): ".
       ACCEPT User-Input.
       IF User-Input = "Y" OR User-Input = "y" THEN
           COMPUTE Final-Salary = Final-Salary + Birthday-Bonus
       END-IF.

       DISPLAY"Final Salary after all deductions/bonuses: " Final-Salary.

       STOP RUN.
