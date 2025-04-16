       IDENTIFICATION DIVISION.
       PROGRAM-ID. TaxCalculation.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 Employee-Salary        PIC 9(7)V99 VALUE 0.
       01 Tax-Amount             PIC 9(7)V99 VALUE 0.
       01 Tax-Bracket            PIC 9(2)    VALUE 0.
       01 Tax-Rate              PIC 9(2)    VALUE 0.

       01 Birthday-Bonus         PIC 9(4)    VALUE 500.
       01 Union-Fee              PIC 9(3)    VALUE 150.
       01 Final-Salary           PIC 9(7)V99 VALUE 0.

       01 User-Input             PIC X VALUE SPACE.

       01 Birth-Day              PIC 99.
       01 Birth-Month            PIC 99.

       01 CURRENT-DATE-FIELDS.
           05 CD-YEAR           PIC 9(4).
           05 CD-MONTH          PIC 9(2).
           05 CD-DAY            PIC 9(2).

       01 WS-Formatted-Salary    PIC Z(6)9.99.
       01 WS-Formatted-Tax       PIC Z(6)9.99.
       01 WS-Formatted-Final     PIC Z(6)9.99.

       PROCEDURE DIVISION.
      * Step 1: Get today’s date
           MOVE FUNCTION CURRENT-DATE(1:4) TO CD-YEAR
           MOVE FUNCTION CURRENT-DATE(5:2) TO CD-MONTH
           MOVE FUNCTION CURRENT-DATE(7:2) TO CD-DAY

      *Step 2:  the Employee's Salary
           DISPLAY "Enter employee salary (Rands): ".
       ACCEPT Employee-Salary.

      *Step 3: Apply Tax Calculation Logic
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

      *Step 4: Calculate Tax
       COMPUTE Tax-Amount = (Employee-Salary * Tax-Rate) / 100.

      *Step 5:Calculate Final Salary after Tax Deduction
       COMPUTE Final-Salary = Employee-Salary - Tax-Amount.

       MOVE Employee-Salary TO WS-Formatted-Salary
       MOVE Tax-Amount      TO WS-Formatted-Tax
       MOVE Final-Salary    TO WS-Formatted-Final

      *Step 6: Display Tax and Final Salary
       DISPLAY "Employee Salary: " "R" WS-Formatted-Salary.
       DISPLAY "Tax Rate: " Tax-Rate "%".
       DISPLAY "Tax Amount: R" WS-Formatted-Tax.
       DISPLAY "Final Salary after Tax: R" WS-Formatted-Final.

      * Step 7: Optional: Apply Union Fee or Birthday Bonus
       DISPLAY "Do you want to apply Union Fee of R150? (Y/N): ".
       ACCEPT User-Input.
       IF User-Input = "Y" OR User-Input = "y" THEN
           COMPUTE Final-Salary = Final-Salary - Union-Fee
       END-IF.

      * Ask for birthday and apply bonus if today
       DISPLAY "Enter employee birth day (DD): ".
           ACCEPT Birth-Day.
           DISPLAY "Enter employee birth month (MM): ".
           ACCEPT Birth-Month.


           IF Birth-Day = CD-DAY AND Birth-Month = CD-MONTH THEN
            DISPLAY "🎉 It's the employee's birthday! Applying bonus."
               COMPUTE Final-Salary = Final-Salary + Birthday-Bonus
           ELSE
               DISPLAY "No birthday bonus applied."
           END-IF.

      * Final Salary Display
       MOVE Final-Salary TO WS-Formatted-Final
       DISPLAY"Final Salary after all deductions/bonuses: " Final-Salary-String.

       STOP RUN.
