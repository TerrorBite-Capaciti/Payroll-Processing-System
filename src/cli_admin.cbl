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
           DISPLAY "            WELCOME TO BYTE BANK PAYROLL SYSTEM   ".
           DISPLAY "--------------------------------------------------".
           DISPLAY " ".
           DISPLAY "Please select your role:".
           DISPLAY "1. Admin Login".
           DISPLAY "2. Employee Login".
           DISPLAY " ".
           DISPLAY "Enter your choice: ".
           ACCEPT USER-CHOICE.
