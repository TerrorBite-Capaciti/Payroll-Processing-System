       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONFIG.
       AUTHOR.     BYTEBANK-DEV.
       DATE-WRITTEN. 2023-11-15.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS WS-CRT-STATUS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CRT-STATUS          PIC 9(4).
       01  WS-CONFIG-CHOICE       PIC 9(1).
       01  WS-TAX-RATES.
           05  WS-FED-TAX-RATE    PIC 99V999 VALUE 0.120.
           05  WS-STATE-TAX-RATE  PIC 99V999 VALUE 0.050.
           05  WS-FICA-RATE       PIC 99V999 VALUE 0.0765.
       01  WS-BENEFIT-RATES.
           05  WS-401K-MAX-RATE   PIC 99V999 VALUE 0.150.
           05  WS-HEALTH-INS-COST PIC 9(4)V99 VALUE 200.00.
       01  WS-SYSTEM-SETTINGS.
           05  WS-PAY-PERIOD-LEN  PIC 99 VALUE 14.
           05  WS-MAX-LOGIN-ATT   PIC 9 VALUE 3.
           05  WS-PWD-EXPIRY      PIC 999 VALUE 90.
       01  WS-TEMP-RATE           PIC 999V999.
       01  WS-TEMP-VALUE          PIC 9(5)V99.
       01  WS-PERCENT             PIC Z9.99.

       LINKAGE SECTION.
       01  LS-USER-ROLE           PIC X(1).
       01  LS-EMP-ID              PIC X(10).

       PROCEDURE DIVISION USING LS-USER-ROLE, LS-EMP-ID.
       100-MAIN-CONFIG.
           IF LS-USER-ROLE NOT = 'A'
               DISPLAY "Access denied. Administrator only."
               GOBACK
           END-IF.

           PERFORM UNTIL WS-CONFIG-CHOICE = 0
               DISPLAY " "
               DISPLAY "ByteBank Configuration Menu"
               DISPLAY "-------------------------"
               DISPLAY "1. Tax Rates Configuration"
               DISPLAY "2. Benefit Settings"
               DISPLAY "3. System Parameters"
               DISPLAY "0. Return to Main Menu"
               DISPLAY "Enter choice: " WITH NO ADVANCING
               ACCEPT WS-CONFIG-CHOICE

               EVALUATE WS-CONFIG-CHOICE
                   WHEN 1 PERFORM 200-TAX-CONFIG
                   WHEN 2 PERFORM 300-BENEFIT-CONFIG
                   WHEN 3 PERFORM 400-SYSTEM-CONFIG
                   WHEN 0 CONTINUE
                   WHEN OTHER DISPLAY "Invalid choice"
               END-EVALUATE
           END-PERFORM.
           GOBACK.

       200-TAX-CONFIG.
           MOVE WS-FED-TAX-RATE TO WS-TEMP-VALUE
           COMPUTE WS-PERCENT = WS-TEMP-VALUE * 100
           DISPLAY "Current Tax Rates:"
           DISPLAY "Federal:   " WS-PERCENT "%"
           MOVE WS-STATE-TAX-RATE TO WS-TEMP-VALUE
           COMPUTE WS-PERCENT = WS-TEMP-VALUE * 100
           DISPLAY "State:     " WS-PERCENT "%"
           MOVE WS-FICA-RATE TO WS-TEMP-VALUE
           COMPUTE WS-PERCENT = WS-TEMP-VALUE * 100
           DISPLAY "FICA:      " WS-PERCENT "%"
           DISPLAY " "
           DISPLAY "Enter new federal tax rate (e.g., 12.5): " 
                   WITH NO ADVANCING
           ACCEPT WS-TEMP-RATE
           COMPUTE WS-FED-TAX-RATE = WS-TEMP-RATE / 100
           DISPLAY "Enter new state tax rate: " WITH NO ADVANCING
           ACCEPT WS-TEMP-RATE
           COMPUTE WS-STATE-TAX-RATE = WS-TEMP-RATE / 100
           DISPLAY "Tax rates updated.".

       300-BENEFIT-CONFIG.
           MOVE WS-401K-MAX-RATE TO WS-TEMP-VALUE
           COMPUTE WS-PERCENT = WS-TEMP-VALUE * 100
           DISPLAY "Current Benefit Settings:"
           DISPLAY "401K Max Rate: " WS-PERCENT "%"
           DISPLAY "Health Ins Cost: $" WS-HEALTH-INS-COST
           DISPLAY " "
           DISPLAY "Enter new 401K max rate (e.g., 15.0): " 
                   WITH NO ADVANCING
           ACCEPT WS-TEMP-RATE
           COMPUTE WS-401K-MAX-RATE = WS-TEMP-RATE / 100
           DISPLAY "Enter new health insurance cost: " WITH NO ADVANCING
           ACCEPT WS-HEALTH-INS-COST
           DISPLAY "Benefit settings updated.".

       400-SYSTEM-CONFIG.
           DISPLAY "Current System Settings:"
           DISPLAY "Pay Period Length: " WS-PAY-PERIOD-LEN " days"
           DISPLAY "Max Login Attempts: " WS-MAX-LOGIN-ATT
           DISPLAY "Password Expiry:   " WS-PWD-EXPIRY " days"
           DISPLAY " "
           DISPLAY "Enter new pay period length (days): " WITH NO ADVANCING
           ACCEPT WS-PAY-PERIOD-LEN
           DISPLAY "Enter new max login attempts: " WITH NO ADVANCING
           ACCEPT WS-MAX-LOGIN-ATT
           DISPLAY "Enter new password expiry (days): " WITH NO ADVANCING
           ACCEPT WS-PWD-EXPIRY
           DISPLAY "System settings updated.".
       END PROGRAM CONFIG.