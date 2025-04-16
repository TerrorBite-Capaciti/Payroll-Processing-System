IDENTIFICATION DIVISION.
PROGRAM-ID. TAX-CALCULATIONS.
AUTHOR.     BYTEBANK-DEV.
DATE-WRITTEN. 2023-11-15.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  WS-TAX-RATES.
    05  WS-PAYE-RATE              PIC V999 VALUE .000. *> Will be calculated
    05  WS-UIF-RATE               PIC V999 VALUE .010. *> 1% of gross pay
    05  WS-SDL-RATE               PIC V999 VALUE .00025. *> 0.25% of gross pay
    
01  WS-TAX-BRACKETS.
    *> 2023/2024 tax year brackets (annual amounts)
    05  WS-BRACKET-1             PIC 9(7)V99 VALUE  237100.00.
    05  WS-BRACKET-2             PIC 9(7)V99 VALUE  370500.00.
    05  WS-BRACKET-3             PIC 9(7)V99 VALUE  512800.00.
    05  WS-BRACKET-4             PIC 9(7)V99 VALUE  673000.00.
    05  WS-BRACKET-5             PIC 9(7)V99 VALUE  857900.00.
    05  WS-BRACKET-6             PIC 9(7)V99 VALUE 1817000.00.
    
01  WS-CALCULATION-VARS.
    05  WS-TAXABLE-INCOME        PIC 9(7)V99.
    05  WS-ANNUAL-INCOME         PIC 9(7)V99.
    05  WS-CALCULATED-TAX        PIC 9(7)V99.
    05  WS-TEMP-VALUE            PIC 9(7)V99.
    05  WS-MONTHLY-PAYE          PIC 9(7)V99.

LINKAGE SECTION.
01  LS-GROSS-PAY                PIC 9(7)V99.
01  LS-FILING-STATUS            PIC X(1). *> S-single, M-married
01  LS-DEDUCTIONS               PIC 9(7)V99.
01  LS-PAYE                     PIC 9(7)V99. *> Primary tax
01  LS-UIF                      PIC 9(7)V99. *> Unemployment Insurance Fund
01  LS-SDL                      PIC 9(7)V99. *> Skills Development Levy
01  LS-TAX-STATUS              PIC X(1). *> S-success, F-failure

PROCEDURE DIVISION USING LS-GROSS-PAY, LS-FILING-STATUS,
                         LS-DEDUCTIONS, LS-PAYE,
                         LS-UIF, LS-SDL,
                         LS-TAX-STATUS.
100-CALCULATE-TAXES.
    MOVE 'S' TO LS-TAX-STATUS
    PERFORM 200-CALCULATE-PAYE
    PERFORM 300-CALCULATE-UIF
    PERFORM 400-CALCULATE-SDL
    GOBACK.

200-CALCULATE-PAYE.
    *> Convert monthly income to annual for tax bracket calculation
    COMPUTE WS-ANNUAL-INCOME = LS-GROSS-PAY * 12
    COMPUTE WS-TAXABLE-INCOME = WS-ANNUAL-INCOME - LS-DEDUCTIONS
    
    *> Calculate tax based on South African progressive tax brackets
    EVALUATE TRUE
        WHEN WS-TAXABLE-INCOME <= WS-BRACKET-1
            COMPUTE WS-CALCULATED-TAX = WS-TAXABLE-INCOME * 0.18
        WHEN WS-TAXABLE-INCOME <= WS-BRACKET-2
            COMPUTE WS-CALCULATED-TAX = (WS-BRACKET-1 * 0.18) + 
                ((WS-TAXABLE-INCOME - WS-BRACKET-1) * 0.26)
        WHEN WS-TAXABLE-INCOME <= WS-BRACKET-3
            COMPUTE WS-CALCULATED-TAX = (WS-BRACKET-1 * 0.18) + 
                ((WS-BRACKET-2 - WS-BRACKET-1) * 0.26) + 
                ((WS-TAXABLE-INCOME - WS-BRACKET-2) * 0.31)
        WHEN WS-TAXABLE-INCOME <= WS-BRACKET-4
            COMPUTE WS-CALCULATED-TAX = (WS-BRACKET-1 * 0.18) + 
                ((WS-BRACKET-2 - WS-BRACKET-1) * 0.26) + 
                ((WS-BRACKET-3 - WS-BRACKET-2) * 0.31) + 
                ((WS-TAXABLE-INCOME - WS-BRACKET-3) * 0.36)
        WHEN WS-TAXABLE-INCOME <= WS-BRACKET-5
            COMPUTE WS-CALCULATED-TAX = (WS-BRACKET-1 * 0.18) + 
                ((WS-BRACKET-2 - WS-BRACKET-1) * 0.26) + 
                ((WS-BRACKET-3 - WS-BRACKET-2) * 0.31) + 
                ((WS-BRACKET-4 - WS-BRACKET-3) * 0.36) + 
                ((WS-TAXABLE-INCOME - WS-BRACKET-4) * 0.39)
        WHEN WS-TAXABLE-INCOME <= WS-BRACKET-6
            COMPUTE WS-CALCULATED-TAX = (WS-BRACKET-1 * 0.18) + 
                ((WS-BRACKET-2 - WS-BRACKET-1) * 0.26) + 
                ((WS-BRACKET-3 - WS-BRACKET-2) * 0.31) + 
                ((WS-BRACKET-4 - WS-BRACKET-3) * 0.36) + 
                ((WS-BRACKET-5 - WS-BRACKET-4) * 0.39) + 
                ((WS-TAXABLE-INCOME - WS-BRACKET-5) * 0.41)
        WHEN OTHER
            COMPUTE WS-CALCULATED-TAX = (WS-BRACKET-1 * 0.18) + 
                ((WS-BRACKET-2 - WS-BRACKET-1) * 0.26) + 
                ((WS-BRACKET-3 - WS-BRACKET-2) * 0.31) + 
                ((WS-BRACKET-4 - WS-BRACKET-3) * 0.36) + 
                ((WS-BRACKET-5 - WS-BRACKET-4) * 0.39) + 
                ((WS-BRACKET-6 - WS-BRACKET-5) * 0.41) + 
                ((WS-TAXABLE-INCOME - WS-BRACKET-6) * 0.45)
    END-EVALUATE
    
    *> Convert annual tax back to monthly amount
    COMPUTE LS-PAYE = WS-CALCULATED-TAX / 12.

300-CALCULATE-UIF.
    *> UIF is 1% of gross pay, capped at R177.12 per month
    COMPUTE WS-TEMP-VALUE = LS-GROSS-PAY * WS-UIF-RATE
    IF WS-TEMP-VALUE > 177.12
        MOVE 177.12 TO LS-UIF
    ELSE
        MOVE WS-TEMP-VALUE TO LS-UIF
    END-IF.

400-CALCULATE-SDL.
    *> SDL is 0.25% of gross pay, only if payroll exceeds R500,000 annually
    COMPUTE WS-ANNUAL-INCOME = LS-GROSS-PAY * 12
    IF WS-ANNUAL-INCOME > 500000.00
        COMPUTE LS-SDL = LS-GROSS-PAY * WS-SDL-RATE
    ELSE
        MOVE 0 TO LS-SDL
    END-IF.
END PROGRAM TAX-CALCULATIONS.