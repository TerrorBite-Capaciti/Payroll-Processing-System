       IDENTIFICATION DIVISION.
       PROGRAM-ID. TAXCALCULATION.
       AUTHOR.     [Your Name].
       DATE-WRITTEN. [Date].

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TAX-BRACKETS.
           05  TAX-BRACKET OCCURS 5 TIMES INDEXED BY TAX-INDEX.
               10  TB-LOWER-LIMIT    PIC 9(7)V99.
               10  TB-UPPER-LIMIT    PIC 9(7)V99.
               10  TB-BASE-TAX       PIC 9(7)V99.
               10  TB-RATE           PIC 9V999.

       01  SOCIAL-SECURITY-RATES.
           05  SS-RATE              PIC V999 VALUE .062.
           05  SS-WAGE-BASE         PIC 9(7)V99 VALUE 147000.00.
           05  MEDICARE-RATE        PIC V999 VALUE .0145.
           05  MEDICARE-ADD-RATE    PIC V999 VALUE .009.
           05  MEDICARE-THRESHOLD   PIC 9(7)V99 VALUE 200000.00.

       01  TAX-CONSTANTS.
           05  STANDARD-DEDUCTION   PIC 9(5)V99 VALUE 12950.00.
           05  DEPENDENT-CREDIT     PIC 9(4)V99 VALUE 500.00.

       01  WS-TAXABLE-EARNINGS      PIC 9(7)V99.

       LINKAGE SECTION.
       01  LS-TAX-PARAMS.
           05  LS-GROSS-PAY         PIC 9(7)V99.
           05  LS-TAX-CODE          PIC X(2).
           05  LS-MARITAL-STATUS    PIC X.
           05  LS-NUM-DEPENDENTS    PIC 99.
           05  LS-YTD-EARNINGS      PIC 9(8)V99.
           05  LS-TAX-AMOUNT        PIC 9(7)V99.
           05  LS-SS-AMOUNT         PIC 9(7)V99.
           05  LS-MEDICARE-AMOUNT   PIC 9(7)V99.
           05  LS-NET-TAX           PIC 9(7)V99.
           05  LS-ERROR-CODE        PIC XX.

       PROCEDURE DIVISION USING LS-TAX-PARAMS.
       000-CALCULATE-TAXES.
           PERFORM 100-INITIALIZE-TAX-TABLES
           PERFORM 200-VALIDATE-INPUTS
           IF LS-ERROR-CODE = '00'
               PERFORM 300-CALCULATE-INCOME-TAX
               PERFORM 400-CALCULATE-SOCIAL-SECURITY
               PERFORM 500-CALCULATE-MEDICARE
               PERFORM 600-CALCULATE-TOTAL-TAXES
           END-IF
           GOBACK.

       100-INITIALIZE-TAX-TABLES.
           MOVE 0        TO TB-LOWER-LIMIT(1)
           MOVE 10275.00 TO TB-UPPER-LIMIT(1)
           MOVE 0        TO TB-BASE-TAX(1)
           MOVE 0.10     TO TB-RATE(1)

           MOVE 10275.01 TO TB-LOWER-LIMIT(2)
           MOVE 41775.00 TO TB-UPPER-LIMIT(2)
           MOVE 1027.50  TO TB-BASE-TAX(2)
           MOVE 0.12     TO TB-RATE(2)

           MOVE 41775.01 TO TB-LOWER-LIMIT(3)
           MOVE 89075.00 TO TB-UPPER-LIMIT(3)
           MOVE 4807.50  TO TB-BASE-TAX(3)
           MOVE 0.22     TO TB-RATE(3)

           MOVE 89075.01 TO TB-LOWER-LIMIT(4)
           MOVE 170050.00 TO TB-UPPER-LIMIT(4)
           MOVE 15213.50 TO TB-BASE-TAX(4)
           MOVE 0.24     TO TB-RATE(4)

           MOVE 170050.01 TO TB-LOWER-LIMIT(5)
           MOVE 9999999.99 TO TB-UPPER-LIMIT(5)
           MOVE 34647.50 TO TB-BASE-TAX(5)
           MOVE 0.32     TO TB-RATE(5).

       200-VALIDATE-INPUTS.
           EVALUATE LS-TAX-CODE
               WHEN 'T1' 'T2' 'T3' 'T4'
                   MOVE '00' TO LS-ERROR-CODE
               WHEN OTHER
                   MOVE 'TC' TO LS-ERROR-CODE
           END-EVALUATE.

       300-CALCULATE-INCOME-TAX.
           COMPUTE LS-GROSS-PAY = LS-GROSS-PAY - STANDARD-DEDUCTION
           IF LS-GROSS-PAY < 0
               MOVE 0 TO LS-GROSS-PAY
           END-IF

           SET TAX-INDEX TO 1
           SEARCH TAX-BRACKET
               AT END
                   MOVE 0 TO LS-TAX-AMOUNT
               WHEN LS-GROSS-PAY >= TB-LOWER-LIMIT(TAX-INDEX) AND
                    LS-GROSS-PAY <= TB-UPPER-LIMIT(TAX-INDEX)
                   COMPUTE LS-TAX-AMOUNT = 
                       TB-BASE-TAX(TAX-INDEX) + 
                       ((LS-GROSS-PAY - TB-LOWER-LIMIT(TAX-INDEX)) * 
                        TB-RATE(TAX-INDEX))
           END-SEARCH

           IF LS-NUM-DEPENDENTS > 0
               COMPUTE LS-TAX-AMOUNT = LS-TAX-AMOUNT - 
                                      (LS-NUM-DEPENDENTS * DEPENDENT-CREDIT)
               IF LS-TAX-AMOUNT < 0
                   MOVE 0 TO LS-TAX-AMOUNT
               END-IF
           END-IF.

       400-CALCULATE-SOCIAL-SECURITY.
           IF LS-YTD-EARNINGS < SS-WAGE-BASE
               COMPUTE LS-SS-AMOUNT = 
                   (SS-WAGE-BASE - LS-YTD-EARNINGS) * SS-RATE
               IF LS-SS-AMOUNT > (LS-GROSS-PAY * SS-RATE)
                   COMPUTE LS-SS-AMOUNT = LS-GROSS-PAY * SS-RATE
               END-IF
           ELSE
               MOVE 0 TO LS-SS-AMOUNT
           END-IF.

       500-CALCULATE-MEDICARE.
           COMPUTE LS-MEDICARE-AMOUNT = LS-GROSS-PAY * MEDICARE-RATE

           IF LS-YTD-EARNINGS + LS-GROSS-PAY > MEDICARE-THRESHOLD
               COMPUTE WS-TAXABLE-EARNINGS = 
                   (LS-YTD-EARNINGS + LS-GROSS-PAY) - MEDICARE-THRESHOLD
               IF WS-TAXABLE-EARNINGS > LS-GROSS-PAY
                   COMPUTE WS-TAXABLE-EARNINGS = LS-GROSS-PAY
               END-IF
               COMPUTE LS-MEDICARE-AMOUNT = LS-MEDICARE-AMOUNT + 
                                           (WS-TAXABLE-EARNINGS * 
                                            MEDICARE-ADD-RATE)
           END-IF.

       600-CALCULATE-TOTAL-TAXES.
           COMPUTE LS-NET-TAX = LS-TAX-AMOUNT + LS-SS-AMOUNT + 
                               LS-MEDICARE-AMOUNT.