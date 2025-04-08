       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEAVEDEDUCTIONS.
       AUTHOR.     [Your Name].
       DATE-WRITTEN. [Date].
      *==============================================================
      * Leave Deductions Module
      * Calculates salary deductions for unpaid leave
      * Integrates with:
      * - payroll.cbl (main processing)
      * - user_management.cbl (for authorization)
      *==============================================================

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Leave Policy Constants
       01  LEAVE-CONSTANTS.
           05  WORK-DAYS-PER-MONTH   PIC 9(02) VALUE 22.
           05  MAX-ANNUAL-LEAVE      PIC 9(02) VALUE 30.
           05  LEAVE-DEDUCTION-RATE  PIC 9V99  VALUE 1.5. *> 1.5x for unpaid leave

      * Calculation Variables
       01  WS-CALCULATION-VARS.
           05  WS-DAILY-RATE         PIC 9(7)V99.
           05  WS-LEAVE-DEDUCTION    PIC 9(7)V99.
           05  WS-ADJUSTED-BALANCE   PIC S9(3).

       LINKAGE SECTION.
      * Parameters from Payroll Module
       01  LS-LEAVE-PARAMS.
           05  LS-CURRENT-BALANCE    PIC S9(3).  *> Signed for +/-
           05  LS-UNPAID-LEAVE       PIC 9(2).
           05  LS-BASE-SALARY        PIC 9(7)V99.
           05  LS-DEDUCTION-AMOUNT   PIC 9(7)V99.
           05  LS-NEW-BALANCE        PIC S9(3).
           05  LS-ERROR-CODE         PIC X(2).
               88  INVALID-LEAVE-REQUEST   VALUE 'LE'.
               88  INSUFFICIENT-BALANCE    VALUE 'IB'.
               88  SUCCESSFUL-CALCULATION  VALUE '00'.

       PROCEDURE DIVISION USING LS-LEAVE-PARAMS.
       000-CALCULATE-LEAVE-DEDUCTIONS.
           PERFORM 100-VALIDATE-LEAVE-REQUEST
           IF SUCCESSFUL-CALCULATION
               PERFORM 200-CALCULATE-DAILY-RATE
               PERFORM 300-CALCULATE-DEDUCTIONS
               PERFORM 400-UPDATE-LEAVE-BALANCE
           END-IF
           GOBACK.

       100-VALIDATE-LEAVE-REQUEST.
      * Validate unpaid leave days are reasonable
           IF LS-UNPAID-LEAVE > 30
               MOVE 'LE' TO LS-ERROR-CODE
               MOVE 0 TO LS-DEDUCTION-AMOUNT
               MOVE LS-CURRENT-BALANCE TO LS-NEW-BALANCE
           ELSE
               MOVE '00' TO LS-ERROR-CODE
           END-IF.

       200-CALCULATE-DAILY-RATE.
           COMPUTE WS-DAILY-RATE = LS-BASE-SALARY / WORK-DAYS-PER-MONTH.

       300-CALCULATE-DEDUCTIONS.
      * Calculate deduction at penalty rate for unpaid leave
           COMPUTE WS-LEAVE-DEDUCTION = WS-DAILY-RATE * LS-UNPAID-LEAVE 
                                      * LEAVE-DEDUCTION-RATE
           MOVE WS-LEAVE-DEDUCTION TO LS-DEDUCTION-AMOUNT.

       400-UPDATE-LEAVE-BALANCE.
      * Check if leave can be deducted from balance first
           IF LS-CURRENT-BALANCE >= LS-UNPAID-LEAVE
               COMPUTE WS-ADJUSTED-BALANCE = LS-CURRENT-BALANCE - 
                                            LS-UNPAID-LEAVE
               MOVE 0 TO LS-DEDUCTION-AMOUNT  *> No deduction if taken from balance
           ELSE
               COMPUTE WS-ADJUSTED-BALANCE = LS-CURRENT-BALANCE
               IF LS-CURRENT-BALANCE > 0
                   SUBTRACT LS-CURRENT-BALANCE FROM LS-UNPAID-LEAVE
                      GIVING LS-UNPAID-LEAVE
                   COMPUTE WS-LEAVE-DEDUCTION = WS-DAILY-RATE * 
                                              LS-UNPAID-LEAVE * 
                                              LEAVE-DEDUCTION-RATE
                   MOVE WS-LEAVE-DEDUCTION TO LS-DEDUCTION-AMOUNT
                   MOVE 0 TO WS-ADJUSTED-BALANCE
               END-IF
           END-IF
           MOVE WS-ADJUSTED-BALANCE TO LS-NEW-BALANCE.