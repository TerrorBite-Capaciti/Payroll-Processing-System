       01 TAX-INPUT.
           05 TI-SALARY      PIC 9(7)V99.
           05 TI-TAX-CODE    PIC X(1).
           05 TI-BIRTHDAY    PIC X(4).  *> DDMM
           
       01 TAX-OUTPUT.
           05 TO-TAX-RATE    PIC 9(2).
           05 TO-TAX-AMOUNT  PIC 9(7)V99.
           05 TO-NET-PAY     PIC 9(7)V99.
           05 TO-BONUS       PIC 9(4).