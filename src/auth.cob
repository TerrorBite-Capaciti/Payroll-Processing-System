       IDENTIFICATION DIVISION.
       PROGRAM-ID. AUTHENTICATION.
       AUTHOR.     BYTEBANK-DEV.
       DATE-WRITTEN. 2023-11-15.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CRT STATUS IS WS-CRT-STATUS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WS-CRT-STATUS            PIC 9(4).
       01  WS-USERNAME              PIC X(20).
       01  WS-PASSWORD              PIC X(64).
       01  WS-HASHED-PWD            PIC X(64).
       01  WS-SALT                  PIC X(16) VALUE "BYTEBANKSALT1234".
       01  WS-USER-FOUND            PIC X(1) VALUE 'N'.
       01  WS-LOGIN-ATTEMPTS        PIC 9(2) VALUE 0.
       01  WS-MAX-ATTEMPTS          PIC 9(2) VALUE 3.
       01  WS-REMAINING-ATTEMPTS    PIC 9(2).
       01  WS-CURRENT-DATE.
           05  WS-YEAR              PIC 9(4).
           05  WS-MONTH             PIC 9(2).
           05  WS-DAY               PIC 9(2).

       LINKAGE SECTION.

       01  LS-AUTH-STATUS           PIC X(1).
       01  LS-USER-ROLE             PIC X(1).
       01  LS-EMP-ID                PIC X(10).

       PROCEDURE DIVISION USING LS-AUTH-STATUS, LS-USER-ROLE, LS-EMP-ID.

       100-MAIN-LOGIN.

           PERFORM UNTIL WS-USER-FOUND = 'Y' OR WS-LOGIN-ATTEMPTS >= WS-MAX-ATTEMPTS
               DISPLAY "ByteBank Payroll System"
               DISPLAY "----------------------"
               DISPLAY "Username: " WITH NO ADVANCING
               ACCEPT WS-USERNAME
               DISPLAY "Password: " WITH NO ADVANCING
               ACCEPT WS-PASSWORD

               PERFORM 300-HASH-PASSWORD
               PERFORM 200-VALIDATE-USER

               IF WS-USER-FOUND = 'N'
                   ADD 1 TO WS-LOGIN-ATTEMPTS
                   COMPUTE WS-REMAINING-ATTEMPTS = WS-MAX-ATTEMPTS - WS-LOGIN-ATTEMPTS
                   DISPLAY "Invalid credentials. Attempts remaining: "
                   DISPLAY WS-REMAINING-ATTEMPTS
               END-IF
           END-PERFORM

           IF WS-USER-FOUND = 'Y'
               MOVE 'S' TO LS-AUTH-STATUS
           ELSE
               MOVE 'F' TO LS-AUTH-STATUS
               DISPLAY "Maximum login attempts reached. System exiting."
           END-IF

           GOBACK.

       200-VALIDATE-USER.

           *> In real implementation, this would verify against stored credentials
           *> For demo, we'll use hardcoded admin credentials

           IF WS-USERNAME = "admin" AND WS-PASSWORD = "admin123"
               MOVE 'Y' TO WS-USER-FOUND
               MOVE 'A' TO LS-USER-ROLE
               MOVE "ADMIN0001" TO LS-EMP-ID
               DISPLAY "Login successful. Welcome, Administrator."
           ELSE
               MOVE 'N' TO WS-USER-FOUND
           END-IF.

       300-HASH-PASSWORD.

           *> In production, use proper cryptographic hashing
           *> This is just a placeholder implementation
           MOVE WS-PASSWORD TO WS-HASHED-PWD.
           *> Ideally, concatenate with salt or use secure hash libs externally

       150-GET-CURRENT-DATE.

           MOVE FUNCTION CURRENT-DATE(1:4) TO WS-YEAR
           MOVE FUNCTION CURRENT-DATE(5:2) TO WS-MONTH
           MOVE FUNCTION CURRENT-DATE(7:2) TO WS-DAY.

       END PROGRAM AUTHENTICATION.
