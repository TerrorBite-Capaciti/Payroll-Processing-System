       IDENTIFICATION DIVISION.
       PROGRAM-ID. USER-MANAGEMENT.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE ASSIGN TO 'users.dat'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE.
       01  USER-RECORD.
           05  USERNAME         PIC X(20).
           05  PASSWORD         PIC X(20).
           05  ROLE             PIC X(10).

       WORKING-STORAGE SECTION.
       01  WS-USERNAME           PIC X(20).
       01  WS-PASSWORD           PIC X(20).
       01  LOGIN-SUCCESSFUL      PIC X(1)  VALUE 'N'.
       01  EOF-FLAG              PIC X(1)  VALUE 'N'.
       01  LOGGED-IN-ROLE        PIC X(10).

       PROCEDURE DIVISION.
       BEGIN.
           OPEN INPUT USER-FILE
           DISPLAY "Enter Username: "
           ACCEPT WS-USERNAME
           DISPLAY "Enter Password: "
           ACCEPT WS-PASSWORD

           PERFORM UNTIL EOF-FLAG = 'Y' OR LOGIN-SUCCESSFUL = 'Y'
               READ USER-FILE INTO USER-RECORD
                   AT END
                       MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       IF USERNAME = WS-USERNAME AND PASSWORD = WS-PASSWORD
                           MOVE ROLE TO LOGGED-IN-ROLE
                           MOVE 'Y' TO LOGIN-SUCCESSFUL
                       END-IF
               END-READ
           END-PERFORM

           CLOSE USER-FILE

           IF LOGIN-SUCCESSFUL = 'Y'
               DISPLAY "Login Successful!"
               DISPLAY "You are logged in as: " LOGGED-IN-ROLE
           ELSE
               DISPLAY "Invalid Login Credentials"
           END-IF

           STOP RUN.
