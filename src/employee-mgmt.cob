       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-MGMT.
       AUTHOR.     BYTEBANK-DEV.
      *****************************************************************
      *****************************************************************
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 
           SELECT EMPLOYEE-FILE ASSIGN TO "data/employees.dat"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS DYNAMIC
              RECORD KEY IS EMP-ID
              FILE STATUS IS FILE-STATUS-CODE
              .
      *****************************************************************
      *****************************************************************
       DATA DIVISION.
       FILE SECTION. 
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05  EMP-ID                    PIC 9(10).
           05  EMP-NAME                  PIC X(50).
           05  EMP-SURNAME               PIC X(50).
           05  EMP-POSITION-TYPE         PIC X(20).
           05  EMP-BIRTH-DATE            PIC X(8).
           05  EMP-AGE                   PIC 9(2).
      *****   THIS IS AN OPTIONAL DATA-ITEM    ****
           05 EMP-UNION-FEE              PIC 9(5).

       WORKING-STORAGE SECTION.
      *****DATA ITEMS FOR INPUTTING AND WRITING TO FILE
       01 WS-EMPLOYEE.
           05  WS-EMP-ID                       PIC 9(10).
           05  WS-EMP-NAME                     PIC X(50).
           05  WS-EMP-SURNAME                  PIC X(50).
           05  WS-EMP-POSITION-TYPE            PIC X(20).
           05  WS-EMP-BIRTH-DATE               PIC X(8).
           05  WS-EMP-AGE                      PIC 9(2).
      *****   THIS IS AN OPTIONAL DATA-ITEM    ****
           05  WS-EMP-UNION-FEE                PIC 9(5).

       77 WS-EOF             PIC X(1) VALUE 'N'.
       77 USER-INPUT         PIC X(1).
       77 FILE-STATUS-CODE   PIC X(2).
       77 FILE-EXISTS        PIC X(1) VALUE 'N'.
       01 WS-LOW-KEY         PIC 9(10) VALUE ZEROS.

       PROCEDURE DIVISION.
       BEGIN.
           PERFORM CHECK-FILE-EXISTS
           IF FILE-EXISTS = 'N'
              PERFORM CREATE-EMPLOYEE-LIST
           END-IF.

           PERFORM UNTIL USER-INPUT = "4"
           DISPLAY "========== EMPLOYEE MANAGEMENT MODULE ============"
           DISPLAY " "
           DISPLAY "1. LIST ALL EMPLOYEES"
           DISPLAY "2. ADD A NEW EMPLOYEE"
           DISPLAY "3. SEARCH EMPLOYEE BY ID"
           DISPLAY " "
           DISPLAY "4. RETURN TO MAIN MENU"
           DISPLAY " "
           DISPLAY "CHOICE: " WITH NO ADVANCING 
           ACCEPT USER-INPUT

           EVALUATE USER-INPUT
             WHEN "1"
              PERFORM LIST-ALL-EMPLOYEES
             
             WHEN "2"
              PERFORM ADD-NEW-EMPLOYEE

             WHEN "3"
              PERFORM SEARCH-BY-ID

      * ALLOWS TO RETURN BACK TO THE CALLING PROGRAM (i.e, MAIN MENU)
             WHEN "4"
             STOP RUN

             WHEN OTHER 
             DISPLAY "INVALID INPUT"
           END-EVALUATE

           END-PERFORM
           .
      *****************************************************************
      *****************************************************************
       CHECK-FILE-EXISTS.
           OPEN INPUT EMPLOYEE-FILE

           IF FILE-STATUS-CODE = "00"
              CLOSE EMPLOYEE-FILE 
              MOVE 'Y' TO FILE-EXISTS
           ELSE
              MOVE 'N' TO FILE-EXISTS
           END-IF
           .
      *****************************************************************
      *****************************************************************
       CREATE-EMPLOYEE-LIST.
      * OUTPUT CREATES FILE OR OVERWRITES EXISTING
      * HERE, IT IS DETERMINED FILE DOESN'T EXIST
      * SO IT IS SAFE TO USE OUTPUT
           OPEN OUTPUT EMPLOYEE-FILE
           DISPLAY "CREATING FILE"

           IF FILE-STATUS-CODE = "00"
              DISPLAY "FILE CREATED. code: " FILE-STATUS-CODE
           CLOSE EMPLOYEE-FILE
           ELSE
              DISPLAY "ERROR CREATING FILE. code: " FILE-STATUS-CODE
           END-IF
           .
      *****************************************************************
      *****************************************************************
       LIST-ALL-EMPLOYEES.
           OPEN I-O EMPLOYEE-FILE
           IF FILE-STATUS-CODE = "00" OR FILE-STATUS-CODE = "97"
              DISPLAY " "
              DISPLAY "BYTEBANK EMPLOYEES"
              DISPLAY " "
           
           MOVE WS-LOW-KEY TO EMP-ID
           START EMPLOYEE-FILE KEY >= EMP-ID
               INVALID KEY 
                   DISPLAY "NO RECORDS FOUND"
                   MOVE 'Y' TO WS-EOF
               NOT INVALID KEY 
                   CONTINUE
           END-START
             
           PERFORM UNTIL WS-EOF = 'Y'
           READ EMPLOYEE-FILE NEXT RECORD 
              AT END MOVE 'Y' TO WS-EOF
              DISPLAY " "
              DISPLAY "||||||||||||||||||||||||||||||||||||||||||||||"
              DISPLAY "                END OF FILE"
              DISPLAY "||||||||||||||||||||||||||||||||||||||||||||||"
              DISPLAY " "
              NOT AT END
                 DISPLAY " "
                 DISPLAY "============================================="
                 DISPLAY "ID: " EMP-ID
                 DISPLAY "NAME: " EMP-NAME
                 DISPLAY "SURNAME: " EMP-SURNAME
                 DISPLAY "POSITION: " EMP-POSITION-TYPE
                 DISPLAY "BIRTHDATE: " EMP-BIRTH-DATE
                 DISPLAY "AGE: " EMP-AGE
                 DISPLAY "UNION FEE: " EMP-UNION-FEE
                 DISPLAY "============================================="
           END-READ
           CLOSE EMPLOYEE-FILE
           END-PERFORM
           
           ELSE
              DISPLAY "ERROR READING EMPLOYEES LIST. code: "
                 FILE-STATUS-CODE
              GOBACK
           END-IF
           .
      *****************************************************************
      *****************************************************************
       ADD-NEW-EMPLOYEE.
           OPEN I-O EMPLOYEE-FILE
           DISPLAY "ENTER EMPLOYEE'S FIRST NAME: " WITH NO ADVANCING 
           ACCEPT WS-EMP-NAME

           DISPLAY "ENTER EMPLOYEE'S SURNAME: " WITH NO ADVANCING 
           ACCEPT WS-EMP-SURNAME

           DISPLAY "ENTER EMPLOYEE'S POSITION TYPE (INTERN, "
           "INTERMEDIATE, SENIOR): " WITH NO ADVANCING 
           ACCEPT WS-EMP-POSITION-TYPE

           DISPLAY "ENTER EMPLOYEE'S BIRTHDATE (YYYY/MM/DD): " 
              WITH NO ADVANCING 
           ACCEPT WS-EMP-BIRTH-DATE

           DISPLAY "ENTER EMPLOYEE'S AGE: " WITH NO ADVANCING 
           ACCEPT WS-EMP-AGE

           DISPLAY "(OPTIONAL) ENTER EMPLOYEE'S UNION FEE: "
            WITH NO ADVANCING 
           ACCEPT WS-EMP-UNION-FEE

           MOVE WS-EMP-NAME           TO EMP-NAME
           MOVE WS-EMP-SURNAME        TO EMP-SURNAME
           MOVE WS-EMP-POSITION-TYPE  TO EMP-POSITION-TYPE
           MOVE WS-EMP-BIRTH-DATE     TO EMP-BIRTH-DATE
           MOVE WS-EMP-AGE            TO EMP-AGE
           MOVE WS-EMP-UNION-FEE      TO EMP-UNION-FEE

           WRITE EMPLOYEE-RECORD
              INVALID KEY
                 DISPLAY WS-EMP-NAME " ALREADY EXISTS!"

              NOT INVALID KEY
                 DISPLAY "ADDING " WS-EMP-NAME WS-EMP-SURNAME
           END-WRITE
           CLOSE EMPLOYEE-FILE
           GOBACK
           .
      *****************************************************************
      *****************************************************************
       SEARCH-BY-ID.

      * INITIALISING VARIABLES FOR SEARCH AND DISPLAY
           INITIALIZE WS-EMP-ID
           DISPLAY "ID: " WS-EMP-ID
           INITIALIZE WS-EMP-NAME
           INITIALIZE WS-EMP-SURNAME
           INITIALIZE WS-EMP-POSITION-TYPE
           INITIALIZE WS-EMP-BIRTH-DATE
           INITIALIZE WS-EMP-AGE
           INITIALIZE WS-EMP-UNION-FEE

           OPEN I-O EMPLOYEE-FILE
           DISPLAY "==================================================="
           DISPLAY "                     SEARCH                        "
           DISPLAY "==================================================="

      * CHECK IF FILE HAS BEEN OPENED CORRECTLY
           IF FILE-STATUS-CODE = "00" OR FILE-STATUS-CODE = "97"
              DISPLAY " "
              DISPLAY "ENTER EMPLOYEE ID: " WITH NO ADVANCING 
              ACCEPT WS-EMP-ID

              MOVE WS-EMP-ID TO EMP-ID

              READ EMPLOYEE-FILE INTO EMPLOYEE-RECORD
                 KEY IS EMP-ID

                 INVALID KEY
                    DISPLAY "EMPLOYEE NOT FOUND!"

                 NOT INVALID KEY 
                 DISPLAY " "
                 DISPLAY "============================================="
                 DISPLAY "ID: " EMP-ID
                 DISPLAY "NAME: " EMP-NAME
                 DISPLAY "SURNAME: " EMP-SURNAME
                 DISPLAY "POSITION: " EMP-POSITION-TYPE
                 DISPLAY "BIRTHDATE: " EMP-BIRTH-DATE
                 DISPLAY "AGE: " EMP-AGE
                 DISPLAY "UNION FEE: " EMP-UNION-FEE
                 DISPLAY "============================================="
                 DISPLAY " "
              END-READ
           ELSE
              DISPLAY "ERROR READING FILE. code: " FILE-STATUS-CODE
           END-IF 
           CLOSE EMPLOYEE-FILE
           .
       END PROGRAM EMPLOYEE-MGMT.
