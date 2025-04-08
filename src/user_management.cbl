       IDENTIFICATION DIVISION.
       PROGRAM-ID. USERMANAGEMENT.
       AUTHOR.     [Your Name].
       DATE-WRITTEN. [Date].

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE ASSIGN TO 'USERFILE'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS USER-ID
               FILE STATUS IS USER-FILE-STATUS.

           SELECT AUDIT-LOG-FILE ASSIGN TO 'AUDITLOG'
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS AUDIT-LOG-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE.
       01  USER-RECORD.
           05  USER-ID               PIC X(08).
           05  USER-NAME             PIC X(40).
           05  USER-PASSWORD         PIC X(16).
           05  USER-ROLE             PIC X(01).
               88  ADMIN-USER        VALUE 'A'.
               88  HR-USER           VALUE 'H'.
               88  MANAGER-USER      VALUE 'M'.
               88  EMPLOYEE-USER     VALUE 'E'.
           05  USER-DEPARTMENT       PIC X(15).
           05  USER-STATUS           PIC X(01).
               88  ACTIVE-USER       VALUE 'A'.
               88  LOCKED-USER       VALUE 'L'.
               88  EXPIRED-USER      VALUE 'E'.
           05  USER-LAST-LOGIN       PIC 9(08).
           05  USER-FAILED-ATTEMPTS  PIC 9(01).
           05  USER-PW-CHANGE-DATE   PIC 9(08).
           05  FILLER                PIC X(20).

       FD  AUDIT-LOG-FILE.
       01  AUDIT-LOG-RECORD.
           05  AL-TIMESTAMP         PIC 9(14).
           05  AL-USER-ID           PIC X(08).
           05  AL-ACTION            PIC X(10).
           05  AL-DETAILS           PIC X(50).
           05  AL-STATUS            PIC X(01).
               88  AL-SUCCESS       VALUE 'S'.
               88  AL-FAILURE       VALUE 'F'.
           05  AL-IP-ADDRESS        PIC X(15).
           05  FILLER               PIC X(20).

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-VARS.
           05  USER-FILE-STATUS     PIC XX.
           05  AUDIT-LOG-STATUS     PIC XX.

       01  SECURITY-CONSTANTS.
           05  MAX-FAILED-ATTEMPTS  PIC 9(01) VALUE 3.
           05  PW-EXPIRY-DAYS       PIC 9(03) VALUE 90.

       01  PROGRAM-CONTROL.
           05  WS-CURRENT-DATE      PIC 9(08).
           05  WS-CURRENT-TIME      PIC 9(06).
           05  WS-USER-FOUND        PIC X(01).
               88  USER-FOUND        VALUE 'Y'.
               88  USER-NOT-FOUND    VALUE 'N'.

       LINKAGE SECTION.
       01  LS-AUTH-PARAMS.
           05  LS-USERID            PIC X(08).
           05  LS-PASSWORD          PIC X(16).
           05  LS-IP-ADDRESS        PIC X(15).
           05  LS-AUTH-RESULT       PIC X(01).
               88  AUTH-SUCCESS      VALUE 'S'.
               88  AUTH-FAILURE      VALUE 'F'.
           05  LS-USER-ROLE         PIC X(01).
           05  LS-ERROR-MESSAGE     PIC X(50).

       01  LS-ADMIN-PARAMS.
           05  LS-ADMIN-ACTION      PIC X(10).
               88  CREATE-USER       VALUE 'CREATE'.
               88  UPDATE-USER       VALUE 'UPDATE'.
               88  DELETE-USER       VALUE 'DELETE'.
               88  LOCK-USER         VALUE 'LOCK'.
               88  UNLOCK-USER       VALUE 'UNLOCK'.
               88  RESET-PW          VALUE 'RESETPW'.
           05  LS-TARGET-USERID     PIC X(08).
           05  LS-NEW-DATA          PIC X(50).
           05  LS-ADMIN-RESULT      PIC X(01).
               88  ADMIN-SUCCESS     VALUE 'S'.
               88  ADMIN-FAILURE     VALUE 'F'.
           05  LS-ADMIN-MESSAGE     PIC X(50).

       PROCEDURE DIVISION USING LS-AUTH-PARAMS LS-ADMIN-PARAMS.
       000-MAIN-USER-MANAGEMENT.
           EVALUATE TRUE
               WHEN LS-USERID NOT = SPACES
                   PERFORM 100-AUTHENTICATE-USER
               WHEN LS-ADMIN-ACTION NOT = SPACES
                   PERFORM 200-ADMINISTER-USER
               WHEN OTHER
                   MOVE 'F' TO LS-AUTH-RESULT
                   MOVE 'Invalid function call' TO LS-ERROR-MESSAGE
           END-EVALUATE
           GOBACK.

       100-AUTHENTICATE-USER.
           PERFORM 110-INITIALIZE-AUTH
           PERFORM 120-VALIDATE-USER
           IF AUTH-SUCCESS
               PERFORM 130-UPDATE-USER-RECORD
               PERFORM 140-LOG-AUTH-SUCCESS
           ELSE
               PERFORM 150-HANDLE-FAILED-LOGIN
               PERFORM 160-LOG-AUTH-FAILURE
           END-IF.

       110-INITIALIZE-AUTH.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           MOVE FUNCTION CURRENT-DATE(9:6) TO WS-CURRENT-TIME
           MOVE 'N' TO WS-USER-FOUND.

       120-VALIDATE-USER.
           MOVE LS-USERID TO USER-ID
           READ USER-FILE
               INVALID KEY
                   MOVE 'User not found' TO LS-ERROR-MESSAGE
                   MOVE 'F' TO LS-AUTH-RESULT
               NOT INVALID KEY
                   MOVE 'Y' TO WS-USER-FOUND
                   PERFORM 121-CHECK-ACCOUNT-STATUS
                   IF AUTH-SUCCESS
                       PERFORM 122-VERIFY-PASSWORD
                   END-IF
           END-READ.

       121-CHECK-ACCOUNT-STATUS.
           EVALUATE TRUE
               WHEN LOCKED-USER
                   MOVE 'Account locked' TO LS-ERROR-MESSAGE
                   MOVE 'F' TO LS-AUTH-RESULT
               WHEN EXPIRED-USER
                   MOVE 'Password expired' TO LS-ERROR-MESSAGE
                   MOVE 'F' TO LS-AUTH-RESULT
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       122-VERIFY-PASSWORD.
           IF LS-PASSWORD = USER-PASSWORD
               MOVE 'S' TO LS-AUTH-RESULT
               MOVE USER-ROLE TO LS-USER-ROLE
           ELSE
               MOVE 'Invalid password' TO LS-ERROR-MESSAGE
               MOVE 'F' TO LS-AUTH-RESULT
           END-IF.

       130-UPDATE-USER-RECORD.
           MOVE WS-CURRENT-DATE TO USER-LAST-LOGIN
           MOVE 0 TO USER-FAILED-ATTEMPTS
           REWRITE USER-RECORD
               INVALID KEY
                   PERFORM 190-LOG-FILE-ERROR
           END-REWRITE.

       140-LOG-AUTH-SUCCESS.
           MOVE WS-CURRENT-DATE TO AL-TIMESTAMP(1:8)
           MOVE WS-CURRENT-TIME TO AL-TIMESTAMP(9:6)
           MOVE LS-USERID TO AL-USER-ID
           MOVE 'LOGIN' TO AL-ACTION
           MOVE 'Successful authentication' TO AL-DETAILS
           MOVE 'S' TO AL-STATUS
           MOVE LS-IP-ADDRESS TO AL-IP-ADDRESS
           PERFORM 900-WRITE-AUDIT-LOG.

       150-HANDLE-FAILED-LOGIN.
           IF USER-FOUND
               ADD 1 TO USER-FAILED-ATTEMPTS
               IF USER-FAILED-ATTEMPTS >= MAX-FAILED-ATTEMPTS
                   MOVE 'L' TO USER-STATUS
                   MOVE 'Account locked due to failed attempts' 
                       TO LS-ERROR-MESSAGE
               END-IF
               REWRITE USER-RECORD
                   INVALID KEY
                       PERFORM 190-LOG-FILE-ERROR
               END-REWRITE
           END-IF.

       160-LOG-AUTH-FAILURE.
           MOVE WS-CURRENT-DATE TO AL-TIMESTAMP(1:8)
           MOVE WS-CURRENT-TIME TO AL-TIMESTAMP(9:6)
           MOVE LS-USERID TO AL-USER-ID
           MOVE 'LOGIN' TO AL-ACTION
           MOVE LS-ERROR-MESSAGE TO AL-DETAILS
           MOVE 'F' TO AL-STATUS
           MOVE LS-IP-ADDRESS TO AL-IP-ADDRESS
           PERFORM 900-WRITE-AUDIT-LOG.

       200-ADMINISTER-USER.
           PERFORM 210-VALIDATE-ADMIN-REQUEST
           IF ADMIN-SUCCESS
               EVALUATE TRUE
                   WHEN CREATE-USER  PERFORM 220-CREATE-USER
                   WHEN UPDATE-USER  PERFORM 230-UPDATE-USER
                   WHEN DELETE-USER  PERFORM 240-DELETE-USER
                   WHEN LOCK-USER    PERFORM 250-LOCK-USER
                   WHEN UNLOCK-USER  PERFORM 260-UNLOCK-USER
                   WHEN RESET-PW     PERFORM 270-RESET-PASSWORD
               END-EVALUATE
           END-IF.

       210-VALIDATE-ADMIN-REQUEST.
           MOVE 'S' TO LS-ADMIN-RESULT.

       220-CREATE-USER.
           MOVE LS-TARGET-USERID TO USER-ID
           MOVE LS-NEW-DATA(1:40) TO USER-NAME
           MOVE 'Temp123!' TO USER-PASSWORD
           MOVE 'E' TO USER-ROLE
           MOVE 'A' TO USER-STATUS
           MOVE WS-CURRENT-DATE TO USER-PW-CHANGE-DATE
           MOVE 0 TO USER-FAILED-ATTEMPTS
           
           WRITE USER-RECORD
               INVALID KEY
                   MOVE 'F' TO LS-ADMIN-RESULT
                   MOVE 'User creation failed' TO LS-ADMIN-MESSAGE
               NOT INVALID KEY
                   MOVE 'User created successfully' TO LS-ADMIN-MESSAGE
                   PERFORM 290-LOG-ADMIN-ACTION
           END-WRITE.

       290-LOG-ADMIN-ACTION.
           MOVE WS-CURRENT-DATE TO AL-TIMESTAMP(1:8)
           MOVE WS-CURRENT-TIME TO AL-TIMESTAMP(9:6)
           MOVE LS-TARGET-USERID TO AL-USER-ID
           MOVE LS-ADMIN-ACTION TO AL-ACTION
           MOVE LS-ADMIN-MESSAGE TO AL-DETAILS
           MOVE 'S' TO AL-STATUS
           MOVE 'SYSTEM' TO AL-IP-ADDRESS
           PERFORM 900-WRITE-AUDIT-LOG.

       900-WRITE-AUDIT-LOG.
           OPEN EXTEND AUDIT-LOG-FILE
           IF AUDIT-LOG-STATUS = '00'
               WRITE AUDIT-LOG-RECORD
               IF AUDIT-LOG-STATUS NOT = '00'
                   DISPLAY 'AUDIT LOG WRITE ERROR: ' AUDIT-LOG-STATUS
               END-IF
           ELSE
               DISPLAY 'AUDIT LOG OPEN ERROR: ' AUDIT-LOG-STATUS
           END-IF
           CLOSE AUDIT-LOG-FILE.

       190-LOG-FILE-ERROR.
           DISPLAY 'FILE ERROR: ' USER-FILE-STATUS.