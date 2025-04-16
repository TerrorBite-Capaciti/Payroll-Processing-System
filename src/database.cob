       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATABASE.
       AUTHOR.     BYTEBANK-DEV.
       DATE-WRITTEN. 2023-11-15.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "../data/employees.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS EMP-ID
               FILE STATUS IS EMP-FILE-STATUS.
               
           SELECT PAYROLL-FILE ASSIGN TO "../data/payroll.dat"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS PAY-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-RECORD.
           05  EMP-ID               PIC X(10).
           05  EMP-NAME             PIC X(50).
           05  EMP-DEPT             PIC X(20).
           05  EMP-POSITION         PIC X(30).
           05  EMP-PAY-RATE         PIC 9(5)V99.
           05  EMP-STATUS           PIC X(1).

       FD  PAYROLL-FILE.
       01  PAYROLL-RECORD.
           05  PR-EMP-ID           PIC X(10).
           05  PR-PAY-DATE         PIC X(10).
           05  PR-GROSS-PAY        PIC 9(7)V99.
           05  PR-NET-PAY          PIC 9(7)V99.

       WORKING-STORAGE SECTION.
       01  FILE-STATUS.
           05  EMP-FILE-STATUS     PIC XX.
           05  PAY-FILE-STATUS     PIC XX.
       01  WS-ERROR-MESSAGE       PIC X(50).

       LINKAGE SECTION.
       01  LS-OPERATION           PIC X(1).
       01  LS-EMP-ID              PIC X(10).
       01  LS-RECORD-DATA         PIC X(150).
       01  LS-OPERATION-STATUS    PIC X(1).

       PROCEDURE DIVISION USING LS-OPERATION, LS-EMP-ID, LS-RECORD-DATA,
                                LS-OPERATION-STATUS.
       100-MAIN-DB-OPERATION.
           EVALUATE LS-OPERATION
               WHEN 'R' PERFORM 200-READ-EMPLOYEE
               WHEN 'W' PERFORM 300-WRITE-EMPLOYEE
               WHEN 'U' PERFORM 400-UPDATE-EMPLOYEE
               WHEN 'P' PERFORM 500-WRITE-PAYROLL
               WHEN OTHER 
                   MOVE 'F' TO LS-OPERATION-STATUS
                   MOVE "Invalid database operation" TO WS-ERROR-MESSAGE
                   PERFORM 900-LOG-ERROR
           END-EVALUATE.
           GOBACK.

       200-READ-EMPLOYEE.
           OPEN INPUT EMPLOYEE-FILE
           IF EMP-FILE-STATUS NOT = '00'
               MOVE 'F' TO LS-OPERATION-STATUS
               MOVE "Error opening employee file" TO WS-ERROR-MESSAGE
               PERFORM 900-LOG-ERROR
               GOBACK
           END-IF.
           
           MOVE LS-EMP-ID TO EMP-ID
           READ EMPLOYEE-FILE
               INVALID KEY 
                   MOVE 'F' TO LS-OPERATION-STATUS
                   MOVE "Employee not found" TO WS-ERROR-MESSAGE
                   PERFORM 900-LOG-ERROR
               NOT INVALID KEY
                   MOVE EMPLOYEE-RECORD TO LS-RECORD-DATA
                   MOVE 'S' TO LS-OPERATION-STATUS
           END-READ.
           
           CLOSE EMPLOYEE-FILE.

       300-WRITE-EMPLOYEE.
           OPEN I-O EMPLOYEE-FILE
           IF EMP-FILE-STATUS NOT = '00'
               OPEN OUTPUT EMPLOYEE-FILE
               IF EMP-FILE-STATUS NOT = '00'
                   MOVE 'F' TO LS-OPERATION-STATUS
                   MOVE "Error creating employee file" TO WS-ERROR-MESSAGE
                   PERFORM 900-LOG-ERROR
                   GOBACK
               END-IF
           END-IF.
           
           MOVE LS-RECORD-DATA TO EMPLOYEE-RECORD
           WRITE EMPLOYEE-RECORD
               INVALID KEY 
                   MOVE 'F' TO LS-OPERATION-STATUS
                   MOVE "Error writing employee record" TO WS-ERROR-MESSAGE
                   PERFORM 900-LOG-ERROR
               NOT INVALID KEY
                   MOVE 'S' TO LS-OPERATION-STATUS
           END-WRITE.
           
           CLOSE EMPLOYEE-FILE.

       400-UPDATE-EMPLOYEE.
           OPEN I-O EMPLOYEE-FILE
           IF EMP-FILE-STATUS NOT = '00'
               MOVE 'F' TO LS-OPERATION-STATUS
               MOVE "Error opening employee file" TO WS-ERROR-MESSAGE
               PERFORM 900-LOG-ERROR
               GOBACK
           END-IF.
           
           MOVE LS-EMP-ID TO EMP-ID
           READ EMPLOYEE-FILE
               INVALID KEY 
                   MOVE 'F' TO LS-OPERATION-STATUS
                   MOVE "Employee not found for update" TO WS-ERROR-MESSAGE
                   PERFORM 900-LOG-ERROR
               NOT INVALID KEY
                   MOVE LS-RECORD-DATA TO EMPLOYEE-RECORD
                   REWRITE EMPLOYEE-RECORD
                       INVALID KEY 
                           MOVE 'F' TO LS-OPERATION-STATUS
                           MOVE "Error updating employee" TO WS-ERROR-MESSAGE
                           PERFORM 900-LOG-ERROR
                       NOT INVALID KEY
                           MOVE 'S' TO LS-OPERATION-STATUS
                   END-REWRITE
           END-READ.
           
           CLOSE EMPLOYEE-FILE.

       500-WRITE-PAYROLL.
           OPEN EXTEND PAYROLL-FILE
           IF PAY-FILE-STATUS NOT = '00'
               OPEN OUTPUT PAYROLL-FILE
               IF PAY-FILE-STATUS NOT = '00'
                   MOVE 'F' TO LS-OPERATION-STATUS
                   MOVE "Error opening payroll file" TO WS-ERROR-MESSAGE
                   PERFORM 900-LOG-ERROR
                   GOBACK
               END-IF
           END-IF.
           
           MOVE LS-RECORD-DATA TO PAYROLL-RECORD
           WRITE PAYROLL-RECORD
           IF PAY-FILE-STATUS NOT = '00'
               MOVE 'F' TO LS-OPERATION-STATUS
               MOVE "Error writing payroll record" TO WS-ERROR-MESSAGE
               PERFORM 900-LOG-ERROR
           ELSE
               MOVE 'S' TO LS-OPERATION-STATUS
           END-IF.
           
           CLOSE PAYROLL-FILE.

       900-LOG-ERROR.
           DISPLAY "DATABASE ERROR: " WS-ERROR-MESSAGE
           DISPLAY "FILE STATUS: " FILE-STATUS.
       END PROGRAM DATABASE.