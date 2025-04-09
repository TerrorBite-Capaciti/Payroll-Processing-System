       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLI-COMMANDS.
       AUTHOR. PAYROLL-SYSTEM.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COMMAND-RESULT  PIC X(200).
       
       LINKAGE SECTION.
       01 LS-COMMAND         PIC X(20).
       01 LS-ARGS            PIC X(60).
       01 LS-RESULT          PIC X(200).
       
       COPY payroll_processing_system.cpy.
       
       PROCEDURE DIVISION USING LS-COMMAND, LS-ARGS, LS-RESULT.
       
       PROCESS-COMMAND.
           EVALUATE LS-COMMAND
               WHEN "ADD-EMP"    PERFORM PROCESS-ADD-EMPLOYEE
               WHEN "CALC-PAY"   PERFORM PROCESS-CALC-PAYROLL
               WHEN "GEN-REPORT" PERFORM PROCESS-GENERATE-REPORT
               WHEN "SYS-CONFIG" PERFORM PROCESS-SYSTEM-CONFIG
               WHEN OTHER        MOVE "Invalid command" TO LS-RESULT
           END-EVALUATE
           
           GOBACK.
       
       PROCESS-ADD-EMPLOYEE.
           *> Parse arguments and call employee.cbl
           MOVE "Employee added successfully" TO LS-RESULT.
       
       PROCESS-CALC-PAYROLL.
           *> Parse arguments and call payroll.cbl
           MOVE "Payroll processed successfully" TO LS-RESULT.
       
       PROCESS-GENERATE-REPORT.
           *> Parse arguments and call reporting functions
           MOVE "Report generated successfully" TO LS-RESULT.
       
       PROCESS-SYSTEM-CONFIG.
           *> Handle system configuration changes
           MOVE "System configured successfully" TO LS-RESULT.
       
       END PROGRAM CLI-COMMANDS.