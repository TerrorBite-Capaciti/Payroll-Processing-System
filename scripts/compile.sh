#!/bin/bash

echo "Compiling COBOL files..."

cobc -x ../src/payroll.cbl \
         ../src/user_management.cbl \
         ../src/cli_menu.cbl \
         ../src/employee.cbl \
         ../src/tax_calculation.cbl \
         ../src/payslip_generator.cbl \
         ../src/leave_deductions.cbl \
         ../src/db_interaction.cbl \
         -o ../payroll

if [ $? -eq 0 ]; then
    echo "Compilation successful! Executable created as '../payroll'."
else
    echo "Compilation failed! Please check for errors."
fi
