#!/bin/bash

echo "🔧 Starting COBOL compilation process..."

# Compile each COBOL source file into object files
cobc -free -c src/main.cob -o obj/main.o
cobc -free -c src/auth.cob -o obj/auth.o
cobc -free -c src/payroll.cob -o obj/payroll.o
cobc -free -c src/database.cob -o obj/database.o
cobc -free -c src/reports.cob -o obj/reports.o
cobc -free -c src/employee-mgmt.cob -o obj/employee-mgmt.o
cobc -free -c src/config.cob -o obj/config.o
cobc -free -c src/benefits.cob -o obj/benefits.o
cobc -free -c src/cli.cob -o obj/cli.o
cobc -free -c src/tax.cob -o obj/tax.o
cobc -free -c src/test-auth-driver.cob -o obj/test-auth-driver.o

# Check if compilation was successful
if [ $? -eq 0 ]; then
    echo "✅ Compilation successful!"
else
    echo "❌ Compilation failed. Check the output for errors."
    exit 1
fi

# Link the object files to create the final executable
echo "🔗 Linking object files into final executable..."
cobc -x obj/*.o -o bin/bytebank-payroll

# Check if linking was successful
if [ $? -eq 0 ]; then
    echo "✅ Linking successful. Executable created at 'bin/bytebank-payroll'."
else
    echo "❌ Linking failed. Check the output for errors."
    exit 1
fi
