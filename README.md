# Payroll Processing System

## Project Overview
The **Payroll Processing System** is a COBOL-based application designed to automate payroll management by calculating employee salaries, applying tax deductions, processing benefits, and generating payslips. This system ensures compliance with tax regulations while improving payroll efficiency for organizations.

## Features
- **Employee Salary Calculation**: Computes employee wages based on hours worked, overtime, bonuses, and deductions.
- **Tax Deductions & Benefits**: Applies tax deductions such as income tax and social security while incorporating benefits like medical insurance and allowances.
- **Payslip Generation**: Generates detailed payslips for employees.
- **Data Storage**: Maintains employee salary history in indexed files or databases (e.g., DB2, VSAM).
- **Leave Deductions**: Deducts unpaid leave from employee salaries.

## Technical Details
- **Programming Language**: COBOL
- **File Handling**:
  - Sequential files for storing salary data.
  - Indexed files (ISAM/VSAM) for employee records.
- **Database Support** (optional for advanced implementation):
  - DB2 or PostgreSQL for payroll data storage.

## Software Development Lifecycle (SDLC) Implementation
The project follows the **Software Development Lifecycle (SDLC)** to ensure a structured and efficient development process:

1. **Requirement Analysis**
   - Gather payroll processing requirements.
   - Define tax deduction rules and benefit calculations.
   - Identify data storage needs.

2. **System Design**
   - Design the architecture for payroll processing.
   - Define data structures for employee records and payroll transactions.
   - Plan file handling and database structure.

3. **Implementation**
   - Develop COBOL modules for salary calculation, deductions, and payslip generation.
   - Implement file handling mechanisms (Sequential, ISAM, VSAM).
   - Integrate with DB2/PostgreSQL if database storage is used.

4. **Testing**
   - Perform unit testing for salary computation and tax deductions.
   - Conduct integration testing with file handling and database operations.
   - Execute user acceptance testing (UAT) to validate system functionality.

5. **Deployment**
   - Set up the production environment.
   - Load initial employee data into the system.
   - Train users on system operations.

6. **Maintenance & Updates**
   - Monitor system performance.
   - Apply updates for tax regulation changes.
   - Fix bugs and enhance features based on feedback.

## Installation & Setup
### Prerequisites
- COBOL Compiler (GnuCOBOL or Micro Focus COBOL)
- DB2/PostgreSQL (if using a database)
- A system with batch processing capabilities

### Steps
1. Clone the repository:
   ```sh
   git clone https://github.com/TerrorBite-Capaciti/Payroll-Processing-System
   ```
2. Navigate to the project directory:
   ```sh
   cd Payroll-Processing-System
   ```
3. Compile COBOL source files:
   ```sh
   cobc -x payroll.cbl -o payroll
   ```
4. Run the application:
   ```sh
   ./payroll
   ```

## Usage
1. Add employee records.
2. Enter salary details including hours worked, overtime, and deductions.
3. Process payroll to compute wages and apply deductions.
4. Generate payslips for employees.
5. Store payroll history for future reference.

## Contribution Guidelines
- Follow COBOL best practices.
- Adhere to structured programming principles.
- Submit pull requests with proper documentation.

## License
This project is licensed under the MIT License.

## Project Structure
```
Payroll-Processing-System/
│── src/                           # Source code folder
│   ├── payroll.cbl                # Main COBOL program
│   ├── employee.cbl               # Employee record management module
│   ├── tax_calculation.cbl        # Tax deduction and benefits calculation
│   ├── payslip_generator.cbl      # Payslip generation module
│   ├── leave_deductions.cbl       # Leave deduction module
│   ├── db_interaction.cbl         # Database interaction (if applicable)
│── data/                          # Data storage folder
│   ├── employees.dat              # Employee data file (Sequential/Indexed)
│   ├── salary_records.dat         # Salary history data file
│   ├── tax_rates.dat              # Tax and benefits rates
│── tests/                         # Testing folder
│   ├── test_payroll.cbl           # Test cases for payroll calculations
│   ├── test_taxes.cbl             # Test cases for tax calculations
│── docs/                          # Documentation folder
│   ├── README.md                  # Project overview and installation guide
│   ├── design_doc.pdf             # System design document
│   ├── user_manual.pdf            # User guide for payroll processing
│── scripts/                       # Utility scripts folder
│   ├── compile.sh                 # Shell script to compile all COBOL files
│   ├── run_tests.sh               # Shell script to run test cases
│── .gitignore                      # Git ignore file
│── payroll_processing_system.cpy   # Copybook for common record structures
│── LICENSE                         # License file
│── README.md                       # Project README