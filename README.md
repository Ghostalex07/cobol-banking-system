# 🏦 COBOL Banking System

A terminal-based **banking management system written in COBOL** designed
to simulate core banking operations such as account management,
transactions, loans, and reporting.

This project runs entirely in the **Linux terminal** using **GnuCOBOL**,
demonstrating how traditional banking systems were historically
implemented in COBOL.

------------------------------------------------------------------------

# 🚀 Features

-   Create and manage bank accounts
-   Search accounts by:
    -   DNI / NIF / CIF
    -   Customer name
    -   Email
-   Manage **bank transactions**
-   Loan management system
-   Card management
-   Executive banking reports
-   Account ranking by balance
-   Statistics by account type
-   Interest calculation and liquidation
-   PIN authentication system
-   Automatic account blocking after failed attempts
-   Demo data included for testing

------------------------------------------------------------------------

# 🧠 Technologies

-   **COBOL (GnuCOBOL)**
-   **Linux**
-   **Command Line Interface (CLI)**

------------------------------------------------------------------------

# ⚙️ Installation

Install GnuCOBOL (if not installed):

``` bash
sudo apt update
sudo apt install gnucobol
```

------------------------------------------------------------------------

# 🔨 Compile

Compile the program with:

``` bash
cobc -x -free banco.cob -o banco
```

------------------------------------------------------------------------

# ▶️ Run

Execute the program:

``` bash
./banco
```

------------------------------------------------------------------------

# 📊 Supported Account Types

  Type   Description        Interest
  ------ ------------------ ----------
  A      Savings Account    3.50%
  C      Checking Account   1.50%
  P      Fixed Deposit      8.00%
  J      Youth Account      4.00%
  E      Business Account   1.00%

------------------------------------------------------------------------

# 📈 Reports

The system can generate:

-   Executive bank report
-   Account ranking by balance
-   Statistics by account type
-   Loan reports
-   Annual interest liquidation

------------------------------------------------------------------------

# 🔐 Security

-   PIN-based authentication
-   Automatic account lock after 3 failed attempts
-   Account status verification before operations

------------------------------------------------------------------------

# 📂 Demo Data

The system includes preloaded demo data:

-   Customers
-   Bank accounts
-   Transactions
-   Loans
-   Cards

This allows you to test all features immediately.

------------------------------------------------------------------------

# 🖥 Example Usage

``` bash
$ cobc -x -free banco.cob -o banco
$ ./banco
```

The program will launch an **interactive terminal menu** to manage the
banking system.

------------------------------------------------------------------------
