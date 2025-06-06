#!/usr/bin/env python3
"""
Converted from COBOL to Python
Conversion Date: 2025-06-06 13:04:01
Original COBOL Lines: 58
Variables Found: 6
Procedures Found: 4
"""

from decimal import Decimal
import sys

# Variable declarations (based on COBOL DATA DIVISION)
user-pin = 0
entered-pin = 0
balance = Decimal('0.0')
choice = 0
amount = Decimal('0.0')
continue = ''

# Procedure definitions
def procedure division():
    # TODO: Implement procedure logic
    pass

def begin():
    # TODO: Implement procedure logic
    pass

def main_menu():
    # TODO: Implement procedure logic
    pass

def stop run():
    # TODO: Implement procedure logic
    pass

def main():
    """Main program logic"""
    # TODO: Convert - BEGIN.
    # Line 17: DISPLAY "==== WELCOME TO COBOL ATM ====".
    print("====, welcome, to, cobol, atm, ====".)
    # Line 18: DISPLAY "ENTER YOUR 4-DIGIT PIN: ".
    print("enter, your, 4-digit, pin:, ".)
    # Line 19: ACCEPT ENTERED-PIN.
    entered = input("Enter entered: ")
    # TODO: Convert - IF ENTERED-PIN NOT = 1234
    # Line 22: DISPLAY "INVALID PIN. ACCESS DENIED."
    print("INVALID PIN. ACCESS DENIED.")
    # Line 23: STOP RUN
    sys.exit(0)
    # TODO: Convert - END-IF.
    # Line 26: DISPLAY "LOGIN SUCCESSFUL.".
    print("login, successful.".)
    # TODO: Convert - MAIN-MENU.
    # Line 29: PERFORM UNTIL CONTINUE NOT = 'Y'
    while not (continue not == 'y'):
    # Line 30: DISPLAY "=============================="
    print("==============================")
    # Line 31: DISPLAY "1. CHECK BALANCE"
    print("1. CHECK BALANCE")
    # Line 32: DISPLAY "2. DEPOSIT MONEY"
    print("2. DEPOSIT MONEY")
    # Line 33: DISPLAY "3. WITHDRAW MONEY"
    print("3. WITHDRAW MONEY")
    # Line 34: DISPLAY "4. EXIT"
    print("4. EXIT")
    # Line 35: DISPLAY "ENTER YOUR CHOICE: "
    print("ENTER YOUR CHOICE: ")
    # Line 36: ACCEPT CHOICE
    choice = input("Enter choice: ")
    # TODO: Convert - EVALUATE CHOICE
    # TODO: Convert - WHEN 1
    # Line 40: DISPLAY "CURRENT BALANCE: $" BALANCE
    print("current, balance:, $", balance)
    # TODO: Convert - WHEN 2
    # Line 42: DISPLAY "ENTER AMOUNT TO DEPOSIT: "
    print("ENTER AMOUNT TO DEPOSIT: ")
    # Line 43: ACCEPT AMOUNT
    amount = input("Enter amount: ")
    # Line 44: ADD AMOUNT TO BALANCE
    balance += amount
    # Line 45: DISPLAY "DEPOSIT SUCCESSFUL. NEW BALANCE: $" BALANCE
    print("deposit, successful., new, balance:, $", balance)
    # TODO: Convert - WHEN 3
    # Line 47: DISPLAY "ENTER AMOUNT TO WITHDRAW: "
    print("ENTER AMOUNT TO WITHDRAW: ")
    # Line 48: ACCEPT AMOUNT
    amount = input("Enter amount: ")
    # TODO: Convert - IF AMOUNT > BALANCE
    # Line 50: DISPLAY "INSUFFICIENT FUNDS."
    print("INSUFFICIENT FUNDS.")
    # TODO: Convert - ELSE
    # Line 52: SUBTRACT AMOUNT FROM BALANCE
    balance -= amount
    # Line 53: DISPLAY "WITHDRAWAL SUCCESSFUL. NEW BALANCE: $" BALANCE
    print("withdrawal, successful., new, balance:, $", balance)
    # TODO: Convert - END-IF
    # TODO: Convert - WHEN 4
    # Line 56: DISPLAY "THANK YOU FOR USING COBOL ATM."
    print("THANK YOU FOR USING COBOL ATM.")
    # Line 57: STOP RUN
    sys.exit(0)
    # TODO: Convert - WHEN OTHER
    # Line 59: DISPLAY "INVALID CHOICE. TRY AGAIN."
    print("INVALID CHOICE. TRY AGAIN.")
    # TODO: Convert - END-EVALUATE
    # Line 62: DISPLAY "DO YOU WANT TO CONTINUE? (Y/N): "
    print("DO YOU WANT TO CONTINUE? (Y/N): ")
    # Line 63: ACCEPT CONTINUE
    continue = input("Enter continue: ")
    # TODO: Convert - END-PERFORM.
    # Line 66: DISPLAY "SESSION ENDED. PLEASE TAKE YOUR CARD."
    print("SESSION ENDED. PLEASE TAKE YOUR CARD.")
    # Line 67: STOP RUN.
    sys.exit(0)

if __name__ == '__main__':
    main()