       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINI-ATM.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  USER-PIN            PIC 9(4) VALUE ZEROS.
       01  ENTERED-PIN         PIC 9(4).
       01  BALANCE             PIC 9(6)V99 VALUE 1000.00.
       01  CHOICE              PIC 9.
       01  AMOUNT              PIC 9(6)V99.
       01  CONTINUE            PIC X VALUE 'Y'.

       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY "==== WELCOME TO COBOL ATM ====".
           DISPLAY "ENTER YOUR 4-DIGIT PIN: ".
           ACCEPT ENTERED-PIN.

           IF ENTERED-PIN NOT = 1234
               DISPLAY "INVALID PIN. ACCESS DENIED."
               STOP RUN
           END-IF.

           DISPLAY "LOGIN SUCCESSFUL.".

       MAIN-MENU.
           PERFORM UNTIL CONTINUE NOT = 'Y'
               DISPLAY "=============================="
               DISPLAY "1. CHECK BALANCE"
               DISPLAY "2. DEPOSIT MONEY"
               DISPLAY "3. WITHDRAW MONEY"
               DISPLAY "4. EXIT"
               DISPLAY "ENTER YOUR CHOICE: "
               ACCEPT CHOICE

               EVALUATE CHOICE
                   WHEN 1
                       DISPLAY "CURRENT BALANCE: $" BALANCE
                   WHEN 2
                       DISPLAY "ENTER AMOUNT TO DEPOSIT: "
                       ACCEPT AMOUNT
                       ADD AMOUNT TO BALANCE
                       DISPLAY "DEPOSIT SUCCESSFUL. NEW BALANCE: $" BALANCE
                   WHEN 3
                       DISPLAY "ENTER AMOUNT TO WITHDRAW: "
                       ACCEPT AMOUNT
                       IF AMOUNT > BALANCE
                           DISPLAY "INSUFFICIENT FUNDS."
                       ELSE
                           SUBTRACT AMOUNT FROM BALANCE
                           DISPLAY "WITHDRAWAL SUCCESSFUL. NEW BALANCE: $" BALANCE
                       END-IF
                   WHEN 4
                       DISPLAY "THANK YOU FOR USING COBOL ATM."
                       STOP RUN
                   WHEN OTHER
                       DISPLAY "INVALID CHOICE. TRY AGAIN."
               END-EVALUATE

               DISPLAY "DO YOU WANT TO CONTINUE? (Y/N): "
               ACCEPT CONTINUE
           END-PERFORM.

           DISPLAY "SESSION ENDED. PLEASE TAKE YOUR CARD."
           STOP RUN.
