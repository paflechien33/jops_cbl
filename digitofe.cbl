       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. DIGITOFE.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
      *-----------------------
       WORKING-STORAGE SECTION.
      *-----------------------
         01 N PIC 9(4) BINARY VALUE 2320.
         01 M PIC 9(4) BINARY.
         01 I PIC 9(4) BINARY.
         01 X PIC 9(9) BINARY VALUE ZERO.
         01 D PIC 9(9) BINARY.
         01 ARRAY.
           03 A PIC 9(9) BINARY OCCURS 2320.
         01 SB PIC 9.

       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **   A = 1+1/N  => 0,2,1,1 ...1
           DISPLAY '2.7' WITH NO ADVANCING
           INITIALIZE ARRAY REPLACING NUMERIC DATA BY 1
           MOVE ZERO TO A(1)
           MOVE 2 TO A(2)
           PERFORM VARYING N FROM 2319 BY -1 UNTIL N < 9
               ADD 1 TO N GIVING M
               PERFORM VARYING I FROM M BY -1 UNTIL I = 1
      *             DISPLAY '1. I=' I '/' N '- ARRAY = ' ARRAY
      *             COMPUTE A(I) = FUNCTION MOD(X,I)
      *    >OPEN COBOL 1.1 : USING DIVIDE IS MUCH FASTER THAN FUNCTION
      *    >MOD
                    DIVIDE X BY I GIVING D REMAINDER A(I)
      *             DISPLAY '2. I=' I ' - ARRAY = ' ARRAY
                   COMPUTE X = 10 * A( I - 1 ) + D
               END-PERFORM
               IF N < 2319 THEN
               MOVE X TO SB
               DISPLAY SB WITH NO ADVANCING
               END-IF
           END-PERFORM.
           STOP RUN.
      ** add other procedures here
       END PROGRAM DIGITOFE.
