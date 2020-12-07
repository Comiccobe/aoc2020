AOC20 * ADVENT OF CODE 2020 DAY ONE PART ONE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAY01P01.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT PROBLEM-INPUT ASSIGN TO "INPUT.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.

       FILE SECTION.
       FD PROBLEM-INPUT.
       01 PROBLEM-FILE.
          04 INPUT-NUMBER PIC 9(4).

       WORKING-STORAGE SECTION.
       01 WS-INPUT-NUMBER PIC 9(4).
       01 WS-EOF PIC 9.
       01 WS-I PIC 9(3).
       01 WS-J PIC 9(3).
       01 WS-K PIC 9(3).
       01 WS-DONE PIC 9.
       01 WS-RESULT PIC 9(9).
       01 WS-ITEMS OCCURS 200 TIMES PIC 9(4).

       PROCEDURE DIVISION.
           MOVE 1 TO WS-I.
           OPEN INPUT PROBLEM-INPUT.
           PERFORM UNTIL WS-EOF=1
             READ PROBLEM-INPUT INTO WS-INPUT-NUMBER
               AT END MOVE 1 TO WS-EOF
               NOT AT END MOVE WS-INPUT-NUMBER TO WS-ITEMS
                 (WS-I)
               ADD 1 TO WS-I
             END-READ
           END-PERFORM.
           CLOSE PROBLEM-INPUT.
           MOVE 0 TO WS-DONE.
           MOVE 1 TO WS-I.
           PERFORM UNTIL WS-I > 200
             MOVE 1 TO WS-J
             PERFORM UNTIL WS-J > 200
               MOVE 1 TO WS-K
               PERFORM UNTIL WS-K > 200
                 ADD WS-ITEMS (WS-I) WS-ITEMS (WS-J) WS-ITEMS (WS-K)
                 GIVING WS-RESULT
                 IF WS-RESULT = 2020 THEN
                   MULTIPLY WS-ITEMS (WS-I) BY WS-ITEMS (WS-J) GIVING
                   WS-RESULT
                   MULTIPLY WS-ITEMS (WS-K) BY WS-RESULT
                   DISPLAY "PART02: " WS-RESULT
                   MOVE 1 TO WS-DONE
                   EXIT PERFORM
                 END-IF
                 ADD 1 TO WS-K
               END-PERFORM
               IF WS-DONE = 1 THEN
                 EXIT PERFORM
               END-IF
               ADD 1 TO WS-J
             END-PERFORM
             IF WS-DONE = 1 THEN
               EXIT PERFORM
             END-IF
             ADD 1 TO WS-I
           END-PERFORM.
           STOP RUN.
