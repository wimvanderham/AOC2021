
/*------------------------------------------------------------------------
    File        : Day_04.p
    Purpose     : Solve Day 4 of Advent of Code 2021
    URL         : https://adventofcode.com/2021/day/4

    Syntax      :

    Description : Solve Day 4 of Advent of Code 2021

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sat Dec 04 08:08:28 CET 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for input handling */
DEFINE VARIABLE lcInput      AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE iLine        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iChr         AS INTEGER   NO-UNDO.

/* Variables for solving */
/* Generic */
DEFINE VARIABLE iSolution    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lOk          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvlDebug     AS LOGICAL   NO-UNDO.
/* Specific */
DEFINE VARIABLE iExtraction  AS INTEGER NO-UNDO EXTENT.
DEFINE VARIABLE iNumberBoard AS INTEGER NO-UNDO.
DEFINE VARIABLE iCurrentRow  AS INTEGER NO-UNDO.
DEFINE VARIABLE iCurrentCol  AS INTEGER NO-UNDO.

/* Temp-table for the Boards */
DEFINE TEMP-TABLE ttBoardNumber RCODE-INFORMATION 
   FIELD iBoard     AS INTEGER LABEL "Board #"
   FIELD iRow       AS INTEGER LABEL "Row"
   FIELD iCol       AS INTEGER LABEL "Col"
   FIELD iNumber    AS INTEGER LABEL "Number" FORMAT "Z9"
   FIELD lExtracted AS LOGICAL LABEL "Extracted?"
INDEX ind_ID IS UNIQUE iBoard iRow iCol
INDEX ind_Number iNumber.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

COPY-LOB FROM FILE "input\04.txt" TO OBJECT lcInput.

/* Part One - */
ETIME (YES).
ReadLine:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   cLine = ENTRY (iLine, lcInput, "~n").
   IF iLine EQ 1 THEN DO:
      /* Load array of extracted numbers */
      EXTENT (iExtraction) = NUM-ENTRIES (cLine).
      
      DO iChr = 1 TO NUM-ENTRIES (cLine):
         ASSIGN 
            iExtraction[iChr] = INTEGER (ENTRY (iChr, cLine))
         .
      END.
      NEXT ReadLine.
   END. /* Load array of extracted numbers */
   
   IF cLine EQ "" THEN DO:
      iNumberBoard = iNumberBoard + 1.
      iCurrentRow = 0.
      NEXT ReadLine.
   END.
   
   iCurrentRow = iCurrentRow + 1.
   iCurrentCol = 0.
   iChr = 1.
   DO WHILE iChr LT LENGTH (cLine):
      iCurrentCol = iCurrentCol + 1.
      CREATE ttBoardNumber.
      ASSIGN
         ttBoardNumber.iBoard     = iNumberBoard
         ttBoardNumber.iRow       = iCurrentRow
         ttBoardNumber.iCol       = iCurrentCol
         ttBoardNumber.iNumber    = INTEGER (SUBSTRING (cLine, iChr, 2))
         ttBoardNumber.lExtracted = FALSE 
      .
      /* Move to next number (each number takes 2 positions
      ** and numbers are separated by a space
      */
      iChr = iChr + 3.
   END.
END.

/* Part One - Find the winner */

PlayBingo:
DO iChr = 1 TO EXTENT (iExtraction):
   RUN extractNumber
      (INPUT iExtraction[iChr]).
      
   RUN chkWinner
      (OUTPUT iNumberBoard,
       OUTPUT lOk,
       OUTPUT cMessage).

   IF lOk EQ TRUE THEN DO:
      /* Found a Winner! */
      FOR EACH ttBoardNumber
      WHERE ttBoardNumber.iBoard     EQ iNumberBoard
      AND   ttBoardNumber.lExtracted EQ FALSE:
         ACCUM ttBoardNumber.iNumber (TOTAL).
      END.
      
      iSolution = iExtraction[iChr] * (ACCUM TOTAL ttBoardNumber.iNumber).
      LEAVE PlayBingo.
   END. /* Found a Winner! */
END. /* PlayBingo */
 
OUTPUT TO "clipboard".
PUT UNFORMATTED iSolution SKIP.
OUTPUT CLOSE.

MESSAGE 
   SUBSTITUTE ("Winner Board: &2~nAfter extraction of: &3 (&4)~nSolution: &1.", 
      iSolution,
      iNumberboard,
      iExtraction[iChr],
      iChr) SKIP (1)
   SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
VIEW-AS ALERT-BOX TITLE " 2021 - Day 04 - Part One".

/* Part Two - Find the last winner board */
FOR EACH ttBoardNumber:
   ASSIGN 
      ttBoardNumber.lExtracted = FALSE
   .
END.

PlayBingo2:
DO iChr = 1 TO EXTENT (iExtraction):
   RUN extractNumber
      (INPUT iExtraction[iChr]).
      
   RUN chkWinner
      (OUTPUT iNumberBoard,
       OUTPUT lOk,
       OUTPUT cMessage).

   IF lvlDebug THEN DO:
      MESSAGE 
      SUBSTITUTE ("PlayBingo2, iChr = &1 (&5), Winner? &2 Which Board? &3, Message: &4",
         iChr,
         lOk,
         iNumberBoard,
         cMessage,
         iExtraction[iChr])
      VIEW-AS ALERT-BOX.
   END.
   IF lOk EQ TRUE THEN DO:
      /* Found a Winner! */
      FIND FIRST ttBoardNumber WHERE ttBoardNumber.iBoard NE iNumberBoard NO-ERROR.
      IF AVAILABLE ttBoardNumber THEN DO:
         /* Still Boards available. Delete this winner and continue extracting */
         RUN deleteBoard
            (INPUT iNumberBoard).
         DO WHILE lOk EQ TRUE:
            RUN chkWinner
               (OUTPUT iNumberBoard,
                OUTPUT lOk,
                OUTPUT cMessage).
            IF lvlDebug THEN DO:
               MESSAGE 
               SUBSTITUTE ("PlayBingo2, iChr = &1 (&5), Another Winner? &2 Which Board? &3, Message: &4",
                  iChr,
                  lOk,
                  iNumberBoard,
                  cMessage,
                  iExtraction[iChr])
               VIEW-AS ALERT-BOX.
            END.
            RUN deleteBoard
               (INPUT iNumberBoard).
         END.                
         NEXT PlayBingo2.
      END.   
      FOR EACH ttBoardNumber
      WHERE ttBoardNumber.iBoard     EQ iNumberBoard
      AND   ttBoardNumber.lExtracted EQ FALSE:
         ACCUM ttBoardNumber.iNumber (TOTAL).
      END.
      
      iSolution = iExtraction[iChr] * (ACCUM TOTAL ttBoardNumber.iNumber).
      LEAVE PlayBingo2.
   END. /* Found a Winner! */
END. /* PlayBingo */
 
OUTPUT TO "clipboard".
PUT UNFORMATTED iSolution SKIP.
OUTPUT CLOSE.

MESSAGE 
   SUBSTITUTE ("Last Winner Board: &2~nAfter extraction of: &3 (&4)~nSolution: &1.", 
      iSolution,
      iNumberboard,
      0,
      iChr) SKIP (1)
   SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
VIEW-AS ALERT-BOX TITLE " 2021 - Day 04 - Part Two".


/* **********************  Internal Procedures  *********************** */

PROCEDURE chkWinner:
/*------------------------------------------------------------------------------
 Purpose: Check if there's a winning board
 Notes:   All numbers in a row or column are extracted
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER opiWinnerBoard AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER oplOk          AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage     AS CHARACTER NO-UNDO.

DEFINE BUFFER ttCheckNumber FOR ttBoardNumber.

DEFINE VARIABLE lAllExtracted AS LOGICAL NO-UNDO.

   FOR EACH ttBoardNumber
   BREAK 
   BY ttBoardNumber.iBoard:
      
      /* Check Numbers by Row */
      FOR EACH ttCheckNumber
      WHERE ttCheckNumber.iBoard EQ ttBoardNumber.iBoard
      BREAK 
      BY ttCheckNumber.iRow:
         IF FIRST-OF (ttCheckNumber.iRow) THEN DO: 
            lAllExtracted = TRUE.
         END.               
   
         IF ttCheckNumber.lExtracted EQ FALSE THEN DO:
            lAllExtracted = FALSE.
         END.
         
         IF LAST-OF (ttCheckNumber.iRow) THEN DO:
            IF lAllExtracted EQ TRUE THEN DO:
               /* Found a Winner! */
               ASSIGN
                  opiWinnerBoard = ttBoardNumber.iBoard
                  oplOk          = TRUE 
                  opcMessage     = SUBSTITUTE ("Found a Winner! The Board # &1 has all numbers on Row &2 extracted!",
                     ttCheckNumber.iBoard,
                     ttCheckNumber.iRow) 
               .
               RETURN.
            END.
         END.
      END.
      
      /* Check Numbers by Column */
      FOR EACH ttCheckNumber
      WHERE ttCheckNumber.iBoard EQ ttBoardNumber.iBoard
      BREAK 
      BY ttCheckNumber.iCol:
         IF FIRST-OF (ttCheckNumber.iCol) THEN DO: 
            lAllExtracted = TRUE.
         END.               
   
         IF ttCheckNumber.lExtracted EQ FALSE THEN DO:
            lAllExtracted = FALSE.
         END.
         
         IF LAST-OF (ttCheckNumber.iCol) THEN DO:
            IF lAllExtracted EQ TRUE THEN DO:
               /* Found a Winner! */
               ASSIGN
                  opiWinnerBoard = ttBoardNumber.iBoard
                  oplOk          = TRUE 
                  opcMessage     = SUBSTITUTE ("Found a Winner! The Board # &1 has all numbers on Column &2 extracted!",
                     ttCheckNumber.iBoard,
                     ttCheckNumber.iCol) 
               .
               RETURN.
            END.
         END.
      END.
   END.
   
   ASSIGN 
      oplOk = FALSE 
      opcMessage = SUBSTITUTE ("No Winner found.")
   .

END PROCEDURE.

PROCEDURE deleteBoard:
/*------------------------------------------------------------------------------
 Purpose: Delete all numbers of a Board
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiBoardNumber AS INTEGER NO-UNDO.

DEFINE BUFFER ttDeleteNumber FOR ttBoardNumber.

   FOR EACH ttDeleteNumber
   WHERE ttDeleteNumber.iBoard EQ ipiBoardNumber:
      DELETE ttDeleteNumber.
   END.

END PROCEDURE.

PROCEDURE extractNumber:
/*------------------------------------------------------------------------------
 Purpose: Mark number as extracted on all Boards
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiNumber AS INTEGER NO-UNDO.

   FOR EACH ttBoardNumber 
   WHERE ttBoardNumber.iNumber EQ ipiNumber:
      ttBoardNumber.lExtracted = TRUE.
   END.

END PROCEDURE.
