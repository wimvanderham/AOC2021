/*------------------------------------------------------------------------
    File        : Day_03.p
    Purpose     : Solve day 3 of Advent of Code 2021
    URL         : https://adventofcode.com/2021/day/3

    Syntax      :

    Description : Solve day 3 of Advent of Code 2021

    Author(s)   : Wim van der Ham (WITS)
    Created     : Fri Dec 03 13:07:24 CET 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE lcInput      AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE iLine        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iChr         AS INTEGER   NO-UNDO.
DEFINE VARIABLE cGamma       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEpsilon     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iOne         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iZero        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iGamma       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iEpsilon     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iSolution    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cOxygen      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCO2         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iOxygen      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCO2         AS INTEGER   NO-UNDO.
DEFINE VARIABLE iSelectedBit AS INTEGER   NO-UNDO.
DEFINE VARIABLE lvlDebug     AS LOGICAL   NO-UNDO.

/* Temp-table for accessing single bits */
DEFINE TEMP-TABLE ttBit NO-UNDO
   FIELD iRow AS INTEGER 
   FIELD iCol AS INTEGER 
   FIELD iBit AS INTEGER 
INDEX indRowCol IS UNIQUE iRow iCol
INDEX indColRow IS UNIQUE iCol iRow.

DEFINE BUFFER ttRemove FOR ttBit.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION Binary2Integer RETURNS INTEGER 
   (INPUT ipcBinary AS CHARACTER) FORWARD.

FUNCTION getLeastCommon RETURNS INTEGER 
   (INPUT ipiCol AS INTEGER) FORWARD.

FUNCTION getMostCommon RETURNS INTEGER 
   (INPUT ipiCol AS INTEGER) FORWARD.

/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE deleteRow:
/*------------------------------------------------------------------------------
 Purpose: Deletes the ttBit records of a specific Row
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiRow AS INTEGER NO-UNDO.

DEFINE BUFFER ttBitRow FOR ttBit.

   FOR EACH ttBitRow
   WHERE ttBitRow.iRow EQ ipiRow:
      DELETE ttBitRow.
   END.

END PROCEDURE.

COPY-LOB FROM FILE "input\03.txt" TO OBJECT lcInput.

/* Part One - */
ETIME (YES).
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   cLine = ENTRY (iLine, lcInput, "~n").
   DO iChr = 1 TO LENGTH (cLine):
      CREATE ttBit.
      ASSIGN
         ttBit.iRow = iLine
         ttBit.iCol = iChr
         ttBit.iBit = INTEGER (SUBSTRING (cLine, iChr, 1))
      .
   END.
END.

FOR EACH ttBit
BREAK 
BY ttBit.iCol
BY ttBit.iRow:
   IF FIRST-OF (ttBit.iCol) THEN DO:
      ASSIGN 
         iOne  = 0
         iZero = 0
      .
   END.
   CASE ttBit.iBit:
      WHEN 1 THEN 
         iOne = iOne + 1.
      WHEN 0 THEN 
         iZero = iZero + 1.
   END.
   
   IF LAST-OF (ttBit.iCol) THEN DO:
      IF iOne GE iZero THEN
         SUBSTRING (cGamma, ttBit.iCol, 1)   = "1".
      ELSE
         SUBSTRING (cGamma,   ttBit.iCol, 1) = "0".


      IF iZero LE iOne THEN 
         SUBSTRING (cEpsilon, ttBit.iCol, 1) = "0".
      ELSE 
         SUBSTRING (cEpsilon, ttBit.iCol, 1) = "1".
   END.
END.

ASSIGN 
   iGamma    = Binary2Integer(cGamma)
   iEpsilon  = Binary2Integer(cEpsilon)
   iSolution = iGamma * iEpsilon
.

OUTPUT TO "clipboard".
PUT UNFORMATTED iSolution SKIP.
OUTPUT CLOSE.

MESSAGE 
   SUBSTITUTE ("Gamma = &1 (&2)~nEpsilon = &3 (&4)~nSolution: &5.", 
      cGamma, iGamma, cEpsilon, iEpsilon, iSolution) SKIP (1)
   SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
VIEW-AS ALERT-BOX TITLE " 2021 - Day 03 - Part One".

/* Part Two - Reduce */
ETIME (YES).

/* Export temp-table for later */
TEMP-TABLE ttBit:WRITE-JSON ("file", "output\ttBit.json").

/* Oxygen */
cOxygen = "".
ReduceBlock:
DO iChr = 1 TO LENGTH (cGamma):
   iSelectedBit = getMostCommon(iChr).
   
   FOR EACH ttRemove
   WHERE ttRemove.iCol EQ iChr
   AND   ttRemove.iBit NE iSelectedBit
   BREAK 
   BY ttRemove.iRow:
      /* Remove the Rows that don't have the bit in this position */
      IF LAST-OF (ttRemove.iRow) THEN DO:
         RUN deleteRow
            (INPUT ttRemove.iRow).
      END.
   END.
   FOR EACH ttRemove
   BREAK 
   BY ttRemove.iRow:
      IF FIRST-OF (ttRemove.iRow) THEN
         ACCUM "" (COUNT).
   END.
   IF (ACCUM COUNT "") EQ 1 THEN DO:
      /* Only one row left, found the solution */
      LEAVE ReduceBlock.
   END.
   ELSE DO:
/*      MESSAGE SUBSTITUTE ("There are &1 rows left.", (ACCUM COUNT ""))*/
/*      VIEW-AS ALERT-BOX.                                              */
   END.
END.

FOR EACH ttBit
BY ttBit.iCol:
   SUBSTRING (cOxygen, ttBit.iCol, 1) = STRING (ttBit.iBit).
END.

iOxygen = Binary2Integer(cOxygen).

/* CO2 */
TEMP-TABLE ttBit:READ-JSON ("file", "output\ttBit.json", "EMPTY").

cCO2 = "".
ReduceBlock:
DO iChr = 1 TO LENGTH (cEpsilon):
   iSelectedBit = getLeastCommon(iChr).
   FOR EACH ttRemove
   WHERE ttRemove.iCol EQ iChr
   AND   ttRemove.iBit NE iSelectedBit
   BREAK 
   BY ttRemove.iRow:
      /* Remove the Rows that don't have the bit */
      IF LAST-OF (ttRemove.iRow) THEN DO:
         RUN deleteRow
            (INPUT ttRemove.iRow).
      END.
   END.
   FOR EACH ttRemove
   BREAK 
   BY ttRemove.iRow:
      IF FIRST-OF (ttRemove.iRow) THEN
         ACCUM "" (COUNT).
   END.
   IF (ACCUM COUNT "") EQ 1 THEN DO:
      /* Only one row left, found the solution */
      LEAVE ReduceBlock.
   END.
   ELSE DO:
/*      MESSAGE SUBSTITUTE ("There are &1 rows left.", (ACCUM COUNT ""))*/
/*      VIEW-AS ALERT-BOX.                                              */
   END.
END.

FOR EACH ttBit
BY ttBit.iCol:
   SUBSTRING (cCO2, ttBit.iCol, 1) = STRING (ttBit.iBit).
END.

iCO2 = Binary2Integer(cCO2).
   
ASSIGN 
   iSolution = iOxygen * iCO2
.

OUTPUT TO "clipboard".
PUT UNFORMATTED iSolution SKIP.
OUTPUT CLOSE.

MESSAGE 
   SUBSTITUTE ("Oxygen = &1 (&2)~nCO2 = &3 (&4)~nSolution: &5.", 
      cOxygen, iOxygen, cCO2, iCO2, iSolution) SKIP (1)
   SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
VIEW-AS ALERT-BOX TITLE " 2021 - Day 03 - Part Two".
   

/* ************************  Function Implementations ***************** */


FUNCTION Binary2Integer RETURNS INTEGER 
   (INPUT ipcBinary AS CHARACTER   ):
/*------------------------------------------------------------------------------
 Purpose: Converts a binary string (with 0's and 1's) to it's integer value
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iExp    AS INTEGER NO-UNDO.
DEFINE VARIABLE iChr    AS INTEGER NO-UNDO.
DEFINE VARIABLE iBit    AS INTEGER NO-UNDO.
DEFINE VARIABLE iResult AS INTEGER NO-UNDO.

   iExp = 1.
   DO iChr = LENGTH (ipcBinary) TO 1 BY -1:
      iBit = INTEGER (SUBSTRING (ipcBinary, iChr, 1)).
      
      iResult = iResult + (iBit * iExp).
      
      iExp = iExp * 2.
   END.
   
   RETURN iResult.
      
END FUNCTION.

FUNCTION getLeastCommon RETURNS INTEGER 
   (INPUT ipiCol AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: Returns the least common value 
 Notes:   In case of a draw 0 wins
------------------------------------------------------------------------------*/   
DEFINE BUFFER ttLeastCommon FOR ttBit.

DEFINE VARIABLE iOne  AS INTEGER NO-UNDO.
DEFINE VARIABLE iZero AS INTEGER NO-UNDO.

   FOR EACH ttLeastCommon
   WHERE ttLeastCommon.iCol EQ ipiCol:
      IF ttLeastCommon.iBit EQ 1 THEN 
         ACCUM "1" (COUNT).
      ELSE 
         ACCUM "0" (COUNT).
   END.
   
   IF (ACCUM COUNT "0") LE (ACCUM COUNT "1") THEN
      RETURN 0.
   ELSE 
      RETURN 1.

END FUNCTION.

FUNCTION getMostCommon RETURNS INTEGER 
   (INPUT ipiCol AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: Returns the most common value in a specific column
 Notes:   In case of a draw 1 wins
------------------------------------------------------------------------------*/   

DEFINE BUFFER ttMostCommon FOR ttBit.

DEFINE VARIABLE iOne  AS INTEGER NO-UNDO.
DEFINE VARIABLE iZero AS INTEGER NO-UNDO.

   FOR EACH ttMostCommon
   WHERE ttMostCommon.iCol EQ ipiCol:
      IF ttMostCommon.iBit EQ 1 THEN 
         ACCUM "1" (COUNT).
      ELSE 
         ACCUM "0" (COUNT).
   END.
   
   IF (ACCUM COUNT "1") GE (ACCUM COUNT "0") THEN
      RETURN 1.
   ELSE 
      RETURN 0.

END FUNCTION.
