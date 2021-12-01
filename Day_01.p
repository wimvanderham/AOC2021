
/*------------------------------------------------------------------------
    File        : Day_01.p
    Purpose     : Solve Day 1 of Advent of Code 2021
    URL         : https://adventofcode.com/2021/day/1
    Title       : --- Day 1: Sonar Sweep ---

    Syntax      : run Day_01.p

    Description : Day One of 2021

    Author(s)   : Wim van der Ham (WITS)
    Created     : Wed Dec 01 06:28:58 CET 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE lcInput     AS LONGCHAR NO-UNDO.
DEFINE VARIABLE iLine       AS INTEGER  NO-UNDO.
DEFINE VARIABLE lDebug      AS LOGICAL  NO-UNDO.

DEFINE VARIABLE iDepth      AS INTEGER   NO-UNDO.
DEFINE VARIABLE iPrevDepth  AS INTEGER   NO-UNDO.
DEFINE VARIABLE lIncrease   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iNrIncrease AS INTEGER   NO-UNDO.
DEFINE VARIABLE cValue      AS CHARACTER NO-UNDO.

COPY-LOB FROM FILE "input\01.txt" TO OBJECT lcInput.

/* Part One - Simple check Line by Line, Increase ? */
ETIME (YES).
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n")
WITH DOWN:
   cValue = ENTRY (iLine, lcInput, "~n").
   
   PAUSE 0 BEFORE-HIDE.
   IF lDebug THEN DO:
      DISPLAY 
         iLine  LABEL "Line#"
         cValue LABEL "Value"
      .
   END.
      
   iDepth = INTEGER (ENTRY (iLine, lcInput, "~n")).
   
   IF iLine GT 1 THEN DO:
      
      lIncrease = iDepth GT iPrevDepth.
      
      IF lIncrease THEN 
         iNrIncrease = iNrIncrease + 1.
      
      IF lDebug THEN DO:
         DISPLAY 
            lIncrease FORMAT "Increase/" LABEL "Increase?"
            iNrIncrease LABEL "NrIncreases"
         .
      END.
            
   END.
   
   iPrevDepth = iDepth.
END.

OUTPUT TO "clipboard".
PUT UNFORMATTED iNrIncrease SKIP.
OUTPUT CLOSE.

MESSAGE 
   SUBSTITUTE ("Number of increases: &1.", iNrIncrease) SKIP (1)
   SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
VIEW-AS ALERT-BOX TITLE " 2021 - Day 01 - Part One".
         
         
/* Part Two - Sliding Window check - 3 lines by 3 lines */
ETIME (YES).
DEFINE VARIABLE iMeasurement AS INTEGER NO-UNDO.
DEFINE VARIABLE iWindow1     AS INTEGER NO-UNDO.
DEFINE VARIABLE iWindow2     AS INTEGER NO-UNDO.

iNrIncrease = 0.
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n") - 3: /* Stop if there aren't three measurements available */
   ASSIGN 
      iWindow1 = 0
      iWindow2 = 0
   .
   
   DO iMeasurement = 1 TO 3:
      iWindow1 = iWindow1 +
         INTEGER (ENTRY (iLine + iMeasurement - 1, lcInput, "~n")).
      iWindow2 = iWindow2 +
         INTEGER (ENTRY (iLine + iMeasurement, lcInput, "~n")).
         
   END.
   
   IF iWindow2 GT iWindow1 THEN
      iNrIncrease = iNrIncrease + 1.
      
END. 

OUTPUT TO "clipboard".
PUT UNFORMATTED iNrIncrease SKIP.
OUTPUT CLOSE.

MESSAGE 
   SUBSTITUTE ("Number of increases: &1.", iNrIncrease) SKIP (1)
   SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
VIEW-AS ALERT-BOX TITLE " 2021 - Day 01 - Part Two".
