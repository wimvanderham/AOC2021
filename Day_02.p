
/*------------------------------------------------------------------------
    File        : Day_02.p
    Purpose     : 
    URL         : https://adventofcode.com/2021/day/2

    Syntax      :

    Description : Solve Day 2 of Advent of Code 2021

    Author(s)   : Wim van der Ham (WITS)
    Created     : Thu Dec 02 06:22:44 CET 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE lcInput     AS LONGCHAR NO-UNDO.
DEFINE VARIABLE iLine       AS INTEGER  NO-UNDO.
DEFINE VARIABLE cLine       AS CHARACTER NO-UNDO.

DEFINE VARIABLE iHorizontal AS INTEGER NO-UNDO.
DEFINE VARIABLE iDepth      AS INTEGER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
COPY-LOB FROM FILE "input\02.txt" TO OBJECT lcInput.

/* Part One - Calculate Horizontal Position and Depth based on input lines */
ETIME (YES).
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   cLine = ENTRY (iLine, lcInput, "~n").
   CASE ENTRY (1, cLine, " "):
      WHEN "forward" THEN 
         iHorizontal = iHorizontal + INTEGER (ENTRY (2, cLine, " ")).
      WHEN "up" THEN 
         iDepth = iDepth - INTEGER (ENTRY (2, cLine, " ")).
      WHEN "down" THEN 
         iDepth = iDepth + INTEGER (ENTRY (2, cLine, " ")).
   END CASE.
END.

OUTPUT TO "clipboard".
PUT UNFORMATTED iHorizontal * iDepth SKIP.
OUTPUT CLOSE.

MESSAGE 
   SUBSTITUTE ("Horizontal Position: &1~nDepth: &2~nSolution: &3.", iHorizontal, iDepth, iHorizontal * iDepth) SKIP (1)
   SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
VIEW-AS ALERT-BOX TITLE " 2021 - Day 02 - Part One".
         
/* Part Two - Calculate Horizontal Position and Depth using Aim */
DEFINE VARIABLE iAim AS INTEGER NO-UNDO.

ETIME (YES).
ASSIGN 
   iHorizontal = 0
   iDepth      = 0
.

DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   cLine = ENTRY (iLine, lcInput, "~n").
   CASE ENTRY (1, cLine, " "):
      WHEN "forward" THEN DO:
         iHorizontal = iHorizontal + INTEGER (ENTRY (2, cLine, " ")).
         iDepth      = iDepth + (iAim * INTEGER (ENTRY (2, cLine, " "))).
      END.
      WHEN "up" THEN 
         iAim = iAim - INTEGER (ENTRY (2, cLine, " ")).
      WHEN "down" THEN 
         iAim = iAim + INTEGER (ENTRY (2, cLine, " ")).
   END CASE.
END.

OUTPUT TO "clipboard".
PUT UNFORMATTED iHorizontal * iDepth SKIP.
OUTPUT CLOSE.

MESSAGE 
   SUBSTITUTE ("Horizontal Position: &1~nDepth: &2~nSolution: &3.", iHorizontal, iDepth, iHorizontal * iDepth) SKIP (1)
   SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
VIEW-AS ALERT-BOX TITLE " 2021 - Day 02 - Part Two".
         