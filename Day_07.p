
/*------------------------------------------------------------------------
    File        : Day_07.p
    Purpose     : Solve Day7 of Advent of Code 2021
    URL         : https://adventofcode.com/2021/day/7

    Syntax      :

    Description : Solve Day 7 of Advent of Code 2021

    Author(s)   : Wim van der Ham (WITS)
    Created     : Tue Dec 07 15:41:51 CET 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for input handling */
DEFINE VARIABLE lcInput      AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE iLine        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iIndex       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lPart        AS LOGICAL   NO-UNDO EXTENT 2.

/* Variables for solving */
/* Generic */
DEFINE VARIABLE iSolution    AS INT64     NO-UNDO.
DEFINE VARIABLE lOk          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvlDebug     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lvlShow      AS LOGICAL   NO-UNDO INITIAL TRUE.
DEFINE VARIABLE iPart        AS INTEGER   NO-UNDO.

/* Specific */
DEFINE VARIABLE iPosition    AS INTEGER NO-UNDO.
DEFINE VARIABLE iMinPosition AS INTEGER NO-UNDO.
DEFINE VARIABLE iMaxPosition AS INTEGER NO-UNDO.
DEFINE VARIABLE iChkPosition AS INTEGER NO-UNDO.
DEFINE VARIABLE iMinDistance AS INTEGER NO-UNDO.
DEFINE VARIABLE iSumDistance AS INT64   NO-UNDO.
/* Part Two */
DEFINE VARIABLE iMinFuel     AS INTEGER NO-UNDO.
DEFINE VARIABLE iSumFuel     AS INT64   NO-UNDO.

DEFINE TEMP-TABLE ttFrequency RCODE-INFORMATION 
   FIELD iPosition  AS INTEGER 
   FIELD iFrequency AS INTEGER
INDEX ind_Position IS UNIQUE iPosition.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION getFuelUsage RETURNS INTEGER 
   (INPUT ipiDistance AS INTEGER) FORWARD.

FUNCTION getSumDistance RETURNS INT64 
   (INPUT ipiChkPosition AS INTEGER) FORWARD.

FUNCTION getSumFuel RETURNS INT64 
   (INPUT ipiChkPosition AS INTEGER ) FORWARD.

/* ***************************  Main Block  *************************** */
COPY-LOB FROM FILE "input\07.txt" TO OBJECT lcInput.

ETIME (YES).

cLine = ENTRY (1, lcInput, "~n").

DO iIndex = 1 TO NUM-ENTRIES (cLine):
   iPosition = INTEGER (ENTRY (iIndex, cLine)).
   FIND  ttFrequency 
   WHERE ttFrequency.iPosition EQ iPosition NO-ERROR.
   IF NOT AVAILABLE ttFrequency THEN DO:
      CREATE ttFrequency.
      ASSIGN
         ttFrequency.iPosition = iPosition
      .
   END.
   
   ASSIGN 
      ttFrequency.iFrequency = ttFrequency.iFrequency + 1
   .
END.

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttFrequency:HANDLE).
END.
   

/* Sets Part to Process */
ASSIGN 
   lPart[1] = FALSE 
   lPart[2] = TRUE  
.

ETIME (YES).

FIND FIRST ttFrequency.
iMinPosition = ttFrequency.iPosition.
FIND LAST ttFrequency.
iMaxPosition = ttFrequency.iPosition.

IF lPart[1] THEN DO:
   /* Process Part One */
   DO iChkPosition = iMinPosition TO iMaxPosition:
      iSumDistance = getSumDistance(iChkPosition).
      IF iMinDistance EQ 0
      OR iSumDistance LT iMinDistance THEN DO:
         iMinDistance = iSumDistance.
      END.
   END.   
   
   /* Part One - Find minimum total distance */
   ASSIGN 
      iSolution = iMinDistance.
   .
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2021 - Day 07 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   ETIME (YES).

   DO iChkPosition = iMinPosition TO iMaxPosition:
      iSumFuel = getSumFuel(iChkPosition).
      IF iMinFuel EQ 0
      OR iSumFuel LT iMinFuel THEN DO:
         iMinFuel = iSumFuel.
      END.
   END.   
   
   /* Part One - Find minimum total distance */
   ASSIGN 
      iSolution = iMinFuel.
   .
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2021 - Day 07 - Part Two".
END. /* Process Part Two */

CATCH oError AS Progress.Lang.Error :
   DEFINE VARIABLE iMessage      AS INTEGER   NO-UNDO.
   DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
   
   cErrorMessage = oError:GetMessage(1).
   iMessage = 2.
   DO WHILE iMessage LT oError:NumMessages:
      cErrorMessage = SUBSTITUTE ("&1~n&2", cErrorMessage, oError:GetMessage(iMessage)).
      iMessage = iMessage + 1.
   END.
   IF oError:CallStack NE ? THEN DO:
      cErrorMessage = SUBSTITUTE ("&1~n~nCall Stack:~n&2", cErrorMessage, oError:CallStack).
   END.
   
   MESSAGE "Error!" SKIP (1)
   SUBSTITUTE ("At line #: &1: &2", iLine, cLine) SKIP
   cErrorMessage SKIP(1) 
   VIEW-AS ALERT-BOX ERROR.

   RETURN.      
END CATCH.

/* ************************  Function Implementations ***************** */

FUNCTION getFuelUsage RETURNS INTEGER 
   (INPUT ipiDistance AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: Return Fuel Usage for Part Two
 Notes:   As it turns out, crab submarine engines don't burn fuel at a constant rate. 
          Instead, each change of 1 step in horizontal position costs 1 more unit 
          of fuel than the last: the first step costs 1, the second step costs 2, 
          the third step costs 3, and so on.
------------------------------------------------------------------------------*/   

   RETURN INTEGER (ipiDistance * (ipiDistance + 1) / 2).
      
END FUNCTION.

FUNCTION getSumDistance RETURNS INT64 
   (INPUT ipiChkPosition AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: Calculate the sum of all distances
 Notes:
------------------------------------------------------------------------------*/   

DEFINE VARIABLE iSumDistance AS INT64 NO-UNDO.

DEFINE BUFFER ttFrequency FOR ttFrequency.

   FOR EACH ttFrequency:
      iSumDistance = iSumDistance + 
         ABSOLUTE (iChkPosition - ttFrequency.iPosition) * ttFrequency.iFrequency.
   END.
   
   RETURN iSumDistance.

END FUNCTION.

FUNCTION getSumFuel RETURNS INT64 
   (INPUT ipiChkPosition AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: Calculate the sum of all fuel required to get to input Check Position
 Notes:
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iSumFuel AS INT64 NO-UNDO.

DEFINE BUFFER ttFrequency FOR ttFrequency.

   FOR EACH ttFrequency:
      iSumFuel = iSumFuel +
         getFuelUsage (ABSOLUTE (iChkPosition - ttFrequency.iPosition)) * ttFrequency.iFrequency.
   END.
   
   RETURN iSumFuel.
      
END FUNCTION.

/* **********************  Internal Procedures  *********************** */
