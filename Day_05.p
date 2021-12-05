
/*------------------------------------------------------------------------
    File        : Day_05.p
    Purpose     : Solve Day 5 of Advent of Code 2021
    URL         : https://adventofcode.com/2021/day/5

    Syntax      :

    Description : Solve Day 5 of Advent of Code 2021

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sun Dec 05 06:30:20 CET 2021
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
DEFINE VARIABLE lvlShow      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iPart        AS INTEGER   NO-UNDO.

/* Specific */
DEFINE VARIABLE iX           AS INTEGER   NO-UNDO EXTENT 2.
DEFINE VARIABLE iY           AS INTEGER   NO-UNDO EXTENT 2.
DEFINE VARIABLE cFrom        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTo          AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttPoint
   FIELD iX     AS INTEGER 
   FIELD iY     AS INTEGER 
   FIELD iLines AS INTEGER
INDEX ind_XY IS UNIQUE iX iY
INDEX ind_Lines iLines. 


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

COPY-LOB FROM FILE "input\05.txt" TO OBJECT lcInput.

DO iPart = 1 TO 2:
   /* Part One and Two Together
   ** In Part One only Horizonal and Vertical Lines
   ** In Part Two also Diagonal Lines 
   */
   ETIME (YES).
   ReadLine:
   DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
      cLine = ENTRY (iLine, lcInput, "~n").
      /* Sample input:
      ** 72,504 -> 422,154
      ** 877,851 -> 680,654
      ** 447,989 -> 517,989
      ** 173,125 -> 981,933
      ** 736,255 -> 374,617
      ** 835,681 -> 693,539
      ** 451,176 -> 451,885
      */
      IF cLine EQ "" THEN 
         NEXT ReadLine.
         
      ASSIGN 
         cFrom = TRIM (ENTRY (1, cLine, " "))
         cTo   = TRIM (ENTRY (3, cLine, " "))
      .
      IF lvlDebug THEN DO:
         MESSAGE 
            SUBSTITUTE ("Extract coordinates from From: '&1' To: '&2'",
               cFrom,
               cTo)
            SKIP (2)
               "Continue with Debug On?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE lvlDebug.
      END.
                  
      ASSIGN
         iX[1] = INTEGER (ENTRY (1, cFrom))
         iY[1] = INTEGER (ENTRY (2, cFrom))
         iX[2] = INTEGER (ENTRY (1, cTo))
         iY[2] = INTEGER (ENTRY (2, cTo))
      .
      
      IF lvlDebug THEN DO:
         MESSAGE 
            SUBSTITUTE ("Line #: &1: &2", iLine, cLine) SKIP 
            SUBSTITUTE ("From (&1,&2) -> To (&3,&4).", iX[1], iY[1], iX[2], iY[2]) SKIP (2)
            "Continue with Debug On?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE lvlDebug.
      END.
      
      IF iPart EQ 1 THEN DO:
         /* In Part One only Horizontal or Vertical Lines */
         IF iX[1] EQ iX[2]
         OR iY[1] EQ iY[2] THEN DO:
            /* Horizontal or Vertical Line, draw it */
            IF lvlDebug THEN DO: 
               MESSAGE "Draw Line" SKIP (2)
                  "Continue with Debug On?"
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE lvlDebug.
            END.
            RUN drawLine
               (INPUT iX,
                INPUT iY).
         END. /* Horizontal or Vertical Line, draw it */
      END. /* In Part One only Horizontal or Vertical Lines */
      ELSE DO:
         /* In Part Two, all lines */
         RUN drawLine
            (INPUT iX,
             INPUT iY).
      END. /* In Part Two, all lines */ 
   END.
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttPoint:HANDLE).
   END.
   
   /* Part One - Find the winner */
   FOR EACH ttPoint 
   WHERE ttPoint.iLines GE 2:
      ACCUM "" (COUNT).
   END.
   ASSIGN 
      iSolution = (ACCUM COUNT "")
   .
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   IF iPart EQ 1 THEN DO: 
      MESSAGE 
         SUBSTITUTE ("Solution: &1.", 
            iSolution) SKIP (1)
         SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2021 - Day 05 - Part One".
      EMPTY TEMP-TABLE ttPoint.
   END.
   ELSE DO: 
      MESSAGE 
         SUBSTITUTE ("Solution: &1.", 
            iSolution) SKIP (1)
         SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2021 - Day 05 - Part Two".
   END.
END. /* DO iPart = 1 TO 2: */  

CATCH oError AS Progress.Lang.Error :
   DEFINE VARIABLE iMessage      AS INTEGER   NO-UNDO.
   DEFINE VARIABLE cErrorMessage AS CHARACTER NO-UNDO.
   
   cErrorMessage = oError:GetMessage(1).
   iMessage = 2.
   DO WHILE iMessage LT oError:NumMessages:
      cErrorMessage = SUBSTITUTE ("&1~n&2", cErrorMessage, oError:GetMessage(iMessage)).
      iMessage = iMessage + 1.
   END.
   MESSAGE "Error!" SKIP (1)
   SUBSTITUTE ("At line #: &1: &2", iLine, cLine) SKIP
   cErrorMessage SKIP(1) 
   "Call Stack:" SKIP 
   oError:CallStack
   VIEW-AS ALERT-BOX ERROR.

   RETURN.      
END CATCH.


/* **********************  Internal Procedures  *********************** */

PROCEDURE drawLine:
/*------------------------------------------------------------------------------
 Purpose: Draws a Line between Two Points
 Notes:   
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiX AS INTEGER NO-UNDO EXTENT 2.
DEFINE INPUT  PARAMETER ipiY AS INTEGER NO-UNDO EXTENT 2.

DEFINE VARIABLE iMoveX AS INTEGER NO-UNDO.
DEFINE VARIABLE iMoveY AS INTEGER NO-UNDO.

   /* Determine direction of X coordinates */
   IF ipiX[1] EQ ipiX[2] THEN
      iMoveX = 0.
   ELSE DO:
      IF ipiX[1] GT ipiX[2] THEN 
         iMoveX = -1.
      ELSE 
         iMoveX = 1.
   END.
   /* Determine direction of Y coordinates */
   IF ipiY[1] EQ ipiY[2] THEN
      iMoveY = 0.
   ELSE DO:
      IF ipiY[1] GT ipiY[2] THEN 
         iMoveY = -1.
      ELSE 
         iMoveY = 1.
   END.
   
   IF lvlDebug THEN DO:
      MESSAGE 
         SUBSTITUTE ("Points along line: (&1,&2) -> (&3, &4).", 
            ipiX[1], ipiY[1],
            ipiX[2], ipiY[2]) SKIP 
         SUBSTITUTE ("Moving X: &1 and Y: &2",
         iMoveX,
         iMoveY) SKIP (2)
            "Continue with Debug On?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE lvlDebug.
   END.
   
   DrawBlock:
   REPEAT:
      /* Move coordinates along the line X[1]Y[1] -> X[2]Y[2] */
      FIND  ttPoint
      WHERE ttPoint.iX EQ ipiX[1]
      AND   ttPoint.iY EQ ipiY[1] NO-ERROR.
      IF NOT AVAILABLE ttPoint THEN DO:
         CREATE ttPoint.
         ASSIGN 
            ttPoint.iX = ipiX[1]
            ttPoint.iY = ipiY[1]
         .
      END.
      ASSIGN 
         ttPoint.iLines = ttPoint.iLines + 1
      .
      
      IF  ipiX[1] EQ ipiX[2]
      AND ipiY[1] EQ ipiY[2] THEN DO:
         /* Reached destination, finished Draw Line */
         LEAVE DrawBlock.
      END.
            
      /* Move forward along the line */
      ASSIGN 
         ipiX[1] = ipiX[1] + iMoveX
         ipiY[1] = ipiY[1] + iMoveY
      .
      IF lvlDebug THEN DO:
         MESSAGE 
            SUBSTITUTE ("New Coordinates of X[1], Y[1]: (&1,&2)",
               ipiX[1],
               ipiY[1]) SKIP (2)
            "Continue with Debug On?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE lvlDebug.
      END.
   END. /* Move coordinates along the line X[1]Y[1] -> X[2]Y[2] */
      
END PROCEDURE.
