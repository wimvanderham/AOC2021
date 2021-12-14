
/*------------------------------------------------------------------------
    File        : Day_13.p
    Purpose     : Solution Day 13 of Advent of Code
    URL         : https://adventofcode.com/2021/day/13

    Syntax      :

    Description : Solution Day 13 of Advent of Code

    Author(s)   : Wim van der Ham (WITS)
    Created     : Mon Dec 13 23:44:14 CET 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for input handling */
DEFINE VARIABLE lcInput      AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE iLine        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iChar        AS INTEGER   NO-UNDO.
DEFINE VARIABLE lPart        AS LOGICAL   NO-UNDO EXTENT 2.

/* Variables for solving */
/* Generic */
DEFINE VARIABLE iSolution    AS INT64     NO-UNDO.
DEFINE VARIABLE lOk          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvlDebug     AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lvlShow      AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE iPart        AS INTEGER   NO-UNDO.

/* Specific */
DEFINE VARIABLE cSection    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLastFoldNr AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcOutput    AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttPoint
   FIELD iX    AS INTEGER 
   FIELD iY    AS INTEGER
   FIELD cChar AS CHARACTER 
INDEX ind_XY IS UNIQUE iX iY.

DEFINE TEMP-TABLE ttFold
   FIELD iFoldNr AS INTEGER 
   FIELD cAxis   AS CHARACTER 
   FIELD iValue  AS INTEGER 
INDEX ind_FoldNr IS UNIQUE iFoldNr.
 
DEFINE BUFFER ttOverlapPoint FOR ttPoint.
 
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

/* ***************************  Main Block  *************************** */
DISPLAY
   lPart[1] LABEL "Solve Part 1?" COLON 15 SKIP
   lPart[2] LABEL "Solve Part 2?" COLON 15 SKIP 
   lvlDebug LABEL "Debug?"        COLON 15 SKIP 
   lvlShow  LABEL "Show?"         COLON 15 SKIP
WITH FRAME fr-Parameters SIDE-LABELS ROW 3 CENTERED TITLE " Parameters ".

UPDATE
   lPart
   lvlDebug
   lvlShow
WITH FRAME fr-Parameters.

/* Start Processing */
ETIME (YES).

COPY-LOB FROM FILE "input\13.txt" TO OBJECT lcInput.

/* Input file is divided in sections: Points and Folds */
cSection = "Points".

/* Read Input into Temp-table */
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).
   IF cLine EQ "" THEN DO:
      IF cSection EQ "Points" THEN
         /* Change of Section */
         cSection = "Folds".
      NEXT ReadBlock.
   END.
      
   CASE cSection:
      WHEN "Points" THEN DO:
         /* Points section contains coordinates of dots */
         CREATE ttPoint.
         ASSIGN 
            ttPoint.iX    = INTEGER (ENTRY (1, cLine))
            ttPoint.iY    = INTEGER (ENTRY (2, cLine))
            ttPoint.cChar = "#"
         .
      END.
      WHEN "Folds" THEN DO:
         /* Folds section contains fold information, e.g.:
         ** fold along x=655
         ** fold along y=447
         */
         iLastFoldNr = iLastFoldNr + 1.
         CREATE ttFold.
         ASSIGN 
            ttFold.iFoldNr = iLastFoldNr
            ttFold.cAxis   = ENTRY (1, ENTRY (3, cLine, " "), "=")
            ttFold.iValue  = INTEGER (ENTRY (2, ENTRY (3, cLine, " "), "="))
         .
      END.
   END.
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttPoint:HANDLE).
      
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttFold:HANDLE).
      
   RUN exportGrid
      (INPUT "output\ttPoint_0.txt").      
END.
   
/* Part One -  */
FOR EACH ttFold:
   /* Process Folds */
   CASE ttFold.cAxis:
      WHEN "x" THEN DO:
         /* Fold over x Axis */
         FOR EACH ttPoint
         WHERE ttPoint.iX GT ttFold.iValue:
            /* All Points to the Right of the Fold Axis
            ** will overlap corresponding point on the same line (Y-coordinate)
            ** with the same distance (ttPoint.iX - ttFold.iValue) 
            ** from the folding axis
            */
            FIND  ttOverlapPoint
            WHERE ttOverlapPoint.iX EQ ttFold.iValue - (ttPoint.iX - ttFold.iValue)
            AND   ttOverlapPoint.iY EQ ttPoint.iY NO-ERROR.
            IF NOT AVAILABLE ttOverlapPoint THEN DO:
               /* Overlap point not yet available */
               CREATE ttOverlapPoint.
               ASSIGN 
                  ttOverlapPoint.iX = ttFold.iValue - (ttPoint.iX - ttFold.iValue)
                  ttOverlapPoint.iY = ttPoint.iY
               .
            END.
            IF ttOverlapPoint.cChar EQ "#"
            OR ttPoint.cChar        EQ "#" THEN DO:
               ttOverlapPoint.cChar = "#".
            END.
            
            /* Remove Point */
            DELETE ttPoint.
         END.
      END. /* Fold over x Axis */
      WHEN "y" THEN DO:
         /* Fold over y Axis */
         FOR EACH ttPoint
         WHERE ttPoint.iY GT ttFold.iValue:
            FIND  ttOverlapPoint
            WHERE ttOverlapPoint.iX EQ ttPoint.iX
            AND   ttOverlapPoint.iY EQ ttFold.iValue - (ttPoint.iY - ttFold.iValue) NO-ERROR.
            IF NOT AVAILABLE ttOverlapPoint THEN DO:
               CREATE ttOverlapPoint.
               ASSIGN 
                  ttOverlapPoint.iX = ttPoint.iX
                  ttOverlapPoint.iY = ttFold.iValue - (ttPoint.iY - ttFold.iValue)
               .
            END.
            IF ttOverlapPoint.cChar EQ "#"
            OR ttPoint.cChar        EQ "#" THEN DO:
               ttOverlapPoint.cChar = "#".
            END.
            
            DELETE ttPoint.
         END.
      END. /* Fold over y Axis */
   END CASE.
   
   IF lvlShow THEN DO:
      RUN exportGrid
         (INPUT SUBSTITUTE ("output\ttPoint_&1.txt", ttFold.iFoldNr)).      
   END.
      
   /* After FIRST Fold, leave */
   IF  lPart[1] EQ TRUE
   AND ttFold.iFoldNr EQ 1 THEN DO:
      /* Process Part One, only One Fold */
      FOR EACH ttPoint
      WHERE ttPoint.cChar EQ "#":
         ACCUM "" (COUNT).
      END.

      ASSIGN
         iSolution = (ACCUM COUNT "")
      .
      
      OUTPUT TO "clipboard".
      PUT UNFORMATTED iSolution SKIP.
      OUTPUT CLOSE.
      
      MESSAGE 
         SUBSTITUTE ("Solution: &1.", 
            iSolution) SKIP (1)
         SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
      VIEW-AS ALERT-BOX TITLE " 2021 - Day 13 - Part One".

      IF lPart[2] EQ FALSE THEN DO: 
         LEAVE.
      END.
      ELSE DO:
         ETIME (YES).
      END. 
   END.
END. /* Process Folds */


IF lPart[2] THEN DO:
   /* Process Part Two */
   RUN exportGrid
      (INPUT "output\ttPoint.txt").      

   MESSAGE 
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2021 - Day 13 - Part Two".
   
   RUN sy\win\show-file.w
      (INPUT "output\ttPoint.txt").
      
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

/* **********************  Internal Procedures  *********************** */

PROCEDURE exportGrid:
/*------------------------------------------------------------------------------
 Purpose: Exports Grid in an output file
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcFileName AS CHARACTER NO-UNDO.

DEFINE VARIABLE iMinX AS INTEGER NO-UNDO.
DEFINE VARIABLE iMaxX AS INTEGER NO-UNDO.
DEFINE VARIABLE iMinY AS INTEGER NO-UNDO.
DEFINE VARIABLE iMaxY AS INTEGER NO-UNDO.
DEFINE VARIABLE iX    AS INTEGER NO-UNDO.
DEFINE VARIABLE iY    AS INTEGER NO-UNDO.

DEFINE BUFFER ttPoint FOR ttPoint.

   FOR EACH ttPoint:
      ACCUM ttPoint.iX (MAX).
      ACCUM ttPoint.iX (MIN).
      ACCUM ttPoint.iY (MAX).
      ACCUM ttPoint.iY (MIN).
   END.
   ASSIGN 
      iMinX = (ACCUM MIN ttPoint.iX)
      iMaxX = (ACCUM MAX ttPoint.iX)
      iMinY = (ACCUM MIN ttPoint.iY)
      iMaxY = (ACCUM MAX ttPoint.iY)
   .

   OUTPUT TO VALUE (ipcFileName).
   DO iY = iMinY TO iMaxY:
      DO iX = iMinX TO iMaxX:
         FIND  ttPoint
         WHERE ttPoint.iX EQ iX
         AND   ttPoint.iY EQ iY NO-ERROR.
         IF AVAILABLE ttPoint THEN DO:
            PUT UNFORMATTED ttPoint.cChar.
         END.
         ELSE DO:
            PUT UNFORMATTED " ".
         END.
      END.
      PUT UNFORMATTED SKIP.
   END.
      
END PROCEDURE.

/* ************************  Function Implementations ***************** */
