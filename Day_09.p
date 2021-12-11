
/*------------------------------------------------------------------------
    File        : Day_09.p
    Purpose     : Solution to Day 9 of Advent of Code 2021
    URL         : https://adventofcode.com/2021/day/9

    Syntax      :

    Description : Solution to Day 9 of Advent of Code 2021

    Author(s)   : Wim van der Ham (WITS)
    Created     : Thu Dec 09 15:05:52 CET 2021
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
DEFINE VARIABLE iRiskLevel AS INTEGER NO-UNDO.
DEFINE VARIABLE lviBasin   AS INTEGER NO-UNDO.
/* Variables for Patrick Tingen's solution */
DEFINE VARIABLE cData AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE iRisk AS INTEGER   NO-UNDO.
DEFINE VARIABLE iX    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iY    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxX AS INTEGER   NO-UNDO.
DEFINE VARIABLE iMaxY AS INTEGER   NO-UNDO.

/* Temp-table of Points with Height */
DEFINE TEMP-TABLE ttPoint RCODE-INFORMATION 
   FIELD iX          AS INTEGER 
   FIELD iY          AS INTEGER 
   FIELD iHeight     AS INTEGER 
   FIELD lLow        AS LOGICAL 
   FIELD iRiskLevel  AS INTEGER
   FIELD iPatrick    AS INTEGER
   FIELD lDifference AS LOGICAL
   /* Part Two - Group Points by Basin */
   FIELD iBasin      AS INTEGER 
   FIELD lExplored   AS LOGICAL  
INDEX ind_XY    IS UNIQUE iX iY
INDEX ind_Basin iBasin iHeight DESCENDING.

DEFINE BUFFER ttBasin   FOR ttPoint.
DEFINE BUFFER ttLow     FOR ttPoint.
DEFINE BUFFER ttPrevLow FOR ttPoint.

DEFINE TEMP-TABLE ttSumBasin RCODE-INFORMATION 
   FIELD iBasin AS INTEGER 
   FIELD iSize  AS INTEGER 
INDEX ind_Basin IS UNIQUE iBasin
INDEX ind_Size  iSize DESCENDING.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION getHeight RETURNS INTEGER 
   (INPUT piX AS INTEGER,
    INPUT piY AS INTEGER) FORWARD.

FUNCTION getLower RETURNS INTEGER 
   (INPUT ipiX      AS INTEGER,
    INPUT ipiY      AS INTEGER,
    INPUT ipiHeight AS INTEGER) FORWARD.

FUNCTION getRiskLevel RETURNS INTEGER 
   (INPUT piX AS INTEGER,
    INPUT piY AS INTEGER) FORWARD.

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

COPY-LOB FROM FILE "input\09.txt" TO OBJECT lcInput.

/* Prepare data for Patrick's Solution */
cData = lcInput.
cData = TRIM(REPLACE(cData,'~r',''),'~n').
iMaxX = LENGTH(ENTRY(1,cData,'~n')).
iMaxY = NUM-ENTRIES(cData,'~n').

/* Read Input into Temp-table */
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).
   IF cLine EQ "" THEN 
      NEXT ReadBlock.
      
   DO iChar = 1 TO LENGTH (cLine):
      CREATE ttPoint.
      ASSIGN 
         ttPoint.iX      = iChar
         ttPoint.iY      = iLine
         ttPoint.iHeight = INTEGER (SUBSTRING (cLine, iChar, 1))
         ttPoint.lLow    = ?
      .
   END.
   
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttPoint:HANDLE).
END.
   
FOR EACH ttPoint
WHERE ttPoint.lLow EQ ?:
   /* All the Point for which we don't know if it's a Low Point */
   ttPoint.lLow = getLower(ttPoint.iX, ttPoint.iY, ttPoint.iHeight) EQ 0.
   IF ttPoint.lLow THEN 
      ttPoint.iRiskLevel = ttPoint.iHeight + 1.
   ttPoint.iPatrick    = getRiskLevel(ttPoint.iX, ttPoint.iY).
   ttPoint.lDifference = ttPoint.iRiskLevel NE ttPoint.iPatrick.
END.

IF lPart[1] THEN DO:
   /* Process Part One */

   /* Part One - Calculate RiskLevel for all Low Points */
   iSolution = 0.
   FOR EACH ttPoint:
      ACCUM ttPoint.iPatrick   (TOTAL).
      ACCUM ttPoint.iRiskLevel (TOTAL).
   END.
   
   ASSIGN
      iSolution = (ACCUM TOTAL ttPoint.iPatrick)
   .

   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttPoint:HANDLE).
   END.

   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2021 - Day 09 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   ETIME (YES).

   lviBasin = 0.   
   BasinBlock:
   REPEAT:
      /* REPEAT Until all (real) Points are part of a Basin */
      FIND FIRST ttPoint
      WHERE ttPoint.iBasin  EQ 0
      AND   ttPoint.iHeight NE 9 NO-ERROR.
      IF NOT AVAILABLE ttPoint THEN 
         LEAVE BasinBlock.
   
      FIND FIRST ttLow
      WHERE ttLow.lLow   EQ TRUE 
      AND   ttLow.iBasin EQ 0.
      ASSIGN 
         lviBasin        = lviBasin + 1
         ttLow.iBasin    = lviBasin
         ttLow.lExplored = FALSE
      .
      
      IF lvlDebug THEN DO:
         MESSAGE SUBSTITUTE ("Process ttLow (&1, &2) in Basin &3.", ttLow.iX, ttLow.iY, lviBasin)
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lvlDebug.
      END.
      
      /* Keep Track of Starting Low Point */
      FIND  ttPrevLow
      WHERE ttPrevLow.iX EQ ttLow.iX
      AND   ttPrevLow.iY EQ ttLow.iY.
      
      ExploreBlock:
      REPEAT:
         /* Explore Block */      
         FOR EACH ttPoint
         WHERE ((ttPoint.iX EQ ttLow.iX     AND ttPoint.iY EQ ttLow.iY - 1)
         OR     (ttPoint.iX EQ ttLow.iX     AND ttPoint.iY EQ ttLow.iY + 1)
         OR     (ttPoint.iX EQ ttLow.iX - 1 AND ttPoint.iY EQ ttLow.iY)
         OR     (ttPoint.iX EQ ttLow.iX + 1 AND ttPoint.iY EQ ttLow.iY))
         AND   ttPoint.iBasin  EQ 0
         AND   ttPoint.iHeight NE 9:
            /* Add all surrounding points (up, left, right, down) to basin */ 
            ASSIGN 
               ttPoint.iBasin    = ttLow.iBasin
               ttPoint.lExplored = FALSE 
            .
         END.
         /* Current ttLow has been explored */
         ASSIGN 
            ttLow.lExplored = TRUE 
         .
         
         /* Find Next ttLow to explore */ 
         FIND FIRST ttLow
         WHERE ttLow.iBasin    EQ lviBasin
         AND   ttLow.lExplored EQ FALSE NO-ERROR.
         IF NOT AVAILABLE ttLow THEN
            /* All done for this ttPrevLow starting Low Point */
            LEAVE ExploreBlock.  
      END. /* REPEAT: Explore Block */
      ASSIGN 
         ttPrevLow.lExplored = TRUE
      .
   END. /* BasinBlock: REPEAT: */
   
   /* All Points should be assigned to a Basin */
   FIND FIRST ttPoint
   WHERE ttPoint.iBasin  EQ 0
   AND   ttPoint.iHeight NE 9 NO-ERROR.
   IF AVAILABLE ttPoint THEN 
      UNDO, THROW NEW Progress.Lang.AppError (SUBSTITUTE ("Still a point (&1,&2) to assign to Basin", ttPoint.iX, ttPoint.iY), 999).      

   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttPoint:HANDLE).
   END.
   
   /* Force Large Scoping */
   FIND FIRST ttSumBasin NO-ERROR.
   FOR EACH ttPoint WHERE ttPoint.iBasin GT 0
   BREAK
   BY ttPoint.iBasin:
      IF FIRST-OF (ttPoint.iBasin) THEN DO:
         CREATE ttSumBasin.
         ASSIGN 
            ttSumBasin.iBasin = ttPoint.iBasin
         .
      END.
      ASSIGN 
         ttSumBasin.iSize = ttSumBasin.iSize + 1
      .
   END.

   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttSumBasin:HANDLE).
   END.
   
   ASSIGN
      iSolution = 0
   .
   
   SolutionBlock:
   FOR EACH ttSumBasin
   BY ttSumBasin.iSize DESCENDING:
      /* SolutionBlock */
      ACCUM "" (COUNT).
      IF iSolution EQ 0 THEN 
         iSolution = ttSumBasin.iSize.
      ELSE 
         iSolution = iSolution * iSize.
      IF (ACCUM COUNT "") EQ 3 THEN
         /* Top 3 */
         LEAVE SolutionBlock. 
   END. /* SolutionBlock */
      
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2021 - Day 09 - Part Two".
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

/* ************************  Function Implementations ***************** */

FUNCTION getHeight RETURNS INTEGER 
   (INPUT piX AS INTEGER,
    INPUT piY AS INTEGER ):
/*------------------------------------------------------------------------------
 Purpose: Returns the Height of a specific point
 Notes:   Taken from Patrick Tingen's Repository
 URL:     https://github.com/patrickTingen/AdventOfCode/blob/master/2021/Day-09/day-09a.p
------------------------------------------------------------------------------*/   

  IF    piX >= 1 AND piX <= iMaxX
    AND piY >= 1 AND piY <= iMaxY THEN
    RETURN INTEGER(SUBSTRING(ENTRY(piY, cData, '~n'), piX, 1)).
  ELSE
    RETURN -1.

END FUNCTION. /* getHeight */
 
FUNCTION getLower RETURNS INTEGER 
   (INPUT ipiX      AS INTEGER,
    INPUT ipiY      AS INTEGER,
    INPUT ipiHeight AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: Calculate the number of neighbours 
          (horizontal, vertical but not diagonal) that are Lower than the input
 Notes:
------------------------------------------------------------------------------*/   
DEFINE BUFFER ttNeighbour FOR ttPoint.

DEFINE VARIABLE iDeltaX     AS INTEGER NO-UNDO.
DEFINE VARIABLE iDeltaY     AS INTEGER NO-UNDO.
DEFINE VARIABLE iNewX       AS INTEGER NO-UNDO.
DEFINE VARIABLE iNewY       AS INTEGER NO-UNDO.
DEFINE VARIABLE iLower      AS INTEGER NO-UNDO.
DEFINE VARIABLE iNeighbours AS INTEGER NO-UNDO.

DEFINE VARIABLE hWindow AS HANDLE  NO-UNDO.

   IF lvlDebug THEN DO:
      CREATE WINDOW hWindow
      ASSIGN
         WIDTH-CHARS  = 134
         HEIGHT-CHARS = 20
      .
      CURRENT-WINDOW = hWindow.
   END.
   
   ASSIGN 
      iLower      = 0
      iNeighbours = 0
   .
   
   SearchX:
   REPEAT iDeltaX = -1 TO 1
   WITH FRAME fr-Show DOWN:
      SearchY:
      REPEAT iDeltaY = -1 TO 1
      WITH FRAME fr-Show:
         IF ABSOLUTE (iDeltaX) EQ ABSOLUTE (iDeltaY) THEN 
            /* Diagonal or Start Point, skip */
            NEXT SearchY.
         
         ASSIGN 
            iNewX = ipiX + iDeltaX
            iNewY = ipiY + iDeltaY
         . 
            
         FIND  ttNeighbour 
         WHERE ttNeighbour.iX EQ iNewX
         AND   ttNeighbour.iY EQ iNewY NO-ERROR.
         IF AVAILABLE ttNeighbour THEN DO:
            iNeighbours = iNeighbours + 1.  
            IF ttNeighbour.iHeight LE ipiHeight THEN
               iLower = iLower + 1.
         END.
         IF lvlDebug THEN DO:
            DOWN WITH FRAME fr-Show. 
            DISPLAY 
               ipiX                  LABEL "(Input X"    FORMAT "zz,zz9"
               ipiY                  LABEL "Input Y)"    FORMAT "zz,zz9"
               iDeltaX               LABEL "(Delta X"    FORMAT "-9"           
               iDeltaY               LABEL "Delta Y)"    FORMAT "-9"
               iNewX                 LABEL "(New X"      FORMAT "zz,zz9"
               iNewY                 LABEL "New Y)"      FORMAT "zz,zz9"
               AVAILABLE ttNeighbour LABEL "Neighbour?" FORMAT "Neighbour/"
               ttNeighbour.iHeight   LABEL "Height LT" WHEN AVAILABLE ttNeighbour FORMAT "9"
               ipiHeight             LABEL "Input Height?" FORMAT "9"
               iLower                LABEL "Lower"        FORMAT "Z"
            WITH FRAME fr-Show WIDTH 132.
         END.
      END.
   END.

   IF lvlDebug THEN DO:
      IF iLower       EQ 0 
      AND iNeighbours EQ 4 THEN    
         MESSAGE "Lower:" iLower
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lvlDebug.
   END.
      
   RETURN iLower. 

   FINALLY:
      IF VALID-HANDLE (hWindow) THEN 
         DELETE OBJECT hWindow.
   END FINALLY.
         
END FUNCTION.

FUNCTION getRiskLevel RETURNS INTEGER 
   (INPUT piX AS INTEGER,
    INPUT piY AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: Returns the Risk Level for a specific Point
 Notes:   Taken from Patrick Tingen's Repository
 URL:     https://github.com/patrickTingen/AdventOfCode/blob/master/2021/Day-09/day-09a.p
------------------------------------------------------------------------------*/   
DEFINE VARIABLE iSelf AS INTEGER NO-UNDO.

  iSelf = getHeight(piX, piY).

  IF    (getHeight(piX - 1, piY) = -1 OR getHeight(piX - 1, piY) > iSelf)
    AND (getHeight(piX + 1, piY) = -1 OR getHeight(piX + 1, piY) > iSelf)
    AND (getHeight(piX, piY - 1) = -1 OR getHeight(piX, piY - 1) > iSelf)
    AND (getHeight(piX, piY + 1) = -1 OR getHeight(piX, piY + 1) > iSelf) THEN
    RETURN iSelf + 1.
  ELSE
    RETURN 0.

END FUNCTION. /* getRiskLevel */
