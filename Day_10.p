
/*------------------------------------------------------------------------
    File        : Day_10.p
    Purpose     : Solution Day 10 Advent of Code 2021
    URL         : https://adventofcode.com/2021/day/10

    Syntax      :

    Description : Solution Day 10 Advent of Code 2021

    Author(s)   : Wim van der Ham (WITS)
    Created     : Fri Dec 10 13:29:01 CET 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for input handling */
DEFINE VARIABLE lcInput      AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE iLine        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iChar        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cChar        AS CHARACTER NO-UNDO.
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
DEFINE VARIABLE lviLevel          AS INTEGER NO-UNDO.
DEFINE VARIABLE iSyntaxErrorScore AS INTEGER NO-UNDO.
DEFINE VARIABLE lviCloseNr        AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttChunk
   FIELD iLevel     AS INTEGER 
   FIELD cCharOpen  AS CHARACTER 
   FIELD cCharClose AS CHARACTER 
INDEX ind_Level IS UNIQUE iLevel.

DEFINE TEMP-TABLE ttCharOpen
   FIELD cCharOpen AS CHARACTER 
INDEX ind_CharOpen IS UNIQUE cCharOpen.

DEFINE TEMP-TABLE ttCharClose
   FIELD cCharClose AS CHARACTER 
   FIELD cCharOpen  AS CHARACTER
   FIELD iPoints    AS INTEGER 
INDEX ind_CharClose IS UNIQUE cCharClose.
   
DEFINE TEMP-TABLE ttClose RCODE-INFORMATION 
   FIELD iLine        AS INTEGER 
   FIELD iCloseNr     AS INTEGER 
   FIELD cCloseString AS CHARACTER 
   FIELD iTotalScore  AS INT64 
INDEX ind_Line IS UNIQUE iLine
INDEX iTotalScore iTotalScore.
   
/* Variables for Part Two */
DEFINE VARIABLE cCloseList AS CHARACTER NO-UNDO INITIAL "),],~},>".

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION addCharClose RETURNS LOGICAL 
   (INPUT ipcCharClose AS CHARACTER,
    INPUT ipcCharOpen  AS CHARACTER,
    INPUT ipiPoints    AS INTEGER) FORWARD.

FUNCTION addCharOpen RETURNS LOGICAL 
   (INPUT ipcCharOpen AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */
lPart[1] = TRUE.

DISPLAY
   lPart[1] LABEL "Solve Part 1?" COLON 15 SKIP
   lPart[2] LABEL "Solve Part 2?" COLON 15 SKIP 
   lvlDebug LABEL "Debug?"        COLON 15 SKIP 
   lvlShow  LABEL "Show?"         COLON 15 SKIP
WITH FRAME fr-Parameters SIDE-LABELS ROW 3 CENTERED TITLE " Parameters ".

UPDATE
   lPart[2]
   lvlDebug
   lvlShow
WITH FRAME fr-Parameters.

/* Initialize temp-table with Open and Close characters */
lOk = addCharOpen("(").
lOk = addCharClose(")", "(", 3).
lOk = addCharOpen("[").
lOk = addCharClose("]", "[", 57).
lOk = addCharOpen("~{").
lOk = addCharClose("~}", "~{", 1197).
lOk = addCharOpen("<").
lOk = addCharClose(">", "<", 25137).

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttCharClose:HANDLE).
END.

/* Start Processing */
ETIME (YES).

COPY-LOB FROM FILE "input\10.txt" TO OBJECT lcInput.

ASSIGN 
   iSyntaxErrorScore = 0
   lviCloseNr        = 0
.

/* Read Input into Temp-table */
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).
   IF cLine EQ "" THEN 
      NEXT ReadBlock.

   lviLevel = 0.
   EMPTY TEMP-TABLE ttChunk.
         
   DO iChar = 1 TO LENGTH (cLine):
      /* For every Character in Line */
      cChar = SUBSTRING (cLine, iChar, 1).
      FIND  ttCharOpen 
      WHERE ttCharOpen.cCharOpen EQ cChar NO-ERROR.
      IF AVAILABLE ttCharOpen THEN DO:
         /* New Character Open */
         lviLevel = lviLevel + 1.
         CREATE ttChunk.
         ASSIGN 
            ttChunk.iLevel    = lviLevel
            ttChunk.cCharOpen = cChar
         .
      END. /* New Character Open */
      ELSE DO: 
         /* Not New Character Open */
         FIND  ttCharClose 
         WHERE ttCharClose.cCharClose EQ cChar NO-ERROR.
         IF AVAILABLE ttCharClose THEN DO:
            /* Character Close */
            FIND  ttChunk 
            WHERE ttChunk.iLevel EQ lviLevel.
            ASSIGN 
               ttChunk.cCharClose = cChar
            .
            IF ttChunk.cCharOpen NE ttCharClose.cCharOpen THEN DO:
               /* Unexpected Character Close */
               iSyntaxErrorScore = iSyntaxErrorScore + ttCharClose.iPoints.
               NEXT ReadBlock.
            END.
            ELSE DO:
               /* Current Level is Closed correctly */
               /* Remove Chunk */
               DELETE ttChunk.
               /* Return to Previous Level */
               lviLevel = lviLevel - 1.
            END. /* Current Level is Closed correctly */
         END. /* Character Close */
         ELSE DO:
            /* Unexpected Character (neither Open nor Close) */
            UNDO, THROW NEW Progress.Lang.AppError(SUBSTITUTE ("Neither Open nor Close character found but &1 on Line: &2 and Postion: &3.", cChar, iLine, iChar), 999).
         END. /* Unexpected Character (neither Open nor Close) */
      END. /* Not New Character Open */
   END. /* For every Character in Line */
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttChunk:HANDLE).
   END.
   
   IF lPart[2] THEN DO:
      /* Close the incomplete string */
      DO WHILE lviLevel GT 0:
         /* Close all Open Levels */
         FIND  ttChunk
         WHERE ttChunk.iLevel EQ lviLevel.
         IF ttChunk.cCharClose EQ "" THEN DO:
            /* Chunk not Closed */
            FIND FIRST ttCharClose
            WHERE ttCharClose.cCharOpen EQ ttChunk.cCharOpen.
            ASSIGN
               ttChunk.cCharClose = ttCharClose.cCharClose
            .
            FIND  ttClose
            WHERE ttClose.iLine EQ iLine NO-ERROR.
            IF NOT AVAILABLE ttClose THEN DO:
               lviCloseNr = lviCloseNr + 1. 
               CREATE ttClose.
               ASSIGN 
                  ttClose.iLine    = iLine
                  ttClose.iCloseNr = lviCloseNr
               .
            END.
            ASSIGN 
               ttClose.cCloseString = SUBSTITUTE ("&1&2", ttClose.cCloseString, ttChunk.cCharClose)
               ttClose.iTotalScore = (ttClose.iTotalScore * 5) + LOOKUP (ttCharClose.cCharClose, cCloseList)
            .
         END. /* Chunk not Closed */
         /* Return to Previous Level */
         lviLevel = lviLevel - 1.
      END. /* Close all Open Levels */
   END. /* Close the incomplete string */
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttClose:HANDLE).
END.

IF lPart[1] THEN DO:
   /* Process Part One */
   ASSIGN
      iSolution = iSyntaxErrorScore
   .

   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2021 - Day 10 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   ETIME (YES).

   ScoreList:
   FOR EACH ttClose
   BY ttClose.iTotalScore:
      /* Order the Scores */
      ACCUM "" (COUNT).
      IF (ACCUM COUNT "") EQ INTEGER ((lviCloseNr + 1) / 2) THEN DO:
         /* We're in the middle of an Odd number of entries */
         ASSIGN
            iSolution = ttClose.iTotalScore
         .
         LEAVE ScoreList.
      END. /* We're in the middle of an Odd number of entries */
   END. /* Order the Scores */
      
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2021 - Day 10 - Part Two".
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
FUNCTION addCharClose RETURNS LOGICAL 
   (INPUT ipcCharClose AS CHARACTER,
    INPUT ipcCharOpen  AS CHARACTER,
    INPUT ipiPoints    AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose: Adds a Character to the temp-table of Close Characters
 Notes:
------------------------------------------------------------------------------*/   
DEFINE BUFFER ttCharClose FOR ttCharClose.

   FIND  ttCharClose 
   WHERE ttCharClose.cCharClose EQ ipcCharClose NO-ERROR.
   IF AVAILABLE ttCharClose THEN DO:
      /* Already defined, return FALSE */
      RETURN FALSE.
   END.
   
   CREATE ttCharClose.
   ASSIGN 
      ttCharClose.cCharClose = ipcCharClose
      ttCharClose.cCharOpen  = ipcCharOpen
      ttCharClose.iPoints    = ipiPoints
   .
   
   RETURN TRUE.
      
END FUNCTION.

FUNCTION addCharOpen RETURNS LOGICAL 
   (INPUT ipcChar   AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Adds a Character to the temp-table of Open Characters
 Notes:
------------------------------------------------------------------------------*/   
DEFINE BUFFER ttCharOpen FOR ttCharOpen.

   FIND  ttCharOpen 
   WHERE ttCharOpen.cCharOpen EQ ipcChar NO-ERROR.
   IF AVAILABLE ttCharOpen THEN DO:
      /* Already defined, return FALSE */
      RETURN FALSE.
   END.
   
   CREATE ttCharOpen.
   ASSIGN 
      ttCharOpen.cCharOpen = ipcChar
   .
   
   RETURN TRUE.
      
END FUNCTION.

/* **********************  Internal Procedures  *********************** */
