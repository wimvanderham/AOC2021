
/*------------------------------------------------------------------------
    File        : Day_14.p
    Purpose     : Solution to Day 14 of Advent of Code
    URL         : https://adventofcode.com/2021/day/14

    Syntax      :

    Description : Solution to Day 14 of Advent of Code

    Author(s)   : Wim van der Ham (WITS)
    Created     : Tue Dec 14 21:19:17 CET 2021
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
DEFINE VARIABLE iSolution    AS INT64   NO-UNDO.
DEFINE VARIABLE lOk          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvlDebug     AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lvlShow      AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE iPart        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iStep        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iSteps       AS INTEGER   NO-UNDO.
DEFINE VARIABLE dtPrevNow    AS DATETIME  NO-UNDO.

/* Specific */
DEFINE TEMP-TABLE ttPolymer
   FIELD IDttPolymer         AS INTEGER 
   FIELD Next_IDttPolymer    AS INTEGER 
   FIELD cPolymer            AS CHARACTER 
   FIELD New_NextIDttPolymer AS INTEGER
INDEX ind_ID IS UNIQUE IDttPolymer.

DEFINE TEMP-TABLE ttRule
   FIELD cPolymer_1 AS CHARACTER 
   FIELD cPolymer_2 AS CHARACTER 
   FIELD cInsert    AS CHARACTER 
INDEX ind_Polymer12 IS UNIQUE cPolymer_1 cPolymer_2.

DEFINE TEMP-TABLE ttPairPolymer
   FIELD iCurrentStep  AS INTEGER 
   FIELD cLeftPolymer  AS CHARACTER 
   FIELD cRightPolymer AS CHARACTER
   FIELD iNrPairs      AS INT64   FORMAT "z,zzz,zzz,zzz,zz9"
INDEX ind_StepPair IS UNIQUE iCurrentStep cLeftPolymer cRightPolymer.

DEFINE BUFFER ttNextPairPolymer FOR ttPairPolymer.   
    
DEFINE VARIABLE iLast_IDttPolymer AS INTEGER NO-UNDO.
DEFINE VARIABLE iCount            AS INTEGER NO-UNDO.
DEFINE VARIABLE iMin              AS INT64 NO-UNDO.
DEFINE VARIABLE iMax              AS INT64 NO-UNDO.

DEFINE BUFFER ttNext_Polymer FOR ttPolymer.
DEFINE BUFFER ttNew_Polymer  FOR ttPolymer.

DEFINE VARIABLE iCounter  AS INT64   NO-UNDO EXTENT.
DEFINE VARIABLE cPolymers AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTemplate AS CHARACTER NO-UNDO.

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
HIDE FRAME fr-Parameters.

/* Start Processing */
ETIME (YES).

COPY-LOB FROM FILE "input\14.txt" TO OBJECT lcInput.

/* First Line contains the Template */
cLine = TRIM (ENTRY (1, lcInput, "~n")).

RUN processTemplate
   (INPUT cLine).
   
/* Read Input into Temp-table */
ReadBlock:
DO iLine = 2 TO NUM-ENTRIES (lcInput, "~n"):
   
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).
   IF cLine EQ "" THEN DO:
      NEXT ReadBlock.
   END.

   /* For every Line, create a pair insertion rule */
   CREATE ttRule.
   ASSIGN 
      ttRule.cPolymer_1 = SUBSTRING (cLine, 1, 1)
      ttRule.cPolymer_2 = SUBSTRING (cLine, 2, 1)
      ttRule.cInsert    = TRIM (ENTRY (2, cLine, ">"))
   .      
END. /* ReadBlock: */

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttPolymer:HANDLE).
      
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttRule:HANDLE).
      
END.

/* Search all available Polymers */
FOR EACH ttPolymer:
   IF INDEX (cPolymers, ttPolymer.cPolymer) EQ 0 THEN DO:
      /* Add New Polymer */
      cPolymers = SUBSTITUTE ("&1&2",
         cPolymers,
         ttPolymer.cPolymer).
   END. /* Add New Polymer */
END.
FOR EACH ttRule:
   IF INDEX (cPolymers, ttRule.cInsert) EQ 0 THEN DO:
      /* Add New Polymer */
      cPolymers = SUBSTITUTE ("&1&2",
         cPolymers,
         ttRule.cInsert).
   END. /* Add New Polymer */
END.
EXTENT (iCounter) = LENGTH (cPolymers).

/* Fill Counters with Initial Counts */
FOR EACH ttPolymer:
   ASSIGN 
      iCounter[INDEX(cPolymers, ttPolymer.cPolymer)] = 
         iCounter[INDEX(cPolymers, ttPolymer.cPolymer)] + 1
   .
END.

IF lPart[1] THEN DO:
   /* Part One - Process 10 Steps */
   iSteps = 10.
END.
IF lPart[2] THEN DO:
   /* Part Two - Process 40 Steps */
   iSteps = 40.
END.

IF lPart[1] THEN DO:
   /* Solve Part One the "Hard" way */   
   StepBlock:
   DO iStep = 1 TO iSteps
   WITH DOWN:
      /* StepBlock: */
      PAUSE 0.
      IF lvlShow THEN DO:
         DISPLAY
            iStep  FORMAT "Z9" COLUMN-LABEL "Step"
            iSteps FORMAT "Z9" COLUMN-LABEL "of Steps"
            NOW    FORMAT "99-99-99 HH:MM:SS" COLUMN-LABEL "Now"
            STRING (INTERVAL (NOW, dtPrevNow, "seconds"), "HH:MM:SS") COLUMN-LABEL "Elapsed"
         WITH CENTERED TITLE " Progress ".
      END.      
      dtPrevNow = NOW. 
      
      FOR EACH ttPolymer,
      FIRST ttNext_Polymer
      WHERE ttNext_Polymer.IDttPolymer EQ ttPolymer.Next_IDttPolymer:
         FIND  ttRule
         WHERE ttRule.cPolymer_1 EQ ttPolymer.cPolymer
         AND   ttRule.cPolymer_2 EQ ttNext_Polymer.cPolymer.
         /* Do the insertion */
         iLast_IDttPolymer = iLast_IDttPolymer + 1.
         CREATE ttNew_Polymer.
         ASSIGN 
            ttNew_Polymer.IDttPolymer         = iLast_IDttPolymer
            ttNew_Polymer.cPolymer            = ttRule.cInsert
            ttNew_Polymer.New_NextIDttPolymer = ttPolymer.Next_IDttPolymer
         .
         ASSIGN 
            ttPolymer.New_NextIDttPolymer = ttNew_Polymer.IDttPolymer
         .
         /* Update Counter */
         ASSIGN 
            iCounter[INDEX(cPolymers, ttPolymer.cPolymer)] = 
               iCounter[INDEX(cPolymers, ttPolymer.cPolymer)] + 1
         .
      END.
      
      FOR EACH ttPolymer:
         IF ttPolymer.New_NextIDttPolymer GT 0 THEN DO:
            /* Apply insertions */
            ASSIGN 
               ttPolymer.Next_IDttPolymer    = ttPolymer.New_NextIDttPolymer
               ttPolymer.New_NextIDttPolymer = 0
            .
         END.
      END. /* Apply insertions */
      
      IF  lPart[1]
      AND iStep EQ 10 THEN DO:
         /* End of Part One */
         ASSIGN 
            iMin = 0
            iMax = 0
         .
         DO iCount = 1 TO LENGTH (cPolymers):
            IF iMin EQ 0
            OR iCounter[iCount] LT iMin THEN DO:
               iMin = iCounter[iCount].
            END.
            IF iMax = 0
            OR iCounter[iCount] GT iMax THEN DO:
               iMax = iCounter[iCount].
            END.
         END.
         
         ASSIGN
            iSolution = iMax - iMin
         .
         
         OUTPUT TO "clipboard".
         PUT UNFORMATTED iSolution SKIP.
         OUTPUT CLOSE.
         
         MESSAGE 
            SUBSTITUTE ("Solution: &1.", 
               iSolution) SKIP (1)
            SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
         VIEW-AS ALERT-BOX TITLE " 2021 - Day 14 - Part One".
         
         ETIME (YES).
      END. /* End of Part One */
      
      IF  lPart[2] 
      AND iStep = 40 THEN DO:
         /* End of Part Two */
         ASSIGN 
            iMin = 0
            iMax = 0
         .
         
         DO iCount = 1 TO LENGTH (cPolymers):
            IF iMin EQ 0
            OR iCounter[iCount] LT iMin THEN DO:
               iMin = iCounter[iCount].
            END.
            IF iMax = 0
            OR iCounter[iCount] GT iMax THEN DO:
               iMax = iCounter[iCount].
            END.
         END.
         
         ASSIGN
            iSolution = iMax - iMin
         .
         
         OUTPUT TO "clipboard".
         PUT UNFORMATTED iSolution SKIP.
         OUTPUT CLOSE.
         
         MESSAGE 
            SUBSTITUTE ("Solution: &1.", 
               iSolution) SKIP (1)
            SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
         VIEW-AS ALERT-BOX TITLE " 2021 - Day 14 - Part One".
      END. /* End of Part Two */
   END. /* StepBlock: */
END. /* Part One */

IF lPart[2] THEN DO:
   /* Part Two - Different Approach */
   cTemplate = TRIM (ENTRY (1, lcInput, "~n")).
   DO iChar = 1 TO LENGTH (cTemplate) - 1:
      CREATE ttPairPolymer.
      ASSIGN 
         ttPairPolymer.iCurrentStep  = 1
         ttPairPolymer.cLeftPolymer  = SUBSTRING (cTemplate, iChar, 1)
         ttPairPolymer.cRightPolymer = SUBSTRING (cTemplate, iChar + 1, 1)
         ttPairPolymer.iNrPairs      = 1
      .
   END.

   /* Initialize Counters */
   iCounter = 0.
   DO iChar = 1 TO LENGTH (cTemplate):
      ASSIGN
         iCounter[INDEX(cPolymers, SUBSTRING (cTemplate, iChar, 1))] = 
            iCounter[INDEX(cPolymers, SUBSTRING (cTemplate, iChar, 1))] + 1
      .
   END.
         
   StepBlock:
   DO iStep = 1 TO iSteps
   WITH DOWN:
      /* StepBlock: */
      PAUSE 0.
      IF lvlShow THEN DO:
         DISPLAY
            iStep  FORMAT "Z9" COLUMN-LABEL "Step"
            iSteps FORMAT "Z9" COLUMN-LABEL "of Steps"
            NOW    FORMAT "99-99-99 HH:MM:SS" COLUMN-LABEL "Now"
            STRING (INTERVAL (NOW, dtPrevNow, "seconds"), "HH:MM:SS") COLUMN-LABEL "Elapsed"
         WITH CENTERED TITLE " Progress ".
      END.      
      dtPrevNow = NOW.
       
      FOR EACH ttPairPolymer
      WHERE ttPairPolymer.iCurrentStep = iStep:
         FIND  ttRule
         WHERE ttRule.cPolymer_1 EQ ttPairPolymer.cLeftPolymer
         AND   ttRule.cPolymer_2 EQ ttPairPolymer.cRightPolymer NO-ERROR.
         IF AVAILABLE ttRule THEN DO:
            /* New Pair to the Left */
            FIND FIRST ttNextPairPolymer
            WHERE ttNextPairPolymer.iCurrentStep  EQ iStep + 1
            AND   ttNextPairPolymer.cLeftPolymer  EQ ttPairPolymer.cLeftPolymer
            AND   ttNextPairPolymer.cRightPolymer EQ ttRule.cInsert NO-ERROR.
            IF NOT AVAILABLE ttNextPairPolymer THEN DO:
               CREATE ttNextPairPolymer.
               ASSIGN 
                  ttNextPairPolymer.iCurrentStep  = iStep + 1
                  ttNextPairPolymer.cLeftPolymer  = ttPairPolymer.cLeftPolymer
                  ttNextPairPolymer.cRightPolymer = ttRule.cInsert
               .
            END.
            ASSIGN
               ttNextPairPolymer.iNrPairs = ttNextPairPolymer.iNrPairs + ttPairPolymer.iNrPairs
            .
            /* New Pair to the Right */
            FIND FIRST ttNextPairPolymer
            WHERE ttNextPairPolymer.iCurrentStep  EQ iStep + 1
            AND   ttNextPairPolymer.cLeftPolymer  EQ ttRule.cInsert
            AND   ttNextPairPolymer.cRightPolymer EQ ttPairPolymer.cRightPolymer NO-ERROR.
            IF NOT AVAILABLE ttNextPairPolymer THEN DO:
               CREATE ttNextPairPolymer.
               ASSIGN 
                  ttNextPairPolymer.iCurrentStep  = iStep + 1
                  ttNextPairPolymer.cLeftPolymer  = ttRule.cInsert
                  ttNextPairPolymer.cRightPolymer = ttPairPolymer.cRightPolymer
               .
            END.
            ASSIGN
               ttNextPairPolymer.iNrPairs = ttNextPairPolymer.iNrPairs + ttPairPolymer.iNrPairs
            .
            
            /* Update Counter with Inserted Polymer */
            ASSIGN
               iCounter[INDEX(cPolymers, ttRule.cInsert)] =
                  iCounter[INDEX(cPolymers, ttRule.cInsert)] + ttPairPolymer.iNrPairs
            .
         END.
         DELETE ttPairPolymer.
      END.
      
      IF lvlShow THEN DO:
         RUN sy\win\wbrowsett.w
            (INPUT TEMP-TABLE ttPairPolymer:HANDLE).
      END.
      
   END.
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttPairPolymer:HANDLE).
   END.
      
   ASSIGN 
      iMin = 0
      iMax = 0
   .
   
   DO iCount = 1 TO LENGTH (cPolymers)
   WITH DOWN CENTERED ROW 3 TITLE " Final Counters ":
      IF lvlShow THEN DO:
         DISPLAY 
         iCount                           COLUMN-LABEL "Count"
         SUBSTRING (cPolymers, iCount, 1) COLUMN-LABEL "Polymer" FORMAT "X"
         iCounter[iCount]                 COLUMN-LABEL "Counted" FORMAT "z,zzz,zzz,zzz,zz9".
      END.
            
      IF iMin EQ 0
      OR iCounter[iCount] LT iMin THEN DO:
         iMin = iCounter[iCount].
      END.
      IF iMax = 0
      OR iCounter[iCount] GT iMax THEN DO:
         iMax = iCounter[iCount].
      END.
      IF lvlShow THEN DO:
         DISPLAY
         "Min" WHEN iMin EQ iCounter[iCount] FORMAT "X(3)"
         "Max" WHEN iMax EQ iCounter[iCount] FORMAT "X(3)".
      END.
   END.
   
   ASSIGN
      iSolution = iMax - iMin
   .
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2021 - Day 14 - Part Two".
   
END. /* Part Two - Different Approach */
   
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

PROCEDURE processTemplate:
/*------------------------------------------------------------------------------
 Purpose: Process a Template turning it into a temp-table
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcTemplate AS CHARACTER NO-UNDO.

DEFINE VARIABLE iChar AS INTEGER NO-UNDO.
DEFINE VARIABLE cChar AS CHARACTER NO-UNDO.

DEFINE BUFFER ttPolymer      FOR ttPolymer.
DEFINE BUFFER ttPrev_Polymer FOR ttPolymer.
   
   /* Force large scoping */
   FIND FIRST ttPrev_Polymer NO-ERROR.
   
   DO iChar = 1 TO LENGTH (ipcTemplate):
      /* For every character in the Template ... */
      cChar = SUBSTRING (ipcTemplate, iChar, 1).
      iLast_IDttPolymer = iLast_IDttPolymer + 1.
      /* ... Create a temp-table record */ 
      CREATE ttPolymer.
      ASSIGN
         ttPolymer.IDttPolymer = iLast_IDttPolymer
         ttPolymer.cPolymer    = cChar
      .
      
      IF AVAILABLE ttPrev_Polymer THEN DO:
         ASSIGN
            ttPrev_Polymer.Next_IDttPolymer = ttPolymer.IDttPolymer
         .
      END.
      
      /* Save Current ttPolymer as Previous Polymer */
      FIND  ttPrev_Polymer
      WHERE ttPrev_Polymer.IDttPolymer EQ ttPolymer.IDttPolymer.
   END. /* For every character in the Template ... */

END PROCEDURE.

/* ************************  Function Implementations ***************** */
