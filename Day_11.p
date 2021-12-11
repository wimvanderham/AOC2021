
/*------------------------------------------------------------------------
    File        : Day_11.p
    Purpose     : Solution to Day 11 of Advent of Code 2021
    URL         : https://adventofcode.com/2021/day/11

    Syntax      :

    Description : Solution to Day 11 of Advent of Code 2021

    Author(s)   : Wim van der Ham (WITS)
    Created     : Sat Dec 11 10:00:38 CET 2021
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
DEFINE VARIABLE iStep           AS INTEGER NO-UNDO.
DEFINE VARIABLE iFlashes        AS INTEGER NO-UNDO.
DEFINE VARIABLE iTotalOctopuses AS INTEGER NO-UNDO.

/* Temp-table of Octopuses */
DEFINE TEMP-TABLE ttOctopus RCODE-INFORMATION 
   FIELD iX          AS INTEGER 
   FIELD iY          AS INTEGER 
   FIELD iEnergy     AS INTEGER 
   FIELD lFlash      AS LOGICAL  
INDEX ind_XY     IS UNIQUE iX iY
INDEX ind_Flash  lFlash iEnergy
INDEX ind_Energy iEnergy.

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

COPY-LOB FROM FILE "input\11.txt" TO OBJECT lcInput.

/* Read Input into Temp-table */
ReadBlock:
DO iLine = 1 TO NUM-ENTRIES (lcInput, "~n"):
   
   cLine = TRIM (ENTRY (iLine, lcInput, "~n")).
   IF cLine EQ "" THEN 
      NEXT ReadBlock.
      
   DO iChar = 1 TO LENGTH (cLine):
      CREATE ttOctopus.
      ASSIGN 
         ttOctopus.iX      = iChar
         ttOctopus.iY      = iLine
         ttOctopus.iEnergy = INTEGER (SUBSTRING (cLine, iChar, 1))
         ttOctopus.lFlash  = FALSE
      .
   END.
   
END. /* ReadBlock: */

/* Save contents of temp-table for later */
lOk = TEMP-TABLE ttOctopus:WRITE-JSON ("file", "output\ttOctopus.json", TRUE).

IF lvlShow THEN DO:
   RUN sy\win\wbrowsett.w
      (INPUT TEMP-TABLE ttOctopus:HANDLE).
END.
   
IF lPart[1] THEN DO:
   /* Process Part One */
   StepBlock:
   DO iStep = 1 TO 100:
      /* Process 100 Steps */
      
      /* Add 1 Energy to all Octopuses */         
      FOR EACH ttOctopus:
         ttOctopus.iEnergy = ttOctopus.iEnergy + 1.
      END.
   
      FlashBlock:
      REPEAT:
         /* Determine Flashing Octopuses */
         FIND FIRST ttOctopus
         WHERE ttOctopus.iEnergy GT 9
         AND   ttOctopus.lFlash  EQ FALSE NO-ERROR.
         IF NOT AVAILABLE ttOctopus THEN
            LEAVE FlashBlock.
            
         RUN setFlash
            (INPUT ttOctopus.iX,
             INPUT ttOctopus.iY).
       END. /* REPEAT: FlashBlock: */
       
       FOR EACH ttOctopus 
       WHERE ttOctopus.lFlash EQ TRUE:
          /* Process Flashing Octopuses */
          iFlashes = iFlashes + 1.
          ASSIGN 
            ttOctopus.iEnergy = 0
            ttOctopus.lFlash  = FALSE 
         .
      END. /* Process Flashing Octopuses */
   END. /* Process 100 Steps */ 
    
   /* Part One - Total Flashes after 100 Steps */
   ASSIGN
      iSolution = iFlashes
   .

   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2021 - Day 11 - Part One".
END. /* Process Part One */

IF lPart[2] THEN DO:
   /* Process Part Two */
   ETIME (YES).

   /* Retrieve previous contents of temp-table */
   lOk = TEMP-TABLE ttOctopus:READ-JSON ("file", "output\ttOctopus.json", "EMPTY").

   FOR EACH ttOctopus:
      ACCUM "" (COUNT).
   END.
   ASSIGN 
      iTotalOctopuses = (ACCUM COUNT "")
   . 
     
   IF lvlDebug THEN DO:
      MESSAGE SUBSTITUTE ("Total Octopuses: &1.", iTotalOctopuses)
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lvlDebug.
   END.
   
   iStep = 1.
   StepBlock2:
   REPEAT:
      /* Add 1 Energy to all Octopuses */         
      FOR EACH ttOctopus:
         ttOctopus.iEnergy = ttOctopus.iEnergy + 1.
      END.
   
      IF lvlShow THEN DO:
         RUN sy\win\wbrowsett.w
            (INPUT TEMP-TABLE ttOctopus:HANDLE).
      END.
      
      FlashBlock2:
      REPEAT:
         /* Determine Flashing Octopuses */
         FIND FIRST ttOctopus
         WHERE ttOctopus.iEnergy GT 9
         AND   ttOctopus.lFlash  EQ FALSE NO-ERROR.
         IF NOT AVAILABLE ttOctopus THEN
            LEAVE FlashBlock2.
            
         RUN setFlash
            (INPUT ttOctopus.iX,
             INPUT ttOctopus.iY).
       END. /* REPEAT: FlashBlock2: */
       
       FOR EACH ttOctopus 
       WHERE ttOctopus.lFlash EQ TRUE:
          /* Process Flashing Octopuses */
          iFlashes = iFlashes + 1.
          ASSIGN 
            ttOctopus.iEnergy = 0
            ttOctopus.lFlash  = FALSE 
         .
         ACCUM "" (COUNT).
      END. /* Process Flashing Octopuses */

      IF (ACCUM COUNT "") EQ iTotalOctopuses THEN DO:
         /* All Octopuses Flashed, found solution */
         ASSIGN 
            iSolution = iStep
         .
         IF lvlShow THEN DO:
            RUN sy\win\wbrowsett.w
               (INPUT TEMP-TABLE ttOctopus:HANDLE).
         END.         
         LEAVE StepBlock2.
      END. /* All Octopuses Flashed, found solution */
      ELSE DO:
         ASSIGN 
            iStep = iStep + 1
         .
      END.
   END. /* StepBlock2 */

   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttOctopus:HANDLE).
   END.
   
   OUTPUT TO "clipboard".
   PUT UNFORMATTED iSolution SKIP.
   OUTPUT CLOSE.
   
   MESSAGE 
      SUBSTITUTE ("Solution: &1.", 
         iSolution) SKIP (1)
      SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
   VIEW-AS ALERT-BOX TITLE " 2021 - Day 11 - Part Two".
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

PROCEDURE setFlash:
/*------------------------------------------------------------------------------
 Purpose: Sets a Flash on the ttOctopus
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiX AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER ipiY AS INTEGER NO-UNDO.

DEFINE BUFFER ttFlash     FOR ttOctopus.
DEFINE BUFFER ttNeighbour FOR ttOctopus.

   FIND  ttFlash
   WHERE ttFlash.iX EQ ipiX
   AND   ttFlash.iY EQ ipiY.

   NeighbourBlock:
   FOR EACH ttNeighbour
   WHERE ttNeighbour.iX GE ttFlash.iX - 1
   AND   ttNeighbour.iX LE ttFlash.iX + 1
   AND   ttNeighbour.iY GE ttFlash.iY - 1
   AND   ttNeighbour.iY LE ttFlash.iY + 1:
      IF  ttNeighbour.iX EQ ttFlash.iX
      AND ttNeighbour.iY EQ ttFlash.iY THEN 
         /* This is the Flashed Octopus, skip */
         NEXT NeighbourBlock.
      
      ASSIGN 
         ttNeighbour.iEnergy = ttNeighbour.iEnergy + 1
      .
   END.
   
   ASSIGN 
      ttFlash.lFlash = TRUE
   .
       
END PROCEDURE. /* setFlash */

/* ************************  Function Implementations ***************** */
