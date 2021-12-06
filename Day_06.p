
/*------------------------------------------------------------------------
    File        : Day_06.p
    Purpose     : Solve Day 6 of Advent of Code 2021
    URL         : https://adventofcode.com/2021/day/6

    Syntax      :

    Description : Solve Day 6 of Advent of Code 2021

    Author(s)   : Wim van der Ham (WITS)
    Created     : Mon Dec 06 13:53:16 CET 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* Variables for input handling */
DEFINE VARIABLE lcInput      AS LONGCHAR  NO-UNDO.
DEFINE VARIABLE iLine        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLine        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iChr         AS INTEGER   NO-UNDO.
DEFINE VARIABLE lPart        AS LOGICAL   NO-UNDO EXTENT 2.

/* Variables for solving */
/* Generic */
DEFINE VARIABLE iSolution    AS INT64     NO-UNDO.
DEFINE VARIABLE lOk          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvlDebug     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lvlShow      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iPart        AS INTEGER   NO-UNDO.
/* Specific */
DEFINE VARIABLE iFish   AS INTEGER NO-UNDO.
DEFINE VARIABLE iLastID AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttFish RCODE-INFORMATION 
   FIELD IDttFish  AS INTEGER 
   FIELD iDay      AS INTEGER 
   FIELD iDaysLeft AS INTEGER 
INDEX ind_ID IS UNIQUE IDttFish
INDEX ind_Day IS UNIQUE iDay IDttFish.

DEFINE TEMP-TABLE ttGroup RCODE-INFORMATION 
   FIELD iDay      AS INTEGER 
   FIELD iDaysLeft AS INTEGER
   FIELD iNrFishes AS INT64  
INDEX ind_Day iDay iDaysLeft.
 
DEFINE VARIABLE iCurrentDay AS INTEGER NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
COPY-LOB FROM FILE "input\06.txt" TO OBJECT lcInput.

ETIME (YES).

cLine = ENTRY (1, lcInput, "~n").

DO iChr = 1 TO NUM-ENTRIES (cLine):
   iLastID = iLastID + 1.
   
   CREATE ttFish.
   ASSIGN 
      ttFish.IDttFish  = iLastID
      ttFish.iDay      = 1
      ttFish.iDaysLeft = INTEGER (ENTRY (iChr, cLine))
   .
END.

lOk = TEMP-TABLE ttFish:WRITE-JSON ("file", "output\ttFish.json").

/* Sets Part to Process */
ASSIGN 
   lPart[1] = TRUE 
   lPart[2] = TRUE 
.

IF lPart[1] THEN DO:
   /* Process Part One */
   DO iCurrentDay = 1 TO 80:
      RUN processDay
         (INPUT iCurrentDay).
   END.
   
   IF lvlShow THEN DO:
      RUN sy\win\wbrowsett.w
         (INPUT TEMP-TABLE ttFish:HANDLE).
   END.
      
   /* Part One - Count the available fishes */
   FOR EACH ttFish: 
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
   VIEW-AS ALERT-BOX TITLE " 2021 - Day 06 - Part One".
   EMPTY TEMP-TABLE ttFish.
END. /* Process Part One */

/* Process Part Two */
ETIME (YES).
lOk = TEMP-TABLE ttFish:READ-JSON ("file", "output\ttFish.json").

/* Forse Scoping to Whole Procedure */
FIND FIRST ttGroup NO-ERROR.

/* Save initial situation of single Fishes in Groups */
FOR EACH ttFish
BREAK 
BY ttFish.iDay
BY ttFish.iDaysLeft:
   /* Create Summary of Initial Situation */
   IF FIRST-OF (ttFish.iDaysLeft) THEN DO:
      /* New Group for this number of Days Left */
      CREATE ttGroup.
      ASSIGN
         ttGroup.iDay      = ttFish.iDay
         ttGroup.iDaysLeft = ttFish.iDaysLeft
      .
   END. /* New Group for this number of Days Left */
   ASSIGN 
      ttGroup.iNrFishes = ttGroup.iNrFishes + 1
   .
 
END. /* Create Summary of Initial Situation */

DO iCurrentDay = 1 TO 256:
   PAUSE 0.
   IF lvlDebug THEN DO: 
      IF iCurrentDay = 1 THEN DO: 
         DISPLAY 
            iCurrentDay LABEL "Processing Day"
            NOW         LABEL "Start Datetime"
         WITH FRAME fr-Start 1 DOWN.
      END.
      DISPLAY 
         iCurrentDay LABEL "Processing Day"
         NOW         LABEL "Datetime"
      .
   END.
   /* Process a Whole Group of Fishes */
   RUN processGroupDay
      (INPUT iCurrentDay).
END.

FOR EACH ttGroup:
   ACCUM ttGroup.iNrFishes (TOTAL).
END.
ASSIGN 
   iSolution = (ACCUM TOTAL ttGroup.iNrFishes).
.

OUTPUT TO "clipboard".
PUT UNFORMATTED iSolution SKIP.
OUTPUT CLOSE.

MESSAGE 
   SUBSTITUTE ("Solution: &1.", 
      iSolution) SKIP (1)
   SUBSTITUTE ("Found solution in &1 msecs.", ETIME)
VIEW-AS ALERT-BOX TITLE " 2021 - Day 06 - Part Two".

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

PROCEDURE addNewFish:
/*------------------------------------------------------------------------------
 Purpose: Adds a new Fish in temp-table
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiStartDay AS INTEGER NO-UNDO.

   iLastID = iLastID + 1.
   
   CREATE ttFish.
   ASSIGN
      ttFish.IDttFish  = iLastID
      ttFish.iDay      = ipiStartDay
      ttFish.iDaysLeft = 8
   .

END PROCEDURE.

PROCEDURE addNewGroupFish:
/*------------------------------------------------------------------------------
 Purpose: Adds a new Group of Fishes
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiStartDay AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER ipiNrFishes AS INT64   NO-UNDO.

DEFINE BUFFER ttGroup FOR ttGroup.

   FIND ttGroup 
   WHERE ttGroup.iDay EQ ipiStartDay
   AND   ttGroup.iDaysLeft EQ 8 NO-ERROR.
   IF NOT AVAILABLE ttGroup THEN DO:
      CREATE ttGroup.
      ASSIGN 
         ttGroup.iDay      = ipiStartDay
         ttGroup.iDaysLeft = 8
      .
   END.
   
   ASSIGN 
      ttGroup.iNrFishes = ttGroup.iNrFishes + ipiNrFishes
   .   

END PROCEDURE.

PROCEDURE processDay:
/*------------------------------------------------------------------------------
 Purpose: Process one Day of Laternfish evolution
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiProcessDay AS INTEGER NO-UNDO.

DEFINE BUFFER ttFish FOR ttFish.

   REPEAT PRESELECT EACH ttFish WHERE ttFish.iDay EQ ipiProcessDay:
      /* Process every ttFish according to the rules:
         Although you know nothing about this specific species of lanternfish, you make some guesses about their attributes. Surely, each lanternfish creates a new lanternfish once every 7 days.
         
         However, this process isn't necessarily synchronized between every lanternfish - one lanternfish might have 2 days left until it creates another lanternfish, while another might have 4. So, you can model each fish as a single number that represents the number of days until it creates a new lanternfish.
         
         Furthermore, you reason, a new lanternfish would surely need slightly longer before it's capable of producing more lanternfish: two more days for its first cycle.
         
         So, suppose you have a lanternfish with an internal timer value of 3:
         
             After one day, its internal timer would become 2.
             After another day, its internal timer would become 1.
             After another day, its internal timer would become 0.
             After another day, its internal timer would reset to 6, and it would create a new lanternfish with an internal timer of 8.
             After another day, the first lanternfish would have an internal timer of 5, and the second lanternfish would have an internal timer of 7.
         
         A lanternfish that creates a new fish resets its timer to 6, not 7 (because 0 is included as a valid timer value). The new lanternfish starts with an internal timer of 8 and does not start counting down until the next day.
      */
      FIND NEXT ttFish.
      IF ttFish.iDaysLeft EQ 0 THEN DO:
         RUN addNewFish
            (INPUT ipiProcessDay + 1).
         ttFish.iDaysLeft = 6.
      END.
      ELSE DO:
         ASSIGN 
            ttFish.iDaysLeft = ttFish.iDaysLeft - 1
         .            
      END.
      ASSIGN 
         ttFish.iDay = ipiProcessDay + 1
      .
   END.
   
END PROCEDURE.

PROCEDURE processGroupDay:
/*------------------------------------------------------------------------------
 Purpose: Process Group of Fishes (with the same Day and Days Left)
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipiProcessDay AS INTEGER NO-UNDO.

DEFINE BUFFER ttGroup FOR ttGroup.

   REPEAT PRESELECT EACH ttGroup WHERE ttGroup.iDay EQ ipiProcessDay:
      FIND NEXT ttGroup.
      IF ttGroup.iDaysLeft EQ 0 THEN DO:
         RUN addNewGroupFish
            (INPUT ipiProcessDay + 1,
             INPUT ttGroup.iNrFishes).
         ttGroup.iDaysLeft = 6.
      END.
      ELSE DO:
         ASSIGN 
            ttGroup.iDaysLeft = ttGroup.iDaysLeft - 1
         .
      END.
      ASSIGN 
         ttGroup.iDay = ipiProcessDay + 1
      . 
   END.
   
END PROCEDURE.

