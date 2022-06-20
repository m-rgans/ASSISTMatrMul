//KC03D28A JOB ,'Joshua Sulouff',MSGCLASS=H
//JSTEP01  EXEC PGM=ASSIST,PARM='MACRO=F'
//STEPLIB  DD DSN=KC00NIU.ASSIST.LOADLIB,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
*
******************************************************************
*                                                                *
*        MACRO: CTRLS
*        AUTO GENERATES ENTRY LINKAGE
*        &LAB - LABEL OF CONTROL SECTION
*        &SVA - LABEL OF SAVE AREA
*                                                                *
******************************************************************
         MACRO               STANDARD ENTRY LINKAGE MACRO
         CTRLS &LAB,&SVA
         STM   14,12,12(13)  SAVE CALLER REGISTERS
         LR    12,15         GET BASE POINTER
         USING &LAB,12       ADDRESSABILITY
         LA    14,&SVA       LOAD NEW SAVE AREA
         ST    14,8(13)      STORE FW PTR IN CALLER
         ST    13,4(14)      STORE BACK PTR
         LR    13,14         ESTABLISH NEW SAVE AREA
         MEND
*
******************************************************************
*                                                                *
*        MACRO: RET
*        AUTO GENERATES EXIT
*                                                                *
******************************************************************
         MACRO              STANDARD EXIT LINKAGE MACRO
&LABEL   RET
&LABEL   L     13,4(,13)     CALLER SAVE -> 13
         L     14,12(,13)    RESTORE 14
         LM    0,12,20(13)   RELOAD REGISTERS
         BR    14
         MEND
*
*
******************************************************************
*                                                                *
*        MACRO: BSR
*        JUMPS INTO A SUBROUTINE WITH THE GIVEN ARGUMENTS
*        &SUB  - SUBROUTINE LABEL
*        &ARGS - ARGUMENTS LABEL
*                                                                *
******************************************************************
         MACRO               BRANCH SUBROUTINE
&LABEL   BSR   &SUB,&ARGS
&LABEL   LA    1,&ARGS       LOAD ARGS
         L     15,=V(&SUB)   LOAD JUMP
         BALR  14,15         JUMP
         MEND
*
******************************************************************
*                                                                *
*        MACRO: MINCR (MEMORY INCREMENT)
*        INCREMENTS A SECTION OF MEMORY, LEAVING THE RESULT
*        IN A REGISTER
*        &ADDR - MEMORY TO INCREMENT
*        &R    - REGISTER
*                                                                *
******************************************************************
         MACRO
&LABEL   MINCR &ADDR,&R
         L     &R,&ADDR
         LA    &R,1(,&R)
         ST    &R,&ADDR
         MEND
*
* DOC BOX HERE
*
*DSECT FOR MATRIXES
MATRMAXZ EQU   7      MAXIMUM SIDE LENGTH OF MATRIX
*
* OFFSETS INCLUDED FOR MANUAL STRUCT DEREFERENCING
*
$MATR    DSECT
$SIZEXO  EQU   *-$MATR   OFFSET OF SIZEX
$SIZEX   DS    1F LENGTH OF ROW
$SIZEYO  EQU   *-$MATR   OFFSET OF SIZEY
$SIZEY   DS    1F LENGTH OF COLUMN
$MTRBODO EQU   *-$MATR   OFFSET OF BODY
$MTRBODY DS    (MATRMAXZ*MATRMAXZ)PL9  BODY OF MATRIX
$MATRZ   EQU   *-$MATR SIZEOF(MATRIX)
$MATRBZ  EQU   9
*
* EXTERNAL PROG FIRST
******************************************************************
*                                                                *
* DECIMAL PARSING SUBPROGRAM                            *
*                                                                *
* TAKES AN EBCDIC REPRESENTATION OF A DECIMAL VALUE AND CONVERTS *
* IT TO A PACKED DECIMAL REPRESENTATION. THE PROVIDED START AND  *
* TRAILING ADDRESSES DETERMINE THE REGION OF STORAGE THAT WILL   *
* BE SCANNED, WITH THE TRAILING ADDRESS 1 POS PAST THE END. SETS *
* THE SIGN DIGIT OF THE RESULTING PACKED DECIMAL IF A PRECEDING  *
* SIGN CHARACTER ACCOMPANIES THE NUMERIC CHARACTERS. IF NO SIGN  *
* CHARACTER IS FOUND ASSUMES A POSITIVE QUANTITY. COUNTS AND     *
* RETURNS THE NUMBER OF IMPLIED DECIMAL PLACES, WHICH WILL BE    *
* BE NON-ZERO IF A DECIMAL CHARACTER IS FOUND.                   *
*                                                                *
* RETURN CODES:                                                  *
*   0 - PARSE WAS SUCCESSFUL                                     *
*   1 - PARSE WAS UNSUCCESSFUL                                   *
*                                                                *
*   NOTE: IF PARSE WAS UNSUCCESSFUL THEN RETURN FIELD WILL NOT   *
*         CONTAIN A VALID PACKED DECIMAL, BUT WILL CONTAIN PREV  *
*         DATA WITH THE LAST BYTE OVERWRITTEN WITH X'00'         *
*         (GUARANTEEING IT WILL NOT CONTAIN A VALID PACKED DEC)  *
*                                                                *
* PARSING WILL FAIL IN THE FOLLOWING CASES:                      *
*   1.) THE SUBPROGRAM ATTEMPTED TO PARSE AN INVALID CHARACTER   *
*       (I.E., NOT C'+', C'-', C'.' OR NUMERIC CHAR)             *
*   2.) NO NUMERIC CHARACTERS WERE FOUND                         *
*   3.) NO TRAILING SPACE CHARACTER WAS FOUND                    *
*   4.) THE NUMBER BEING PARSED CONTAINED MORE THAN 15 DIGITS    *
*                                                                *
* TODO: IMPLEMENT PARSE END CONSTRAINTS (STOP AT BUFFER END,     *
*       > 15 SOURCE DIGITS)                                      *
*                                                                *
* REGISTER USAGE:                                                *
*   2 - (IN) PTR TO BUFFERING LOC BEGIN ADDR (F)                 *
*   3 - (IN) PTR TO BUFFERING LOC TRAILING ADDR (F)              *
*   4 - (OUT) PTR TO PACKED DECIMAL'S RECEIVING FIELD (PL8)      *
*   5 - (OUT) ADDRESS OF FIRST TRAILING SPACE IN BUFFER (F)      *
*   6 - (OUT) IMPLIED DECIMALS (F)                               *
*   7 - BUFFER READ PTR                                          *
*   8 - COPY FIELD WRITE PTR                                     *
*   9 - COUNTER, IMPLIED DECIMAL PLACES                          *
*                                                                *
******************************************************************
PARSEDEC CSECT
* ==== Entry linkage ===========
*      Back up caller's register state
         STM   14,12,12(13)   SAVE REGS IN CALLER'S SAVE AREA
*      Establish local addressability
         LR    12,15          COPY CSECT ADDRESS INTO R12
         USING PARSEDEC,12    ESTABLISH R12 AS THE BASE REG
*      Store backwards, forwards pointers
         LA    14,PARSSAVE    R14 POINTS TO THIS CSECT'S SAVE AREA
         ST    13,4(,14)      STORE ADDRESS OF CALLER'S SAVE AREA
         ST    14,8(,13)      STORE ADDRESS OF THIS CSECT'S SAVE AREA
*      Preemptively point r13 at local save area
         LR    13,14          POINT R13 AT THIS CSECT'S SAVE AREA
*
         LM    2,6,0(1)       R2 -> PARSE START ADDR
*                             R3 -> PARSE END ADDR
*                             R4 -> PACKED RETURN FIELD
*                             R5 -> ADDR IN BUFFER, TRAILING SPACE
*                             R6 -> IMPLIED DECIMAL PLACES
*
* (RE)INITIALIZE STATE VARS
         MVI   7(4),X'00'     SET LAST BYTE TO ERROR CODE
         MVI   SCODE,X'00'    (RE)SET DEFAULT SIGN CODE
         MVI   DECFLAG,X'00'  (RE)SET DECIMAL FLAG
         LR    7,2            SET SCAN HEAD TO BUFFER START
         LA    9,0            RESET DECIMAL PLACE COUNTER
*
* FAST-FORWARD PAST LEADING SPACES
FFLOOP   CLI   0(7),C' '      TEST FOR LEADING SPACE
         BNE   TESTNEG        NON-SPACE CHAR FOUND, BEGIN PARSE
         LA    2,1(,2)        MOVE START ADDR FORWARD 1 POS
         LA    7,1(,7)        MOVE READ HEAD FORWARD 1 POS
         B     FFLOOP         TEST NEXT POS
*
** TEST FOR LEADING +/-
*
* TEST IF FIRST CHAR IS '-'
TESTNEG  CLI   0(7),C'-'      TEST FOR NEGATIVE SIGN
         BNE   TESTPOS        NO NEGATIVE SIGN, TEST FOR +
         MVI   SCODE,X'FF'    SET SIGN CODE BYTE TO -1
         LA    2,1(,7)        MOVE BUFFER START PAST SIGN
         B     UPDTSCHD       GO TO NEXT CHAR
*
* TEST IF FIRST CHAR IS '+'
TESTPOS  CLI   0(7),C'+'      TEST FOR POSITIVE SIGN
         BNE   SCANLOOP       NO POSITIVE SIGN, START SCANNING
         LA    2,1(,7)        MOVE BUFFER START PAST SIGN
         BE    UPDTSCHD       POS SIGN FOUND, GO TO NEXT CHAR
*
** SCAN FROM LEFT TO RIGHT FOR FIRST TRAILING SPACE **
*
* TEST AND HANDLE SPACE FOUND CASE
SCANLOOP CLI   0(7),C' '      TEST FOR SPACE CHAR
         BNE   TSTDEC         NOT A SPACE, TEST FOR DECIMAL CHAR
         CR    7,2            TEST OFFSET INTO BUFFER
         BNE   COPYPREP       FOUND TRAIL SPC, BEGIN NEXT STEPS
         LA    15,1           OTHERWISE SET R15 = 1 (PARSE ERROR)
         B     PARSEXIT       EXIT IMMEDIATELY
*
* TEST FOR DECIMAL CHARACTER
TSTDEC   CLI   0(7),C'.'      TEST FOR DECIMAL CHAR
         BNE   TSTCXLOW       NOT FOUND, TEST FOR NON-NUMERIC
         CLI   DECFLAG,X'01'  TEST IF FLAG ALREADY SET
         BNE   SETDFLAG       IF NOT THEN SET FLAG BYTE
         LA    15,1           OTHERWISE SET R15 = 1 (PARSE ERROR)
         B     PARSEXIT       EXIT IMMEDIATELY
SETDFLAG MVI   DECFLAG,X'01'  SET DECIMAL FLAG TO 1 (TRUE)
         BCTR  9,0            SET COUNT TO -1 (IGNORES DEC CHAR)
         B     UPDTSCHD       UPDATE AND CONTINUE
*
* TEST FOR NON-NUMERIC CHARACTERS
TSTCXLOW CLI   0(7),X'F0'     TEST IF < X'F0' (CHAR 0)
         BNL   TSTCXHI        IF NOT LOW THEN TEST IF HIGH
         LA    15,1           R15 = 1 (PARSE ERROR)
         B     PARSEXIT       OTHERWISE DO NOTHING AND EXIT
TSTCXHI  CLI   0(7),X'F9'     TEST IF > X'F9' (CHAR 9)
         BNH   UPDTSCHD       IF NOT HIGH THEN UPDATE AND CONTINUE
         LA    15,1           OTHERWISE R15 = 1 (PARSE ERROR)
         B     PARSEXIT       DO NOTHING AND EXIT
*
* UPDATE AND CONTINUE
UPDTSCHD LA    7,1(,7)        INCREMENT READ PTR
         CLI   DECFLAG,X'01'  TEST IF DECIMAL FLAG IS SET
         BNE   SCANLOOP       IF NOT THEN TEST NEXT CHAR
         LA    9,1(,9)        INCREMENT IMPLIED DEC COUNTER
         B     SCANLOOP       TEST NEXT CHAR
*
* PREPARE FOR COPY INTO TEMP FIELD
COPYPREP ST    7,0(,5)         WRITE OUT TRAILING SPACE ADDR
         MVI   ZNTEMP,X'F0'    RESET COPY FIELD TO 0
         MVC   ZNTEMP+1(14),ZNTEMP
         LA    8,ZNTEMP+14     GET ADDR OF LAST CHAR IN COPY FIELD
         BCTR  7,0             MOVE READ PTR BACK TO LAST DIGIT
*
* COPY EBCDIC CODES RIGHT TO LEFT INTO CONVERSION FIELD
COPYLOOP CLI   0(7),C'.'      TEST IF DECIMAL CHARACTER
         BE    UPDTNCPY       SKIP THIS POS IF SO (NO COPY)
         MVC   0(1,8),0(7)    COPY EBCDIC CODE
UPDTCPY  BCTR  8,0            DECREMENT COPY HEAD
UPDTNCPY BCTR  7,0            DECREMENT SCAN HEAD
         CR    7,2            TEST IF BACK AT FIRST DIGIT
         BNL   COPYLOOP       IF NOT THEN CONTINUE
*
* SET RESULTS IN RECEIVER FIELDS
         ST    9,0(,6)            WRITE BACK # IMPLIED DECIMALS
         PACK  0(8,4),ZNTEMP(15)  WRITE BACK PACKED DECIMAL
         CLI   SCODE,X'FF'        SET SIGN DIGIT IF NEGATIVE
         BNE   SETRCODE
         MP    0(8,4),=PL1'-1'
*
* ==== Exit linkage ============
SETRCODE LA    15,0           R15 = RETURN CODE OF 0
PARSEXIT L     13,4(,13)      POINT R13 TO CALLER'S SAVE AREA
         L     14,12(,13)     RESTORE REGISTER 14
         LM    0,12,20(13)    RESTORE R0 THRU R12
         BR    14             RETURN TO CALLER
*
         LTORG
*
PARSSAVE DS    18F'-1'        PARSE SUBPROGRAM'S SAVE AREA
*
SCODE    DC    X'00'          SIGN CODE BYTE, DEFAULT 0 (POSITIVE)
DECFLAG  DC    X'00'          DECIMAL FOUND FLAG, DEFAULT 0 (FALSE)
ZNTEMP   DS    CL15           CONVERSION COPY FIELD
         DS    0X
         DS    0X
******************************************************************
*
*    CSCI 360-H    HONORS PROJECT                SPRING 2022
*
*    DATE DUE: 5/8/2022
*    TIME DUE: 11:59
*
*    AUTHOR: JOSHUA SULOUFF
*
*    Takes in a set of 2 matrices from input and multiplies them.
*    Maximum matrix side length of 7 (MATRMAXZ)
*
*    (Note: Size restriction does not take into account
*     restrictions on output.)
*
*    main registers:
*    2 - copying addresses into arguments
*    1 - argument addressing (this is implied to be the case
*        for the rest of the program)
*
******************************************************************
MAIN     CSECT
         CTRLS MAIN,RSAVE
*********** START ************
         BSR   GETMATR,GMPTR    Get first matrix
         C     15,=F'1'         PANIC IF INPUT FAILS
         BE    EXBADIN
*
         LA    2,MATR2          Add second matrix to
         ST    2,GMPTR          get matrix arguments
         BSR   GETMATR,GMPTR    Get second matrix contents
         C     15,=F'1'         PANIC IF INPUT FAILS
         BE    EXBADIN
*
         BSR   MULTMATR,MULARG  Multiply the matrices together
         C     15,=F'1'         Panic if not doable
         BE    EXNONCMP
         LA    2,MATRRES        load the result matrix
         ST    2,GMPTR          address into pointer
         BSR   ECHOMATR,GMPTR   Print result matrix
*
*
***********  END  ************
MAINEXIT DS    0X             Instant main normal exit
         LA    15,0           Indicate normal exit
MAINPAN  RET                  Immediately stop (error)
RSAVE    DS    18F            Save area
*********PANIC STATES*********
EXBADIN  DS    0X             Show error for bad input
         XPRNT =CL20' ERROR: BAD INPUT',20
         LA    15,1
         B     MAINPAN
EXNONCMP DS    0X             Show error for MatrMul Failure
         XPRNT =CL50' ERROR: INCOMPATIBLE MATRICES',50
         LA    15,2
         B     MAINPAN
******************************
*
         LTORG
*
*********** STORAGE **********
* Argument for subs that take 1 matrix
GMPTR    DC    A(MATR1)
* Output buffer
OCTL     DC    C' '
OBUFFER  DC    CL132' '
OBUFFERE DS    0X
* Multiply arguments
MULARG   DC    A(MATR1,MATR2,MATRRES)
* Matrices
         DS    0F               ALIGNMENT
MATR1    DC    ($MATRZ)X'AB'
         DS    0F               ALIGNMENT
MATR2    DC    ($MATRZ)X'AC'
         DS    0F               ALIGNMENT
MATRRES  DC    ($MATRZ)X'AF'    Result matrix
******************************
         DS    0H
******************************************************************
*
*                       GET MATRIX
*                       (MATRIX*)
*               Populates the supplied matrix
*               with info from input cards.
*
*                R2 -> MATRIX BASE PTR (F)
*                R3 -> INPUT PTR
*                R4 -> RUN TRACKER
*                R5 -> COLUMN COUNTER
*                R6 -> ROW LENGTH
*                R7 -> ENTRY POINTER
*
******************************************************************
GETMATR  CSECT
         CTRLS GETMATR,RSVE2
*********** START ************
         LM    2,2,0(1)
         USING $MATR,2    Address given matrix
*
         XREAD GMIBUF,80
         SR    5,5        ZERO COLUMN LENGTH
         SR    6,6        ZERO ROW LENGTH
         SR    4,4        ZERO 4
         ST    4,$SIZEX   ZERO LENGTH
         ST    4,$SIZEY   AND WIDTH
         LA    7,$MTRBODY LOAD MATRIX ENTRY
*
         LA    3,GMIBUF      RESET 3
         ST    3,PARSSTRT    RESET START ADDR
*
GETRWADV DS    0X  LOOP PER NUMBER
         BSR   PARSEDEC,PARSARG     GET NEXT NUMBER
         C     15,=F'0'             CHECK FOR FAIL
         BNE   GETCARDA             CARD ADVANCE IF SO
*
         ZAP   0($MATRBZ,7),RPAQRES STORE IN SPOT
         LA    7,$MATRBZ(,7)        INCREMENT SPOT
*
         LA    5,1(,5)              INCREMENT COL COUNTER
         L     3,TRAILSPC          GET ADDR OF TRAILING
         LA    3,1(,3)              INCREMENT PAST SPACE
         ST    3,PARSSTRT          SET START POSITION
         B     GETRWADV             LOOP
*
GETCARDA DS    0X  CARD & ROW ADVANCE
         LA    6,1(,6)    INCREMENT ROW COUNTER
         C     4,=F'0'    CHECK FOR FIRST RUNTHROUH
         BE    GETCARDF   STORE COLUMN COUNT IF SO
         LA    15,1       LOAD PANIC CODE
         C     5,$SIZEX   OTHERWISE, CHECK FORMAT
         BNE   GETPANIC   PANIC IF NO MATCH
GCNOCHK  DS    0X         SKIP CHECK
         ST    5,$SIZEX   STORE LENGTH
         SR    5,5        CLEAR R5
         XREAD GMIBUF,80  GET NEXT CARD
*
         BNZ   GETEXIT     EXIT IF LAST CARD
         CLI   GMIBUF,C'*' ALSO BRANCH IF STOP CARD
         BE    GETEXIT     TO EXIT
*
         LA    3,GMIBUF   SET INPUT BACK TO START
         ST    3,PARSSTRT SET START ADDRESS
         B     GETRWADV               ADVANCE ROW OTHERWISE
GETCARDF DS    0X                     CARD ADV FIRST TIME
         LA    4,1                    MARK COMPLETED
         B     GCNOCHK                SKIP CHECK
*
GETEXIT  DS    0X
         ST    6,$SIZEY  STORE MATRIX HEIGHT
         SR    15,15     CLEAR 15 FOR NORMAL RETURN
*
***********  END  ************
GETPANIC DS    0X
         RET
RSVE2    DS    18F
*
         LTORG
*
*********** STORAGE **********
PARSARG  DS   0X                 Arguments for PARSEDEC
PARSSTRT DC   A(GMIBUF,GMIBUFE)
         DC   A(RPAQRES,TRAILSPC,RPAQDEC)
*
RPAQRES  DC   PL8'0'             Result of PARSEDEC
TRAILSPC DC   1F'0'              Address of trailing space
RPAQDEC  DC   1F'0'              Number of implied decimals
*
DMATR    DC   ($MATRZ)X'AC'      Dummy matrix
FMATRP   DC   1F'0'
*
GMIBUF   DC   CL80' '            Output buffer
GMIBUFE  DS   0X
         DC   C'   *'
******************************
         DS    0H
         DS    0H
         DS    0H
******************************************************************
*
*                   ECHO MATRIX
*                   (MATRIX*)
*              Prints the contents of a matrix
*              to output.
*
*                R2 -> MATRIX PTR          (F)
*                R3 -> MATRIX BODY PTR
*                R4 -> OUTPUT PTR
*                R5 -> ROW LOOP COUNTER
*                R6 -> COLUMN LOOP COUNTER
*
******************************************************************
ECHOMATR CSECT
         CTRLS ECHOMATR,RSVE3
*********** START ************
         LM    2,2,0(1)      GET ARGS
         USING $MATR,2       ADDRESS *R2 AS MATRIX
*
         L     5,$SIZEY      LOAD SIZE Y
         L     6,$SIZEX      LOAD SIZE X
         LA    3,$MTRBODY    LOAD BODY PTR
         LA    4,ECHOBUF     POINT 4 AT OUTPUT
*
ECROWLP  DS    0H            PER ROW LOOP
         L     6,$SIZEX      RESET COLUMN COUNTER
ECCOLLP  DS    0H            PER COLUMN LOOP
         ST    4,EVTARG      LOAD CHAR ARG
         ST    3,EVNARG      LOAD NUMERIC ARG
         BSR   ECHOVAL,EVTARG   WRITE TO BUFFER
         LA    3,$MATRBZ(,3)    INCREMENT NUMBER INDEX
         L     4,EVENDR TRANSFER PTR
         BCT   6,ECCOLLP     PROCEED FOR EVERY COLUMN
*
         XPRNT ECHOCTL       PRINT ROW
         LA    4,ECHOBUF
         BCT   5,ECROWLP     PROCEED FOR EVERY ROW
***********  END  ************
         RET
RSVE3    DS    18F
*
         LTORG
*
*********** STORAGE **********
ECHOCTL  DC    C' '          Echo control character
ECHOBUF  DC    CL132' '      Echo buffer
*
EVTARG   DC    A(ECHOBUF)    TEXT POINTER ARG
EVNARG   DC    F'0'          NUMBER POINTER ARG
EVENDA   DC    A(EVENDR)     POINTER TO LAST CHAR
*
EVENDR   DC    F'0'
******************************
         DS    0H
******************************************************************
*
*                 ECHO MATRIX ENTRY
*                 (char*, PL9*)
*
*            PRINTS A 9 BYTE PACKED TO
*            THE SUPPLIED ADDRESS
*            IN THE FORM OF
*            +XXXX.XXXX
*
*            R2 -> TEXT BUFFER (F)
*            R3 -> PACKED NUMBER
*            R4 -> END OF TEXT (F) (OUTPUT)
*
******************************************************************
ECHOLEN  EQU   19
*----------------------------------------------------------------*
ECHOVAL  CSECT
         CTRLS ECHOVAL,RSVE4
*********** START ************
         LM    2,4,0(1)
         MVC   0(FORMATZ,2),FORMAT    Load in format string
         LA    2,0(,2)
         LR    1,2         Load 1 with text buffer
         A     1,=F'13'    Increment to default Sign position
         EDMK  0(FORMATZ,2),0(3)  Format number
         BCTR  1,0                Decrement 1
         MVI   0(1),C'+'          Place sign
         CP    0($MATRBZ,3),=PL1'0' Check for negativity
         BNL   ECHNNEG              skip if not,
         MVI   0(1),C'-'            otherwise place -
ECHNNEG  DS    0X
         LA    2,FORMATZ(2)         return stop value
         ST    2,0(,4)              on 4th argument
***********  END  ************
         RET
RSVE4    DS    18F
*
         LTORG
*
*********** STORAGE **********
FORMAT   DC    C' '        ZERO SUPPRESSION
         DC    3X'202020'   ECHO FIRST 11 NUMS BYTES
         DC    X'20202120'
         DC    C'.'        DECIMAL
         DC    X'20202020' FRACTIONAL COMPONENT
FORMATZ  EQU   *-FORMAT
******************************
         DS    0H
******************************************************************
*
*                     MATRIX MULTIPLY
*                     (MATRIX*, MATRIX*, MATRIX*)
*                 Multiplies first two supplied matrices,
*                 stores result in 3rd matrix.
*
*                     R2-> MATRIX 1      (F)
*                     R3-> MATRIX 2      (F)
*                     R4-> RESULT MATRIX (F)
*
******************************************************************
*----------------------------------------------------------------*
MULTMATR CSECT
         CTRLS MULTMATR,RSVE5
*********** START ************
         LM    2,4,0(1)
         ST    2,DPMRA       Store matrix pointers
         ST    3,DPMRB
         USING $MATR,4       Target result matrix
* CHECK FOR COMPATABILITY
         L     5,$SIZEXO(2) COLUMNS IN A MUST EQUAL
         C     5,$SIZEYO(3) ROWS IN B OR
         LA    15,1         PANIC CODE 1: INCOMPAT MATR
         BNE   MULPANC
*
*        SETUP RESMATRIX
         MVC   $SIZEX(4),$SIZEXO(3)  RES COLS = B COLS
         MVC   $SIZEY(4),$SIZEYO(2)  RES ROWS = A ROWS
*
         BSR   ECHOMATR,DPMRA        Echo input matrices
         XPRNT =C' ',1               Newline
         BSR   ECHOMATR,DPMRB
         XPRNT =C' ',1
         LA    5,$MTRBODY            Track to matrix body
         L     6,$SIZEYO(,2)   Load 6 to iterate over Rows
         SR    7,7             clear 7
         ST    7,MULROWI    CLEAR ROW INDEX
*
RESROW   DS    0X
         L     8,$SIZEXO(,3)  SET COLUMN COUNTER
         SR    7,7          ZERO SEVEN
         ST    7,MULCOLI    CLEAR COLUMN INDEX
RESCOL   DS    0X
         ST    5,DPRESP     POINT DP AT RESULT INDEX
         L     7,MULROWI    OUTPUT ROW NUMBER
         BSR   DOTPROD,MDPARGS         Compute dot product
         MINCR MULCOLI,7               increment column
         LA    5,$MATRBZ(,5) INCREMENT TO NEXT SPOT
         BCT   8,RESCOL                count along columns
*
         MINCR MULROWI,7               increment row
         SR    7,7
         ST    7,MULCOLI               reset columns
         BCT   6,RESROW                count along rows
*
*
***********  end  ************
MULEXT   DS    0X
         LA    15,0
MULPANC  DS    0H
         RET
RSVE5    DS    18F
*
         LTORG
*
*********** STORAGE **********
*
MULDCTL  DC    C' '
MULDOBF  DC    CL133' '
*
MDPARGS  DS    0F
DPMRA    DS    F   MATRIX A POINTER
DPMRB    DS    F   MATRIX B POINTER
DPRESP   DS    F   RESULT POINTER
MULROWI  DS    F   Row index
MULCOLI  DS    F   Column index
*
         DS    0H
******************************************************************
*
*                       DOT PROD
*             (MATRIX*, MATRIX*, PL9*, INT, INT)
*                       Calculates the dot product
*                       between the given matrices
*                       for the given row and column
*
*                       R2-> MATRIX A      (F)
*                       R3-> MATRIX B      (F)
*                       R4-> RESULT ADDRSS (F)
*                       R5-> ROW (A)       (F)
*                       R6-> COL (B)       (F)
*                       R8-> MUL UPPER
*                       R9-> MUL LOWER
*                      R10-> ARRAY PTR
*                      R10-> ARRAY PTR
*
******************************************************************
DOTPROD  CSECT
         CTRLS DOTPROD,RSVE6
*********** START ************
         LM    2,6,0(1)
*         USING $MATR,3       REFER TO B
         SR    8,8           CLEAR BOTH MULTIPLICATION
         SR    9,9           REGISTERS
         LA    9,$MATRBZ     LOAD SIZE OF ENTRY
         M     8,$SIZEXO(3)      MULTIPLY ENTRY SIZE BY LEN
         ST    9,BINC        TO GET SIZE OF A ROW IN BYTES
*
*         USING $MATR,2       REFER TO A
         SR    8,8           CLEAR BOTH MUL
         SR    9,9           REGISTERS
         LA    9,$MATRBZ      LOAD SIZE OF ENTRY
         M     8,$SIZEXO(2)       MULTIPLY BY ROW OF A
         ST    9,AINC        STORE A INCREMENT
*
         USING $MATR,2          REFER TO A
         SR    8,8              ROW START = SIZEOF(ROW) * NUMBER
         SR    9,9              CLEAR
         L     9,AINC           LOAD WIDTH
         MR    8,5              MULTIPLY BY ROW NUMBER
         LR    5,9              STORE BODY OFFSET BACK INTO 5
         LA    5,$MTRBODO(5,2)  CONVERT TO RAW ADDRESS
*
         USING $MATR,3       REFER TO B
         SR    8,8           COLUMN START = SIZEOF(ITEM) * NUMBER
         SR    9,9           CLEAR
         LA    9,$MATRBZ     LOAD ITEM SIZE
         MR    8,6           MULTIPLY BY COLUMN NUMBER
         LR    6,9           STORE RESULT BACK INTO 6
         LA    6,$MTRBODO(6,3)      TURN INTO AN OFFSET FROM MATR
*
*        LOOP OVER INDICES
*
         ZAP   DPTEMPP(DPTEMPPZ),=PL1'0'
         ZAP   DPACCUM(DPACCUMZ),=PL1'0'
         L     8,$SIZEYO(3)   NUMBER OF INDICES = YSIZE
DPLP     DS    0X
*         XDUMP
         ZAP   DPTEMPP(DPTEMPPZ),0($MATRBZ,5) LOAD COLUMN ITEM
         MP    DPTEMPP(DPTEMPPZ),1($MATRBZ-1,6) MULTIPLY BY ROW ITEM
*
         AP    DPACCUM(DPACCUMZ),DPTEMPP(DPTEMPPZ)
*
         LA    5,$MATRBZ(,5) INCREMENT TO NEXT IN ROW
         A     6,BINC       INCREMENT TO NEXT IN COLUMN
         BCT   8,DPLP
         SRP   DPACCUM(DPACCUMZ),64-4,5
         ZAP   0(9,4),DPACCUM STORE RESULT
***********  END  ************
         RET
RSVE6    DS    18F
*
         LTORG
*
*********** STORAGE **********
         DS    0F
BINC     DC    4X'BC'           ROW INCREMENT VAL FOR B
AINC     DC    4X'BE'           ROW INCREMENT VAL FOR A
DPTEMPP  DS    PL16
DPTEMPPZ EQU   *-DPTEMPP
DPACCUM  DS    PL9
DPACCUMZ EQU   *-DPACCUM
DBBUF    DC    C' '
         DC    C'ROW NUMBER: '
DBRN     DC    CL12' '
         DC    C' COLUMN NUMBER: '
DBCN     DC    CL12' '
DBBUFZ   EQU   *-DBBUF
         END MAIN
/*
//*
//* IN-STREAM PROGRAM DATA
//FT05F001 DD *
+0021.0001 +0000.0002 +0004.0004
-0005.0006 +0006.0005 -0000.0032
+0006.0006 +0006.0005 +0000.0032
+0005.0006 +0006.0005 +0000.0032
*
0012.0453 0024.0232 0044.0242 0044.0252
0076.1222 0220.0446 0044.0222 0044.0222
0330.0349 0034.6557 0044.0222 0044.0222
/*
//