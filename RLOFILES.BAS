DECLARE SUB PrintSplashScreen ()
DECLARE SUB PrintInstructions ()
DECLARE SUB FinalPass (n$, o$)
DECLARE SUB PrintStatusBar (a%)
DECLARE FUNCTION ShortenNames$ (a$)
DECLARE SUB FourthPass (n1$, n2$, o1$, o2$, o3$)
DECLARE SUB ThirdPass (n$, o$)
DECLARE SUB UpdateStatusBar ()
DECLARE SUB GetFileLength (n$)
DECLARE FUNCTION GetFrom$ (d$, e$)
DECLARE SUB SecondPass (n$, o$)
DECLARE SUB FirstPass (n$, o$)
DECLARE FUNCTION CheckSubstring% (d$, e$)
REM
REM 27 April to 12 May 2002
REM **QBASIC**
REM This utility takes a directory listing and produces two batch files,
REM one which shortens long filenames so that they can be put on CD/DVD
REM and another one which will restore them to their originality. Place
REM the latter with the shortened name files on the CD/DVD.
REM
REM Use at own risk. This program was written in a robust manner but
REM author takes no responsibility for disasters.

'Directory and file line identification parameters are in sub FirstPass
CONST OVERLENGTH = 64 'names longer than this will be shortened
CONST NAMELENGTH = 40 'new length of names

CLS
PrintSplashScreen
PRINT
strt:
PrintInstructions
PRINT
PRINT "Press Ctrl-C to exit"
PRINT
INPUT " Enter Filename to Process: "; n$

GetFileLength (n$) 'FirstPass has a status bar
FirstPass n$, "dat1.txt"
SecondPass "dat1.txt", "dat2.txt"
ThirdPass "dat2.txt", "dat3.txt"
GetFileLength ("dat3.txt") 'FourthPass has a status bar
FourthPass "dat3.txt", "dat1.txt", "ren1.bat", "dat5.txt", "names.txt"
FinalPass "dat5.txt", "ren2.bat"
PRINT
PRINT " Directory listing has been processed..."
PRINT
PRINT " Concerning files are REN1.BAT and REN2.BAT."
PRINT " The other files can be disposed of."

END

FUNCTION CheckSubstring% (d$, e$)

REM d$ is the input string, e$ is the match

n = LEN(d$)
r = LEN(e$)
FLAG = 0

FOR a = 1 TO n
 IF e$ = MID$(d$, a, r) THEN FLAG = 1
 IF FLAG = 1 THEN EXIT FOR
NEXT a

CheckSubstring = FLAG

END FUNCTION

SUB FinalPass (n$, o$)

'This sub reverses the restoration file as lines

'let's calculate the array size needed
OPEN n$ FOR INPUT AS #1
arrsize% = 0
DO
LINE INPUT #1, a$
arrsize% = arrsize% + 1
LOOP UNTIL EOF(1)
CLOSE #1

OPEN n$ FOR INPUT AS #1
count = 0
DIM b$(arrsize%) 'b$() is really a dynamic array
DO
LINE INPUT #1, a$
b$(count) = a$
count = count + 1
LOOP UNTIL EOF(1)
CLOSE #1

OPEN o$ FOR OUTPUT AS #1
PRINT #1, "rem Rename this file `restore_long_filenames'(.bat)."
PRINT #1, "rem You might have to use the replace function in Notepad on"
PRINT #1, "rem this batch file in order to fix drive/directory paths."
FOR a = count TO 0 STEP -1
IF LEN(b$(a)) > 0 THEN PRINT #1, b$(a)
NEXT a
CLOSE #1

END SUB

SUB FirstPass (n$, o$)

'This sub creates a file holding all the directory and file names, with full path

SHARED a&

OPEN n$ FOR INPUT AS #1
OPEN o$ FOR OUTPUT AS #2

b$ = ""
c$ = ""
b& = 0

PRINT
PrintStatusBar (1)

DO
LINE INPUT #1, a$: REM input 1 line into a$
'b& = b& + 1
IF CheckSubstring(a$, "Directory of") THEN c$ = MID$(a$, 15): PRINT #2, c$: REM PRINT c$
IF CheckSubstring(a$, "Directory of") + CheckSubstring(a$, "<DIR>") + CheckSubstring(a$, "File(s)") + CheckSubstring(a$, "Volume in drive") + CheckSubstring(a$, "Volume Serial Number") + CheckSubstring(a$, "Total Files Listed") + CheckSubstring(a$, "Dir(s)") < 1 THEN  ELSE GOTO skp1
n = LEN(a$): REM make n equal to a$'s length
IF n < 1 THEN GOTO skp1
IF n > 39 THEN b$ = MID$(a$, 40)
REM PRINT c$ + "\" + b$
PRINT #2, c$ + "\" + b$
skp1:
UpdateStatusBar
LOOP UNTIL EOF(1)

CLOSE #2
CLOSE #1

PRINT
PRINT "Done."

END SUB

SUB FourthPass (n1$, n2$, o1$, o2$, o3$)

'a$ is the whole line
'b$ is the name part
'c$ is the 'directory' part
'd$ is the shortened name part
'If d$ is fed again into ShortenNames() then a second different shortened
'  name comes out

OPEN n1$ FOR INPUT AS #1
OPEN o1$ FOR OUTPUT AS #2
OPEN o2$ FOR OUTPUT AS #3
OPEN o3$ FOR OUTPUT AS #4
PRINT #4, "Today: "; DATE$
CLOSE #4
PRINT #2, "rem Rename this file `shorten_long_filenames'(.bat)."

PRINT
PrintStatusBar (4)

DO
LINE INPUT #1, a$
b$ = GetFrom(a$, "\")
l% = LEN(a$) - LEN(b$)
c$ = LEFT$(a$, l%)
d$ = ShortenNames(b$)

'Entering duplicate name check
chkns1:
OPEN n2$ FOR INPUT AS #4
FLAG = 0
DO
LINE INPUT #4, e$
e$ = GetFrom(e$, "\")
IF e$ = d$ THEN d$ = ShortenNames(d$): FLAG = 1
LOOP UNTIL EOF(4)
CLOSE #4
IF FLAG = 1 THEN GOTO chkns1

chkns2:
OPEN o3$ FOR INPUT AS #4
FLAG = 0
DO
LINE INPUT #4, e$
IF e$ = d$ THEN d$ = ShortenNames(d$): FLAG = 1
LOOP UNTIL EOF(4)
CLOSE #4
IF FLAG = 1 THEN GOTO chkns2
'Duplicate name check part ends here

'Name is now unique and thus append to #4
OPEN o3$ FOR APPEND AS #4
PRINT #4, d$
CLOSE #4

PRINT #2, "rename " + CHR$(34) + c$ + b$ + CHR$(34) + " " + CHR$(34) + d$ + CHR$(34)
PRINT #3, "rename " + CHR$(34) + c$ + d$ + CHR$(34) + " " + CHR$(34) + b$ + CHR$(34)

UpdateStatusBar

LOOP UNTIL EOF(1)

CLOSE #1
CLOSE #2
CLOSE #3

PRINT
PRINT "Done."

END SUB

SUB GetFileLength (n$)

SHARED a&

'Let's calculates how long the input file is
a& = 0
OPEN n$ FOR INPUT AS #1
DO
LINE INPUT #1, a$
a& = a& + 1
LOOP UNTIL EOF(1)
CLOSE #1

END SUB

FUNCTION GetFrom$ (d$, e$)
REM d$ is the input string, e$ is the delimiter

n = LEN(d$)
r = LEN(e$)
FLAG = 0

FOR a = n TO 1 STEP -1
 IF e$ = MID$(d$, a, r) THEN EXIT FOR
NEXT a

GetFrom = MID$(d$, a + r)

END FUNCTION

SUB PrintInstructions
PRINT " This program takes a directory listing (eg. dir"
PRINT " C:\SomeFolder /s > dir.txt) and produces two batch"
PRINT " files, one which shortens long filenames and the other"
PRINT " which restores them."
PRINT
PRINT " The idea is that one shortens long filenames before"
PRINT " placing them on a CDFS filesystem and includes with"
PRINT " them the batch file which will restore them. (CDFS"
PRINT " such as used by CDs/DVDs can't hold long filenames.)"
PRINT
PRINT " Files with names of DAT1.TXT, DAT2.TXT, DAT3.TXT,"
PRINT " REN1.BAT, DAT5.TXT, REN2.BAT will be created. Make"
PRINT " sure that such files do not already exist in the"
PRINT " working directory. Make also sure that the directory"
PRINT " listing you created does reside in the active"
PRINT " directory."
PRINT
PRINT " When shortening long filenames, it is recomended not"
PRINT " to operate on flash disks directly but instead to copy"
PRINT " flash disk content to the HD and operate there."
PRINT
PRINT "Press a key to continue"
DO
c$ = INKEY$
LOOP UNTIL c$ <> ""
CLS
END SUB

SUB PrintSplashScreen
PRINT
PRINT
PRINT
PRINT
PRINT
PRINT
PRINT
PRINT
PRINT
PRINT
PRINT "                         ** Rename My Long Filenames **"
PRINT "                                A work in QBASIC"
PRINT
PRINT
PRINT
PRINT
PRINT
PRINT
PRINT
PRINT
PRINT
PRINT
PRINT
PRINT "Press a key to continue"
DO
c$ = INKEY$
LOOP UNTIL c$ <> ""
CLS

END SUB

SUB PrintStatusBar (stpp%)

PRINT "Step"; stpp%; "of 4:"
LOCATE CSRLIN, 1
PRINT "¯";
LOCATE CSRLIN, 23
PRINT "®";

END SUB

SUB SecondPass (n$, o$)

'This sub sorts out the long filenames/directories

PRINT
PRINT "Step 2 of 4:"
OPEN n$ FOR INPUT AS #1
OPEN o$ FOR OUTPUT AS #2
DO
LINE INPUT #1, a$
IF LEN(GetFrom(a$, "\")) > OVERLENGTH THEN PRINT #2, a$
LOOP UNTIL EOF(1)

CLOSE #1
CLOSE #2

PRINT "Done."

END SUB

FUNCTION ShortenNames$ (a$)

LOCALLENGTH = NAMELENGTH
ext$ = GetFrom(a$, ".")

'get first 40 characters and fix if necessary
b$ = LEFT$(a$, LOCALLENGTH)
DO WHILE ASC(MID$(b$, LOCALLENGTH, 1)) < 48 OR ASC(MID$(b$, LOCALLENGTH, 1)) > 122
MID$(b$, LOCALLENGTH, 1) = "~"
LOCALLENGTH = LOCALLENGTH - 1
LOOP


IF ext$ = a$ THEN 'treatment for no extention
'get next five characters in separate variables
DIM c$(4)
c$(0) = MID$(a$, 41, 1)
c$(1) = MID$(a$, 42, 1)
c$(2) = MID$(a$, 43, 1)
c$(3) = MID$(a$, 44, 1)
c$(4) = MID$(a$, 45, 1)
'the next part fixes c$ to a "~1" if the five characters are not a valid numeral (i.e. ~ + number)
c$ = ""
SELECT CASE c$(0) 'note that c$ and c$(0 to 4) are two separate entities
CASE "~" 'if c$(0) is a tilde, fall through
  SELECT CASE c$(1)
  CASE "1" TO "9" 'if c$(1) is a digit, fall through ...
  'PRINT "c$(1) is a digit"
    SELECT CASE c$(2)
    CASE "0" TO "9"
    'PRINT "c$(2) is a digit"
      SELECT CASE c$(3)
      CASE "0" TO "9"
      'PRINT "c$(3) is a digit"
        SELECT CASE c$(4)
        CASE "0" TO "9"
        'PRINT "c$(4) is a digit"
        CASE ""
        'PRINT "c$(4) is absent"
        CASE ELSE
        c$ = "~1"
        END SELECT
      CASE ""
      'PRINT "c$(3) is absent"
      CASE ELSE
      c$ = "~1"
      END SELECT
    CASE ""
    'PRINT "c$(2) is absent"
    CASE ELSE
    c$ = "~1"
    END SELECT
  CASE ELSE
  c$ = "~1"
  END SELECT
CASE ELSE
c$ = "~1"
END SELECT

IF c$ = "" THEN 'if c$ is still empty then the last part is a valid numeral
  c$ = c$(0) + c$(1) + c$(2) + c$(3) + c$(4)
  a = VAL(MID$(c$, 2))
  c$ = "~" + MID$(STR$(a + 1), 2)
END IF

'PRINT b$ + c$
ShortenNames = b$ + c$

ELSE 'treatmentForDotInName
'PRINT "Dot in name here" 'remove this silly line
'coding redundancy is only apparent here
'get next five characters in separate variables
DIM c$(4)
c$(0) = MID$(a$, 41, 1)
c$(1) = MID$(a$, 42, 1)
c$(2) = MID$(a$, 43, 1)
c$(3) = MID$(a$, 44, 1)
c$(4) = MID$(a$, 45, 1)
'the next part fixes c$ to a "~1" if the five characters are not a valid numeral (i.e. ~ + number)
c$ = ""
SELECT CASE c$(0) 'note that c$ and c$(0 to 4) are two separate entities
CASE "~" 'if c$(0) is a tilde, fall through
  SELECT CASE c$(1)
  CASE "1" TO "9" 'if c$(1) is a digit, fall through ...
  'PRINT "c$(1) is a digit"
    SELECT CASE c$(2)
    CASE "0" TO "9"
    'PRINT "c$(2) is a digit"
      SELECT CASE c$(3)
      CASE "0" TO "9"
      'PRINT "c$(3) is a digit"
        SELECT CASE c$(4)
        CASE "0" TO "9"
        'PRINT "c$(4) is a digit"
        CASE ""
        'PRINT "c$(4) is absent"
        CASE "."
        'PRINT "c$(4) is a `.'"
        CASE ELSE
        c$ = "~1"
        END SELECT
      CASE ""
      'PRINT "c$(3) is absent"
      CASE "."
      'PRINT "c$(3) is a `.'"
      CASE ELSE
      c$ = "~1"
      END SELECT
    CASE ""
    'PRINT "c$(2) is absent"
    CASE "."
    'PRINT "c$(2) is a `.'"
    CASE ELSE
    c$ = "~1"
    END SELECT
  CASE ELSE
  c$ = "~1"
  END SELECT
CASE ELSE
c$ = "~1"
END SELECT

IF c$ = "" THEN 'if c$ is still empty then the last part is a valid numeral
  c$ = c$(0) + c$(1) + c$(2) + c$(3) + c$(4)
  a = VAL(MID$(c$, 2))
  c$ = "~" + MID$(STR$(a + 1), 2)
END IF

'PRINT b$ + c$ + "." + LEFT$(ext$, 8)
ShortenNames = b$ + c$ + "." + LEFT$(ext$, 8)



END IF
'ShortenNames = ext$
END FUNCTION

SUB ThirdPass (n$, o$)

'This sub sorts out the files/directories from deep to the shallow. It does
'  this by counting the number of backlashes.

PRINT
PRINT "Step 3 of 4:"
OPEN n$ FOR INPUT AS #1
OPEN o$ FOR OUTPUT AS #2

c = 0

IF LOF(1) < 1 THEN
PRINT "Done."
PRINT
PRINT " No long filenames found!"
CLOSE #1
CLOSE #2
END
END IF

DO
b = 0
LINE INPUT #1, a$
FOR a = 1 TO LEN(a$)
IF MID$(a$, a, 1) = "\" THEN b = b + 1
NEXT a
IF c < b THEN c = b
LOOP UNTIL EOF(1)
'so now c holds the greatest number of backslashes for any line

FOR b = c TO 1 STEP -1
'PRINT "b now is: "; b

CLOSE #1
OPEN n$ FOR INPUT AS #1

DO
b1 = 0
LINE INPUT #1, a$
FOR a = 1 TO LEN(a$)
IF MID$(a$, a, 1) = "\" THEN b1 = b1 + 1
NEXT a
IF b1 = b THEN PRINT #2, a$
LOOP UNTIL EOF(1)

NEXT b

CLOSE #1
CLOSE #2

PRINT "Done."

END SUB

SUB UpdateStatusBar

SHARED a&
STATIC b&
STATIC FLAG&

IF FLAG& <> a& THEN
FLAG& = a&
b& = 0
END IF
    
b& = b& + 1
LOCATE CSRLIN, (INT(b& * 20 / a&)) + 2
PRINT "±";
END SUB
