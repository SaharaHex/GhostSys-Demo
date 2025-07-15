
Cls
currentFolder$ = "C:\"

RANDOMIZE TIMER     ' optional: ensures different results each run
randomValue = INT(RND * 10) + 1

DIM fileList$(10) ' create a Source List of File Names
fileList$(1) = "secret.sys"
fileList$(2) = "junk.bat"
fileList$(3) = "notes.txt"
fileList$(4) = "config.ini"
fileList$(5) = "readme.md"
fileList$(6) = "patch.hex"
fileList$(7) = "trace.log"
fileList$(8) = "data.tmp"
fileList$(9) = "boot.scr"
fileList$(10) = "virus.txt"

DIM fileUsed(10) ' track Used Files With a Parallel Boolean Array
FOR i = 1 TO 10
    fileUsed(i) = 0
NEXT

DIM folderList$(4) ' create a Source List of Folder Names
folderList$(1) = "SYSTEM"
folderList$(2) = "LOGS"
folderList$(3) = "OS"
folderList$(4) = "APPS"

DIM folderUsed(4) ' track Used Folder With a Parallel Boolean Array
FOR i = 1 TO 4
    folderUsed(i) = 0
NEXT

DIM folderNames$(2) ' number of folders in game 
DIM folderFiles$(2, 5) ' number of files in each folder

StartScreen
WelcomeScreen
GameSetUp
MainScreen

'==============================
' delay compatible with QBASIC as - _DELAY is a QB64-specific command
'==============================
SUB Delay (ticks)
    FOR i = 1 TO ticks
        ' burn time
    NEXT
    _Delay 0.05 ' waits for 0.5 seconds
END SUB

'==============================
' start screen
'==============================
Sub StartScreen
    SHARED randomValue
    Color 10, 0 ' light green on black

    Print "GhostSys v1.0.0 [Shadow Kernel]"
    Sleep 1
    Print "Initializing system components..."
    Sleep 1
    Print "POST Diagnostics: MEM OK, GPU Detected, I/O Stable"
    Sleep 1
    Print

    For i = 1 To 3
        Locate 1 + i, 60
        Print ">> Loading CORE_" + Str$(i) + "..."
        Sleep 1
    Next

    Color 14
    Print
    Print "Mounting directories..."
    For i = 1 To 60
        Print Chr$(219);
        Delay 300
    Next

    Print
    Color 12
    Print "Warning: Rogue AI signature detected..."
    Sleep 1

    Color 15
    Print "Deploying containment protocols..."
    For i = 1 To 5
        Color Int(Rnd * 15) + 1
        Locate 10 + i, 60
        Print Chr$(219) + " SYSTEM SCAN " + Chr$(219)
        Delay 300
    Next

    Color 11
    Print
    Print "Boot sequence complete. Launching GhostSys Shell... "; randomValue
    Sleep 1

    For i = 1 To 5
        Locate 20, 10
        Color Int(Rnd * 15) + 1
        Print "Loading..."
        Delay 300
    Next
    Sleep 2
    Print
End Sub

'==============================
' welcome screen
'==============================
Sub WelcomeScreen
    Color 7, 0 ' light grey on black

    Print ""
    Print "  ..|'''.|  '||                       .    .|'''.|                  "
    Print " .|'     '   || ..     ...    ....  .||.   ||..  '  .... ...  ....  "
    Print " ||    ....  ||' ||  .|  '|. ||. '   ||     ''|||.   '|.  |  ||. '  "
    Print " '|.    ||   ||  ||  ||   || . '|..  ||   .     '||   '|.|   . '|.. "
    Print "  ''|...'|  .||. ||.  '|..|' |'..|'  '|.' |'....|'     '|    |'..|' "
    Print "                                                    .. |            "
    Print "                                                     ''             "

    Print ""
    Sleep 2
    Print "____________________________________________________________________"
    Print " Welcome to GhostSys, a retro-inspired, text-based puzzle-defence experience"; Chr$(13); " styled in the classic DOS terminals and built using the"; Chr$(13); " BASIC programming language."
    Print ""
    Color 15, 0 ' white on black
    Print " As you navigate this simulated system, a rogue AI --codenamed NULL.HEX-- has "; Chr$(13); " begun corrupting directories by spawning junk files."; Chr$(13); " If the file count exceeds critical limits, the system collapses."; Chr$(13); " Your mission: log in to the root system "; Chr$(13); " and shut it all down before its too late."
    Color 7, 0 ' light grey on black
    Print ""
    Sleep 1
    Print " But you're not alone. AI bots roam the shell-some may assist, others deceive."
    Print ""
    Print " Use the HELP command to discover the tools at your disposal."; Chr$(13); " From DIR to TYPE, every command counts."
    Print ""
    Sleep 1
    Print " Each session is seeded for randomized layouts and file structures,"; Chr$(13); " so no two runs are exactly alike."
    Print ""
    Print " Prepare to decrypt, defend, and dive deep into the ghost of a forgotten system."
    Print ""
    Sleep 1
    Print " Enjoy. And good luck."
    Print "____________________________________________________________________"
End Sub

'==============================
' game set up - folder and file layout
'==============================
Sub GameSetUp
    SHARED fileList$()
    SHARED fileUsed()
    SHARED folderList$()
    SHARED folderUsed()
    SHARED folderNames$()
    SHARED folderFiles$()

    folderNumber = 1
    FOR fileSlot = 1 TO 2 ' two random folders
        DO
            rf = INT(RND * 4) + 1
        LOOP WHILE folderUsed(rf) = 1
                
        folderNames$(folderNumber) = folderList$(rf)
        folderUsed(rf) = 1
        folderNumber = folderNumber + 1
    NEXT

    FOR folderIndex = 1 TO 2       ' two random folders
         FOR fileSlot = 1 TO 5     ' 5 random files per folder
            DO
                r = INT(RND * 10) + 1
            LOOP WHILE fileUsed(r) = 1

            folderFiles$(folderIndex, fileSlot) = fileList$(r)
            fileUsed(r) = 1
        NEXT
    NEXT
End Sub


'==============================
' main Screen
'==============================
Sub MainScreen ()
    SHARED currentFolder$
    
    Do
        Color 7, 0 ' light grey on black
        Print currentFolder$ + "> ";
        Input action$

        action$ = UCase$(action$)
    Loop
End Sub
