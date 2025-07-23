'This is a Prototype / Demo of a wider project, this is built to test out the functions and see what’s possible.
'This .BAS file should work with both QBASIC and QB64.
Cls
currentFolder$ = "C:\"

Randomize Timer     ' optional: ensures different results each run
randomValue = Int(Rnd * 10) + 1

Dim fileList$(11)   ' create a Source List of File Names
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
fileList$(11) = "toolkit.sys"


Dim fileUsed(11)    ' track Used Files With a Parallel Boolean Array
For i = 1 To 11
    fileUsed(i) = 0
Next

Dim folderList$(4)  ' create a Source List of Folder Names
folderList$(1) = "SYSTEM"
folderList$(2) = "LOGS"
folderList$(3) = "OS"
folderList$(4) = "APPS"

Dim folderUsed(4)   ' track Used Folder With a Parallel Boolean Array
For i = 1 To 4
    folderUsed(i) = 0
Next

Dim folderNames$(2) ' number of folders in game
Dim folderFiles$(2, 5)  ' number of files in each folder

foundPassword = 0   ' to login to system
hasKey = 0          ' todo
hasNote = 0         ' todo

DIM cleanUses       ' to remove junk files
DIM maxCleanUses
maxCleanUses = 3
cleanUses = 0

DIM hasToolkit
hasToolkit = 0

DIM patchActive     ' junk spawning paused
DIM patchTurns
Const patchDuration = 4 ' lasts for 4 turns
patchActive = 0
patchTurns = 0

CALL WarningMessage
StartScreen
WelcomeScreen
GameSetUp
MainScreen

'==============================
' delay compatible with QBASIC as - _DELAY is a QB64-specific command
'==============================
SUB Delay (slow, seconds)
    For i = 1 To slow
        ' burn time
    Next
    _Delay seconds ' waits for 0.5 seconds
End SUB

'==============================
' ui message with colour
'==============================
SUB UIMessage(textColour, message$, resetColour)
    COLOR textColour, 0
    PRINT message$
    COLOR resetColour, 0
END SUB

'==============================
' ui bar animation with colour
'==============================
SUB UIBar(textColour, length, resetColour)    
    Color textColour, 0
    For i = 1 To length
        Print Chr$(219);
        Call Delay(300, 0.05)
    Next 
    COLOR resetColour, 0
END SUB

'==============================
' warning message
'==============================
SUB WarningMessage
    CALL UIMessage(14, Chr$(13) + Chr$(176) + " Note " + Chr$(176), 7) ' yellow text
    PRINT Chr$(176); " This program may cause minor screen flicker due to rapid display updates."
    PRINT Chr$(13); " Press any key to begin..."
    INPUT wm$
    Cls
END SUB

'==============================
' start screen
'==============================
SUB StartScreen
    SHARED randomValue
    Color 10, 0 ' light green on black

    Print "GhostSys v0.0.1 [Shadow Kernel]"
    Sleep 1
    Print "Initializing system components..."
    Sleep 1
    Print "POST Diagnostics: MEM OK, GPU Detected, I/O Stable"
    Sleep 1
    Print

    For i = 1 To 3
        Locate 1 + i, 60
        Print ">> Loading CORE_" + Str$(i) + "..."
        Call Delay(300, 0.05)
    Next

    Color 14
    Print
    Print "Mounting directories..."
    For i = 1 To 60
        Print Chr$(219);
        Call Delay(300, 0.05)
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
        Call Delay(300, 0.05)
    Next

    Color 11
    Print
    Print "Boot sequence complete. Launching GhostSys Shell... "; randomValue
    Sleep 1

    For i = 1 To 5
        Locate 20, 10
        Color Int(Rnd * 15) + 1
        Print "Loading..."
        Call Delay(300, 0.05)
    Next
    Sleep 2
    Print
End SUB

'==============================
' welcome screen
'==============================
SUB WelcomeScreen
    Color 7, 0 ' light grey on black

    Print ""
    Print "  ..|'''.|  '||                       .    .|'''.|                  "
    Print " .|'     '   || ..     ...    ....  .||.   ||..  '  .... ...  ....  "
    Print " ||    ....  ||' ||  .|  '|. ||. '   ||     ''|||.   '|.  |  ||. '  "
    Print " '|.    ||   ||  ||  ||   || . '|..  ||   .     '||   '|.|   . '|.. "
    Print "  ''|...'|  .||. ||.  '|..|' |'..|'  '|.' |'....|'     '|    |'..|' "
    Print "                                                    .. |            "
    Print "                                          Prototype  ''   Demo      "

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
End SUB

'==============================
' game set up - folder and file layout
'==============================
SUB GameSetUp
    SHARED fileList$()
    SHARED fileUsed()
    SHARED folderList$()
    SHARED folderUsed()
    SHARED folderNames$()
    SHARED folderFiles$()

    folderNumber = 1
    For folderSlot = 1 To 2 ' two random folders
        Do
            rf = Int(Rnd * 4) + 1
        Loop While folderUsed(rf) = 1
                
        folderNames$(folderNumber) = folderList$(rf)
        folderUsed(rf) = 1
        folderNumber = folderNumber + 1
    Next

    For folderIndex = 1 To 2 ' two random folders
        For fileSlot = 1 To 5 ' 5 random files per folder
            Do
                r = Int(Rnd * 11) + 1
            Loop While fileUsed(r) = 1

            folderFiles$(folderIndex, fileSlot) = fileList$(r)
            fileUsed(r) = 1
        Next
    Next
End SUB


'==============================
' main screen, game logic
'==============================
SUB MainScreen
    SHARED currentFolder$
    SHARED folderNames$()
    SHARED folderFiles$()
    SHARED showPrompt
    SHARED foundPassword
    SHARED hasKey
    SHARED hasNote
    SHARED cleanUses
    SHARED maxCleanUses
    SHARED hasToolkit
    SHARED patchTurns
    SHARED patchActive

    DIM folderJunkFiles$(100)
    DIM junkFileCount
    DIM turnsPassed
    DIM currentFolderIndex

    turnsPassed = 0
    junkFileCount = 0

    DO
        COLOR 7, 0 ' light grey on black 
        PRINT currentFolder$ + "> ";
        INPUT action$
        action$ = UCASE$(LTRIM$(RTRIM$(action$)))
        turnsPassed = turnsPassed + 1

        IF patchActive = 0 THEN
            ' AI junk file spawner every 2 turns
            IF turnsPassed MOD 2 = 0 THEN
                CALL SpawnJunk(folderJunkFiles$(), junkFileCount)
            END IF
        ELSE
            patchTurns = patchTurns - 1
            IF patchTurns = 0 THEN
                patchActive = 0
                CALL UIMessage(13, "AI disruption ended. Junk files may resume.", 7) ' light magenta text
            ELSE
                PRINT "AI patch active. Junk spawning paused ("; patchTurns; " turns left)."
            END IF
        END IF

        ' command dispatcher
        SELECT CASE action$
            CASE "DIR"
                CALL HandleDIR(folderJunkFiles$(), junkFileCount, currentFolder$, folderNames$(), folderFiles$(), currentFolderIndex)            
            CASE "CLEAN"
                CALL HandleClean(junkFileCount, cleanUses, maxCleanUses, hasToolkit)
            CASE "LOGIN"
                CALL HandleLOGIN(foundPassword)
            CASE "HELP"
                CALL ShowHelp
            CASE "ITEMS"
                CALL ShowInventory(foundPassword, hasNote, hasKey, hasToolkit)
            CASE "EXIT"
                PRINT "Session terminated. Goodbye!"
                END
            CASE ELSE
                IF LEFT$(action$, 3) = "CD " THEN
                    CALL HandleCD(action$, currentFolder$, folderNames$(), currentFolderIndex)
                ELSEIF LEFT$(action$, 5) = "TYPE " THEN
                    CALL HandleTYPE(action$, folderFiles$(), currentFolderIndex, foundPassword, hasNote, hasToolkit, patchActive, patchTurns)
                ELSE
                    PRINT "Bad command or filename"
                END IF
        END SELECT

        PRINT "------------------------------"
    LOOP
END SUB

'==============================
' spawnJunk, add junk file
'==============================
SUB SpawnJunk(folderJunkFiles$(), junkFileCount)
    junkFileCount = junkFileCount + 1
    folderJunkFiles$(junkFileCount) = "junk" + STR$(junkFileCount) + ".tmp"

    IF junkFileCount > 10 THEN
        CALL UIMessage(12, "DIRECTORY OVERLOAD! The rogue AI has filled your drive!", 7) ' light red text
        END
    END IF
END SUB

'==============================
' handle DIR
'==============================
SUB HandleDIR(folderJunkFiles$(), junkFileCount, currentFolder$, folderNames$(), folderFiles$(), currentFolderIndex)
    IF currentFolder$ = "C:\" THEN
        FOR i = 1 TO UBOUND(folderNames$)
            PRINT "Folder: "; folderNames$(i)
        NEXT
    ELSE
        PRINT "Files in "; folderNames$(currentFolderIndex)
        FOR j = 1 TO 5
            IF folderFiles$(currentFolderIndex, j) <> "" THEN
                PRINT " - "; folderFiles$(currentFolderIndex, j)
            END IF
        NEXT
    END IF

    ' show junk files
    FOR i = 1 TO junkFileCount
        CALL UIMessage(8, " - " + folderJunkFiles$(i), 7) ' dark gray text
    NEXT
END SUB

'==============================
' handle CD
'==============================
SUB HandleCD(action$, currentFolder$, folderNames$(), currentFolderIndex)
    dirName$ = MID$(action$, 4)
    foundFolder = 0

    FOR i = LBOUND(folderNames$) TO UBOUND(folderNames$)
        IF dirName$ = ".." THEN
            currentFolder$ = "C:\"
            foundFolder = 1
        ELSEIF dirName$ = folderNames$(i) AND currentFolder$ = "C:\" THEN
            currentFolder$ = "C:\" + folderNames$(i) + "\"
            currentFolderIndex = i
            foundFolder = 1
        END IF
    NEXT

    IF foundFolder = 0 THEN
        PRINT "Directory not found"
    END IF
END SUB

'==============================
' handle TYPE
'==============================
SUB HandleTYPE(action$, folderFiles$(), currentFolderIndex, foundPassword, hasNote, hasToolkit, patchActive, patchTurns)
    fileName$ = MID$(action$, 6)
    foundFile = 0

    FOR j = 1 TO 5
        IF UCASE$(fileName$) = UCASE$(folderFiles$(currentFolderIndex, j)) THEN
            foundFile = 1
            EXIT FOR
        END IF
    NEXT

    IF foundFile THEN
        SELECT CASE UCASE$(fileName$)
            CASE "SECRET.SYS"
                PRINT "Password = ECHOBASE"
                foundPassword = 1
            CASE "NOTES.TXT"
                PRINT "Clue: Check the SYSTEM folder"
                hasNote = 1
            CASE "TOOLKIT.SYS"                
                PRINT "Toolkit activated. This will reset CLEAN when reach limit"
                hasToolkit = 1
                folderFiles$(currentFolderIndex, j) = ""  ' removes the file from current folder
            CASE "PATCH.HEX"
                PRINT "Patch initialized. AI disruption engaged."
                patchActive = 1
                patchTurns = patchDuration
                folderFiles$(currentFolderIndex, j) = ""  ' removes the file from current folder
            CASE ELSE
                PRINT "File opened: "; fileName$
                CALL UIBar(8, 30, 7) ' dark gray bar
                PRINT
                PRINT "No readable content."
        END SELECT
    ELSE
        PRINT "File not found in current folder."
    END IF
END SUB

'==============================
' handle CLEAN
'==============================
SUB HandleClean(junkFileCount, cleanUses, maxCleanUses, hasToolkit)
    IF cleanUses >= maxCleanUses THEN
        IF hasToolkit THEN
            CALL UIMessage(3, "Toolkit used. CLEAN command restored.", 7) ' green text
            cleanUses = 0
            hasToolkit = 0
        ELSE
            CALL UIMessage(13, "CLEAN command limit reached. System tools disabled.", 7) ' light magenta text
            EXIT SUB
        END IF
    END IF

    PRINT "Junk files deleted. Directory stabilized."
    junkFileCount = 0
    cleanUses = cleanUses + 1
    PRINT "CLEAN uses remaining: "; maxCleanUses - cleanUses
END SUB

'==============================
' handle LOGIN
'==============================
SUB HandleLOGIN(foundPassword)
    IF foundPassword = 1 THEN
        PRINT "Access Granted! System unlocking..."
        END
    ELSE
        CALL UIMessage(12, "Access Denied. Missing password.", 7) ' light red text
    END IF
END SUB

'==============================
' display help, list of valid commands 
'==============================
SUB ShowHelp
    PRINT "dir    - displays working directory contents"
    PRINT "cd     - change the working directory (cd .. To go back directory)"
    PRINT "type   - display contents of a text file"
    PRINT "login  - attempt system login (game goal)"
    PRINT "clean  - delete junk files (limited number of use)"
    PRINT "items  - show inventory"
    PRINT "exit   - quit the game"
END SUB

'==============================
' display inventory
'==============================
SUB ShowInventory(foundPassword, hasNote, hasKey, hasToolkit)
    PRINT "Inventory:"
    IF foundPassword THEN PRINT " - Have Password fragment"
    IF hasNote THEN PRINT " - System clue note"
    IF hasKey THEN PRINT " - Access key"
    IF hasToolkit THEN PRINT " - Have Toolkit ON - this will reset CLEAN when reach limit"
END SUB
