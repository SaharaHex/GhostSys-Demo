'This is a Prototype / Demo of a wider project, this is built to test out the functions and see what’s possible.
'This .BAS file should work with both the Community QBasic version and QB64.
Cls
_Title "GhostSys v0.0.1 Prototype Demo"
currentFolder$ = "C:\"

Dim restartFlag
restartFlag = 0
Dim endFlag
endFlag = 0

Randomize Timer ' optional: ensures different results each run
randomValue = Int(Rnd * 10) + 1 ' only use to indicate new game

Dim fileList$(12) ' create a Source List of File Names
fileList$(1) = "secret.sys"
fileList$(2) = "es2ui.dll"
fileList$(3) = "notes.txt"
fileList$(4) = "config.ini"
fileList$(5) = "readme.md"
fileList$(6) = "patch.hex"
fileList$(7) = "trace.log"
fileList$(8) = "data.tmp"
fileList$(9) = "boot.scr"
fileList$(10) = "virus.txt"
fileList$(11) = "toolkit.sys"
fileList$(12) = "null.hex"


Dim fileUsed(12) ' track Used Files With a Parallel Boolean Array
For i = 1 To 12
    fileUsed(i) = 0
Next

Dim folderList$(4) ' create a Source List of Folder Names
folderList$(1) = "SYSTEM"
folderList$(2) = "LOGS"
folderList$(3) = "OS"
folderList$(4) = "APPS"

Dim folderUsed(4) ' track Used Folder With a Parallel Boolean Array
For i = 1 To 4
    folderUsed(i) = 0
Next

Dim folderNames$(2) ' number of folders in game
Dim folderFiles$(2, 5) ' number of files in each folder

foundPassword = 0 ' to login to system
areLogin = 0 ' have logged in to system
hasNote = 0 ' todo

Dim cleanUses ' to remove junk files
Dim maxCleanUses
maxCleanUses = 3
cleanUses = 0

Dim hasToolkit
hasToolkit = 0

Dim patchActive ' junk spawning paused
Dim patchTurns
Const patchDuration = 4 ' lasts for 4 turns
patchActive = 0
patchTurns = 0

Dim hasNullHex
hasNullHex = 0
Dim overrideReady
overrideReady = 0

Call WarningMessage
StartScreen

Do
    Cls
    Call ResetGameState
    Call RunGame

    If restartFlag = 0 Then
        Color 7, 0 ' Reset to light grey on black
        Print "------------------------------------------"
        Print " Would you like to restart the game? (Y/N)"
        Print "------------------------------------------"
        Print ""
        Input restart$
        restart$ = UCase$(LTrim$(RTrim$(restart$)))
        If restart$ = "Y" Then restartFlag = 1
    End If
Loop While restartFlag = 1

Print "Thanks for playing GhostSys Demo. Goodbye!"
End

'==============================
' delay compatible with QBASIC as - _DELAY is a QB64-specific command
'==============================
Sub Delay (slow, seconds)
    For i = 1 To slow
        ' burn time
    Next
    _Delay seconds ' waits for 0.5 seconds
End Sub

'==============================
' ui message with colour
'==============================
Sub UIMessage (textColour, message$, resetColour)
    Color textColour, 0
    Print message$
    Color resetColour, 0
End Sub

'==============================
' ui bar animation with colour
'==============================
Sub UIBar (textColour, length, resetColour)
    Color textColour, 0
    For i = 1 To length
        Print Chr$(219);
        Call Delay(300, 0.05)
    Next
    Color resetColour, 0
    Print " "
End Sub

'==============================
' warning message
'==============================
Sub WarningMessage
    Call UIMessage(14, Chr$(13) + Chr$(176) + " Note " + Chr$(176), 7) ' yellow text
    Print Chr$(176); " This program may cause minor screen flicker due to rapid display updates."
    Print Chr$(176); " You can toggle Fullscreen manually with ALT + ENTER"
    Print Chr$(13); " Press Enter to begin..."
    Input wm$
    Cls
End Sub

'==============================
' play winning sound
'==============================
Sub SoundWin
    For i = 1 To 15
        Sound 1000 + Int(Rnd * 500), 2
        Call Delay(200, 0.01)
    Next
End Sub

'==============================
' start screen
'==============================
Sub StartScreen
    Shared randomValue
    Color 10, 0 ' light green on black

    Print "GhostSys v0.0.1 [Prototype Demo]"
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
    Print "                                          Prototype  ''   Demo      "

    Print ""
    Sleep 2
    Print "____________________________________________________________________"
    Print " Welcome to GhostSys, a retro-inspired, text-based puzzle-defence experience"; Chr$(13); " styled in the classic DOS terminals and built using the"; Chr$(13); " BASIC programming language."
    Print ""
    Color 15, 0 ' white on black
    Print " As you navigate this simulated system, a rogue AI --codenamed NULL.HEX-- has "; Chr$(13); " begun corrupting directories by spawning junk files."; Chr$(13); " If the file count exceeds critical limits, the system collapses."; Chr$(13); " Your mission: login to the root system "; Chr$(13); " and shut it all down before its too late."
    Color 7, 0 ' light grey on black
    Print ""
    Sleep 1
    Print " But you're not alone. AI bots roam the shell-some may assist, others deceive."
    Print ""
    Print " Use the HELP command to discover the tools at your disposal."; Chr$(13); " From DIR to TYPE, every command counts."
    Print ""
    Sleep 1
    Print " Each session has randomized layouts and file structures,"; Chr$(13); " so no two runs are exactly alike."
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
    Shared fileList$()
    Shared fileUsed()
    Shared folderList$()
    Shared folderUsed()
    Shared folderNames$()
    Shared folderFiles$()

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
                r = Int(Rnd * 12) + 1
            Loop While fileUsed(r) = 1

            folderFiles$(folderIndex, fileSlot) = fileList$(r)
            fileUsed(r) = 1
        Next
    Next
End Sub


'==============================
' main screen, game logic
'==============================
Sub MainScreen
    Shared restartFlag
    Shared endFlag
    Shared currentFolder$
    Shared folderNames$()
    Shared folderFiles$()
    Shared showPrompt
    Shared foundPassword
    Shared areLogin
    Shared hasNote
    Shared cleanUses
    Shared maxCleanUses
    Shared hasToolkit
    Shared patchTurns
    Shared patchActive
    Shared hasNullHex
    Shared overrideReady

    Dim folderJunkFiles$(100)
    Dim junkFileCount
    Dim turnsPassed
    Dim currentFolderIndex

    turnsPassed = 0
    junkFileCount = 0

    Do
        Color 7, 0 ' light grey on black
        Print currentFolder$ + "> ";
        Input action$
        action$ = UCase$(LTrim$(RTrim$(action$)))
        turnsPassed = turnsPassed + 1

        If patchActive = 0 Then
            ' AI junk file spawner every 2 turns
            If turnsPassed Mod 2 = 0 Then
                Call SpawnJunk(folderJunkFiles$(), junkFileCount)
                If endFlag = 1 Then
                    restartFlag = 0
                    Exit Sub
                End If
            End If
        Else
            patchTurns = patchTurns - 1
            If patchTurns = 0 Then
                patchActive = 0
                Call UIMessage(13, "AI disruption ended. Junk files may resume.", 7) ' light magenta text
            Else
                Print "AI patch active. Junk spawning paused ("; patchTurns; " turns left)."
            End If
        End If

        ' command dispatcher
        Select Case action$
            Case "DIR"
                Call HandleDIR(folderJunkFiles$(), junkFileCount, currentFolder$, folderNames$(), folderFiles$(), currentFolderIndex)
            Case "CLEAN"
                Call HandleClean(junkFileCount, cleanUses, maxCleanUses, hasToolkit)
            Case "LOGIN"
                Call HandleLOGIN(foundPassword)
            Case "HELP"
                Call ShowHelp
            Case "ITEMS"
                Call ShowInventory(foundPassword, hasNote, areLogin, hasToolkit, hasNullHex, overrideReady)
            Case "RESTART"
                Print "Restarting game..."
                restartFlag = 0
                Exit Sub
            Case "EXIT"
                Print "Session terminated. Goodbye!"
                End
            Case "OVERRIDE"
                If overrideReady = 1 And areLogin = 1 Then
                    Call UIMessage(10, "Shutting down rogue AI NULL.HEX...", 7) ' light green
                    Call UIBar(10, 30, 7)
                    Call SoundWin
                    Print "System restored. NULL.HEX neutralized."
                    Call UIMessage(10, "Completed demo, thank you for playing.", 7) ' light green
                    restartFlag = 0
                    Exit Sub
                ElseIf overrideReady = 1 And areLogin = 0 Then
                    Call UIMessage(12, "OVERRIDE failed. You must LOGIN first.", 7) ' light red
                Else
                    Call UIMessage(13, "OVERRIDE protocol unavailable. NULL.HEX not detected / located.", 7) ' light magenta
                End If
            Case Else
                If Left$(action$, 3) = "CD " Or Left$(action$, 4) = "CD.." Then
                    Call HandleCD(action$, currentFolder$, folderNames$(), currentFolderIndex)
                ElseIf Left$(action$, 5) = "TYPE " Then
                    Call HandleTYPE(action$, folderFiles$(), currentFolderIndex, foundPassword, hasNote, hasToolkit, patchActive, patchTurns)
                Else
                    Print "Bad command or filename"
                End If
        End Select

        Print "------------------------------"
    Loop
End Sub

'==============================
' spawnJunk, add junk file
'==============================
Sub SpawnJunk (folderJunkFiles$(), junkFileCount)
    Shared endFlag
    junkFileCount = junkFileCount + 1
    folderJunkFiles$(junkFileCount) = "junk" + Str$(junkFileCount) + ".tmp"

    If junkFileCount > 10 Then
        Call UIMessage(12, "DIRECTORY OVERLOAD! The rogue AI has filled your drive!", 7) ' light red text
        endFlag = 1
    End If
End Sub

'==============================
' handle DIR
'==============================
Sub HandleDIR (folderJunkFiles$(), junkFileCount, currentFolder$, folderNames$(), folderFiles$(), currentFolderIndex)
    If currentFolder$ = "C:\" Then
        For i = 1 To UBound(folderNames$)
            Print "Folder: "; folderNames$(i)
        Next
    Else
        Print "Files in "; folderNames$(currentFolderIndex)
        For j = 1 To 5
            If folderFiles$(currentFolderIndex, j) <> "" Then
                Print " - "; folderFiles$(currentFolderIndex, j)
            End If
        Next
    End If

    ' show junk files
    For i = 1 To junkFileCount
        Call UIMessage(8, " - " + folderJunkFiles$(i), 7) ' dark gray text
    Next
End Sub

'==============================
' handle CD
'==============================
Sub HandleCD (action$, currentFolder$, folderNames$(), currentFolderIndex)
    If action$ = "CD.." Then
        dirName$ = ".."
    Else
        dirName$ = Mid$(action$, 4)
    End If

    foundFolder = 0
    For i = LBound(folderNames$) To UBound(folderNames$)
        If dirName$ = ".." Then
            currentFolder$ = "C:\"
            foundFolder = 1
        ElseIf dirName$ = folderNames$(i) And currentFolder$ = "C:\" Then
            currentFolder$ = "C:\" + folderNames$(i) + "\"
            currentFolderIndex = i
            foundFolder = 1
        End If
    Next

    If foundFolder = 0 Then
        Print "Directory not found"
    End If
End Sub

'==============================
' handle TYPE
'==============================
Sub HandleTYPE (action$, folderFiles$(), currentFolderIndex, foundPassword, hasNote, hasToolkit, patchActive, patchTurns)
    Shared hasNullHex
    Shared overrideReady
    fileName$ = Mid$(action$, 6)
    foundFile = 0

    For j = 1 To 5
        If UCase$(fileName$) = UCase$(folderFiles$(currentFolderIndex, j)) Then
            foundFile = 1
            Exit For
        End If
    Next

    If foundFile Then
        Select Case UCase$(fileName$)
            Case "SECRET.SYS"
                Print "Password = ECHOBASE"
                foundPassword = 1
            Case "NOTES.TXT"
                Print "Clue: Trace the rogue AI by locating NULL.HEX."
                Print "Use the TYPE command to reveal its location."
                hasNote = 1
            Case "TOOLKIT.SYS"
                Call UIMessage(13, "Toolkit activated. This will reset CLEAN when reach limit.", 7) ' light magenta text
                hasToolkit = 1
                folderFiles$(currentFolderIndex, j) = "" ' removes the file from current folder
            Case "PATCH.HEX"
                Call UIMessage(13, "Patch initialized. AI disruption engaged.", 7) ' light magenta text
                patchActive = 1
                patchTurns = patchDuration
                folderFiles$(currentFolderIndex, j) = "" ' removes the file from current folder
            Case "NULL.HEX"
                If hasNullHex = 0 Then
                    hasNullHex = 1
                    overrideReady = 1
                    Call UIMessage(12, "WARNING: Rogue AI NULL.HEX Located!", 7) ' light red
                    Call UIBar(12, 30, 7)
                    Print "System corruption accelerating. Use OVERRIDE protocol."
                Else
                    Print "NULL.HEX already located."
                End If
            Case Else
                Print "File opened: "; fileName$
                Call UIBar(8, 30, 7) ' dark gray bar
                Print
                Print "No readable content."
        End Select
    ElseIf Left$(UCase$(fileName$), 4) = "JUNK" And Right$(UCase$(fileName$), 4) = ".TMP" Then
        Print "These are files created by rogue AI, do not let it fill up the drive."
    Else
        Print "File not found in current folder."
    End If
End Sub

'==============================
' handle CLEAN
'==============================
Sub HandleClean (junkFileCount, cleanUses, maxCleanUses, hasToolkit)
    If cleanUses >= maxCleanUses Then
        If hasToolkit Then
            Call UIMessage(3, "Toolkit used. CLEAN command restored.", 7) ' green text
            cleanUses = 0
            hasToolkit = 0
        Else
            Call UIMessage(13, "CLEAN command limit reached. System tools disabled.", 7) ' light magenta text
            Exit Sub
        End If
    End If

    Print "Junk files deleted. Directory stabilized."
    junkFileCount = 0
    cleanUses = cleanUses + 1
    Print "CLEAN uses remaining: "; maxCleanUses - cleanUses
End Sub

'==============================
' handle LOGIN
'==============================
Sub HandleLOGIN (foundPassword)
    Shared areLogin

    If foundPassword = 1 Then
        areLogin = 1
        Call UIMessage(10, "Access Granted! System unlocking...", 7) ' light green
        Call UIBar(10, 30, 7)
        Print "New option available: OVERRIDE"
    Else
        Call UIMessage(12, "Access Denied. Missing password.", 7) ' light red text
    End If
End Sub

'==============================
' display help, list of valid commands
'==============================
Sub ShowHelp
    Print "dir      - displays working directory contents"
    Print "cd       - change the working directory (cd .. To go back directory)"
    Print "type     - display contents of a text file"
    Print "login    - attempt system login (game goal)"
    Print "override - terminate rogue AI if NULL.HEX is located (game goal)"
    Print "clean    - delete junk files (limited number of use)"
    Print "items    - show inventory"
    Print "restart  - to restart game again"
    Print "exit     - quit the game"
    Print "--------------------------------------------------------------------"
    Print "You can toggle Fullscreen manually with ALT + ENTER"
End Sub

'==============================
' display inventory
'==============================
Sub ShowInventory (foundPassword, hasNote, areLogin, hasToolkit, hasNullHex, overrideReady)
    Print "Inventory:"
    If foundPassword Then Print " - Have Password fragment"
    If hasNote Then Print " - System clue : Locate NULL.HEX. Use TYPE to extract rogue AI location data."
    If areLogin Then Print " - Are logged in to System"
    If hasToolkit Then Print " - Have Toolkit ON - this will reset CLEAN when reach limit"
    If hasNullHex Then Print " - Rogue AI NULL.HEX located"
    If overrideReady And areLogin Then
        Print " - OVERRIDE protocol ready"
    ElseIf overrideReady Then
        Print " - OVERRIDE protocol unlocked (LOGIN required)"
    End If
End Sub

'==============================
' run / restart game
'==============================
Sub RunGame
    WelcomeScreen
    GameSetUp
    MainScreen
End Sub

'==============================
' reset all the game values
'==============================
Sub ResetGameState
    Shared currentFolder$
    Shared restartFlag
    Shared endFlag
    Shared randomValue
    Shared fileUsed()
    Shared folderUsed()
    Shared folderFiles$
    Shared folderNames$
    Shared foundPassword
    Shared areLogin
    Shared hasNote
    Shared cleanUses
    Shared hasToolkit
    Shared patchActive
    Shared patchTurns
    Shared hasNullHex
    Shared overrideReady

    currentFolder$ = "C:\"
    restartFlag = 0
    endFlag = 0

    Randomize Timer
    randomValue = Int(Rnd * 10) + 1

    ' track Used Files With a Parallel Boolean Array
    For i = 1 To 12
        fileUsed(i) = 0
    Next

    ' track Used Folder With a Parallel Boolean Array
    For i = 1 To 4
        folderUsed(i) = 0
    Next

    Dim newfolderNames$(2) ' number of folders in game
    Dim newfolderFiles$(2, 5) ' number of files in each folder

    folderNames$ = newfolderNames$
    folderFiles$ = newfolderFiles$

    ' Reset game flags and progress
    foundPassword = 0
    areLogin = 0
    hasNote = 0
    cleanUses = 0
    hasToolkit = 0
    patchActive = 0
    patchTurns = 0
    hasNullHex = 0
    overrideReady = 0
End Sub
