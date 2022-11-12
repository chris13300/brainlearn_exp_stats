Imports VB = Microsoft.VisualBasic

Module modFonctions
    Public processus As System.Diagnostics.Process
    Public entree As System.IO.StreamWriter
    Public sortie As System.IO.StreamReader

    Public Function binToMove(chaineBIN As String) As String
        Dim move As String

        '0 111 110 000 111 001
        '  Q   7   a   8   b

        move = ""

        Select Case gauche(droite(chaineBIN, 9), 3) 'a
            Case "000"
                move = move & "a"

            Case "001"
                move = move & "b"

            Case "010"
                move = move & "c"

            Case "011"
                move = move & "d"

            Case "100"
                move = move & "e"

            Case "101"
                move = move & "f"

            Case "110"
                move = move & "g"

            Case "111"
                move = move & "h"

        End Select

        Select Case gauche(droite(chaineBIN, 12), 3) '7
            Case "000"
                move = move & "1"

            Case "001"
                move = move & "2"

            Case "010"
                move = move & "3"

            Case "011"
                move = move & "4"

            Case "100"
                move = move & "5"

            Case "101"
                move = move & "6"

            Case "110"
                move = move & "7"

            Case "111"
                move = move & "8"

        End Select

        Select Case droite(chaineBIN, 3) 'b
            Case "000"
                move = move & "a"

            Case "001"
                move = move & "b"

            Case "010"
                move = move & "c"

            Case "011"
                move = move & "d"

            Case "100"
                move = move & "e"

            Case "101"
                move = move & "f"

            Case "110"
                move = move & "g"

            Case "111"
                move = move & "h"

        End Select

        Select Case gauche(droite(chaineBIN, 6), 3) '8
            Case "000"
                move = move & "1"

            Case "001"
                move = move & "2"

            Case "010"
                move = move & "3"

            Case "011"
                move = move & "4"

            Case "100"
                move = move & "5"

            Case "101"
                move = move & "6"

            Case "110"
                move = move & "7"

            Case "111"
                move = move & "8"

        End Select


        Select Case droite(gauche(chaineBIN, 4), 3) 'Q
            Case "001"
                move = move & "N"

            Case "101"
                move = move & "B"

            Case "110"
                move = move & "R"

            Case "111"
                move = move & "Q"

        End Select

        'grand roque blanc ou noir
        If gauche(chaineBIN, 4) = "1100" Then
            If move = "e1a1" Or move = "e8a8" Then
                move = Replace(move, "a", "c")
            End If
        End If

        Return move
    End Function

    Public Function brainlearn_expListe(fichierBIN As String, keyUCI As String) As String
        Dim tabTampon(23) As Byte, chaineExperience As String, compteur As Integer
        Dim tabBIN(0) As Byte, pos As Long, i As Integer, j As Integer
        Dim tabCoups(1000) As String, tabProfondeurs(1000) As Integer, tabScores(1000) As Integer, tabPerformances(1000) As Integer
        Dim coup As String, profondeur As Integer, scoreCP As String, scoreMAT As String, performance As Integer
        Dim keyBIN As String
        Dim lectureBIN As IO.FileStream, posLecture As Long, tailleBIN As Long, tailleTampon As Long, reservation As Boolean

        posLecture = 0
        tailleBIN = FileLen(fichierBIN)
        tailleTampon = tailleBIN
        i = 50
        lectureBIN = New IO.FileStream(fichierBIN, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.ReadWrite)

        compteur = 1
        While posLecture < tailleBIN
            If posLecture + tailleTampon <= tailleBIN Then
                reservation = False
                Do
                    Try
                        ReDim tabBIN(tailleTampon - 1)
                        reservation = True
                    Catch ex As Exception
                        i = i - 1
                        tailleTampon = 24 * i * 1000000
                    End Try
                Loop Until reservation
                lectureBIN.Read(tabBIN, 0, tabBIN.Length)
            Else
                tailleTampon = tailleBIN - posLecture
                ReDim tabBIN(tailleTampon - 1)
                lectureBIN.Read(tabBIN, 0, tabBIN.Length)
            End If

            pos = 0
            Do
                Array.Copy(tabBIN, pos, tabTampon, 0, 24) 'clé inversée
                keyBIN = ""
                For i = 0 To 7
                    keyBIN = keyBIN & hexa(tabTampon(i))
                Next

                If keyUCI = keyBIN Then
                    'inversed key (8 octets) | Depth |          | score       | inv. move |       | Performance |
                    'h0 h1 h2 h3 h4 h5 h6 h7 | h8    | h9 hA hB | hC hD hE hF | h0 h1     | h2 h3 | h4          | h5 h6 h7 (24 octets)
                    '00 01 02 03 04 05 06 07   08      09 10 11   12 13 14 15   16 17       18 19   20            21 22 23 tabTampon
                    '-----------------------------------------------------------------------------------------------------
                    'fb 59 2f 56 d4 01 8f 8f | 1e    | 00 00 00 | 3f 00 00 00 | 1c 03     | 00 00 | 64          | 00 00 00
                    'decimal                   30                 0000003f      031c                100%
                    '										      63/208        001 100 011 100
                    '										      +0.30         2   e   4   e
                    '                                                           e2e4

                    coup = hexa(tabTampon(17)) & hexa(tabTampon(16))
                    '031c => 001 100 011 100 => e2e4
                    coup = binToMove(hexadecimalToBinaire(coup))

                    profondeur = tabTampon(8)

                    scoreCP = hexa(tabTampon(15)) & hexa(tabTampon(14)) & hexa(tabTampon(13)) & hexa(tabTampon(12))
                    scoreMAT = scoreCP
                    'score positif
                    If tabTampon(15) = 0 And tabTampon(14) = 0 Then
                        If hexa(tabTampon(13)) = "7D" And hexa(tabTampon(12)) = "02" Then
                            scoreCP = "<empty>"
                        ElseIf hexa(tabTampon(13)) = "7C" Then
                            scoreMAT = "+" & Format((32001 - Convert.ToInt64(scoreMAT, 16)) / 2, "0")
                            scoreCP = ""
                        Else
                            scoreCP = Format(Convert.ToInt64(scoreCP, 16) / 2.08, "0")
                        End If
                    ElseIf tabTampon(15) = 255 And tabTampon(14) = 255 Then
                        If hexa(tabTampon(13)) = "83" Then
                            scoreMAT = "-" & Format((32000 - (4294967296 - Convert.ToInt64(scoreMAT, 16))) / 2, "0")
                            scoreCP = ""
                        Else
                            scoreCP = Format((Convert.ToInt64(scoreCP, 16) - 4294967295) / 2.08, "0")
                        End If
                    Else
                        MsgBox("scoreCP = " & scoreCP, MsgBoxStyle.Exclamation, "en travaux")
                    End If

                    performance = tabTampon(20)

                    tabCoups(compteur) = coup
                    tabProfondeurs(compteur) = profondeur
                    If scoreCP <> "" Then
                        tabScores(compteur) = CInt(Replace(scoreCP, "<empty>", "0"))
                    Else
                        tabScores(compteur) = CInt(scoreMAT) * 1000
                    End If
                    tabPerformances(compteur) = performance
                    '1 : e2e4 , depth: 30, eval: cp 30, quality: 100      

                    compteur = compteur + 1
                End If

                pos = pos + 24
            Loop While pos < tabBIN.Length

            posLecture = lectureBIN.Position
            tabBIN = Nothing
        End While
        lectureBIN.Close()
        compteur = compteur - 1

        'on classe les scores
        For i = 1 To compteur
            For j = 1 To compteur
                If tabScores(j) < tabScores(i) _
                Or (tabScores(j) = tabScores(i) And tabProfondeurs(j) < tabProfondeurs(i)) Then
                    coup = tabCoups(i)
                    tabCoups(i) = tabCoups(j)
                    tabCoups(j) = coup

                    profondeur = tabProfondeurs(i)
                    tabProfondeurs(i) = tabProfondeurs(j)
                    tabProfondeurs(j) = profondeur

                    pos = tabScores(i)
                    tabScores(i) = tabScores(j)
                    tabScores(j) = pos

                    performance = tabPerformances(i)
                    tabPerformances(i) = tabPerformances(j)
                    tabPerformances(j) = performance
                End If
            Next
        Next

        'on rassemble les infos
        chaineExperience = ""
        For i = 1 To compteur
            chaineExperience = chaineExperience & i & " : " & tabCoups(i) & ", depth: " & tabProfondeurs(i)
            If tabScores(i) <= -1000 Or 1000 <= tabScores(i) Then
                chaineExperience = chaineExperience & ", eval: mate " & Format(tabScores(i) / 1000, "0")
            Else
                chaineExperience = chaineExperience & ", eval: cp " & tabScores(i)
            End If

            chaineExperience = chaineExperience & ", quality: " & tabPerformances(i) & vbCrLf
        Next

        Return chaineExperience

    End Function

    Public Sub chargerMoteur(chemin As String)
        Dim chaine As String

        processus = New System.Diagnostics.Process()

        processus.StartInfo.RedirectStandardOutput = True
        processus.StartInfo.UseShellExecute = False
        processus.StartInfo.RedirectStandardInput = True
        processus.StartInfo.CreateNoWindow = True
        processus.StartInfo.WorkingDirectory = My.Application.Info.DirectoryPath
        processus.StartInfo.FileName = chemin
        processus.Start()
        processus.PriorityClass = 64 '64 (idle), 16384 (below normal), 32 (normal), 32768 (above normal), 128 (high), 256 (realtime)

        entree = processus.StandardInput
        sortie = processus.StandardOutput

        entree.WriteLine("uci")
        chaine = ""
        While InStr(chaine, "uciok") = 0
            chaine = sortie.ReadLine
            Threading.Thread.Sleep(1)
        End While

        'options communes
        entree.WriteLine("setoption name Read only learning value true")
        entree.WriteLine("setoption name Use NNUE value false")
        entree.WriteLine("setoption name EvalFile value <empty>")
        entree.WriteLine("setoption name SyzygyPath value <empty>")

        entree.WriteLine("isready")
        chaine = ""
        While InStr(chaine, "readyok") = 0
            chaine = sortie.ReadLine
            Threading.Thread.Sleep(1)
        End While
    End Sub

    Public Sub dechargerMoteur()
        entree.Close()
        sortie.Close()
        processus.Close()

        entree = Nothing
        sortie = Nothing
        processus = Nothing
    End Sub

    Public Function droite(texte As String, longueur As Integer) As String
        If longueur > 0 Then
            Return VB.Right(texte, longueur)
        Else
            Return ""
        End If
    End Function

    Public Function gauche(texte As String, longueur As Integer) As String
        If longueur > 0 Then
            Return VB.Left(texte, longueur)
        Else
            Return ""
        End If
    End Function

    Public Function heureFin(depart As Integer, i As Long, max As Long, Optional reprise As Long = 0, Optional formatCourt As Boolean = False) As String
        If formatCourt Then
            Return Format(DateAdd(DateInterval.Second, (max - i) * ((Environment.TickCount - depart) / 1000) / (i - reprise), Now), "dd/MM/yy HH:mm:ss")
        Else
            Return Format(DateAdd(DateInterval.Second, (max - i) * ((Environment.TickCount - depart) / 1000) / (i - reprise), Now), "dddd' 'd' 'MMM' @ 'HH'h'mm'm'ss")
        End If
    End Function

    Public Function hexa(valeur As Integer) As String
        Dim chaine As String

        chaine = Hex(valeur)
        If Len(chaine) = 1 Then
            chaine = "0" & chaine
        End If
        Return chaine

    End Function

    Public Function hexadecimalToBinaire(hexadecimal As String) As String
        Dim entier As Integer, chaine As String

        entier = Convert.ToInt64(hexadecimal, 16)
        chaine = ""

        Do
            chaine = Format(entier Mod 2) & chaine
            entier = Fix(entier / 2)
        Loop While (entier / 2 > 0)
        chaine = CDbl(chaine).ToString(StrDup(4 * Len(hexadecimal), "0"))
        Return chaine
    End Function

    Public Function nomFichier(chemin As String) As String
        Return My.Computer.FileSystem.GetName(chemin)
    End Function

    Public Function uciKEY(entreeKEY As System.IO.StreamWriter, sortieKEY As System.IO.StreamReader, movesUCI As String, Optional startpos As String = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") As String
        Dim key As String

        If movesUCI <> "" Then
            entreeKEY.WriteLine("position fen " & startpos & " moves " & movesUCI)
        Else
            entreeKEY.WriteLine("position fen " & startpos)
        End If

        entreeKEY.WriteLine("d")

        key = ""
        While InStr(key, "Key: ", CompareMethod.Text) = 0
            key = sortieKEY.ReadLine
        End While

        Return Replace(key, "Key: ", "")
    End Function

End Module
