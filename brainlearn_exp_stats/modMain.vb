Module modMain

    Sub Main()
        Dim fichierINI As String, fichierECO As String, fichierBIN As String, moteurBIN As String
        Dim lectureBIN As IO.FileStream, pos As Long, posLecture As Long, tailleBIN As Long, tailleTampon As Long, reservation As Boolean
        Dim tabChaine() As String, tabTmp() As String
        Dim tabProf(256) As Integer, prof As Integer, totProf As Long, nbCoups As Integer, minProf As Integer, maxProf As Integer
        Dim tabECO(0) As String, i As Integer, chaine As String, compteur As Integer
        Dim depart As Integer, moteur_court As String, keyBIN As String, keyUCI As String
        Dim tabBIN(0) As Byte

        If My.Computer.FileSystem.GetFileInfo(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile) & "\Documents\Visual Studio 2013\Projects\brainlearn_exp_stats\brainlearn_exp_stats\bin\Debug\brainlearn_exp_stats.exe").LastWriteTime > My.Computer.FileSystem.GetFileInfo(My.Application.Info.AssemblyName & ".exe").LastWriteTime Then
            MsgBox("Il existe une version plus récente de ce programme !", MsgBoxStyle.Information)
            End
        End If

        fichierBIN = Replace(Command(), """", "")
        If fichierBIN = "" Or Not My.Computer.FileSystem.FileExists(fichierBIN) Then
            End
        End If

        fichierINI = My.Computer.Name & ".ini"
        fichierECO = "ouvertures.txt"
        moteurBIN = "E:\JEUX\ARENA CHESS 3.5.1\Engines\BrainLearn\20T BrainLearn 13.1 x64 BMI2.exe"
        If My.Computer.Name = "PLEXI" Then
            moteurBIN = "D:\JEUX\ARENA CHESS 3.5.1\Engines\BrainLearn\20T BrainLearn 13.1 x64 PCNT.exe"
        End If
        If My.Computer.FileSystem.FileExists(fichierINI) Then
            chaine = My.Computer.FileSystem.ReadAllText(fichierINI)
            If chaine <> "" And InStr(chaine, vbCrLf) > 0 Then
                tabChaine = Split(chaine, vbCrLf)
                For i = 0 To UBound(tabChaine)
                    If tabChaine(i) <> "" And InStr(tabChaine(i), " = ") > 0 Then
                        tabTmp = Split(tabChaine(i), " = ")
                        If tabTmp(0) <> "" And tabTmp(1) <> "" Then
                            If InStr(tabTmp(1), "//") > 0 Then
                                tabTmp(1) = Trim(gauche(tabTmp(1), tabTmp(1).IndexOf("//") - 1))
                            End If
                            Select Case tabTmp(0)
                                Case "moteurBIN"
                                    moteurBIN = tabTmp(1)

                                Case "fichierECO"
                                    fichierECO = tabTmp(1)

                                Case Else

                            End Select
                        End If
                    End If
                Next
            End If
        End If
        My.Computer.FileSystem.WriteAllText(fichierINI, "moteurBIN = " & moteurBIN & vbCrLf _
                                                      & "fichierECO = " & fichierECO & vbCrLf, False)
        moteur_court = nomFichier(moteurBIN)

        'ETAPE 1/2 : PROFONDEURS
        posLecture = 0
        tailleBIN = FileLen(fichierBIN)
        tailleTampon = tailleBIN
        i = 50
        lectureBIN = New IO.FileStream(fichierBIN, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.ReadWrite)

        totProf = 0
        nbCoups = 0
        minProf = 1000
        maxProf = 0
        depart = Environment.TickCount

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

            For pos = 8 To UBound(tabBIN) Step 24
                prof = tabBIN(pos)

                totProf = totProf + prof
                nbCoups = nbCoups + 1

                If prof < minProf Then
                    minProf = prof
                ElseIf prof > maxProf Then
                    maxProf = prof
                End If

                Select Case prof
                    Case Is >= 100
                        tabProf(100) = tabProf(100) + 1

                    Case Is >= 90
                        tabProf(90) = tabProf(90) + 1

                    Case Is >= 80
                        tabProf(80) = tabProf(80) + 1

                    Case Is >= 70
                        tabProf(70) = tabProf(70) + 1

                    Case Is >= 60
                        tabProf(60) = tabProf(60) + 1

                    Case Is >= 50
                        tabProf(50) = tabProf(50) + 1

                    Case Is >= 40
                        tabProf(40) = tabProf(40) + 1

                    Case Is >= 30
                        tabProf(30) = tabProf(30) + 1

                    Case Is >= 20
                        tabProf(20) = tabProf(20) + 1

                    Case Is >= 10
                        tabProf(10) = tabProf(10) + 1

                    Case Else
                        tabProf(0) = tabProf(0) + 1
                End Select

                If nbCoups Mod 10000000 = 0 Then '10 millions
                    Console.Clear()
                    Console.Title = My.Computer.Name & " : " & nomFichier(fichierBIN) & " @ " & Format((posLecture + pos) / tailleBIN, "0.00%") & " (" & heureFin(depart, posLecture + pos, tailleBIN, , True) & ")"

                    Console.WriteLine("Moves : " & Trim(Format(nbCoups, "# ### ### ##0")) & vbCrLf)

                    Console.WriteLine("Stats : min D" & minProf & " < avg D" & Format(totProf / nbCoups, "0") & " < max D" & maxProf & vbCrLf)

                    Console.WriteLine("Depth :  < D10 | >= D10 | >= D20 | >= D30 | >= D40 | >= D50 | >= D60 | >= D70 | >= D80 | >= D90 | >=D100")
                    Console.WriteLine("..... : " & Format(tabProf(0) / nbCoups, "00.00%") & " | " & Format(tabProf(10) / nbCoups, "00.00%") & " | " & Format(tabProf(20) / nbCoups, "00.00%") & " | " & Format(tabProf(30) / nbCoups, "00.00%") & " | " & Format(tabProf(40) / nbCoups, "00.00%") & " | " & Format(tabProf(50) / nbCoups, "00.00%") & " | " & Format(tabProf(60) / nbCoups, "00.00%") & " | " & Format(tabProf(70) / nbCoups, "00.00%") & " | " & Format(tabProf(8) / nbCoups, "00.00%") & " | " & Format(tabProf(90) / nbCoups, "00.00%") & " | " & Format(tabProf(100) / nbCoups, "00.00%") & vbCrLf)
                End If
            Next
            posLecture = lectureBIN.Position

            tabBIN = Nothing
        End While
        Console.Clear()
        Console.Title = My.Computer.Name & " : " & nomFichier(fichierBIN) & " @ " & Format(posLecture / taillebin, "0.00%")

        Console.WriteLine("Moves : " & Trim(Format(nbCoups, "# ### ### ##0")) & vbCrLf)

        Console.WriteLine("Stats : min D" & minProf & " < avg D" & Format(totProf / nbCoups, "0") & " < max D" & maxProf & vbCrLf)

        Console.WriteLine("Depth :  < D10 | >= D10 | >= D20 | >= D30 | >= D40 | >= D50 | >= D60 | >= D70 | >= D80 | >= D90 | >=D100")
        Console.WriteLine("..... : " & Format(tabProf(0) / nbCoups, "00.00%") & " | " & Format(tabProf(10) / nbCoups, "00.00%") & " | " & Format(tabProf(20) / nbCoups, "00.00%") & " | " & Format(tabProf(30) / nbCoups, "00.00%") & " | " & Format(tabProf(40) / nbCoups, "00.00%") & " | " & Format(tabProf(50) / nbCoups, "00.00%") & " | " & Format(tabProf(60) / nbCoups, "00.00%") & " | " & Format(tabProf(70) / nbCoups, "00.00%") & " | " & Format(tabProf(8) / nbCoups, "00.00%") & " | " & Format(tabProf(90) / nbCoups, "00.00%") & " | " & Format(tabProf(100) / nbCoups, "00.00%") & vbCrLf)

        lectureBIN.Close()

        'ETAPE 2/2 : OUVERTURES

        If My.Computer.FileSystem.FileExists(fichierECO) Then
            Console.Write("Loading " & moteur_court & "... ")
            chargerMoteur(moteurBIN)
            Console.WriteLine("OK" & vbCrLf)

            chaine = My.Computer.FileSystem.ReadAllText(fichierECO)
            If chaine <> "" And InStr(chaine, vbCrLf) > 0 Then
                tabECO = Split(chaine, vbCrLf)
            End If

            For i = 0 To UBound(tabECO)
                If tabECO(i) <> "" Then
                    tabTmp = Split(tabECO(i), ":")
                    compteur = 0
                    While tabTmp(1) <> ""
                        If InStr(tabTmp(1), "|") = 0 Then
                            keyUCI = uciKEY(entree, sortie, Trim(tabTmp(1)))
                        Else
                            keyUCI = uciKEY(entree, sortie, Trim(tabTmp(1).Substring(0, tabTmp(1).IndexOf("|"))))
                        End If

                        keyBIN = ""
                        For j = 0 To 15 Step 2
                            chaine = keyUCI.Substring(j, 2)
                            keyBIN = chaine & keyBIN
                        Next
                        keyUCI = keyBIN
                        keyBIN = ""

                        chaine = brainlearn_expListe(fichierBIN, keyUCI)

                        If chaine <> "" Then
                            tabChaine = Split(chaine, vbCrLf)
                            For j = 0 To UBound(tabChaine)
                                If tabChaine(j) <> "" Then
                                    compteur = compteur + 1
                                End If
                            Next
                        End If
                        If InStr(tabTmp(1), "|") = 0 Then
                            tabTmp(1) = ""
                        Else
                            tabTmp(1) = tabTmp(1).Substring(tabTmp(1).IndexOf("|") + 1)
                        End If
                    End While
                    If compteur = 0 Then
                        Console.WriteLine(tabTmp(0) & " : never played")
                    Else
                        Console.WriteLine(tabTmp(0) & " : " & Format(compteur, "0 000") & " times played")
                    End If
                End If
            Next
            Console.WriteLine("")

            dechargerMoteur()
        End If


        Console.WriteLine("Press ENTER to close the window.")
        Console.ReadLine()

    End Sub

End Module
