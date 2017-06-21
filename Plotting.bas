 /' Plot of mass distribution (Plotting.bas) '/



If I_thread < 2 Then  ' Plotting with multiple GEF jobs does not work (program crashes).
                       ' Therefore, plotting is not allowed for I_thread > 1.
Scope
   ' set window title
  Dim WinTitle As String
  WinTitle = "GEF: Mass yields for Z="+Str(P_Z_CN)+", A="+Str(P_A_CN)
  Select Case Emode
    Case 0
        WinTitle = WinTitle+", E* over Eb = "+Str(P_E_EXC)+" MeV"
    Case 1
        WinTitle = WinTitle+", E* = "+Str(P_E_EXC)+" MeV"
    Case 2
        WinTitle = WinTItle+" (CN), En = "+Str(P_E_EXC)+" MeV"
  End Select

' create a window with given resolution and 32-bit colours
'Screenres 500,350,32
  Screenres 500,350,32
  WindowTitle WinTitle

End Scope

' set foreground and background colours
Color RGB(0,0,0), RGB(255,255,255)
' draw background
Cls

Print " "
Print "    nu-bar = ";Round(Nmean,4) '; "Bf = ";B_F;" EB = ";E_B

' write numbers on axes
If P_Ylog = 1 Then
  Draw String (30,40), "10"
  Draw String (38,118), "1"
  Draw String (22,196), "0.1"
  Draw String (14,274), "0.01"
Else
  Draw String (38,295), "0"
  Draw String (38,255), "2"
  Draw String (38,215), "4"
  Draw String (38,175), "6"
  Draw String (38,135), "8"
  Draw String (30,95), "10"
  Draw String (30,55), "12"
End If  
Draw String (81,308), "80"
Draw String (156,308), "100"
Draw String (234,308), "120"
Draw String (313,308), "140"
Draw String (391,308), "160"

Draw String (200,330), "Post-neutron mass"


' define a viewport (clipping region)
View (50,20) - (480,300), RGB(255,255,150), RGB(0,0,0)

Dim As Single Xmin, Xmax, Ymin, Ymax
Xmin = 70
Xmax = 180
If P_ylog = 1 Then
  Ymin = - 2.3
  Ymax = 1.3
Else
  Ymin = 0
  Ymax = 14
End If
' define new coordinate system on viewport region
Window (Xmin,Ymin) - (Xmax, Ymax)

' draw tic marks
Dim Ytic As Single
If P_ylog = 1 Then
  For Ytic = -5 To 5 Step 1
    Line (Xmin,Ytic) - (Xmax,Ytic)
  Next
Else
  For Ytic = 0 To 20 Step 2
    Line (Xmin,Ytic) - (Xmax,Ytic)
  Next
End If

Dim Xtic As Single
For Xtic = 80 To 170 Step 10
 Line (Xtic,Ymin) - (Xtic,Ymax)
Next

' draw a line with red colour
' Line (90,-2) - (100,0), RGB(255,0,0)


' draw mass distribution
'Logcircle (100,0.1)
Scope
Dim As Single Dplus,Dminus
Dim As Ubyte Bdraw
  For I = 20 To 190
    Bdraw = 0
    If P_Ylog = 1 Then
      If APost(I) > 10^Ymin Then Bdraw = -1
    Else
      If APost(I) > 0 Then Bdraw = -1
    End If  
   	If Bdraw Then
   	  If P_Ylog = 1 Then 
   	    Logcircle (I,APost(I),0.6)
   	  Else
   	    Lincircle (I,APost(I),0.6)
   	  End If  
   	  If d_APost(0,I) > 0 Then
         Dplus = APost(I) + d_APost(0,I)  
         Dminus = APost(I) - d_APost(0,I) 
         If Dminus < 0.1*APost(I) Then Dminus = 0.1 * APost(I)
         If P_Ylog = 1 Then
           Logline3(I,Dplus,I,Dminus)
         Else
           LinLine3(I,Dplus,I,Dminus)
         End If    
      End If      	  
   	End If  
  Next
End Scope

' draw contribution of fission channels
Scope
Dim As Single IFirst
Dim As Ubyte Bdraw
For I = 0 To 6
  IFirst = 0
  For J = 20 To 190
    Bdraw = 0
    If P_Ylog = 1 Then
      If AMPost(I,J) > 10^Ymin Then Bdraw = -1
    Else 
      If AMPOst(I,J) > 0 Then Bdraw = -1
    End If  
    If Bdraw Then
      IFirst = IFirst + 1
      If IFirst = 2 Then
        If P_Ylog = 1 Then
          Logline1(J-1,AMpost(I,J-1),J,AMpost(I,J))
        Else
          Linline1(J-1,AMpost(I,J-1),J,AMpost(I,J))
        End If  
      End If
      If IFirst > 2 Then
        If P_Ylog = 1 Then 
          Logline2(J,AMpost(I,J))
        Else
          Linline2(J,AMpost(I,J))
        End If  
      End If
    Else
      IFirst = 0
    End If
  Next
Next
End Scope

Dim As Single Chilin,Chilog
' Spontaneous fission
If EMODE = 1 And P_E_EXC = 0 Then
  If P_A_CN = 252 And P_Z_CN = 98 Then
    Restore CF252S
    Ploteval("CF252S")
    Restore CF252S
    Chisqr_Apost("CF252S",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 250 And P_Z_CN = 98 Then
    Restore CF250S
    Ploteval("CF250S")
    Restore CF250S
    Chisqr_Apost("CF250S",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 244 And P_Z_CN = 96 Then
    Restore CM244S
    Ploteval("CM244S")
    Restore CM244S
    Chisqr_Apost("CM244S",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 246 And P_Z_CN = 96 Then
    Restore CM246S
    Ploteval("CM246S")
    Restore CM246S
    Chisqr_Apost("CM246S",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 248 And P_Z_CN = 96 Then
    Restore CM248S
    Ploteval("CM248S")
    Restore CM248S
    Chisqr_Apost("CM248S",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 253 And P_Z_CN = 99 Then
    Restore ES253S
    Ploteval("ES253S")
    Restore ES253S
    Chisqr_Apost("ES253S",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 254 And P_Z_CN = 100 Then
    Restore FM254S
    Ploteval("FM254S")
    Restore FM254S
    Chisqr_Apost("FM254S",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If  
  If P_A_CN = 256 And P_Z_CN = 100 Then
    Restore FM256S
    Ploteval("FM256S")
    Restore FM256S
    Chisqr_Apost("FM256S",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If  
  If P_A_CN = 238 And P_Z_CN = 92 Then
    Restore U238S
    Ploteval("U238S")
    Restore U238S
    Chisqr_Apost("U238S",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If  
End If

' Thermal-neutron-induced fission
If EMODE = 2 And (P_E_EXC >= 0 And P_E_EXC <= 0.05) Then
  If P_A_CN = 242 And P_Z_CN = 95 Then
    Restore AM241T
    Ploteval("AM241T")
    Restore AM241T
    Chisqr_Apost("AM241T",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 243 And P_Z_CN = 95 Then
    Restore AM242T
    Ploteval("AM242T")
    Restore AM242T
    Chisqr_Apost("AM242T",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 250 And P_Z_CN = 98 Then
    Restore CF249T
    Ploteval("CF249T")
    Restore CF249T
    Chisqr_Apost("CF249T",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 252 And P_Z_CN = 98 Then
    Restore CF251T
    Ploteval("CF251T")
    Restore CF251T
    Chisqr_Apost("CF251T",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 244 And P_Z_CN = 96 Then
    Restore CM243T
    Ploteval("CM243T")
    Restore CM243T
    Chisqr_Apost("CM243T",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 246 And P_Z_CN = 96 Then
    Restore CM245T
    Ploteval("CM245T")
    Restore CM245T
    Chisqr_Apost("CM245T",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 255 And P_Z_CN = 99 Then
    Restore ES254T
    Ploteval("ES254T")
    Restore ES254T
    Chisqr_Apost("ES254T",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 256 And P_Z_CN = 100 Then
    Restore FM255T
    Ploteval("FM255T")
    Restore FM255T
    Chisqr_Apost("FM255T",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 238 And P_Z_CN = 93 Then
    Restore NP237T
    Ploteval("NP237T")
    Restore NP237T
    Chisqr_Apost("NP237T",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If  
  If P_A_CN = 240 And P_Z_CN = 94 Then
    Restore PU239T
    Ploteval("PU239T")
    Restore PU239T
    Chisqr_Apost("PU239T",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If  
  If P_A_CN = 241 And P_Z_CN = 94 Then
    Restore PU240T
    Ploteval("PU240T")
    Restore PU240T
    Chisqr_Apost("PU240T",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If  
  If P_A_CN = 242 And P_Z_CN = 94 Then
    Restore PU241T
    Ploteval("PU241T")
    Restore PU241T
    Chisqr_Apost("PU241T",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If  
  If P_A_CN = 243 And P_Z_CN = 94 Then
    Restore PU242T
    Ploteval("PU242T")
    Restore PU242T
    Chisqr_Apost("PU242T",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If  
  If P_A_CN = 228 And P_Z_CN = 90 Then
    Restore TH227T
    Ploteval("TH227T")
    Restore TH227T
    Chisqr_Apost("TH227T",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If  
  If P_A_CN = 230 And P_Z_CN = 90 Then
    Restore TH229T
    Ploteval("TH229T")
    Restore TH229T
    Chisqr_Apost("TH229T",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If  
  If P_A_CN = 233 And P_Z_CN = 92 Then
    Restore U232T
    Ploteval("U232T")
    Restore U232T
    Chisqr_Apost("U232T",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If  
  If P_A_CN = 234 And P_Z_CN = 92 Then
    Restore U233T
    Ploteval("U233T")
    Restore U233T
    Chisqr_Apost("U233T",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If  
  If P_A_CN = 236 And P_Z_CN = 92 Then
    Restore U235T
    Ploteval("U235T")
    Restore U235T
    Chisqr_Apost("U235T",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
End If

' Fast-neutron-induced fission
If EMODE = 2 And (P_E_EXC >= 0.4 And P_E_Exc <= 2) Then
  If P_A_CN = 242 And P_Z_CN = 95 Then
    Restore AM241F
    Ploteval("AM241F")
    Restore AM241F
    Chisqr_Apost("AM241F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 244 And P_Z_CN = 95 Then
    Restore AM243F
    Ploteval("AM243F")
    Restore AM243F
    Chisqr_Apost("AM243F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 243 And P_Z_CN = 96 Then
    Restore CM242F
    Ploteval("CM242F")
    Restore CM242F
    Chisqr_Apost("CM242F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 244 And P_Z_CN = 96 Then
    Restore CM243F
    Ploteval("CM243F")
    Restore CM243F
    Chisqr_Apost("CM243F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 245 And P_Z_CN = 96 Then
    Restore CM244F
    Ploteval("CM244F")
    Restore CM244F
    Chisqr_Apost("CM244F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 247 And P_Z_CN = 96 Then
    Restore CM246F
    Ploteval("CM246F")
    Restore CM246F
    Chisqr_Apost("CM246F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 249 And P_Z_CN = 96 Then
    Restore CM248F
    Ploteval("CM248F")
    Restore CM248F
    Chisqr_Apost("CM248F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If  
  If P_A_CN = 238 And P_Z_CN = 93 Then
    Restore NP237F
    Ploteval("NP237F")
    Restore NP237F
    Chisqr_Apost("NP237F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 239 And P_Z_CN = 93 Then
    Restore NP238F
    Ploteval("NP238F")
    Restore NP238F
    Chisqr_Apost("NP238F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If  
  If P_A_CN = 232 And P_Z_CN = 91 Then
    Restore PA231F
    Ploteval("PA231F")
    Restore PA231F
    Chisqr_Apost("PA231F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 239 And P_Z_CN = 94 Then
    Restore PU238F
    Ploteval("PU238F")
    Restore PU238F
    Chisqr_Apost("PU238F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 240 And P_Z_CN = 94 Then
    Restore PU239F
    Ploteval("PU239F")
    Restore PU239F
    Chisqr_Apost("PU239F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If       
  If P_A_CN = 241 And P_Z_CN = 94 Then
    Restore PU240F
    Ploteval("PU240F")
    Restore PU240F
    Chisqr_Apost("PU240F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If        
  If P_A_CN = 242 And P_Z_CN = 94 Then
    Restore PU241F
    Ploteval("PU241F")
    Restore PU241F
    Chisqr_Apost("PU241F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If        
  If P_A_CN = 243 And P_Z_CN = 94 Then
    Restore PU242F
    Ploteval("PU242F")
    Restore PU242F
    Chisqr_Apost("PU242F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If    
  If P_A_CN = 233 And P_Z_CN = 90 Then
    Restore TH232F
    Ploteval("TH232F")
    Restore TH232F
    Chisqr_Apost("TH232F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If      
  If P_A_CN = 234 And P_Z_CN = 92 Then
    Restore U233F
    Ploteval("U233F")
    Restore U233F
    Chisqr_Apost("U233F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If        
  If P_A_CN = 235 And P_Z_CN = 92 Then
    Restore U234F
    Ploteval("U234F")
    Restore U234F
    Chisqr_Apost("U234F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If        
  If P_A_CN = 236 And P_Z_CN = 92 Then
    Restore U235F
    Ploteval("U235F")
    Restore U235F
    Chisqr_Apost("U235F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If       
  If P_A_CN = 237 And P_Z_CN = 92 Then
    Restore U236F
    Ploteval("U236F")
    Restore U236F
    Chisqr_Apost("U236F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If      
  If P_A_CN = 238 And P_Z_CN = 92 Then
    Restore U237F
    Ploteval("U237F")
    Restore U237F
    Chisqr_Apost("U237F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If     
  If P_A_CN = 239 And P_Z_CN = 92 Then
    Restore U238F
    Ploteval("U238F")
    Restore U238F
    Chisqr_Apost("U238F",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If      
End If

' HE (14 MeV) -neutron-induced fission
If EMODE = 2 And (P_E_EXC >= 13.5 And P_E_Exc <= 14.5) Then
  If P_A_CN = 233 And P_Z_CN = 90 Then
    Restore TH232HE
    Ploteval("TH232HE")
    Restore TH232HE
    Chisqr_Apost("TH232HE",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 234 And P_Z_CN = 92 Then
    Restore U233HE
    Ploteval("U233HE")
    Restore U233HE
    Chisqr_Apost("U233HE",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 235 And P_Z_CN = 92 Then
    Restore U234HE
    Ploteval("U234He")
    Restore U234HE
    Chisqr_Apost("U234HE",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 236 And P_Z_CN = 92 Then
    Restore U235HE
    Ploteval("U235HE")
    Restore U235HE
    Chisqr_Apost("U235HE",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If 
  If P_A_CN = 237 And P_Z_CN = 92 Then
    Restore U236HE
    Ploteval("U236HE")
    Restore U236HE
    Chisqr_Apost("U236HE",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 239 And P_Z_CN = 92 Then
    Restore U238HE
    Ploteval("U238HE")
    Restore U238HE
    Chisqr_Apost("U238HE",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 238 And P_Z_CN = 93 Then
    Restore NP237HE
    Ploteval("NP237HE")
    Restore NP237HE
    Chisqr_Apost("NP237HE",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 240 And P_Z_CN = 94 Then
    Restore PU239HE
    Ploteval("PU239HE")
    Restore PU239HE
    Chisqr_Apost("PU239HE",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 241 And P_Z_CN = 94 Then
    Restore PU240HE
    Ploteval("PU240HE")
    Restore PU240HE
    Chisqr_Apost("PU240HE",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 243 And P_Z_CN = 94 Then
    Restore PU242HE
    Ploteval("PU242HE")
    Restore PU242HE
    Chisqr_Apost("PU242HE",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
  If P_A_CN = 242 And P_Z_CN = 95 Then
    Restore AM241HE
    Ploteval("AM241HE")
    Restore AM241HE
    Chisqr_Apost("AM241HE",Chilin,Chilog)
    PrintChisqr(Chilin)
  End If
End If

Locate 45

End If 'If I_thread < 2

