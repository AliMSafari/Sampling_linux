    Dim As Single Var_P_att_pol = _P_att_pol * 0.2
    Dim As Single Var_Jscaling = _Jscaling * 0.1

/'<'/    
 /' Internal parameters for error analysis: '/
    Dim Shared As Single P_DZ_Mean_S1
    Dim Shared As Single P_DZ_Mean_S2
    Dim Shared As Single P_DZ_Mean_S3
    Dim Shared As Single P_DZ_Mean_S4
    Dim Shared As Single P_Z_Curv_S1
    Dim Shared As Single P_Z_Curv_S2
    Dim Shared As Single P_A_Width_S2
    Dim Shared As Single P_Z_Curv_S3
    Dim Shared As Single P_Z_Curv_S4
    Dim Shared As Single Delta_S0
    Dim Shared As Single P_Shell_S1
    Dim Shared As Single P_Shell_S2
    Dim Shared As Single P_Shell_S3
    Dim Shared As Single P_Shell_S4
    Dim Shared As Single T_low_S1
    Dim Shared As Single T_low_S2
    Dim Shared As Single T_low_S3
    Dim Shared As Single T_low_S4
    Dim Shared As Single T_low_SL
    Dim Shared As Single P_att_pol
    Dim Shared As Single HOMPOL
    Dim Shared As Single POLARadd  
    Dim Shared As Single Jscaling
    
/'>'/    
 /' Memory for perturbed parameter values '/   
    ReDim Shared As Single Double_P_DZ_Mean_S1(1)
    ReDim Shared As Single Double_P_DZ_Mean_S2(1)
    ReDim Shared As Single Double_P_DZ_Mean_S3(1)
    ReDim Shared As Single Double_P_DZ_Mean_S4(1)
    ReDim Shared As Single Double_P_Z_Curv_S1(1)
    ReDim Shared As Single Double_P_Z_Curv_S2(1)
    ReDim Shared As Single Double_P_A_Width_S2(1)
    ReDim Shared As Single Double_P_Z_Curv_S3(1)
    ReDim Shared As Single Double_P_Z_Curv_S4(1)
    ReDim Shared As Single Double_Delta_S0(1)
    ReDim Shared As Single Double_P_Shell_S1(1)
    ReDim Shared As Single Double_P_Shell_S2(1)
    ReDim Shared As Single Double_P_Shell_S3(1)
    ReDim Shared As Single Double_P_Shell_S4(1)
    ReDim Shared As Single Double_T_low_S1(1)
    ReDim Shared As Single Double_T_low_S2(1)
    ReDim Shared As Single Double_T_low_S3(1)
    ReDim Shared As Single Double_T_low_S4(1)
    ReDim Shared As Single Double_T_low_SL(1)
    ReDim Shared As Single Double_P_att_pol(1)
    ReDim Shared As Single Double_HOMPOL(1)
    ReDim Shared As Single Double_POLARadd(1)  
    Redim Shared As Single Double_Jscaling(1)
/'<'/

     
 /' Control parameters: '/
    Dim Shared As Single B_F = 0              /' Fission barrier '/
    Dim Shared As Single B_F_ld = 0           /' Fission barrier, liquid drop '/
    Dim Shared As Single E_B = 0              /' Outer fission barrier '/
    Dim Shared As Single E_B_ld = 0           /' Outer fission barrier, liquid drop '/
    Dim Shared As Single R_E_exc_Eb = 0       /' Energy above outer barrier '/
    Dim Shared As Single R_E_exc_GS = 0       /' Energy above ground state '/
    Dim Shared As Single P_Z_Mean_S0 = 0      /' Mean Z of Mode 1 '/
    Dim Shared As Single P_Z_Mean_S1 = 52.8   /' Mean Z of Mode 1 '/
    Dim Shared As Single P_Z_Mean_S2 = 55     /' Mean Z of Mode 2 '/
    Dim Shared As Single P_Z_Mean_S3 = 65     /' Mean Z of Mode 3 '/
    Dim Shared As Single P_Z_Mean_S4 = 42.05  /' Mean Z of Mode 4 '/
    Dim Shared As Single NC_Mode_0 = 0        /' Mean N of symm. Mode '/
    Dim Shared As Single NC_Mode_1 = 0        /' Mean N of Mode 1 '/
    Dim Shared As Single NC_Mode_2 = 0        /' Mean N of Mode 2 '/
    Dim Shared As Single NC_Mode_3 = 0        /' Mean N of Mode 3 '/
    Dim Shared As Single NC_Mode_4 = 0
    Dim Shared As Single B_S1 = 0             /' Barrier S1, relative to SL '/
    Dim Shared As Single B_S2 = 0             /' Barrier S2, relative to SL '/
    Dim Shared As Single B_S3 = 0             /' Barrier S3, relative to SL '/
    Dim Shared As Single B_S4 = 0
    Dim Shared As Single B_S11 = 0            /' Barrier S11, relative to SL '/
    Dim Shared As Single B_S22 = 0            /' Barrier S22, relative to SL '/
    Dim Shared As Single DES11ZPM = 0         /' Mod. of eff. barrier due to ZPM in overlap '/
    Dim Shared As Single Delta_NZ_Pol = 0      /' Polarization for 132Sn '/
    Dim Shared As Single Yield_Mode_0 = 0     /' Relative yield of SL '/
    Dim Shared As Single Yield_Mode_1 = 0     /' Relative yield of S1 '/
    Dim Shared As Single Yield_Mode_2 = 0     /' Relative yield of S2 '/
    Dim Shared As Single Yield_Mode_3 = 0     /' Relative yield of S3 '/
    Dim Shared As Single Yield_Mode_4 = 0     /' Relative yield of S4 '/
    Dim Shared As Single Yield_Mode_11 = 0    /' Relative yield of S11 '/
    Dim Shared As Single Yield_Mode_22 = 0    /' Relative yield of S22 '/
    Dim Shared As Single P_POL_CURV_S0 = 0    /' Stiffnes in N/Z '/
    Dim Shared As Single T_Coll_Mode_0 = 0    /' Effective collective temperature '/
    Dim Shared As Single E_Exc_S0 = 0         /' Energy over barrier of symmetric channel '/
    Dim Shared As Single E_Exc_S1 = 0         /' Energy over barrier of S1 channel '/
    Dim Shared As Single E_Exc_S2 = 0         /' Energy over barrier of S2 channel '/
    Dim Shared As Single E_Exc_S3 = 0         /' Energy over barrier of S3 channel '/
    Dim Shared As Single E_Exc_S4 = 0         /' Energy over barrier of S4 channel '/
    Dim Shared As Single E_Exc_S11 = 0        /' Energy over barrier of S11 channel '/
    Dim Shared As Single E_Exc_S22 = 0        /' Energy over barrier of S22 channel '/
    Dim Shared As Single E_POT_SCISSION = 0   /' Potential-energy gain saddle-scission '/
    Dim Shared As Single EINTR_SCISSION = 0   /' Intrinsic excitation energy at scission '/
    Dim Shared As Single EeffS2 = 0           /' Governs S1 reduction by pairing '/
    Dim Shared As Single Sigpol_Mode_0 = 0    /' Width of isobaric Z distribution '/
/'>'/

   Randomize,3   ' Random initialisation of the random generator


                               
/'<'/

  #Include Once "BEldmTF.bas"
  
  #Include Once "BEexp.bas"
  
  #Include Once "DEFO.bas"

  #Include Once "ShellMO.bas"
  


/'>'/
  Static Shared CElement(1 To 120)   As String   ' Chemical element names
  #Include Once "ElmtNames.bas"


   #Include Once "NucProp.bas"  ' uses MAT numbers from JEFF3 decay file
'  #Include Once "NucPropf.bas" ' JEFF3 decay file extended by shape isomers
                                ' only suited for spont. fission of shape isomers
'  #Include Once "NucPropmf.bas" ' contains all isomers of fissioning nuclei
'  #Include Once "NucPropx.bas"  ' contains extensions for all nuclei

/' *********************** Only for GEFSUB *****************************
/'<'/

Declare Sub GEFSUB(P_Z_CN As Integer, P_A_CN As Integer, P_E_EXC As Single, _
   P_J_CN As Single)
   
GEFSUB(92,236,6.,0.0)

/'
Dim As Single Zsum

Print
Print "Z, A, Yield"
For J = 10 To 140
  Zsum = 0
  For I = 10 To 190
    Zsum = Zsum + NZpre(I,J)
    If NZPRE(I,J) > 0.0 Then
      Print J,I+J,NZPRE(I,J)*200
    End If
  Next
Next 


Print
Print "Z yields"
For J = 10 To 140
  Zsum = 0
  For I = 10 To 190
    Zsum = Zsum + NZpre(I,J)
  Next
  If Zsum > 0.0 Then
    Print J, Zsum * 200
  End If   
Next '/ 


/'
Dim As Single Asum
Print
Print "N yields"
For I = 10 To 140
  Asum = 0
  For J = 10 To 190
    Asum = Asum + NZpre(I,J)
 '   If NZPRE(I,J) > 0.001 Then
 '     Print J,I+J,NZPRE(I,J)*200
 '   End If
  Next
  Print I, Asum * 200
Next   '/

End
  

 Sub GEFSUB(P_Z_CN As Integer, P_A_CN As Integer, P_E_EXC As Single, _
   P_J_CN As Single)
   /' Input parameters: '/
   /' Atomic number, mass number, excitation energy/MeV, spin/h_bar of CN '/
   /' Results are stored in external arrays. '/
   
/'<FO INCLUDE "GEFSUBdcl2.FOR" FO>'/
/'>'/   

  Bsub = 1
  
 End Sub 
' *********************** End Only for GEFSUB **************************                         
'/

  /'  EXCMD('AFETCH BElmdTF');  '/
  /'  EXCMD('AFETCH ShellMO');  '/


  /'  EXCMD('AFETCH EVOD');  '/
  
 
  
Prompting:  
  /' User prompting: Input values '/
  
   /'BGUI controls the function of the GEF GUI '/
   Dim As Integer iblock
   Dim As Integer fMutex,fTransfer
   Dim As String Cname
   Dim As String Cvalue
   Static As String Cfileout,Cfileout_single,Cfileout_full
   Static As String Cfileoutlmd,Cfileoutlmd_full
   Static As String Cfilemvd_single,Cfilemvd
   Dim As String CEref
   Dim As LongInt Fenhance = 0
   Const As LongInt Ten_to_five = 1.E5
   Dim As Byte BGUI  'true: -1, false: 0
   Static As String Cerr
   Static As String Clocal
   Dim As String Crec
   Dim As Byte Brec = 0
   Dim As Byte Bcov = 0
   Dim As Byte Bcor = 0
   Dim As String Cplot
   Dim As Byte Bplot = -1
   
   ' List-mode parameter choice
   Dim As Byte PZ1, PZ2, PA1pre, PA2pre, PA1post, PA2post
   Dim As Byte PI1,PI2,Pn1,Pn2,PTKEpre,PTKEpost,PnCN,PEnpost
   Dim As Byte PEgpost
   Dim As Byte PXE
   Dim As Byte Pndel  ' delayed neutrons
   
   Static As Byte Bfilein = 0   ' Bfilein is set if file.in is found
   Dim As Integer I_Warning = 0
   Static As Integer Iline
   Static As Integer Iline_tot  ' number of lines in input file 
   Static As Integer N_E_steps = 0   ' number of energy steps for input file
   Static As Integer I_E_step 
   Static As Integer I_first_E_step = 1
   Dim As Single Eabsgs     ' Energy above ground state
   
/'<'/   
   Static As Integer I_E_iso  ' numbered in sequence of increasing energy
   Static As Single Spin_CN  
   Dim As Single Spin_pre_fission
   Dim As Single Spin_gs_light
   Dim As Single Spin_gs_heavy

/'>'/
  
   Dim As Integer Ivalid   
   
   Static As Integer Ffilein
   Static As Longint Fenhance_global
   Dim As Single NEVTspectrum
   Dim As Integer Fthread
   Static As Integer I_thread=0  ' Thread number of this process
   Static Shared As Integer I_thread_max = 8 '6   ' Value must be adapted to maximun of allowed processes on the system.
   Static As Byte B_thread 
   Static As String CFthread 
   CFthread = "thread.ctl"   ' (fileexists does not work with subfolders.)
   Static As String CFdone
   CFdone = "done.ctl"       ' (fileexists does not work with subfolders.)
   Dim Shared As Integer foutlmd
   Dim Shared As Integer f 
   Dim Shared As Integer fsync, Isync

   Type OCovarEvt1d
     IParameter As Integer     ' # of parameter set
     ICoordinate As Integer    ' Index of cov. matrix
     RYield As Single          ' Yield value
   End Type

   Type OCovarEvt2d
     IParameter As Integer     ' # of parameter set
     ICoordinate1 As Integer   ' Parameter 1st dimension
     ICoordinate2 As Integer   ' Parameter 2st dimension
     RYield As Single          ' Yield value
   End Type
   
   Type OCovar
     Rval As Single      ' Covariance value
     Nval As Integer     ' Number of original values for one matrix element
     Declare Constructor
   End Type    
   
   Constructor OCovar
     This.Rval = 0
     This.Nval = 0
   End Constructor
   
   Type OCorr
     Rval As Single
     Declare Constructor
   End Type
     
   Constructor OCorr
     This.Rval = 0
   End Constructor  

   
   If Fileexists("file.in") Then
     Bfilein = -1
     
     ' Assign thread number to this process
     CHDIR("ctl")
     If Fileexists(CFthread) Then 
       Fthread = Freefile
       Open CFthread For Input As #Fthread
       Input #Fthread, I_thread
       Close #Fthread
     End If  
     I_thread = I_thread + 1
     Fthread = Freefile
     Open CFthread for Output As #Fthread
     Print #Fthread, I_thread
     Close #Fthread

     If I_thread > I_thread_max Then
       Print "The maximum number of "+Str(I_thread_max)+" processes was reached."
       Print "Please consider the following hints:"
       Print "GEF supports parallel computing for a series of systems given in the"
       Print "file 'file.in'."
       Print "GEF keeps track of the number of GEF calculations that run in parallel and"
       Print "compares it with the maximum number of processes than can run in parallel"
       Print "on your copmuter." 
       Print "For a proper functioning of this feature, two conditinos must be met."
       Print "1. The tracking information of active processes in GEF must be correct."
       Print "   For this purpose, please delete /ctl/tread.ctl (and also /ctl/done.ctl,"
       Print "   where the systems are listed that have already been calculated, and"
       Print "   /ctl/sync.ctl that synchronizes the output of GEF)"
       Print "   before starting a new series of calculations."
       Print "2. The maximum number of processes that can run on your computer must be"
       Print "   correct. This number is stored in the variable I_thread_max in GEF.bas."
       Print "   Please check and eventually correct the value of I_thread_max."    
       Print "   For example, 8 processes can run in parallel on an Intel i7 processor."
       Print "   After changing the value of I_thread_max in GEF.bas,"
       Print "   recompilation of GEF.bas is necessary."
       End 
     End If
     CHDIR("..")       
   Else
     CHDIR("ctl")
     If Fileexists(CFthread) Then
       I_thread = 0
       Fthread = Freefile
       Open CFthread For Output As #Fthread
       Print #Fthread, I_thread
       Close #Fthread  
     End If  
     CHDIR("..")       
   End If

   Ivalid = 1
 RepeatInput:   
   P_I_rms_CN = 0 
   /'************************  Input by GUI  *************************'/

   If Bfilein = 0 Then
     #ifdef __FB_WIN32__
       BGUI = -1   
       Do 
         WaitSynch_GUI
         fMutex = FreeFile
         Open "GUI\Mutex.ctl" For Input as #fMutex
         Input #fMutex, iblock
         Close #fMutex
       Loop until iblock <> 0
       Screen 0
       If iblock = -1 Then 
         End  /' stop GEF '/
       End If
       fTransfer = FreeFile
       Open "GUI\Transfer.ctl" For Input as #fTransfer
       CFileoutlmd = ""
       Clocal = "localoff"
       P_Z_CN_Double = 0
       P_A_CN_Double = 0
       P_E_exc_Double = 0
       Do
         Input #fTransfer, Cname,Cvalue
         If Cname = "P_Z_CN" Then P_Z_CN = Cast(Single,Cvalue)
         If Cname = "P_Z_CN_Double" Then P_Z_CN_Double = Cast(Single,Cvalue)
         If Cname = "P_A_CN" Then P_A_CN = Cast(Single,Cvalue)
         If Cname = "P_A_CN_Double" Then P_A_CN_Double = Cast(Single,Cvalue)
         If P_Z_CN_Double > 0 Or P_A_CN_Double > 0 Or P_E_Exc_Double <> 0 Then B_Double_Covar = 1
         If Cname = "P_I_rms" Then P_I_rms_CN = Cast(Single,Cvalue)
         If Cname = "Isomer" Then I_E_Iso = Cast(Integer,Cvalue)
         If Cname = "P_E_exc" Then P_E_exc = Cast(Single,Cvalue)
         If Cname = "P_E_exc_Double" Then P_E_Exc_Double = Cast(Single,Cvalue) 
         If Cname = "Emode" Then 
           CEref = Cvalue
           Select Case Left(CEref,2)
             Case "GS"   ' CN fission, energy above ground state
               Emode = 1
             Case "FC"   ' CN fission, first chance only
               Emode = -1
             Case "IS"   ' spontaneous fission from isomer
               Emode = 1
               I_E_iso = Val(Mid(CEref,3))
             Case "EN"   ' neutron-induced fission
               Emode = 2
               If Val(Mid(CEref,3)) > 0 Then
                 I_E_iso = Val(Mid(CEref,3))
               End If
             Case "EP"   ' proton-induced fission
               Emode = 12
               If Val(Mid(CEref,3)) > 0 Then
                 I_E_iso = Val(Mid(CEref,3))
               End If               
             Case "ES"  ' energy spectrum from input file (only FC)               
               Emode = 3 
             Case Else
               Print "Option ";CEref;" is not valid."       
               End
           End Select           
         End If  
         If Cname = "Delta_S0" Then _Delta_S0 = Cast(Single,Cvalue)
         If Cname = "Clocal" Then Clocal = Cvalue
         If Cname = "Fout" Then 
           CFileout_full = Cvalue
           If CFileout_full <> "" Then
             If Instr(CFileout_full,".") = 0 Then
               CFileout_full = CFileout_full+".dat"
             End If
             CFileout = Mid(CFileout_full,Instrrev(CFileout_full,"\")+1)
             CFileout = Mid(CFileout,1,Instr(CFileout,".")-1)
             CFileout_Single = CFileout+"_Single"
           End If
         End If  
         If Cname = "Foutlmd" Then 
           CFileoutlmd_full = Cvalue
           If CFileoutlmd_full <> "" Then
             If Instr(CFileoutlmd_full,".") = 0 Then
               CFileoutlmd_full = CFileoutlmd_full+".lmd"
             End If
             CFileoutlmd = Mid(CFileoutlmd_full,Instr(CFileoutlmd_full,"\")+1)
             CFileoutlmd = Mid(CFileoutlmd,1,Instr(CFileoutlmd,".")-1)
           End If  
         End If
         If Cname = "Fenhance" Then Fenhance = Cast(Single,Cvalue)
         If Cname = "Cerr" Then 
           Cerr = Cvalue
           If Cerr = "eron" Then 
               B_Error_On = 1
           Else 
               B_Error_On = 0
           End If
         End If
         If Cname = "Ccov" Then 
           If Cvalue = "covon" Then 
               BCov = 1
           Else 
               BCov = 0
           End If
         End If
         If Cname = "Ccor" Then 
           If Cvalue = "coron" Then 
               BCor = 1
           Else 
               BCor = 0
           End If
         End If
         If Cname = "Crec" Then   ' record perturbed results
           Crec = Cvalue
           If Crec = "recon" Then 
             Brec = 1
             #ifdef B_ENDF 
               B_Random_On = 1
             #endif  
           Else 
             Brec = 0
             B_Random_On = 0
           End If    
         End If
         If Cname = "Cplot" Then
           Cplot = Cvalue
           if Cplot = "ploton" Then
             Bplot = -1
           Else
             Bplot = 0
           End If    
         End If         
         If Cname = "PZ1" Then PZ1 = Cast(Integer,Cvalue)
         If Cname = "PZ2" Then PZ2 = Cast(Integer,Cvalue)
         If Cname = "PA1pre" Then PA1pre = Cast(Integer,Cvalue)
         If Cname = "PA2pre" Then PA2pre = Cast(Integer,Cvalue)
         If Cname = "PA1post" Then PA1post = Cast(Integer,Cvalue)
         If Cname = "PA2post" Then PA2post = Cast(Integer,Cvalue)
         If Cname = "PI1" Then PI1 = Cast(Integer,Cvalue)
         If Cname = "PI2" Then PI2 = Cast(Integer,Cvalue)
         If Cname = "PXE" Then PXE = Cast(Integer,Cvalue)
         If Cname = "Pn1" Then Pn1 = Cast(Integer,Cvalue)
         If Cname = "Pn2" Then Pn2 = Cast(Integer,Cvalue)
         If Cname = "PTKEpre" Then PTKEpre = Cast(Integer,Cvalue)
         If Cname = "PTKEpost" Then PTKEpost = Cast(Integer,Cvalue)
         If Cname = "PnCN" Then PnCN = Cast(Integer,Cvalue) 
         If Cname = "PEnpost" Then PEnpost = Cast(Integer,Cvalue) 
         If Cname = "PEgpost" Then PEgpost = Cast(Integer,Cvalue)  
         If Cname = "Pndel" Then Pndel = Cast(Integer,Cvalue)  
       Loop Until EOF(fTransfer)
       Close #fTransfer
       Ivalid = U_Valid(P_Z_CN,P_A_CN)
       If Ivalid = 0 Then Print "Fissioning nucleus outside supported range."
       If P_E_exc > 100 Then
         Print "Energy too high."
         Ivalid = 0
       End If
       If Ivalid = 0 Then
         Print "Please try again."
         GoTo RepeatInput
       End If	
     #endif   ' If Windows system
   End If
   
   
   /'*******************  Input by dialogue  *************************'/
   If Bfilein = 0 And BGUI = 0 Then  ' Input by dialog on Linux

    RepeatNucleus:
     Input "Enter Z and A of fissioning nucleus: ",P_Z_CN,P_A_CN
     Ivalid = U_Valid(P_Z_CN,P_A_CN)
     If Ivalid = 0 Then
       Print "Fissioning nucleus out of supported range."
       Goto RepeatNucleus
     End If  
    RepeatReference:  
     Print
     Print "Chose the input option for the energy:" 
     Print "  GS (energy above ground state),"
     Print "  FC (energy above ground state, only first-chance fission),"
     Print "  ISx (isomer x=1,2; energy above isomeric state x),"
     Print "  EN (neutron incident energy),"
     Print "  EP (proton incident energy),"
     Print "  ENx (x=1,2; neutron incident energy; target in isomeric state x),"
     Print "  EB (energy above outer fission barrier),"
     Input "  ES (excitation-energy spectrum from file Espectrum.in, first-chance): ",CEref
     CEref = Ucase(CEref)
     If CEref <> "EB" And CEref <> "GS" And Left(CEref,2) <> "IS" _
                      And Left(CEref,2) <> "EN" And Left(CEref,2) <> "EP" And CEref <> "ES" _ 
                      And CEref <> "FC" Then
       Print "Option ";CEref;" is not valid. Enter again!"  
       Goto RepeatReference
     End If
         
     ' Selection of list-mode values (for LINUX version) (0 or 1)
     PZ1 = 1
     PZ2 = 1
     PA1pre = 1
     PA2pre = 1
     PA1post = 1
     PA2post = 1
     PI1 = 1
     PI2 = 1
     PXE = 1
     Pn1 = 1
     Pn2 = 1
     PTKEpre = 1
     PTKEpost = 1    
     PnCN = 1
     Pndel = 1
 
     I_E_iso = 0
     Select Case Left(CEref,2)
       Case "EB"
         Emode = 0
         Print "Reference for energy input is the outer barrier."
       Case "GS"
         Emode = 1
         Print "Reference for energy input is the ground state."
       Case "FC"
         Emode = -1
         Print "Reference for energy input is the ground state."
       Case "IS"
         Emode = 1
         Print "Reference for energy input is the isomeric state."  
         I_E_iso = Val(Mid(CEREF,3))
       Case "EN"
         Emode = 2
         Print "(n,f) reaction assumed."
         Print "Energy input is the incident neutron energy."
         If Val(Mid(CEREF,3)) > 0 Then
           I_E_iso = Val(Mid(CEREF,3))
         End If
       Case "EP"
         Emode = 12
         Print "(p,f) reaction assumed."
         Print "Energy input is the incident proton energy."
         If Val(Mid(CEref,3)) > 0 Then
           I_E_iso = Val(Mid(CEref,3))
         End If         
       Case "ES"
          Emode = 3
          Print "Excitation-energy spectrum from input file Espectrum.in"  
          Print "(Only first-chance fission is calculated.)"
       Case Else
         Print "Option ";CEref;" is not valid. Enter again!"
         Goto RepeatReference
     End Select
   RepeatEnergy:  
     If Emode Mod 10 < 3 Then Input "Enter energy value (MeV): ",P_E_exc
     Print " "
     If P_E_exc > 100 Then
       Print "Energy too high. Enter again!"
       Goto RepeatEnergy
     End If
     
     If (Emode = 1 Or Emode = -1) And P_E_exc > 0 Then
       Input "Enter rms angular momentum of CN (0 for ground-state spin): ",P_I_rms_CN
     End If
     Print " "
     Scope
       Dim As String C_eo
       Input "Enter scaling factor for even-odd effect in Z and N yields (default = 1): ",C_eo
       If C_eo = "" Then 
         EOscale = 1
       Else
         EOscale = Val(C_eo)
       End If  
     End Scope  
     Print " "
/'<'/
     /' Shell effects for the symmetric fission channel '/
     _Delta_S0 = U_Delta_S0(P_Z_CN,P_A_CN)   ' default values
     
/'>'/     
     Scope
       Dim As Single Delta_S0_mod=0
       Dim As String Modi
       Print "Shell effect in the symmetric channel is assumed to be ";_Delta_S0;" MeV."
       Delta_S0_mod = _Delta_S0
       If Bfilein = 0 And BGUI = 0 Then
         Input "You may enter another guess value if you want to change it: ",Modi
       End If  
       If Modi <> "" Then
         Delta_S0_mod = Val(Modi)
         _Delta_S0 = Delta_S0_mod
       End If
     End Scope  
     Print " "

     Scope
       Dim As Integer Ilocal = 0
       Clocal = "localoff"
       Input "Use locally adjusted model parameters, if available (0 or 1): ",Ilocal
       If Ilocal = 1 Then Clocal = "localon"
       Print " "
     End Scope

     #ifdef B_ENDF
       B_Error_On = 1   
       Print "Error analysis is on (required for ENDF output)."   
       If Emode = 2 Then Input "Write ENDF random files (0 or 1): ",B_Random_On
     #else
       Input "Switch for error analysis by perturbed model parameters (0 or 1): ",B_Error_On
     #endif    
     If B_Error_On = 1 Then
       Input "Switch for output of covariances (0 or 1): ",Bcov
       Input "Switch for output of correlation coefficients (0 or 1): ",Bcor
     End If
     Print " " 
     
     If B_Error_On = 1 And (Bcov = 1 Or BCor = 1) And Emode Mod 10 < 3 Then
       Input "Determine covariances and/or correlations related to another system (0 or 1): ",B_Double_Covar
       If B_Double_Covar = 1 Then
         RepeatDoubleNucleus:
         Input "Enter Z and A of the second fissioning nucleus: ",P_Z_CN_Double,P_A_CN_Double
         Ivalid = U_Valid(P_Z_CN_Double,P_A_CN_Double)
         If Ivalid = 0 Then
           Print "Fissioning nucleus out of supported range."
           Goto RepeatDoubleNucleus
         End If  
         Input "Enter energy value (MeV): ",P_E_exc_Double
       End If
       Print " "
     End If
     
   '  Input "Include delayed gammas in prompt-gamma results (default = 0)? (0,1): ",I_DelGam
   '  Print " "

     Input "You may specify an enhancement factor to increase the statistics: ",Fenhance     
     Print " "
     
     Input "Enter file for LMD output (if desired): ",Cfileoutlmd
     If Cfileoutlmd <> "" Then
       If Instr(Cfileoutlmd,".") = 0 Then
         Cfileoutlmd_full = "out\"+Cfileoutlmd+".lmd"
       Else
         Cfileoutlmd_full = "out\"+Cfileoutlmd
       End If
       Input "List energies and angles of prompt post-scission neutrons (0 or 1): ",PEnpost
       Input "List energies of post-scission prompt gammas (0 or 1): ",PEgpost
     End If
     Print " "
     
     If B_Error_On = 1 Then
       Input "Switch for extended output of perturbed calculations (0,1): ",Brec
       If Brec = 1 Then 
         print " "
         print "- Full tables of all GEF results are written to /out/XXX.ptb."
         print "- Tables of FF yields (Y(Z), Y(Apre), Y(Apost), Y(Apre,Z), Y(Apost,Z)"
         print "  are written to /tmp/XXX.mvd."
         print "  These are the raw data for calculating the corresponding"
         print "  multivariant distributions of the GEF results and the"
         print "  covariance matrices."
         print "- List-mode data of all fission observables are written to /out/XXX.lmd."
         print "  (This option is only activated if list-mode output is enabled.)"
         print " "
       End If
     End If
     
   End If  ' End input by dialogue    
   
   
    '*** Insert locally adjusted model parameters ***
      
   ' Reset used parameters to values from global fit 
   S2leftmod = S2leftmod_global

   If Bfilein Then
     Clocal = "localon"
   End If  
        
   ' Modify values according to local fits   
   Scope
     Dim As Integer Ilocal = 0
     If Clocal = "localon" Then
       If P_Z_CN = 90 Then
         _P_Shell_S1 = -1
         S2leftmod = 0.7
         Ilocal = 1
       End If
       If Ilocal = 0 Then 
         Clocal = "localoff"
         If Bfilein = 0 Then
           Print " "
           Print "Locally adjusted model parameters are not available."
           Print "Global model parameters will be used."
           Print " "
         End If
       Else
         Print "Model parameters specifically adjusted for the selected system will be used."  
       End If   
     End If    
   End Scope
   
   
   If Bfilein = 0 Then
     If B_Error_On = 1 Then
       If P_Z_CN > 106 And P_Z_CN <= 120 Then
         Print " "
         Print "***********************************************"
         Print " Uncertainty estimates are doubtful for Z>106! "
         Print "***********************************************"
         Print " "
       End If    
     End If   
  
     N_E_steps = 1
     Iline_tot = 1   
   End If
 
   
   /' Support of multiprocessing with task list from file '/    

   ' Notice: Before starting a new multiprocessing calculation, the
   ' files "ctl/thread.ctl" and "ctl/done.ctl" must be deleted!
   
   Dim As String C_Energy_Table
   Redim Energy_Table(1) As Single
   Dim As Integer N_E_values = 1
   Dim As Integer I_first,I_last,N_values
   Static As Single En_min    ' excitation energy, for (n,f) neutron energy
   Static As Single En_max    ' excitation energy, for (n,f) neutron energy

   Type S_par_type          ' Case parameters
      Dim Z As Integer
      Dim A As Integer
      Dim CE As String          ' kind of entrance channel
   End Type
   Static N_par As Integer   ' Number of cases in input file
   Static S_par(5000) As S_par_type  ' List of case parameters
   
   
   
   Dim As Integer iexist

   Redim As Single E_spectrum(1)
   Redim As Single L_spectrum(1)
   Redim As Single W_spectrum(1)


'  Scope     
     
   Dim As String Chelp  
   Redim As String Cfilein(1000)
   Dim As Integer Ifilein 
   Dim As Integer Nfilein = 0
   
 '  N_E_steps = CInt(En_max) + 5
   ' Sequence of calculations:
   '     1    ,    2    ,    3    , 4 ,5,6,7,8,9,    ,N_E_steps-1,N_E_steps   
   '  En_min,    En_max,   En_min,0.4, 1,2,3,4,5 ... ,En_max,      En_min  
  

     
   ' Important note:  
   ' If the file "Input.dat" exists, the GEF program does not accept
   ' any input from the keybord and calculates the sequence of nuclei
   ' given in the file defined by "Input.dat" automatically!  
   
   /'*****************  Read task list from file  ********************'/
   If Bfilein = -1 Then
     Print "File 'file.in' found."
     Print "Manual input suspended, input will be taken from file."
  /' Dim As Integer I_allow
     Print " "
     If I_thread = 1 Then
       Input "Enter 1 to allow for graphics output (default = 0): ",I_allow
       Print " "
     End If  
     If I_allow <> 1 Then '/ Bplot = 0
     Ffilein = Freefile
     Open "file.in" for input as #Ffilein
     Do Until EOF(Ffilein)
       Nfilein = Nfilein + 1
       If Nfilein >= 1000 Then
         Print "<E> Maximum number of input files reached."
         Nfilein = 1000
         Exit Do
       End If
       Line Input #Ffilein,Chelp
       Chelp = Trim(Chelp)
       If Ucase(Chelp) = "END" Then
         Nfilein = Nfilein - 1 
         Exit Do
       End If  
       If Left(Chelp,1) <> "'" Then
         If Len(Chelp) > 2 Then
           If Instr(Chelp,"'") > 0 Then Chelp = Mid(Chelp,1,Instr(Chelp,"'")-1)
           Chelp = Rtrim(Chelp)
           If Left(Chelp,1) = """" And Right(Chelp,1) = """" Then
             Chelp = Mid(Chelp,2,Len(Chelp)-2)
           End If
         End If  
         Cfilein(Nfilein) = Chelp
       Else
         Nfilein = Nfilein - 1
         Print "Input ***";Chelp;"*** skipped."
       End If  
     Loop
     If Nfilein = 0 Then Print "No file with input data specified in file.in."
     Close #Ffilein
   End If
     
 
   /'*************** Loop for list of input files  ******************'/

   For Ifilein = 1 To Max(Nfilein,1)         
   If Nfilein > 0 Then
     Print "Sequence of nuclei given in ";Cfilein(Ifilein);" will be calculated!"
     Bfilein = -1
     Ffilein = Freefile
       
     If Fileexists(Cfilein(Ifilein)) Then
       Open Cfilein(Ifilein) for input as #Ffilein
       Input #Ffilein,Fenhance_global
       Line Input #Ffilein,C_Energy_Table
       If Instr(C_Energy_Table,":") > 0 Then
          ' This allows to set the first energy step number in random ENDF files
         I_first_E_step = Cint(Left(C_Energy_Table,Instr(C_Energy_Table,":")-1))
         C_Energy_Table = Mid(C_Energy_Table,Instr(C_Energy_Table,":")+1)
       End If       
       N_E_values = CC_Count(C_Energy_Table,",") 
       Redim Energy_Table(N_E_values) As Single
       Redim Ccut(N_E_values) As String
       CC_Cut(C_Energy_Table,",",Ccut(),N_E_values)
       For I = 1 To N_E_values
         Energy_Table(I) = Val(Ccut(I))
         If I > 1 Then 
           If Energy_Table(I) <= Energy_Table(I-1) Then
             Print "Energy values in input file must be ascending."
             Sleep
           End If
         End If
       Next I
       En_min = Energy_Table(1) 
       En_max = Energy_Table(N_E_values)
       Iline = 0
       Do Until EOF(Ffilein)
         Line Input #Ffilein,Chelp
         Chelp = Trim(Chelp)   
'Print "Chelp",Chelp
         If Left(Chelp,1) <> "'" Then 
           If Ucase(Chelp) = "END" Then
             Print "Input after ***";Chelp;"*** skipped." 
             Exit Do
           End If  
           N_values = CC_Count(Chelp,",")  
           If N_values = 3 Then
             Redim Ccut(3)
             CC_Cut(Chelp,",",Ccut(),3)  
             Ccut(3) = Trim(Ccut(3))
             Ccut(3) = Trim(Ccut(3),"""")
             If Instr(Ccut(2),"-") > 0 Then
               I_first = Val(Mid(Ccut(2),1,Instr(Ccut(2),"-")-1))
               I_last = Val(Mid(Ccut(2),Instr(Ccut(2),"-")+1))
               For I = I_first To I_last
                 Iline = Iline + 1
                 S_par(Iline).Z = Val(Ccut(1))
                 S_par(Iline).A = I
                 S_par(Iline).CE = Ccut(3)
               Next I
             Else
               Iline = Iline + 1
               S_par(Iline).Z = Val(Ccut(1))
               S_Par(Iline).A = Val(Ccut(2))
               S_par(Iline).CE = Ccut(3)  
             End If
           Else
             Print "<E> Number of data not matching in ";Chelp;"."
           End If
         Else
           Print "Input ***";Chelp;"*** skipped."
         End If
       Loop
       Close #Ffilein
       Iline_tot = Iline
     Else
       Print "<E> File ";Cfilein(Ifilein);" not found."
       End
     End If
     
     Fenhance = Fenhance_global
   Else
     Bfilein = 0
     En_min = P_E_exc
     En_max = P_E_exc         
   End If
 

   /'****** Loop for double calculation for covariances between two systems ******'/ 

   If B_Double_Covar = 1 Then 
     N_Double_Covar = 2
   Else
     N_Double_Covar = 1
   End If
     
   For I_Double_Covar = 1 To N_Double_Covar   
   #ifdef B_plotting 
     If Bplot And I_Double_Covar = 2 Then
       SCREEN 0
     End If   
   #endif
   
   If I_Double_Covar = 2 Then
     P_Z_CN = P_Z_CN_Double
     P_A_CN = P_A_CN_Double
     P_E_exc = P_E_exc_Double
   End If
   

   /'*************** Loop for executing task list  ******************'/
   
   For Iline = 1 To Iline_tot   
     If Bfilein = -1 Then
       P_Z_CN = S_par(Iline).Z
       P_A_CN = S_par(Iline).A
       CEref = Ucase(S_par(Iline).CE)

       _Delta_S0 = U_Delta_S0(P_Z_CN,P_A_CN)   

       await(I_thread,I_thread_max)
         ' Avoid collisions in reading and writing list of finished calculations        
       Dim As Integer Fdone,I,Ifound
       Dim As Single Zdone,Adone,Edone
       Ifound = 0
       CHDIR("ctl")
       If Fileexists(CFdone) Then
         Fdone = Freefile
         Open CFdone For Input As #Fdone
         Do 
           Input #Fdone, Zdone,Adone,Edone
           If Zdone = P_Z_CN Then
 'Print "Z",Zdone,P_Z_CN
             If Adone = P_A_CN Then
 'Print "A",Adone,P_A_CN
               If Edone = En_max Then
 'Print "E",Edone,En_max
                 Ifound = 1
                 Print "Z = ";P_Z_CN;" A = ";P_A_CN;" skipped (already in done.ctl)."
               End If
             End If      
           End If
         Loop Until EOF(Fdone)
         Close #Fdone
       End If  
       CHDIR("..")
         
       If U_valid(P_Z_CN,P_A_CN) = 0 Then
         Ifound = 1
         Print "Nucleus Z =";P_Z_CN;", A =";P_A_CN; _
             " out of supported range, will be skipped."
       End If
         
       If Ifound = 0 Then
         CHDIR("ctl")       
         Fdone = Freefile
         Open CFdone For Append As #Fdone
         Print #Fdone, P_Z_CN,P_A_CN,En_max  ' register new case in Fdone
         Close #Fdone
         CHDIR("..")
       End If
       If Ifound = 1 Then 
         Continue For
       End If           

       N_E_steps = 1      ' valid for most options
       P_E_exc = En_max   ' valid for most options
       Select Case Left(CEref,2)
         Case "GS"
           Emode = 1
         Case "FC"   ' CN fission, first chance only
           Emode = -1
         Case "IS"   ' spontaneous fission from isomer
           Emode = 1
           I_E_iso = Val(Mid(CEREF,3))
         Case "EN"
           Emode = 2
           #ifdef B_ENDF 
             If N_E_Values > 1 Then N_E_steps = N_E_Values + 3   ' with ENDF output
           #else
             N_E_steps = N_E_Values   ' without ENDF output
           #endif             
           If Val(Mid(CEref,3)) > 0 Then
             I_E_iso = Val(Mid(CEref,3))
           End If
         Case "EP"
           Emode = 12  
           If Val(Mid(CEref,3)) > 0 Then
             I_E_iso = Val(Mid(CEref,3))
           End If           
         Case Else
           Print "Option ";CEref;" is not valid."       
           End
       End Select
     End If
     If Emode = 2 And B_Random_on = 1 And N_E_Values = 1 Then
       N_E_steps = 2            ' Additional calculation for ENDF limits
     End If
     
     Print " "
     Print "Fission barrier of CN (Z=";P_Z_CN;", A=";P_A_CN;"):"
     Print "Macrosc. barrier = ";Round(BFTF(P_Z_CN,P_A_CN,0),3);" MeV"
     If P_A_CN - P_Z_CN > 130 Then
'Print "Inner saddle = ";Round(BFTFA(P_Z_CN,P_A_CN,4),3); _
'  " MeV, outer saddle = ";Round(BFTFB(P_Z_CN,P_A_CN,4),3);" MeV"
       Print "Inner saddle = ";Round(BFTFA(P_Z_CN,P_A_CN,1),3); _
          " MeV, outer saddle = ";Round(BFTFB(P_Z_CN,P_A_CN,1),3);" MeV"
     Else
' Print "Fission-barrier height = ";Round(BFTF(P_Z_CN,P_A_CN,4),3);" MeV"
       Print "Fission-barrier height = ";Round(BFTF(P_Z_CN,P_A_CN,1),3);" MeV"
     End If
     Print "exp. Mass = ";AME2012(P_Z_CN,P_A_CN);" MeV"  
     Print "Lymass    = ";Lymass(P_Z_CN,P_A_CN,0);" MeV"
     Print "G.S.Shell effect (Moeller) = ";Round(U_SHell(P_Z_CN,P_A_CN),3);" MeV, ";
     Print "G.S.Shell effect (exp,TF) = ";Round(U_Shell_exp(P_Z_CN,P_A_CN),3);" MeV"  
     Print "S_n(exp.) = ";AME2012(P_Z_CN,P_A_CN-1) - AME2012(P_Z_CN,P_A_CN);" MeV"      
     
     If BGUI = 0 Then
       CFileout_Single = "GEF_"+Trim(Str(P_Z_CN))+"_"+Trim(Str(P_A_CN))
       If I_Double_Covar = 1 Then  ' single system or first of the two fissioning systems
         If B_Double_Covar = 0 Then
           CFileout = "GEF_"+Trim(Str(P_Z_CN))+"_"+Trim(Str(P_A_CN))
         Else
           CFileout = "GEF_"+Trim(Str(P_Z_CN))+"_"+Trim(Str(P_A_CN))+"+"+ _
                             Trim(Str(P_Z_CN_Double))+"_"+Trim(Str(P_A_CN_Double))
         End If
         If I_E_iso > 0 Then
           If Abs(Emode) = 1 Then CFileout = CFileout + "f" + Trim(Str(I_E_iso))
           If Emode = 2 Then CFileout = Cfileout + "m" + Trim(Str(I_E_iso))
         End If  
         If Emode = 2 Then CFileout = CFileout + "_n" 
         If Emode = 12 Then CFileout = CFileout + "_p" 
         If Abs(Emode) = 1 Then
           If P_E_exc = 0 Then  
             CFileout = Cfileout + "_sf"
           Else
             If Emode = -1 Then 
               CFileout = CFileout + "_fc"
             Else 
               CFileout = Cfileout + "_cf"
             End If  
           End If  
         End If  
         If Emode = 3 Then CFileout = Cfileout + "_ed"  ' energy distribution
       End If    
       CFileout_full = "out\"+CFileout + ".dat"
       If CFileoutlmd <> "" Then
         If Instr(CFileoutlmd,".") = 0 Then
           CFileoutlmd_full = "out\"+CFileoutlmd + ".lmd"
         Else
           CFileoutlmd_full = "out\"+CFileoutlmd
         End If  
       End If  
     End If       


     For I_E_step = 1 To N_E_steps 

       #include "CLEARerrors.bas"
       I_Error = 0
       
       If N_E_steps = 2 Then
         If B_Random_on Then
           If I_E_step = 1 Then B_Error_On = 0
           If I_E_step = 2 Then B_Error_On = 1
         End If
       Else
         If Bfilein Then
           /' Sequence for ENDF output of n-induced fission '/     
           If Emode = 2 Then
             If N_E_steps = 1 Then
               B_Error_On = 1
             Else
               #ifdef B_ENDF    ' with ENDF output: additional calculations 
                 If I_E_step = 1 Then 
                   P_E_exc = En_min
                   B_Error_On = 0
                 End If  
                 If I_E_step = 2 Then 
                   P_E_exc = En_max
                   B_Error_On = 0
                 End If  
                 If I_E_step >= 3 and I_E_step < N_E_steps Then 
                   P_E_exc = Energy_table(I_E_step-2)
                   B_Error_On = 1
                 End If  
                 If I_E_step = N_E_steps Then 
                   P_E_exc = En_min
                   B_Error_On = 0
                 End If
               #else    ' without ENDF output
                 P_E_exc = Energy_table(I_E_step)
               #endif 
             End If  
           End If
         End If
       End If   
 
       /' Get data for isomer from table '/
       Scope
         Dim I_MAT As Integer
         Dim As Integer ZT,AT
         ZT = P_Z_CN
         AT = P_A_CN
         E_EXC_ISO = 0
         E_EXC_TRUE = P_E_exc
         If Emode = 2 Then AT = AT -1
         If Emode = 12 Then
           AT = AT - 1
           ZT = ZT - 1 
         End If         
         If I_E_iso > 0 Then 
           Print " "
           Print "isomer # ";I_E_iso
           I_MAT = I_MAT_ENDF(ZT,AT)
           IF N_ISO_MAT(I_MAT) < I_E_iso Then
             Print "The isomer is not in the table of nuclear properties."
             Print "Z, A, #iso ",ZT,AT,I_E_iso
             Print "Please stop GEF, complete table and restart GEF."
             Goto RepeatInput
           End If
           Print "spin = ";NucTab(I_MAT + I_E_iso).R_SPI
           If Abs(Emode) = 1 Then Print "Energy above ground state ->"
           If Emode = 2 Then Print "Fictive neutron energy for capt. in g.s. ->"
           If Emode = 12 Then Print "Fictive proton energy for capt. in g.s. ->"
           E_EXC_ISO = NucTab(I_MAT + I_E_iso).R_EXC
           If Abs(Emode) = 1 Then P_E_exc = P_E_exc + E_EXC_ISO
           If Emode = 2 Then P_E_exc = P_E_exc + E_EXC_ISO
           If Emode = 12 Then P_E_exc = P_E_exc + E_EXC_ISO
         End If  
       End Scope 
 
       Print "CN: Z = ";P_Z_CN;", A = ";P_A_CN;
       If Emode Mod 10 < 3 Then
         Print ", E = ";P_E_exc;" MeV"
       Else
         Print ", E/MeV from file Espectrum.in"
       End If

       ' Read excitation-energy distribution from file
       Scope
         Dim As Integer FEspectrum
         Dim As Integer Ichannel=0
         Dim As Single Renergy,Rspin,Rweight,REmax
         Dim As Single Norm = 0
         Dim As String C_read
         Dim As Integer N_Count
         REmax = 0
         If Emode = 3 Then
           If Fileexists("Espectrum.in") Then 
             FEspectrum = Freefile
             Open "Espectrum.in" For Input As #FEspectrum  
             Line Input #FEspectrum, C_read
             N_Count = CC_Count(C_read, " ")
             Close #FEspectrum
             FEspectrum = Freefile
             Open "Espectrum.in" For Input As #FEspectrum  
             If N_Count = 2 Then
               Do
                 Input #FEspectrum, Renergy,Rweight
                 REmax = Max(REmax,Renergy)
                 Ichannel = Ichannel + 1
                 If Ichannel > Ubound(W_spectrum,1) Then
                   Redim Preserve E_spectrum(Ichannel)
                   Redim Preserve W_spectrum(Ichannel)
                 End If  
                 E_spectrum(Ichannel) = Renergy
                 W_spectrum(Ichannel) = Rweight
 Print E_spectrum(Ichannel),W_spectrum(Ichannel)            
               Loop Until EOF(FEspectrum)       
             End If
             If N_Count = 3 Then
               Do
                 Input #FEspectrum, Renergy,Rspin,Rweight
                 REmax = Max(REmax,Renergy)
                 Ichannel = Ichannel + 1
                 If Ichannel > Ubound(W_spectrum,1) Then
                   Redim Preserve E_spectrum(Ichannel)
                   Redim Preserve L_spectrum(Ichannel)
                   Redim Preserve W_spectrum(Ichannel)
                 End If  
                 E_spectrum(Ichannel) = Renergy
                 L_spectrum(Ichannel) = Rspin
                 W_spectrum(Ichannel) = Rweight
 Print E_spectrum(Ichannel),L_spectrum(Ichannel),W_spectrum(Ichannel)            
               Loop Until EOF(FEspectrum)       
             End If
             Close #FEspectrum
           Else
             Print "<S> File Espectrum.in not found."
             End  
           End If
           If REmax > 11 Then I_Warning = 1
         End If
         ' Normalize energy spectrum
         For Ichannel = 1 To Ubound(W_spectrum)
           Norm = Norm + W_spectrum(Ichannel)
         Next
         For Ichannel = 1 To Ubound(W_spectrum)
           W_spectrum(Ichannel) = W_spectrum(Ichannel) / Norm
         Next
       End Scope

       If Bfilein = -1 Then
         Fenhance = Fenhance_global
       End If


       ' Excitation energy above ground state
       Select Case Emode
         Case 0
           Eabsgs = P_E_exc + BFTFB(P_Z_CN, P_A_CN, 1) 
         Case 1, -1
           Eabsgs = P_E_exc
         Case 2  ' P_E_exc is the kinetic energy of the neutron
                ' 2/3 * 1.16 * sqr(2 * 939.65) / 197.33 = 0.1699 
           If P_E_exc > 0 Then     
             Spin_pre_fission = sqr(P_I_rms_CN^2 + _ 
              + 0.5^2 + (0.1699 * (P_A_CN - 1)^0.333333 * sqr(P_E_exc))^2)
           Else
             Spin_pre_fission = P_I_rms_CN
           End If   
'           Eabsgs = P_E_exc + U_Mass(P_Z_CN,P_A_CN-1) + Lypair(P_Z_CN,P_A_CN-1) _
'                 - (U_Mass(P_Z_CN,P_A_CN) + Lypair(P_Z_CN,P_A_CN)) 
           Eabsgs = P_E_exc * ((P_A_CN-1) / P_A_CN) + AME2012(P_Z_CN,P_A_CN-1) - AME2012(P_Z_CN,P_A_CN)
                                                     '           target                      CN
         Case 12  ' P_E_exc is the kinetic energy of the proton
                ' 2/3 * 1.16 * sqr(2 * 939.65) / 197.33 = 0.1699 
           If P_E_exc > 0 Then     
             Spin_pre_fission = sqr(P_I_rms_CN^2 + _ 
             + 0.5^2 + (0.1699 * (P_A_CN - 1)^0.333333 * sqr(P_E_exc))^2)  ' preliminary !
           Else
             Spin_pre_fission = P_I_rms_CN
           End If            
           Eabsgs = P_E_exc * ((P_A_CN-1) / P_A_CN) + AME2012(P_Z_CN-1,P_A_CN-1) - AME2012(P_Z_CN,P_A_CN)
                                                     '           target                      CN
         Case 3  ' Energy values are set later (label Energy_loop:)
       End Select
       If Eabsgs > 100 Then
         If Bfilein Then
         Else
           Print " "
           Print "Energy of CN > 100 MeV is not supported, please try again."
           Print " "
           Goto RepeatInput
         End If
       End If


   Print " "
  ' Print "Calculation started at ";time
   Print "Please wait ... code is running."
   Print " "


   /'*** Multi-chance fission ***'/
   
   ' Arrays for preliminary storage of pre-fission emission:

   ReDim E_multi_chance(10,10,1000) As Single 
   Dim As Integer Inofirst = 0      ' counts events > first-chance fission
   Dim As Longint Imulti = 0
   Dim As Integer Bmulti = 0        ' multi-chance possible
   Dim As Longint N_multi_sample 
   If Fenhance < 1 Then Fenhance = 1   
   N_multi_sample = sqr(Fenhance) * Ten_to_five
  ' N_multi_sample = Ten_to_five 
   ReDim As Single W_chances(10,10)  ' neutron loss, proton loss
   ReDim As ULongint En_multi_1(0)   ' neutron energy for second-chance fission (100 keV bins)
   ReDim As ULongint En_multi_2(0)   ' neutron energy for third-chance fission (100 keV bins)
   ReDim As ULongint En_multi_3(0)   ' (neutron energies stored in 10 bits per neutron)
   ReDim As ULongint En_multi_4(0)
   ReDim As Ulongint En_multi_5(0)
   ReDim As Ulongint En_multi_6(0)
   ReDim As Integer J_multi_last(6) 
   
   Redim As UInteger I_emit_1(0)
   Redim As UInteger I_emit_2(0)
   Redim As UInteger I_emit_3(0)
   Redim As UInteger I_emit_4(0)
   Redim As UInteger I_emit_5(0)
   Redim As UInteger I_emit_6(0)
   
   Dim As Single DUF = 0

   Bmulti = 0
   /' Enter only if E_exc > Sn (smooth) + 3 MeV '/
   If EMode Mod 10 < 3 And EMode >= 0 Then
   IF Eabsgs > U_Mass(P_Z_CN,P_A_CN-1) - U_Mass(P_Z_CN,P_A_CN) + 3 Then
   
     Bmulti = 1
     Print "Calculation of fission chances."
       
     /' Determine the weights of the different fission chances '/
     /' and the excitation-energy distributions at fission '/
'     Print "Competition of neutron emission and fission is determined../"    
'     Print 

     Scope
     
     Dim As Longint I,J,K
     
     ' Multi-chance fission-energy distribution
  '     Redim E_multi_chance(10,10,1000) As Single  ' Chance(n,p), E/100keV
       For I = 0 To 10  ' neutron loss
         For J = 0 To 10  ' proton loss
           For K = 0 To 1000  ' energy
             E_multi_chance(I,J,K) = 0
           Next  
         Next
       Next    
       


       Dim As Integer A_f,Z_f,A_n,Z_n,A_p,Z_p
       Dim As Single T_f,T_f_low,T_f_eff
       Dim As Single T_n,T_n_low,T_n_eff
       Dim As Single T_p,T_p_low,T_p_eff
       Dim As Single Tm
       
   

       Dim As Single SN,SNTF,SNTFnopair,SNmean
       Dim As Single SP,SPTF,SPTFnopair,SPmean,BP,BPnopair,BPmean
       Dim As Single BF,BFLD,BFA,BFB,BFM,GNGF,TF
       Dim As Single BFNP,BFANP,BFBNP,BFMNP
       Dim As Single SNpre,GNPref,GNpre,GPpre,GNPpre,GNPstat
       Dim As Single GAmod,GBmod
       Dim As Single G,GN,GP,GF,GNPtot
       Dim As Single Ggamma,Gtot,Grandom,Eg
       Dim As Single ypsilon,l_lim_2
       Dim As Single Frot     ' Enhanced fission due to angular momentum
       Dim As Single DUN
       Dim As Single FredF = 1
       Dim As Single FredN = 1
       Dim As Single Tequi 
       Dim As SIngle Pfis
       Dim As Single Etest,E_left,A_left,Z_left,E_left_pre
       Dim As Single A_loss,Z_loss,N_loss
       Dim As Single x,y
       Dim As Single Ftunn,FtunnA,FtunnB
       Dim As Single En_kin,Ep_kin
       Redim As ULongInt En_multi_mem(11)   ' 100 keV bins 
       Redim As Integer EnCN_mem(11)
       Redim As Integer EpCN_mem(11)
       Dim As Uinteger I_emit_mem
       Dim As Single P_precompound
       Dim As Ubyte B_pre  ' pre-equilibrium emission
       Dim As Integer I_pre_last
       Dim As Integer IPcnt,INcnt,IENtry
       
       Dim As ULongint Ilong   
       
  '*********************************************************************     

       
  ' *** This section is just for testing ***
  ' Gamma_n and Gamma_f according to the statistical model
  ' using the analytical description of Smirenkin et al.
  Dim As Single Ework
 ' Print "First-chance fission without pre-equilibrium emission"
 ' Print "Aleft,Eleft,Shell,SN,BFld,BFM,SN-BFM,GN,GF,Pfis " 
  
  
 ' Parameters for multi-chance fission 
  Dim As Single GA     ' parameter including coll. enh.
  Dim As Single GB     ' parameter including coll. enh.
  GA = 0.14 / sqr(pi/2)   ' triaxiality (compared to quadrupole deformation)
  GB = 0.5    ' mass asymmetry (compared to quadrupole deformation)
  Dim As Single hbom = 0.9  ' effective curvature of fission barrier
'  Dim As Single hbom = 1  ' effective curvature of fission barrier
  Tequi = hbom /(2 * pi)  ' equivalent temperature parameter
 ' Dim As Single cmul = 0.1
  Dim As Single cmul = 0.05  ' shape transition due to washing of shells
  Dim As Single E100keV
  Dim As Single Tmax,Tfinal
  Dim As Single I_Exciton   ' For distribution of exciton configurations
  
/'  
' Nur zum Testen von U_temp:
 Dim As Single E_test,tau1,tau2,tau3,T1,T2,T3,GN1,GN2,GN3
 Print "Z = 54, A = 140"
 Print "E    T (U_temp)"
 For E_test = 5 To 50 Step 0.1
    T1 = U_Temp(54,140,E_test-2,1,0,Tscale,Econd)
    T2 = U_Temp(54,140,E_test,1,0,Tscale,Econd)
    T3 = U_Temp(54,140,E_test+2,1,0,Tscale,Econd)
    GN1 = (140)^0.66667 * 0.13 * T1^2 / exp(6/T1)
    GN2 = (140)^0.66667 * 0.13 * T2^2 / exp(6/T2)
    GN3 = (140)^0.66667 * 0.13 * T3^2 / exp(6/T3)
    tau1 = 0.658 / GN1
    tau2 = 0.658 / GN2  
    tau3 = 0.658 / GN3  
    Print E_test,Round((T1*T2*T3)^0.333333,4),Round((GN1*GN2*GN3)^0.333333,4),Round((tau1*tau2*tau3)^0.333333,4)
 Next     
 sleep
 '/

 ' Print "* A_left     E_left    GN     GF     Ggamma    Pfis" 
  For Ework = 4 To 10.5 Step 0.1
         E_left = Ework
         For I = 0 To 0
           A_left = P_A_CN - I
           SN = AME2012(P_Z_CN,A_left-1) - AME2012(P_Z_CN,A_left)
           SNTF = U_Mass(P_Z_CN,A_left-1) + Lypair(P_Z_CN,A_left-1) _
                     - (U_Mass(P_Z_CN,A_left) + Lypair(P_Z_CN,A_left))
           SNTFnopair = U_Mass(P_Z_CN,A_left-1) - U_Mass(P_Z_CN,A_left) 
           SNmean = 0.5E0 * (U_Mass(P_Z_CN,A_left-2) - U_Mass(P_Z_CN,A_left))
           BF = BFTF(P_Z_CN,A_left,1)   
           BFNP = BFTF(P_Z_CN,A_left,2)
           BFLD = BFTF(P_Z_CN,A_left,0) 
           BFA = BFTFA(P_Z_CN,A_left,1)
           BFANP = BFTFA(P_Z_CN,A_left,2)
           BFB = BFTFB(P_Z_CN,A_left,1)  
           BFBNP = BFTFB(P_Z_CN,A_left,2)
           BFM = Max(BFA,BFB) 
           BFMNP = MAX(BFANP,BFBNP)
           DUN = U_SHELL(P_Z_CN,A_left-1)
           If E_left > SN Then
             T_n = U_Temp(P_Z_CN,A_left-1,E_left-SNmean,1,0,Tscale,Econd)
             T_n_low = U_Temp(P_Z_CN,A_left-1,E_left-SNmean-4.E0*T_n,1,0,Tscale,Econd)
             T_n_eff = sqr(T_n * T_n_low)
             GN = (A_left-1)^0.66667 * 0.13 * T_n_eff^2 / exp(SNmean/T_n_eff)  _
                          * (1.E0 - exp(-(E_left-SN)/(1.6 * T_n_eff)))
                         ' last line for E factor in Maxwellian (analytical approximation)
           Else
             T_n = 0
             GN = 0 
           End If

           GAmod = exp(cmul*(E_left - BFANP))/(1/GA + exp(cmul*(E_left - BFANP)))
           FtunnA = 1/(1+exp(-(E_left-BFA)/Tequi)) 
           GAmod = GAmod / FtunnA
      '     Ftunn = 1/(1+exp(-(E_left-BFA)/( Tequi*T_f_eff/(T_f_eff-Tequi) )))
           GBmod = exp(cmul*(E_left - BFBNP))/(1/GB + exp(cmul*(E_left - BFBNP)))
           FtunnB = 1/(1+exp(-(E_left-BFB)/Tequi))
           GBmod = GBmod / FtunnB
      '     Ftunn = 1/(1+exp(-(E_left-BFB)/( Tequi*T_f_eff/(T_f_eff-Tequi) )))
      '     Ftunn = 1/(1+exp(-(E_left-BFA)/Tequi)) * 1/(1+exp(-(E_left-BFB)/Tequi))
           T_f = U_Temp2(P_Z_CN,A_left,E_left-BFMNP,DUF,0,Tscale,Econd)
           T_f_low = U_Temp2(P_Z_CN,A_left,E_left-BFMNP-4*T_f,DUF,0,Tscale,Econd)
           T_f_eff = sqr(T_f * T_f_low)
           G = GAmod * exp((BFANP-BFMNP)/T_f_eff) + GBmod * exp((BFBNP-BFMNP)/T_f_eff) 
     '      Ftunn = 1/(1+exp(-(E_left-BFM)/( Tequi*T_f_eff/(T_f_eff-Tequi) )))
           ypsilon = 1.0 - P_Z_CN^2 / (P_A_CN * 50.0)  ' 1 - fissility
           ypsilon = max(ypsilon,0.1)   ' limitation for very heavy nuclei
           l_lim_2 = 15.0^2 / (ypsilon/0.28) * T_f_eff / TEgidy(P_A_CN,DUF,1.0)  ' Hasse & Myers
           Frot = exp( P_I_rms_CN^2/l_lim_2 )  ' enhancement of GF by angular momentum
           GF = T_f_eff / (G * exp(BFMNP/T_f_eff)) * Frot 
 
            ' Low level density below the pairing gap          
           If P_Z_CN Mod 2 = 0 And A_left Mod 2 = 0 Then
             If E_left - BFM < 2 * 12 / sqr(A_left) Then
               GF = GF * Max(0.01 + 0.99*(Abs( (E_left-BFM) / (2*12/sqr(A_left) )) ),0.1  ) 
             End If
           End If           
           
           Tm = U_temp(P_Z_CN,A_left,E_left,1,0,Tscale,Econd)       ' emitting nucleus 
           Ggamma = 0.624 * A_left^1.6 * Tm^5    ' in meV (Ignatyuk, Bologna)
           Ggamma = Ggamma * 1.E-9        ' in MeV
           Pfis = GF / (GF + GN + Ggamma)
   '        Print A_left;" ";E_left;" ";DUN;" ";SN;" ";BFld;" ";BFM;" ";SN-BFM;" ";GN;" ";GF;" ";Pfis 
   '        Print A_left;" ";SNTFnopair;" ";E_left;" ";T_n;" ";T_f;" ";GN;" ";GF;" ";Pfis 
   '        Print A_left;" ";E_left;" ";GN;" ";GF;" ";Ggamma;" ";Pfis 
           
        Next  
 '      Print
        
   Next 
  
' Print 
' Print
 ' Sleep 

  '*********************************************************************     

       For I = Lbound(EgammaCN,1) To Ubound(EgammaCN,1)
         EgammaCN(I) = 0
       Next I 
       For I = Lbound(EnCN,1) To Ubound(EnCN,1)
         EnCN(I) = 0
       Next I 
       For I = Lbound(EpCN,1) To Ubound(EpCN,1)
         EpCN(I) = 0
       Next I 
       
       ' Multi-chance fission
       ' Extended formalism based on 
       ' V. M. Kupriyanov, K. K. Istekov, B. I. Fursov, G. N. Smirenkin
       ' Sov. J. Nucl. Phys. 32 (1980) 184
       K = 0
       
       Do Until Imulti >= N_multi_sample 
         E_left = Eabsgs
         A_left = P_A_CN
         Z_left = P_Z_CN
         
         Eg = 0
         I_emit_mem = 0  
         I_pre_last = 0
         B_pre = 1   ' Allow pre-compound fission
         For I = 0 To 10
      Aftergamma:   
           If E_left < 0.1 Then Exit For
'  Print "I,Z,A,E ";I;" ";Z_left;" ";A_left;" ";E_left       
           Z_loss = P_Z_CN - Z_left
           A_loss = P_A_CN - A_left
           N_loss = A_loss - Z_loss
           
           SN = AME2012(Z_left,A_left-1) - AME2012(Z_left,A_left)                     
           SNTF = U_Mass(Z_left,A_left-1) + Lypair(Z_left,A_left-1) _
                     - (U_Mass(Z_left,A_left) + Lypair(Z_left,A_left))
           SNTFnopair = U_Mass(Z_left,A_left-1) - U_Mass(Z_left,A_left) 
           SNmean = 0.5E0 * (U_Mass(Z_left,A_left-2) - U_Mass(Z_Left,A_left))
           SP = AME2012(Z_left-1,A_left-1) - AME2012(Z_Left,A_left)  
           BP = SP + 1.44 * (Z_left-1) / (2.0 * (1 + (A_left-1)^0.333333))  
           SPTF =  U_Mass(Z_left-1,A_left-1) + Lypair(Z_left-1,A_left-1) _
                     - (U_Mass(Z_left,A_left) + Lypair(Z_left,A_left))
           SPTFnopair = U_Mass(Z_left-1,A_left-1) - U_Mass(Z_left,A_left)
           BPnopair = SPTFnopair + _ 
                  1.44 * (Z_left-1) / (2.0 * (1 + (A_left-1)^0.333333))  
           SPmean = 0.5E0 * (U_Mass(Z_left-2,A_left-2) - U_Mass(Z_Left,A_left))
           BPmean = SPmean + _
                  1.44 * (Z_left-1) / (2.0 * (1 + (A_left-1)^0.333333))  
           BF = BFTF(Z_left,A_left,1) 
           BFNP = BFTF(Z_left,A_left,2) 
  '         BFLD = BFTF(Z_left,A_left,0) 
           BFA = BFTFA(Z_left,A_left,1)
           BFANP = BFTFA(Z_left,A_left,2)
           BFB = BFTFB(Z_left,A_left,1)  
           BFBNP = BFTFB(Z_left,A_left,2)
           BFM = Max(BFA,BFB)        
           BFMNP = Max(BFA,BFB)
           DUN = U_SHELL(Z_left,A_left-1)
           
           If E_left > SN Then
             T_n = U_Temp(Z_left,A_left-1,E_left-SNmean,1,0,Tscale,Econd)
             T_n_low = U_Temp(Z_Left,A_left-1,E_left-SNmean - 4.E0*T_n,1,0,Tscale,Econd)
             T_n_eff = sqr(T_n * T_n_low)
             GN = (A_left-1)^0.66667 * 0.13 * T_n_eff^2 / exp(SNmean/T_n_eff) _
                        * (1.E0 - exp(-(E_left-SN)/(1.6 * T_n_eff)))
                         ' last line for E factor in Maxwellian (analytical approximation)
           Else
             GN = 0 
           End If
           
           If E_left > BP Then
             T_p = U_Temp(Z_Left-1,A_Left-1,E_Left-BPmean,1,0,Tscale,Econd)
             T_p_low = U_Temp(Z_Left-1,A_left-1,E_left-BPmean - 4.E0*T_p,1,0,Tscale,Econd)
             T_p_eff = sqr(T_p * T_p_low)
             GP = (A_left-1)^0.666667 * 0.13 * T_p_eff^2 / exp(BPmean/T_p_eff)
           Else 
             GP = 0  
           End If           
           GNPstat = GN + GP

 '   Print "Imulti,I,A_left,E_left",Imulti;" ";I;" ";A_left,E_left         

           GNPpre = 0
           If Emode = 2 And B_pre = 1 And GNPstat > 0 Then  ' Only (n,f), starting with first-chance fission
             If E_left - Sn > 2 Then
               GNPpre = GNPstat * (E_left - 1.3E0) / 38.E0   'pre-equilibrium emission
               ' for neutron-induced fission
               ' from Ignatyuk et al. Sov. J. Nucl. Phys. 47 (1988) 224
    '          GNPpre = GNPstat * (E_left - 2.E0) / 30.E0   'zum Testen
               GNPpre = Max(0,GNPpre)
             End If  
           End If  
           GNPtot = GNPpre + GNPstat 
           If GNPtot > 0 Then 
             P_precompound = GNPpre / GNPtot   ' Probability for pre-compound emission
           Else
             P_precompound = 0
             B_pre = 0
           End If    
 '   Print "GNPpre,GNPstat,GNPtot",GNPpre,GNPstat,GNPtot       
 '   Print "B_pre,P_precompound",B_pre,P_precompound
           
    '      GAmod = cmul*(E_left - BFA)/(1/GA + cmul*(E_left - BFA))
    '      GBmod = cmul*(E_left - BFB)/(1/GB + cmul*(E_left - BFB))  

           GAmod = exp(cmul*(E_left - BFANP))/(1/GA + exp(cmul*(E_left - BFANP)))
                  ' empirical fit             
           FtunnA = 1/(1+exp(-(E_left-BFA)/Tequi)) 
           GAmod = GAmod / FtunnA   
           GBmod = exp(cmul*(E_left - BFBNP))/(1/GB + exp(cmul*(E_left - BFBNP)))
                  ' empirical fit             
           FtunnB = 1/(1+exp(-(E_left-BFB)/Tequi))
           GBmod = GBmod / FtunnB  
                  
 ' GAmod = GA
 ' GBmod = GB       

           T_f = U_Temp2(Z_left,A_left,E_left-BFMNP,DUF,0,Tscale,Econd)
           T_f_low = U_Temp2(Z_left,A_left,E_left-BFMNP-4*T_f,DUF,0,Tscale,Econd)
           T_f_eff = sqr(T_f * T_f_low)
           G = GAmod * exp((BFANP-BFMNP)/T_f_eff) + GBmod * exp((BFBNP-BFMNP)/T_f_eff) 
           ypsilon = 1.0 - P_Z_CN^2 / (A_left * 50.0)  ' 1 - fissility
           ypsilon = max(ypsilon,0.1)   ' limitation for very heavy nuclei
           l_lim_2 = 15.0^2 / (ypsilon/0.28) * T_f_eff / TEgidy(P_A_CN,DUF,1.0)  ' Hasse & Myers
           Frot = exp( P_I_rms_CN^2/l_lim_2 )  ' enhancement of GF by angular momentum
           GF = T_f_eff / (G * exp(BFMNP/T_f_eff)) * Frot

            ' Low level density below the pairing gap          
           If Z_left Mod 2 < 0.5 And A_left Mod 2 < 0.5 Then
'  Print "I, E-BFM ";I,E_left - BFM         
'  Print "   GF, GG, Pf vorher  ";GF,Ggamma,GF / (GF + GNPtot+Ggamma)           
             If E_left - BFM < 2 * 12 / sqr(A_left) Then
               GF = GF * Max(0.01 + 0.99*(Abs( (E_left-BFM) / (2*12/sqr(A_left) )) ),0.1  )
             End If
'  Print "   GF, GG, Pf nachher ";GF,Ggamma,GF / (GF + GNPtot+Ggamma)              
           End If       
         
           Tm = U_temp(Z_left,A_left,E_left,1,0,Tscale,Econd)       ' emitting nucleus 
           Ggamma = 0.624 * A_left^1.6 * Tm^5    ' in meV (Ignatyuk, Bologna)
           Ggamma = Ggamma * 1.E-9        ' in MeV

   ' A way to switch off the gammas: (Ggamma = 0 does not work!)
   ' If Ggamma/(GF + GNPtot) < 0.5 Then Ggamma = 0

           Gtot = GF + GNPtot + Ggamma
           
           Grandom = Rnd * Gtot
           If Grandom < Ggamma Then   ' gamma decay, no other decay any more
             ' Provisional treatment: 
             ' One gamma per reaction accumulated.
             ' (Justification: Probability for gamma emission is small.
             ' It would change the observables of fission
             ' and particle emission only little. 
             Pfis = GF / (GF + GNPtot)
             Eg = P_Egamma_high(Z_left,A_left,E_left)   
  '  Print "        Gamma decay, Eleft, Pgamma, Eg =";E_left,Ggamma/Gtot,Eg       
             E_left = E_left - Eg
             If Pfis < 0.01 Then Exit For ' Later fission is very unprobable
             Pfis = 0
             Goto Aftergamma
           Else 
             Pfis = GF / (GF + GNPtot)
           End if   
  '   Print "Z,A,E_left ";Z_left;" ";A_left;" ";E_left, "Pfis ";Pfis
           
            
'If Z_loss = 0 Then
 ' Print A_loss;" ";E_left;" "; GN ;" "; GP ; " "; GNPpre;" "; Gtot;" ";GF;" ";Pfis    
'End If
'sleep

 '  If J < 10 Then 
 '    Print "Pf (first chance) = ";Round(Pfis,2);", GN/GF = "; _
 '                     ROUND(GN/GF,3);", GN,GF:";ROUND(GN,3);" ";ROUND(GF,3)
 '    Print "Ftunn = ";Round(Ftunn,3);" Eleft = ";E_left;", G = ";G;", E100keV = ";E100keV                 
 '    Print "SN = ";SN;", BFld = ";BFLD;", BF = ";BF;", EA =";BFA;", EB = ";BFB
 '    Print A_left,E_left,Pfis          
 '    J = J + 1
 '  End If                     

           If Rnd <= Pfis Then    ' fission occurs, no more particles emitted before fission
 ' Print "   Fission, N_loss *** ";N_loss;" ***"          
 ' Print "Fission, Z_loss, N_loss,I_emit_mem";" ";Z_loss;" ";N_loss;" ";Oct(I_emit_mem)  
 ' Print  
' Print "Fission: Z_loss, N_loss, E_left ";Z_loss;" ";N_loss;" ";E_left     
             If Eg > 0 Then ' Accumulate pre-fission gammas
' Print "Fission after gamma decay, Eleft, Eg = ";E_left, Cint(1000*Eg)            
               EgammaCN(Int(1000*Eg)) = EgammaCN(Int(1000*Eg)) + 1
               Eg = 0
             End If    

             E100keV = CInt(10*E_left)
             If N_loss <= Ubound(E_multi_chance,1) And _
               Z_loss <= Ubound(E_multi_chance,2) And _
               E100keV <= Ubound(E_multi_chance,3) Then
               E_multi_chance(N_loss,Z_loss,E100keV) = E_multi_chance(N_loss,Z_loss,E100keV) + 1
              ' Store the excitation energy at fission
             End If  
  
             ' Fill spectra EnCN and EpCN
             Dim C_test As String
             For J = 1 To A_loss
               If J <= Ubound(EnCN_mem,1) Then
                 C_test = Mid(Oct(I_emit_mem),J,1)
                 If C_test = "1" Or C_test = "3" Then   ' Only neutrons
                   EnCN(EnCN_mem(J)) = EnCN(EnCN_mem(J)) + 1
                 End If  
                 If C_test = "2" Or C_test = "4" Then   ' Only protons
                   EpCN(EnCN_mem(J)) = EpCN(EpCN_mem(J)) + 1
                 End If  
               End If
             Next J
                           
'             Select Case I   ' A_loss = number of pre-fission particles
             Select Case A_loss
               Case 1
                 J = Ubound(En_multi_1) + 1
                 Redim Preserve En_multi_1(J)
                 En_multi_1(J) = E100keV _         ' E* at fission
                                  + En_multi_mem(1) SHL 10   ' E_particle pre-fission
                 Redim Preserve I_emit_1(J)
                 I_emit_1(J) = I_emit_mem 
               Case 2
                 J = Ubound(En_multi_2) + 1
                 Redim Preserve En_multi_2(J)
                 En_multi_2(J) = E100keV _
                                  + En_multi_mem(1) SHL 10 _
                                  + En_multi_mem(2) SHL 20 
                 Redim Preserve I_emit_2(J)
                 I_emit_2(J) = I_emit_mem 
               Case 3
                 J = Ubound(En_multi_3) + 1
                 Redim Preserve En_multi_3(J)
                 En_multi_3(J) = E100keV _
                                   + En_multi_mem(1) SHL 10 _
                                   + En_multi_mem(2) SHL 20 _
                                   + En_multi_mem(3) SHL 30
                 Redim Preserve I_emit_3(J)
                 I_emit_3(J) = I_emit_mem 
               Case 4
                 J = Ubound(En_multi_4) + 1
                 Redim Preserve En_multi_4(J)
                 En_multi_4(J) = E100keV _
                                   + En_multi_mem(1) SHL 10 _
                                   + En_multi_mem(2) SHL 20 _
                                   + En_multi_mem(3) SHL 30 _
                                   + En_multi_mem(4) SHL 40
  '                Ilong = Fix(En_multi_4(J) * 2^-10)              
                 Redim Preserve I_emit_4(J)
                 I_emit_4(J) = I_emit_mem 
               Case 5 
                 J = Ubound(En_multi_5) + 1
                 Redim Preserve En_multi_5(J)
                 En_multi_5(J) = E100keV _
                                   + En_multi_mem(1) SHL 10 _
                                   + En_multi_mem(2) SHL 20 _
                                   + En_multi_mem(3) SHL 30 _
                                   + En_multi_mem(4) SHL 40 _
                                   + En_multi_mem(5) SHL 50              
                 Redim Preserve I_emit_5(J)
                 I_emit_5(J) = I_emit_mem 
               Case 6 
                 J = Ubound(En_multi_6) + 1
                 Redim Preserve En_multi_6(J)
                 En_multi_6(J) = E100keV _
                                   + En_multi_mem(1) SHL 10 _
                                   + En_multi_mem(2) SHL 20 _
                                   + En_multi_mem(3) SHL 30 _
                                   + En_multi_mem(4) SHL 40 _
                                   + En_multi_mem(5) SHL 50 _
                                   + En_multi_mem(6) SHL 58
   '  Print  En_multi_mem(6), Modulo(Fix(En_multi_6(J)*2^-58),2^8)
                 Redim Preserve I_emit_6(J)
                 I_emit_6(J) = I_emit_mem  
               Case Else
             End Select 
             Imulti = Imulti + 1
             If I > 0 Then
               Inofirst = Inofirst + 1
             End If
             Exit For
           Else
 ' Print "   No fission" 
             Repeat_multi:
             If P_precompound > 0 Then
               If RND < P_precompound Then
                 If RND < Z_left / A_left Then  ' pre-equilibrium proton emitted
                   I_Exciton = I_pre_last + Int(11.E0*RND)
                   I_pre_last = I_Exciton
            '       Ep_kin = PPower_Griffin_v(I_Exciton+1,E_Left-SP,BP-SP)  ' sample proton kinetic energy
                   Ep_kin = PPower_Griffin_v(I_Exciton+1,E_left-BP,0) - SP + BP
                   If Ep_kin > 0 And Ep_kin > (BP-SP) and E_left - SP - Ep_kin > 0 Then
 '        Print "Pre-CN: EP = ",Ep_kin   
                     E_left = E_left - SP - Ep_kin
                     Z_left = Z_left - 1
                     A_left = A_left - 1
                     I_emit_mem = I_emit_mem + 2*8^(10-I-1)  ' marks a pre-CN proton
                     EpCN_mem(I+1) = Int(Ep_kin * 1000)
                     En_multi_mem(I+1) = CInt(10*Ep_kin)
                     If En_multi_mem(I+1) > 1023 Then
                       Print "<E> Pre-fission proton energy > 103 MeV!"
                       En_multi_mem(I+1) = 1023
                     End If                     
                   Else
                     P_precompound = 0
                   End If
                 Else                            ' pre-equilibrium neutron emitted
                   I_Exciton = I_pre_last + Int(11.E0*RND)
                   I_pre_last = I_Exciton
                   En_kin = PPower_Griffin_v(I_Exciton+1,E_left-SN,0)  ' sample neutron kinetic energy
           '        En_kin = PPower_Griffin_v(I_Exciton+1,E_left,0) - SN
                   If En_kin > 0 And E_left - SN - En_kin > 0 Then
  '       Print "Pre-CN: EN = ",En_kin         
                     E_left = E_left - SN - En_kin
                     A_left = A_left - 1
                     I_emit_mem = I_emit_mem + 1*8^(10-I-1)  ' marks a pre-CN neutron
                     EnCN_mem(I+1) = Int(En_kin * 1000)
                     En_multi_mem(I+1) = CInt(10*En_kin)
                     If En_multi_mem(I+1) > 1023 Then
                       Print "<E> Pre-fission neutron energy > 103 MeV!"
                       En_multi_mem(I+1) = 1023
                     End If
                   Else
                     P_Precompound = 0
                   End If  
                 End If
               Else
                 P_Precompound = 0
               End If      
             End If
 'Print "After possible PreCN decay: P_Precompound",P_Precompound            
             If P_Precompound = 0 Then
               IPcnt = 0
               B_pre = 0   ' No pre-equilibrium emission after CN decay
               If RND < GP / (GN + GP) Then  ' proton evaporation
                 Tmax = U_Temp(Z_left-1,A_left-1,E_left-SP,1,1,Tscale,Econd)
              REpkin:   
                 Ep_kin = PMaxwell(Tmax)
                 Tfinal = U_Temp(Z_left-1,A_left-1,E_left-SP-Ep_kin,1,1,Tscale,Econd)
                 If E_left-Ep_kin-Tmax > 10.E0 Then  ' Fermi gas regime
                   If RND > sqr(Exp(Ep_kin/Tmax)/Exp(Ep_kin/Tfinal)) Then Goto REpkin
                 End If                 
                 If E_left - SP - Ep_kin > 0 Then
                   IPcnt = 1
     ' Print "CN: EP = ",EP_kin          
                   E_left = E_left - SP - Ep_kin
                   A_left = A_left - 1
                   Z_left = Z_left - 1
                   I_emit_mem = I_emit_mem + 4*8^(10-I-1)  ' marks a CN proton
                   EpCN_mem(I+1) = Int(Ep_kin * 1000)
       '           If E_left < 0 Then Goto Repeat_multi 
                   En_multi_mem(I+1) = CInt(10*Ep_kin)
                   If En_multi_mem(I+1) > 1023 Then
                     Print "<E> Pre-fission proton energy > 103 MeV!"
                     En_multi_mem(I+1) = 1023
                   End If
        '        Else Exit For  
                 End If
               End If  
               If IPcnt = 0 Then  ' neutron evaporation
                 INcnt = 0
                 Tmax = U_Temp(Z_left,A_left-1,E_left-SN,1,1,Tscale,Econd)
              IENtry = 0
              REnkin:  
                 IENtry = IENtry + 1 
                 En_kin = PMaxwell(Tmax)
                 If IENtry <= 10 Then
                   If E_left - SN - En_kin <= 0 Then Goto REnkin
                 End If
                 Tfinal = U_Temp(Z_left,A_left-1,E_left-SN-En_kin,1,1,Tscale,Econd)
                 If E_left-En_kin-Tmax > 10.E0 Then  ' Fermi gas regime
                   If RND > sqr(Exp(En_kin/Tmax)/Exp(En_kin/Tfinal)) Then Goto REnkin
                 End If
                 If E_left - SN - En_kin > 0 Then
                   INcnt = 1
   '  Print "CN: EN = ",En_kin          
                   E_left = E_left - SN - En_kin
                   A_left = A_left - 1
                   I_emit_mem = I_emit_mem + 3*8^(10-I-1)  ' marks a CN neutron
       '           If E_left < 0 Then Goto Repeat_multi 
                   If I+1 <= Ubound(En_multi_mem,1) Then
                     EnCN_mem(I+1) = Int(En_kin * 1000)
                     En_multi_mem(I+1) = CInt(10*En_kin)
                     If En_multi_mem(I+1) > 1023 Then
                       Print "<E> Pre-fission neutron energy > 103 MeV!"
                       En_multi_mem(I+1) = 1023
                     End If
                   Else
                     Print "<E> Index of En_multi_mem = ";I+1  
                   End If
        '        Else Exit For  
                 End If
                 If INcnt = 0 Then Exit For
               End If  
             End If
           End If
' Print "Event finished"
' Print           
         Next
         K = K + 1
         If K >= N_multi_sample Then
           If Imulti = 0 Then
             Print " Multi-chance fission cannot be determined due to low fission probability."
             Print " No fission event found."
             Print " 100% first-chance fission assumed!"        
           End If
         End If  
         If K >= N_multi_sample Then
           If Imulti > 0 Then           
             Print " Fission chances deduced from ";Imulti;" fission events."
             If Imulti < 100 Then
               Print " Determined fission chances are subject to large statistical uncertainties."
             End If
           End If           
           Print " "
           Exit Do
         End If
'Sleep         
       Loop   
     End Scope



   End If  ' IF Eabsgs < ... (Multi-chance fission)
   
   
   /' Normalize E_multi_chance array to one '/
   For I = 0 To Ubound(W_chances,1)
     For J = 0 To Ubound(W_chances,2)
       W_chances(I,J) = 0
     Next  
   Next
   If Inofirst > 0 Then  ' Inofirst is the number of multi-chance fission events (not first-chance!)
     For I = 0 To Ubound(E_multi_chance,1)   ' number of pre-fission neutrons
       For J = 0 To Ubound(E_multi_chance,2)  ' number of pre-fission protons
         For K = 0 To Ubound(E_multi_chance,3) ' excitation energy in units of 100 keV
           If E_multi_chance(I,J,K) > 0 Then
            E_multi_chance(I,J,K) = E_multi_chance(I,J,K) / Imulti
                    ' Imulti = total number of fission events for multi-chance fission
            W_chances(I,J) = W_chances(I,J) + E_multi_chance(I,J,K)
           End If
         Next K 
       Next J
     Next I
     Print "Probabilities of fission chances (pre-fission neutrons and protons) "
     Print "(Z_CN = ";P_Z_CN;", A_CN = ";P_A_CN;", E = ";P_E_exc;" MeV) :"
     Print " neutrons    protons    probability"
     For I = 0 To Ubound(W_chances,1)
       For J = 0 To Ubound(W_chances,2)
         If W_chances(I,J) > 0 Then
           Print I,J,Round(W_chances(I,J),3)
         End If
       Next J 
     Next I 
     Print 
   End If
   End If  ' If Emode < 3   


   If CFileoutlmd <> "" Then
     foutlmd = freefile
     Open Cfileoutlmd_full For Append As #foutlmd   
     Print #foutlmd,"* ";
     If PZ1 = 1 Then Print #foutlmd,"Z1 ";
     If PZ2 = 1 Then Print #foutlmd,"Z2 ";
     If PA1pre = 1 Then Print #foutlmd,"A1pre ";
     If PA2pre = 1 Then Print #foutlmd,"A2pre ";
     If PA1post = 1 Then Print #foutlmd,"A1post ";
     If PA2post = 1 Then Print #foutlmd,"A2post ";
     If PI1 = 1 Then Print #foutlmd,"I1pre ";
     If PI2 = 1 Then Print #foutlmd,"I2pre ";
     If PI1 = 1 Then Print #foutlmd,"I1gs ";
     If PI2 = 1 Then Print #foutlmd,"I2gs ";
     If PXE = 1 Then Print #foutlmd,"Eexc1 Eexc2 ";
     If Pn1 = 1 Then Print #foutlmd,"n1 ";
     If Pn2 = 1 Then Print #foutlmd,"n2 ";
     If PTKEpre = 1 Then Print #foutlmd,"TKEpre ";
     If PTKEpost = 1 Then Print #foutlmd,"TKEpost ";
     If PnCN = 1 And Inofirst > 0 Then 
       Print #foutlmd,"E@fission ";
       Print #foutlmd,"particle-list E1(CN) E2(CN) E3(CN) ... ";
     End If  
     Print #foutlmd," "
     
     If PZ1 = 1 Then Print #foutlmd,"* Z1: Atomic number of first fragment"
     If PZ2 = 1 Then Print #foutlmd,"* Z2: Atomic number of second fragment"
     If PA1pre = 1 Then Print #foutlmd,"* A1pre: Pre-neutron mass number of first fragment"
     If PA2pre = 1 Then Print #foutlmd,"* A2pre: Pre-neutron mass number of second fragment"
     If PA1post = 1 Then Print #foutlmd,"* A1post: Post-neutron mass number of first fragment"
     If PA2post = 1 Then Print #foutlmd,"* A2post: Post-neutron mass number of second fragment"
     If PI1 = 1 Then Print #foutlmd,"* I1pre: Spin of first fragment after scission"
     If PI2 = 1 Then Print #foutlmd,"* I2pre: Spin of second fragment after scission"
     If PI1 = 1 Then Print #foutlmd,"* I1gs: Ground-state spin of first fragment"
     If PI2 = 1 Then Print #foutlmd,"* I2gs: Ground-state spin of second fragment"
     If PXE = 1 Then Print #foutlmd,"* Eexc1: Excitation energy of first fragment [MeV]"
     If PXE = 1 Then Print #foutlmd,"* Eexc2: Excitation energy of second fragment [MeV]"
     If Pn1 = 1 Then Print #foutlmd,"* n1: Prompt neutrons emitted from first fragment"
     If Pn2 = 1 Then Print #foutlmd,"* n2: Primpt neutrons emitted from second fragment"
     If PTKEpre = 1 Then Print #foutlmd,"* TKEpre: Pre-neutron total kinetic energy [MeV]"
     If PTKEpost = 1 Then Print #foutlmd,"* TKEpost: Post-neutron total kinetic energy [MeV]"
     If PnCN = 1 And Inofirst > 0 Then 
       Print #foutlmd,"* E@fission: Excitation energy at fission [MeV]"
       Print #foutlmd,"* Key number for kind of pre-fission particles (1,3: neutrons, 2,4: protons, 1,2: pre-compound)"
       Print #foutlmd,"* E1(CN) ...: kinetic energies of pre-fission particles (first 5 only) [MeV]"
     End If  
     If PEnpost = 1 Then
       Print #foutlmd," "
       Print #foutlmd,"* In separate lines: Prompt post-scission neutrons (including acceleration phase)"
       Print #foutlmd,"*  0  E1, cos(theta1), phi1, E2, cos(theta2), phi2, E3, cos(theta3, phi3, ...: "
       Print #foutlmd,"*    Energies [MeV] in lab. frame and angles vs. direction of light fragment of all post-scission neutrons"
       Print #foutlmd,"*  1  E1l, E2l, E3l, ...: Energies [MeV] of neutrons emitted from light fragment in frame of light fragment"
       Print #foutlmd,"*  2  E1h, E2h, E3h, ...: Energies [MeV] of neutrons emitted from heavy fragment in frane of heavy fragment"
     End If    
     If PEgpost = 1 Then
       Print #foutlmd,"* In separate lines: Prompt post-scission gammas"
       Print #foutlmd,"*  3  E1l, E2l, E3l, ...: Energies [MeV] of gammas emitted from light fragment in competition with neutrons"        
       Print #foutlmd,"*  4  E1l, E2l, E3l, ...: Energies [MeV] of statistical gammas emitted from light fragment after neutron emission"        
       Print #foutlmd,"*  5  E1l, E2l, E3l, ...: Energies [MeV] of collective gammas emitted from light fragment"        
       Print #foutlmd,"*  6  E1h, E2h, E3h, ...: Energies [MeV] of gammas emitted from heavy fragment in competition with neutrons"        
       Print #foutlmd,"*  7  E1h, E2h, E3h, ...: Energies [MeV] of statistical gammas emitted from heavy fragment after neutron emission"        
       Print #foutlmd,"*  8  E1h, E2h, E3h, ...: Energies [MeV] of collective gammas emitted from heavy fragment"        
     End If
     If Imulti > 1 Then
       Print #foutlmd," "
       Print #foutlmd,"* Note: The sequence of the events in the list-mode output is sorted" 
       Print #foutlmd,"*       in the case of multi-chance fission in order to save computing time"
       Print #foutlmd,"*       (first-chance fission appears first)."
     End If   
     Print #foutlmd," "
   End If
  

   B_Error_Analysis = B_Error_On
 



  /' Here the calculation of the fission process starts '/
  
  calcstart:
  ' Start point of the loop for the error analysis. 
  ' Several calculations with perturbed parameter sets and
  ' finally one calculation with the nominal parameter set are performed.
  
   #include "CLEARspectra.bas" 
   Redim As Longint Mode_Events(10)  ' Reset counter of mode events

   If CFileoutlmd <> "" Then  
     If B_Error_Analysis = 1 Then
       If Brec Then Print #foutlmd,"* Calculation with perturbed model parameters"
     End If
     If B_Error_Analysis = 0 Then
       Print #foutlmd,"* Calculation with nominal model parameters"
     End If
   End If     
 
   Dim As LongInt NEVTused,NEVTtot,IEVTtot
   Dim As Single Racc
   Dim As LongInt ILoop
'  Dim As Integer I_Energy_loop
   If Fenhance < 1 Then Fenhance = 1   
   If B_Error_Analysis = 1 Then
     N_Error_Max = Int(sqr(Fenhance * 100.0)) 
      ' Increase the number of perturbed-parameter sets to 10*sqr(Fenhance)
     NEVTtot = Fenhance * Ten_to_five / N_Error_Max  
      ' Increase statistics for calculation with one set of perturbed parameters
      ' approximately with sqr(Fenhance), while fixing the total number of calculations
      ' to Ten_to_five * Fenhance
     Print "Calculation with perturbed model parameters."
  /' If Emode = 3 Then
       Print Cfileout, ", energy distribution from Espectrum.in"
     Else
       Print Cfileout,P_E_exc;" MeV"
     End If '/ 
   Else
     NEVTtot = Fenhance * Ten_to_five
     Print "Calculation with nominal model parameters."
  /' If Emode = 3 Then
       Print Cfileout, ", energy distribution from Espectrum.in"
     Else
       Print Cfileout,P_E_exc;" MeV"
     End If '/  
   End If
   Print NEVTtot;" events will be calculated."      
   Racc = 100.E0 / NEVTtot  ' Normalization of spectra

 
 
 
  /' Variation of model parameters for error analysis '/
  
   Delta_S0 = _Delta_S0  
   
   If B_Error_Analysis = 1 Then 
  
     If I_Double_Covar = 1 Then  ' Mode for single system or for passage of first system
     ' set values to perturbed parameters
       P_DZ_Mean_S1 = PGauss(_P_DZ_Mean_S1,Var_P_DZ_Mean_S1)
       P_DZ_Mean_S2 = PGauss(_P_DZ_Mean_S2,Var_P_DZ_Mean_S2)
       P_DZ_Mean_S3 = PGauss(_P_DZ_Mean_S3,Var_P_DZ_Mean_S3)
       P_DZ_Mean_S4 = PGauss(_P_DZ_Mean_S4,Var_P_DZ_Mean_S4)
       P_Z_Curv_S1 = PGauss(_P_Z_Curv_S1,Var_P_Z_Curv_S1)
       P_Z_Curv_S2 = PGauss(_P_Z_Curv_S2,Var_P_Z_Curv_S2)
       P_A_Width_S2 = PGauss(_P_A_Width_S2,Var_P_A_Width_S2)
       P_Z_Curv_S3 = PGauss(_P_Z_Curv_S3,Var_P_Z_Curv_S3)
       P_Z_Curv_S4 = PGauss(_P_Z_Curv_S4,Var_P_Z_Curv_S4)
       Delta_S0 = PGauss(_Delta_S0,Var_Delta_S0)  
       P_Shell_S1 = PGauss(_P_Shell_S1,Var_P_Shell_S1)
       P_Shell_S2 = PGauss(_P_Shell_S2,Var_P_Shell_S2)
       P_Shell_S3 = PGauss(_P_Shell_S3,Var_P_Shell_S3)
       P_Shell_S4 = PGauss(_P_Shell_S4,Var_P_Shell_S4)
       T_low_SL = PGauss(_T_low_SL,Var_T_low_SL)
       T_low_S1 = PGauss(_T_low_S1,Var_T_low_S1)
       T_low_S2 = PGauss(_T_low_S2,Var_T_low_S2)
       T_low_S3 = PGauss(_T_low_S3,Var_T_low_S3)
       T_low_S4 = PGauss(_T_low_S4,Var_T_low_S4)
       P_att_pol = _P_att_pol
      ' Delta_NZ_Pol = 78.26 - 50/P_Z_CN * (P_A_CN - P_Z_CN) ' relative to 236U
      ' P_Shell_S1 = P_Shell_S1 * (1 - PGauss(0,Var_P_Att_Pol) * Delta_NZ_Pol)
       HOMPOL = PGauss(_HOMPOL,Var_HOMPOL)
       POLARadd = PGauss(_POLARadd,Var_POLARadd)  
       Jscaling = PGauss(_Jscaling,Var_Jscaling)
    
       If B_Double_Covar = 1 Then  ' Passage for first fissionning system
         If I_Error = 0 Then 
         ' Allocate array for storing the sequence of perturbed parameters
           ReDim As Single Double_P_DZ_Mean_S1(N_Error_Max)
           ReDim As Single Double_P_DZ_Mean_S2(N_Error_Max)
           ReDim As Single Double_P_DZ_Mean_S3(N_Error_Max)
           ReDim As Single Double_P_DZ_Mean_S4(N_Error_Max)
           ReDim As Single Double_P_Z_Curv_S1(N_Error_Max)
           ReDim As Single Double_P_Z_Curv_S2(N_Error_Max)
           ReDim As Single Double_P_A_Width_S2(N_Error_Max)
           ReDim As Single Double_P_Z_Curv_S3(N_Error_Max)
           ReDim As Single Double_P_Z_Curv_S4(N_Error_Max)
           ReDim As Single Double_Delta_S0(N_Error_Max)
           ReDim As Single Double_P_Shell_S1(N_Error_Max)
           ReDim As Single Double_P_Shell_S2(N_Error_Max)
           ReDim As Single Double_P_Shell_S3(N_Error_Max)
           ReDim As Single Double_P_Shell_S4(N_Error_Max)
           ReDim As Single Double_T_low_S1(N_Error_Max)
           ReDim As Single Double_T_low_S2(N_Error_Max)
           ReDim As Single Double_T_low_S3(N_Error_Max)
           ReDim As Single Double_T_low_S4(N_Error_Max)
           ReDim As Single Double_T_low_SL(N_Error_Max)
           ReDim As Single Double_P_att_pol(N_Error_Max)
           ReDim As Single Double_HOMPOL(N_Error_Max)
           ReDim As Single Double_POLARadd(N_Error_Max)  
           ReDim As Single Double_Jscaling(N_Error_Max)
         End If
      
        ' Store perturbed parameter values for calculation with second fissioning system:
         Double_P_DZ_Mean_S1(I_Error) = P_DZ_Mean_S1
         Double_P_DZ_Mean_S2(I_Error) = P_DZ_Mean_S2
         Double_P_DZ_Mean_S3(I_Error) = P_DZ_Mean_S3
         Double_P_DZ_Mean_S4(I_Error) = P_DZ_Mean_S4
         Double_P_Z_Curv_S1(I_Error) = P_Z_Curv_S1
         Double_P_Z_Curv_S2(I_Error) = P_Z_Curv_S2
         Double_P_A_Width_S2(I_Error) = P_A_Width_S2
         Double_P_Z_Curv_S3(I_Error) = P_Z_Curv_S3
         Double_P_Z_Curv_S4(I_Error) = P_Z_Curv_S4
         Double_Delta_S0(I_Error) = Delta_S0
         Double_P_Shell_S1(I_Error) = P_Shell_S1
         Double_P_Shell_S2(I_Error) = P_Shell_S2
         Double_P_Shell_S3(I_Error) = P_Shell_S3
         Double_P_Shell_S4(I_Error) = P_Shell_S4
         Double_T_low_S1(I_Error) = T_low_S1
         Double_T_low_S2(I_Error) = T_low_S2
         Double_T_low_S3(I_Error) = T_low_S3
         Double_T_low_S4(I_Error) = T_low_S4
         Double_T_low_SL(I_Error) = T_low_SL
         Double_P_att_pol(I_Error) = P_att_pol
         Double_HOMPOL(I_Error) = HOMPOL
         Double_POLARadd(I_Error) = POLARadd  
         Double_Jscaling(I_Error) = Jscaling
      
       End If
    
     End If
     If I_Double_Covar = 2 Then  ' Passage for second fissioning system
       ' Use parameter values of first fissioning system again
       P_DZ_Mean_S1 = Double_P_DZ_Mean_S1(I_Error)  
       P_DZ_Mean_S2 = Double_P_DZ_Mean_S2(I_Error)  
       P_DZ_Mean_S3 = Double_P_DZ_Mean_S3(I_Error)  
       P_DZ_Mean_S4 = Double_P_DZ_Mean_S4(I_Error)  
       P_Z_Curv_S1 = Double_P_Z_Curv_S1(I_Error)  
       P_Z_Curv_S2 = Double_P_Z_Curv_S2(I_Error)  
       P_A_Width_S2 = Double_P_A_Width_S2(I_Error) 
       P_Z_Curv_S3 = Double_P_Z_Curv_S3(I_Error) 
       P_Z_Curv_S4 = Double_P_Z_Curv_S4(I_Error) 
       Delta_S0 = Double_Delta_S0(I_Error) 
       P_Shell_S1 = Double_P_Shell_S1(I_Error) 
       P_Shell_S2 = Double_P_Shell_S2(I_Error) 
       P_Shell_S3 = Double_P_Shell_S3(I_Error) 
       P_Shell_S4 = Double_P_Shell_S4(I_Error) 
       T_low_S1 = Double_T_low_S1(I_Error) 
       T_low_S2 = Double_T_low_S2(I_Error) 
       T_low_S3 = Double_T_low_S3(I_Error) 
       T_low_S4 = Double_T_low_S4(I_Error) 
       T_low_SL = Double_T_low_SL(I_Error) 
       P_att_pol = Double_P_att_pol(I_Error) 
       HOMPOL = Double_HOMPOL(I_Error) 
       POLARadd = Double_POLARadd(I_Error)   
       Jscaling = Double_Jscaling(I_Error)
     End If
    
   Else
/'<'/
    ' Use nominal parameter values:
     P_DZ_Mean_S1 = _P_DZ_Mean_S1
     P_DZ_Mean_S2 = _P_DZ_Mean_S2
     P_DZ_Mean_S3 = _P_DZ_Mean_S3
     P_DZ_Mean_S4 = _P_DZ_Mean_S4
     P_Z_Curv_S1 = _P_Z_Curv_S1
     P_Z_Curv_S2 = _P_Z_Curv_S2
     P_A_Width_S2 = _P_A_Width_S2
     P_Z_Curv_S3 = _P_Z_Curv_S3
     P_Z_Curv_S4 = _P_Z_Curv_S4
     Delta_S0 = _Delta_S0
     P_Shell_S1 = _P_Shell_S1
     P_Shell_S2 = _P_Shell_S2
     P_Shell_S3 = _P_Shell_S3
     P_Shell_S4 = _P_Shell_S4
     T_low_S1 = _T_low_S1
     T_low_S2 = _T_low_S2
     T_low_S3 = _T_low_S3
     T_low_S4 = _T_low_S4
     T_low_SL = _T_low_SL
     P_att_pol = _P_att_pol
     HOMPOL = _HOMPOL
     POLARadd = _POLARadd
     Jscaling = _Jscaling
  
/'>'/   
   End If  
   
' Print I_Double_Covar,I_Error, P_DZ_Mean_S1   


/'<'/
    Dim As Single R_E_exc_used
    R_E_exc_used = P_E_exc
    I_A_CN = P_A_CN
    I_Z_CN = P_Z_CN
/'>'/    
    
    IEVTtot = 0              ' Counts total number of processed events
    
    NEVTspectrum = 1         ' Default: only one energy

  ' Loop over array of initial excitation energy
  
    Dim As Integer N_E_distr,N_E_Multi,N_N_Multi,N_Z_Multi 
    Dim As Integer I_E_distr,I_E_Multi,I_N_Multi,I_Z_Multi  
    Dim As Integer I_A_Multi
    Dim As Integer I_N_Multi_Max = 0
    Dim As Integer I_Z_Multi_Max = 0
    N_E_distr = 1
    N_N_Multi = 0
    N_Z_Multi = 0
    N_E_Multi = 0
    If Emode = 3 Then  ' Energy distribution on input
      N_E_distr = Ubound(E_spectrum,1)
    End If
    If Inofirst > 0 Then  ' Multi-chance fission
      N_N_Multi = 10
      N_Z_Multi = 10
      N_E_Multi = 1000
    End If

    /'** Loop for energy distribution from file **'/
    For I_E_Distr = 1 To N_E_distr

    /'** Loop for Multi-chance fission (A and E) **'/

    For I_N_Multi = 0 To N_N_Multi
    For I_Z_Multi = 0 To N_Z_Multi
  '  For I_N_Multi = N_N_Multi To 0 Step -1
  '  For I_Z_Multi = N_Z_Multi To 0 Step -1
    I_A_Multi = I_N_Multi + I_Z_Multi
    For I_E_Multi = N_E_Multi To 0 Step -1

  Energy_loop:
    If Emode = 3 Then  ' Excitation-energy spectrum given by file
       R_E_exc_used = E_spectrum(I_E_distr)  ' energy value
       P_I_rms_CN = L_spectrum(I_E_distr)    ' spin value
       NEVTspectrum = W_spectrum(I_E_distr)  ' weight of spectrum
            ' (Integral over weight must be normalized to one!)
       Print "Energy: ";R_E_exc_used;", weight: ";NEVTspectrum     
    End If
    If Inofirst > 0 Then        ' Multi-chance fission
       R_E_exc_used = 0.1 * I_E_Multi - Eabsgs + P_E_Exc
           ' The meaning of R_E_exc_used depends on EMode! 
       I_A_CN = P_A_CN - I_N_Multi - I_Z_Multi
       I_Z_CN = P_Z_CN - I_Z_Multi
       NEVTspectrum =  E_multi_chance(I_N_Multi,I_Z_Multi,I_E_Multi) 
          /' E_multi_chance(*,*,*) is normalized to 1 '/
    End If   
    
    
   
    NEVTused = NEVTtot * NEVTspectrum 
    If NEVTused > 0 Then   ' avoid considering channels with W_spectrum( )=0

/'
Scope
  Static as Single Test = 0
  Test = Test + NEVTspectrum
Print "***";I_A_CN;I_Z_CN;I_E_Multi;NEVTspectrum,Test;IEVTtot; Ievttot + NEvtused    
End Scope
 '  Print Inofirst, R_E_exc_used, I_A_CN, NEVTused 
'/
 
      If I_N_Multi > I_N_Multi_Max Then I_N_Multi_Max = I_N_Multi
      If I_Z_Multi > I_Z_Multi_Max Then I_Z_Multi_Max = I_Z_Multi

   /' Shell effects for the symmetric fission channel '/
   ' (This works only properly if the shell at symmetry is known for all nuclei.
   ' Since this is not the case, it is safer to keep Delta_S0 of the 1. chance.)
  '  If I_A_Multi > 0 Then 
    ' get correct shell effect at symmetry for chance fission
  '    Delta_S0 = U_Delta_S0(I_Z_CN,I_A_CN)
  '  End If  
  
  
    /'*** Neutrons emitted between saddle and scission ***'/ 
      Dim As Single Escission_lim
    Scope
      Dim As Single ZsqrA_fis
       
      ZsqrA_fis = Csng(I_Z_CN^2) / Csng(I_A_CN)
 '     Escission_lim = 1150.0 * exp(-ZsqrA_fis/13.0) ' from PRC 86 (2012) 034605
      Escission_lim = 900.0 * exp(-ZsqrA_fis/13.0) ' from PRC 86 (2012) 034605
    End Scope  
    
   

/'<'/  

    /' Central Z values of fission modes '/

    /' Fit to positions of fission channels (Boeckstiegel et al., 2008) '/
    /' P_DZ_Mean_S1 and P_DZ_Mean_S2 allow for slight adjustments '/
    Scope
    Dim As Single R_Z_mod
      R_Z_mod = I_Z_CN
      ZC_Mode_0 = R_Z_mod * 0.5E0      /' Central Z value of SL mode '/
      ZC_Mode_1 = (53.0E0 - 51.5E0) / (1.56E0 - 1.50E0) * _
                   (R_Z_mod^1.3E0 / I_A_CN - 1.50E0) + 51.5E0 + P_DZ_Mean_S1
      ZC_Mode_2 = (55.8E0 - 54.5E0) / (1.56E0 - 1.50E0) * _
                   (R_Z_mod^1.3E0 / I_A_CN - 1.50E0) + 54.5E0 + P_DZ_Mean_S2
      ZC_Mode_3 = ZC_Mode_2 + 4.5E0 + P_DZ_Mean_S3
    '  ZC_Mode_4 = 38.5 + P_DZ_Mean_S4  ' structure in nuclei with A around 190 for 201Tl
    '  ZC_Mode_4 = 35.5 + P_DZ_Mean_S4  ' for 180Hg  ( 36.2 for 208Po )

  ' Do not delete these lines (,because this is a very good fit!):
  '    ZC_Mode_4 = 38.5 + (I_A_CN-I_Z_CN-110)*0.12 - (I_A_CN-I_Z_CN-110)^2 * 0.009 _
  '                - (I_Z_CN-77)*0.34 + P_DZ_Mean_S4 

      ZC_Mode_4 = 38.5 + (I_A_CN-I_Z_CN-110)*0.12 - (I_A_CN-I_Z_CN-110)^2 * 0.009 _
                  - (I_Z_CN-77)*0.34 + P_DZ_Mean_S4 
           ' assumption: mode position moves with Z and A (adjusted to exp. data
           ' of Itkis and Andreyev et al.

    End Scope

/'>'/
    P_Z_Mean_S1 = ZC_Mode_1  /' Copy to global parameter '/
    P_Z_Mean_S2 = ZC_Mode_2  /'             "            '/
    P_Z_Mean_S3 = ZC_Mode_3  /'             "            '/
    P_Z_Mean_S4 = ZC_Mode_4  /'             "            '/
/'<'/


    I_N_CN = I_A_CN - I_Z_CN
    /' Mean deformation at scission as a function of mass '/
    
    /' Mode 0: liquid drop and mode 4: Z = 38 '/
    beta1_prev = 0.3
    beta2_prev = 0.3
    beta1_opt = beta1_prev
    beta2_opt = beta2_prev
    For I = 10 to I_Z_CN - 10
      IZ1 = I
      Z1 = Csng(IZ1)
      IZ2 = I_Z_CN - IZ1
      Z2 = Csng(IZ2)
      A1 = Z1 / Csng(I_Z_CN) * Csng(I_A_CN)
      A2 = I_A_CN - A1

      Beta_Equi(A1,A2,Z1,Z2,dneck,beta1_prev,beta2_prev,beta1_opt,beta2_opt)

'Print "Mode 0, Z1,Z2,beta1,beta2 ";Z1;" ";Z2;" ";beta1_opt,beta2_opt
'Print Z1;" ";Z2;" ";beta1_opt,beta2_opt
      Beta(0,1,IZ1) = beta1_opt /' "light" fragment '/
      Beta(4,1,IZ1) = beta1_opt
      Beta(0,2,IZ2) = beta2_opt /' "heavy" fragment '/
      Beta(4,2,IZ2) = beta2_opt
      beta1_prev = beta1_opt
      beta2_prev = beta2_opt
      E_defo = Lymass(Z1,A1,beta1_opt) - Lymass(Z1,A1,0.0)
      Edefo(0,1,IZ1) = E_defo  /' "light" fragment '/
      Edefo(4,1,IZ1) = E_defo
      E_defo = Lymass(Z2,A2,beta2_opt) - Lymass(Z2,A2,0.0)
      Edefo(0,2,IZ2) = E_defo  /' "heavy" fragment '/
      Edefo(4,2,IZ2) = E_defo
    Next

    /' Mode 1: deformed shells (light) and spherical (heavy) '/
    For I = 10 to  I_Z_CN - 10
      Z1 = I
      Z2 = I_Z_CN - Z1
      A1 = (Z1 - 0.5E0) / Csng(I_Z_CN) * Csng(I_A_CN) /' polarization roughly considered '/
      A2 = I_A_CN - A1
      If I_Z_CN * 0.5 < ZC_Mode_1 Then
      ' Beta_opt_light(A1,A2,Z1,Z2,dneck,0,rbeta_ld)
        /' nu_mean of Cf requires shells in the light fragment: '/
        rbeta = beta_light(I,betaL0,betaL1) - 0.1 
                ' smaller than general deformation of light fragment   
                '        (less neck influence due to spherical heavy fragment)
        If rbeta < 0 Then rbeta = 0
      Else
        rbeta = beta_heavy(I,betaH0,betaH1)  ' equal to S2 channel
        if rbeta < 0 Then rbeta = 0
      End If
      Beta(1,1,I) = rbeta    /' "light" fragment '/
      E_defo = Lymass(Z1,A1,rbeta) - Lymass(Z1,A1,0.0)
      Edefo(1,1,I) = E_defo /' "light" fragment '/
    Next
    
    For I = 10 To I_Z_CN - 10
      rbeta = 0
      Beta(1,2,I) = rbeta
      Edefo(1,2,I) = 0   /' "heavy" fragment (at S1 shell) '/
    Next

    /' Mode 2: deformed shells (light and heavy) '/
    For I = 10 to I_Z_CN - 10
      Z1 = I
      Z2 = I_Z_CN - Z1
      A1 = (Z1 - 0.5E0) / Csng(I_Z_CN) * Csng(I_A_CN) /' polarization roughly considered '/
      A2 = I_A_CN - A1
      If I_Z_CN * 0.5 < ZC_Mode_2 Then
    ' Beta_opt_light(A1,A2,Z1,Z2,dneck,beta_heavy(Z2),rbeta_ld)
        rbeta = beta_light(I,betaL0,betaL1)   ' general deformation of light fragment
        If rbeta < 0 Then rbeta = 0  ' negative values replaced by 0
      Else
        rbeta = beta_heavy(I,betaH0,betaH1)  ' equal to S2 channel
      End If  
      Beta(2,1,I) = rbeta
      E_defo = Lymass(Z1,A1,rbeta) - Lymass(Z1,A1,0.0)
      Edefo(2,1,I) = E_defo
    Next
    For I = 10 To I_Z_CN - 10
      rbeta = beta_heavy(I,betaH0,betaH1)   /' "heavy" fragment (at S2 shell)'/
      If rbeta < 0 Then rbeta = 0  ' negative values replaced by 0  
      Beta(2,2,I) = rbeta
      Z1 = I
      A1 = (Z1 + 0.5E0) / I_Z_CN * I_A_CN /' polarization roughly considered '/
      E_defo = Lymass(Z1,A1,rbeta) - Lymass(Z1,A1,0.0)
      Edefo(2,2,I) = E_defo
    Next

    /' Mode 3 '/
    For I = 10 to I_Z_CN - 10
      Z1 = I
      Z2 = I_Z_CN - Z1
      A1 = (Z1 - 0.5E0) / Csng(I_Z_CN) * Csng(I_A_CN) /' polarization roughly considered '/
      A2 = I_A_CN - A1
      rbeta = beta_light(I,betaL0,betaL1) 
      rbeta = Max(rbeta-0.10,0.0)  /' for low nu-bar of lightest fragments '/
   '  Beta_opt_light(A1,A2,Z1,Z2,dneck,beta_heavy(Z2,betaH0,betaH1),rbeta)  
      Beta(3,1,I) = rbeta
      E_defo = Lymass(Z1,A1,rbeta) - Lymass(Z1,A1,0.0)
      Edefo(3,1,I) = E_defo
    Next
    For I = 10 To I_Z_CN - 10
      rbeta = beta_heavy(I,betaH0,betaH1) + 0.2   /' for high nu-bar of heaviest fragments '/
      If rbeta < 0 Then rbeta = 0
      Beta(3,2,I) = rbeta
      Z1 = I
      A1 = (Z1 + 0.5E0) / Csng(I_Z_CN) * Csng(I_A_CN) /' polarization roughly considered '/
      E_defo = Lymass(Z1,A1,rbeta) - Lymass(Z1,A1,0.0)
      Edefo(3,2,I) = E_defo
    Next

    /' Mode 5: (Channel ST1 in both fragments) '/
    For I = 10 To I_Z_CN - 10
      Z1 = I
      Z2 = I_Z_CN - Z1
      rbeta = Beta(1,2,I)
      if rbeta < 0 Then rbeta = 0
      Beta(5,1,Int(Z1)) = rbeta
      Beta(5,2,Int(Z1)) = rbeta
    Next

    /' Mode 6: (Channel ST2 in both fragments) '/
    For I = 10 To I_Z_CN - 10
      Z1 = I
      Z2 = I_Z_CN - Z1
      rbeta = Beta(2,2,I)
      if rbeta < 0 Then rbeta = 0
      Beta(6,1,Int(Z1)) = rbeta
      Beta(6,2,Int(Z1)) = rbeta
    Next


    /' Mean Z as a function of mass '/

    /' Mode 0 '/
    For I = 10 To I_A_CN - 10
      ZUCD = Csng(I) / Csng(I_A_CN) * Csng(I_Z_CN)
      beta1 = Beta(0,1,Int(ZUCD + 0.5))
      beta2 = Beta(0,2,Int(I_Z_CN - ZUCD + 0.5))
      Z1 = Z_equi(I_Z_CN,I, I_A_CN - I, beta1, beta2, dneck,0,_
               0.0,1.0)
      Zmean(0,1,I) = Z1
      Zshift(0,1,I) = Z1 - ZUCD
      Zmean(0,2,I_A_CN - I) = I_Z_CN - Z1
      Zshift(0,2,I_A_CN - I) = ZUCD - Z1
    Next

    /' Mode 1 '/
    For I = 10 To I_A_CN - 10
      ZUCD = Csng(I) / Csng(I_A_CN) * Csng(I_Z_CN)
      Z = ZUCD + ZPOL1 /' Charge polarisation is considered in a crude way '/
      beta1 = Beta(1,1,CInt(Z)) /' "light" fragment '/
      Z = ZUCD - ZPOL1
      beta2 = Beta(1,2,CInt(I_Z_CN-Z)) /' "heavy" fragment  at S1 shell '/
      If Csng(I_Z_CN) * 0.5 < ZC_Mode_1 Then
        Z1 = Z_equi(I_Z_CN,I, I_A_CN - I, beta1, beta2, dneck,1,_
            POLARadd,POLARfac)
      Else
        Z1 = Z_equi(I_Z_CN,I, I_A_CN - I, beta1, beta2, dneck,1,0.0,0.0)
      End If      
      Z1 = Z1 + ZPOL1  /' Charge polarization by shell '/

      If I_Z_CN - Z1 < 50 And (I_Z_CN - Z1) > Z1 Then
        Z1 = I_Z_CN - 50    /' Z of mean heavy fragment not below 50 '/
      EndIf

      Zmean(1,1,I) = Z1
      Zshift(1,1,I) = Z1 - ZUCD     ' neutron-deficient
      Zmean(1,2,I_A_CN - I) = I_Z_CN - Z1
      Zshift(1,2,I_A_CN - I) = ZUCD - Z1  ' neutron rich at shell
    Next

    /' Mode 2 '/
    For I = 10 To I_A_CN - 10
      ZUCD = Csng(I) / Csng(I_A_CN) * Csng(I_Z_CN)
      Z = ZUCD /' Charge polarisation is here neglected '/
      beta1 = Beta(2,1,CInt(Z))
      beta2 = Beta(2,2,CInt(I_Z_CN-Z))
      If Csng(I_Z_CN) * 0.5 < ZC_Mode_2 Then
        Z1 = Z_equi(I_Z_CN,I, I_A_CN-I, beta1, beta2, dneck,2, _
               POLARadd,POLARfac)
      Else
        Z1 = Z_equi(I_Z_CN,I, I_A_CN-I, beta1, beta2, dneck,2,0.0,0.0)
      End If      
      
      Zmean(2,1,I) = Z1
      Zshift(2,1,I) = Z1 - ZUCD        ' neutron deficieint
      Zmean(2,2,I_A_CN - I) = I_Z_CN - Z1  
      Zshift(2,2,I_A_CN - I) = ZUCD - Z1  ' neutron rich at shell
    Next

    /' Mode 3 '/
    For I = 10 To I_A_CN - 10
      ZUCD = Csng(I) / Csng(I_A_CN) * Csng(I_Z_CN)
      Z = ZUCD /' Charge polarisation is here neglected '/
      beta1 = Beta(3,1,CInt(Z))
      beta2 = Beta(3,2,CInt(I_Z_CN-Z))
      Z1 = Z_equi(I_Z_CN,I, I_A_CN - I, beta1, beta2, dneck,3, _
           POLARadd,POLARfac)
      Zmean(3,1,I) = Z1
      Zshift(3,1,I) = Z1 - ZUCD
      Zmean(3,2,I_A_CN - I) = I_Z_CN - Z1
      Zshift(3,2,I_A_CN - I) = ZUCD - Z1
    Next

    /' Mode 4 (assumed to be equal to mode 0) '/
    For I = 10 To I_A_CN - 10
      Zmean(4,1,I) = Zmean(0,1,I)
      Zshift(4,1,I) = Zshift(0,1,I)
      Zmean(4,2,I_A_CN - I) = Zmean(0,2,I_A_CN - I)
      Zshift(4,2,I_A_CN - I) = Zshift(0,2,I_A_CN - I)
    Next


    /' General relations between Z and A of fission channels '/
    RZpol = 0
    For I = 1 To 3
      RA = (ZC_Mode_0 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN)
      RZpol = Zshift(0,2,CInt(RA))
    Next
    AC_Mode_0 = (ZC_Mode_0 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN) /' mean position in mass '/
    NC_Mode_0 = AC_Mode_0 - ZC_Mode_0

    RZpol = 0
    For I = 1 To 3
      RA = (ZC_Mode_1 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN)
      RZpol = Zshift(1,2,CInt(RA))
    Next
    AC_Mode_1 = (ZC_Mode_1 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN)
    NC_Mode_1 = AC_Mode_1 - ZC_Mode_1
    
    RZpol = 0
    For I = 1 To 3
      RA = (ZC_Mode_2 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN)
      RZpol = Zshift(2,2,CInt(RA))
    Next
    AC_Mode_2 = (ZC_Mode_2 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN)
    NC_Mode_2 = AC_Mode_2 - ZC_Mode_2

    RZpol = 0
    For I = 1 To 3
      RA = (ZC_Mode_3 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN)
      RZpol = Zshift(3,2,CInt(RA))
    Next
    AC_Mode_3 = (ZC_Mode_3 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN)
    NC_Mode_3 = AC_Mode_3 - ZC_Mode_3

    RZpol = 0
    For I = 1 To 3
      RA = (ZC_Mode_4 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN)
      RZpol = Zshift(4,2,CInt(RA))
    Next
    AC_Mode_4 = (ZC_Mode_4 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN)
    NC_Mode_4 = AC_Mode_4 - ZC_Mode_4


    /' Potential curvatures of fission modes '/

   ' For the width of the mass distribution (potential between saddle and scission):
' Print Spin_pre_fission,  P_I_rms_CN 
    R_Z_Curv_S0 = 8.E0 / Csng(I_Z_CN)^2 * Masscurv(Csng(I_Z_CN), Csng(I_A_CN), Spin_pre_fission, kappa)
   ' For the yields of the fission channels (potential near saddle):
    R_Z_Curv1_S0 = 8.E0 / Csng(I_Z_CN)^2 * Masscurv1(Csng(I_Z_CN), Csng(I_A_CN), 0.0, kappa)
    R_A_Curv1_S0 = 8.E0 / Csng(I_A_CN)^2 * Masscurv1(Csng(I_Z_CN), Csng(I_A_CN), 0.0, kappa)


    /' Energy transformation '/
   
    Select Case Emode
      Case 0   ' Energy above outer barrier given
        R_E_exc_Eb = R_E_exc_used
        R_E_exc_GS = R_E_exc_used + BFTFB(Csng(I_Z_CN),Csng(I_A_CN),1)
      Case 1,3,-1   ' Energy above ground state given
        R_E_exc_Eb = R_E_exc_used - BFTFB(Csng(I_Z_CN),Csng(I_A_CN),1)
        R_E_exc_GS = R_E_exc_used
      Case 2     ' kinetic energy of neutron given (SN = neutron separation energy)
    '    SN = (U_Mass(Csng(I_Z_CN),Csng(I_A_CN-1)) + Lypair(I_Z_CN,I_A_CN-1)) _
    '       -(U_Mass(Csng(I_Z_CN),Csng(I_A_CN)) + Lypair(I_Z_CN,I_A_CN))
    '    R_E_exc_GS = R_E_exc_used + SN 
        SN = AME2012(I_Z_CN,I_A_CN-1) - AME2012(I_Z_CN,I_A_CN)
        R_E_exc_GS = R_E_exc_used * ((P_A_CN-1) / P_A_CN) + SN                                            '           target                      CN           
        R_E_exc_Eb = R_E_exc_GS - BFTFB(Csng(I_Z_CN),Csng(I_A_CN),1)
      Case 12     ' kinetic energy of proton given (Sprot = proton separation energy)
    '    Sprot = (U_Mass(Csng(I_Z_CN-1),Csng(I_A_CN-1)) + Lypair(I_Z_CN-1,I_A_CN-1)) _
    '       -(U_Mass(Csng(I_Z_CN),Csng(I_A_CN)) + Lypair(I_Z_CN,I_A_CN))
    '    R_E_exc_GS = R_E_exc_used + Sprot 
        Sprot = AME2012(I_Z_CN-1,I_A_CN-1) - AME2012(I_Z_CN,I_A_CN)
        R_E_exc_GS = R_E_exc_used * ((P_A_CN-1) / P_A_CN) + Sprot    
        R_E_exc_Eb = R_E_exc_GS - BFTFB(Csng(I_Z_CN),Csng(I_A_CN),1)
    End Select
   
   
    /' Fission barriers -> global parameters '/
   
    B_F = BFTF(Csng(I_Z_CN),Csng(I_A_CN),1)   
    B_F_ld = BFTF(Csng(I_Z_CN),Csng(I_A_CN),0)
    E_B = BFTFB(Csng(I_Z_CN),Csng(I_A_CN),1)   
    E_B_ld = BFTFB(Csng(I_Z_CN),Csng(I_A_CN),0)


    /' Barriers and excitation energies of the fission modes '/

    E_exc_S0_prov = R_E_exc_Eb


    /' Additional influence of N=82 assumed '/
    Delta_NZ_Pol = 82.E0/50.E0 - Csng(I_N_CN)/Csng(I_Z_CN)
    R_Shell_S1_eff = P_Shell_S1 * (1.E0 - P_Att_Pol * Abs(Delta_NZ_Pol))

    
    /' In Pu, the Z=50 shell meets Z=44 in the light fragment. '/
    /' A deformed shell at Z=44 is assumed to explain the enhancement _ 
       of the S1 channel around Pu '/
    /' This very same shell automatically produces the double-humped '/
    /' mass distribution in 180Hg '/   
    S1_enhance = P_Shell_SL4 + _
              (Csng(I_Z_CN) - ZC_Mode_1 - ZC_Mode_4L)^2 * P_Z_Curv_SL4
'Print "ZC_Mode_1,ZC_Mode_4",ZC_Mode_1,ZC_Mode_4
'Print "Delta-Z S1-S4, S1_enhance",I_Z_CN-ZC_Mode_1 - ZC_Mode_4L, S1_enhance              
    If S1_enhance > 0 Then S1_enhance = 0
    R_Shell_S1_eff = R_Shell_S1_eff + S1_enhance

    /' The high TKE of S1 in 242Pu(sf) (and neighbours) is obtained by assuming '/
    /' that the Z=44 shell reduces the deformation of the light fragment. '/
    For I = 10 To I_Z_CN - 10
      Z1 = I
      A1 = (Z1 - 0.5E0) / Csng(I_Z_CN) * Csng(I_A_CN) /' polarization roughly considered '/
'      Beta(1,1,Z1) = Beta(1,1,Z1) + 0.15 * S1_enhance   /' "light" fragment '/
      Beta(1,1,I) = exp(S1_enhance) * Beta(1,1,I) _
                       + (1.E0-exp(S1_enhance)) * (Beta(1,1,I)-0.25)
      Beta(1,1,I) = Max(Beta(1,1,I),0.0)
      E_defo = Lymass(Z1,A1,Beta(1,1,I)) - Lymass(Z1,A1,0.0)
      Edefo(1,1,I) = E_defo /' "light" fragment '/
    Next   

   ' Influence of S2 shell in complementary fragment
   ' May be called "S12 fission channel"
    T_Asym_Mode_2 = 0.5
    SigZ_Mode_2 = Sqr(0.5E0 * T_Asym_Mode_2/(P_Z_Curv_S2))
    SigA_Mode_2 = SigZ_Mode_2 * Csng(I_A_CN) / Csng(I_Z_CN)
    S1_enhance = P_Shell_S2 * U_Box(Csng(P_A_CN) - AC_Mode_2 - AC_Mode_1, _
             SigA_Mode_2,P_A_Width_S2) *P_A_Width_S2
    If S1_enhance < 0.01 Then
      R_Shell_S1_eff = R_Shell_S1_eff + S1_enhance
    End If   
    ' Modify deformation of complementary fragment in corresponding analyzer

    ' Overlap of S2 and shell in light fragment
     R_Shell_S2_eff = P_Shell_S2 
 '   S2_enhance = P_Shell_S4 + _
 '             (Csng(I_Z_CN) - ZC_Mode_2 - ZC_Mode_4)^2 * P_Z_Curv_S4
 '   If S2_enhance > 0 Then S2_enhance = 0
 '   R_Shell_S2_eff = R_Shell_S2_eff + S2_enhance              
    
    ' Overlap of S3 and shell in light fragment  
    R_Shell_S3_eff = P_Shell_S3 * (1.E0 - PZ_S3_olap_curv _
         * (Csng(I_Z_CN) - ZC_Mode_3 - PZ_S3_olap_pos)^2)
'        * (Csng(I_Z_CN) - 60.5E0 - PZ_S3_olap_pos)^2)
    R_Shell_S3_eff = Min(R_Shell_S3_eff,0.0)    

'   R_Shell_S4_eff = 2.0 * (P_Shell_S4 + P_Z_Curv_S4*(ZC_Mode_4 - ZC_Mode_0)^2)
    R_Shell_S4_eff = 2.0 * (P_Shell_S4 + P_Z_Curv_S4 * (ZC_Mode_4 - ZC_Mode_0)^2)     
       ' overlap of S4 in both fragments       
    If R_Shell_S4_eff > P_Shell_S4 Then R_Shell_S4_eff = P_Shell_S4 
       ' no overlap at large distance

    E_ld_S1 = R_A_Curv1_S0 * (Csng(I_A_CN)/Csng(I_Z_CN)*(ZC_MODE_1 - ZC_MODE_0) )^2
    B_S1 = E_ld_S1 + R_Shell_S1_eff
    E_exc_S1_prov = E_Exc_S0_prov - B_S1

    E_ld_S2 = R_A_Curv1_S0 * (Csng(I_A_CN)/Csng(I_Z_CN)*(ZC_MODE_2 - ZC_MODE_0) )^2
    B_S2 = E_ld_S2 + R_Shell_S2_eff
    E_exc_S2_prov = E_Exc_S0_prov - B_S2   

    E_ld_S3 = R_A_Curv1_S0 * (Csng(I_A_CN)/Csng(I_Z_CN)*(ZC_MODE_3 - ZC_MODE_0) )^2
    B_S3 = E_ld_S3 + R_Shell_S3_eff
    E_exc_S3_prov = E_Exc_S0_prov - B_S3

    If I_A_CN < 220 Then  ' Only here S4 is close enough to symmetry to have a chance
      E_ld_S4 = R_A_Curv1_S0 * (Csng(I_A_CN)/Csng(I_Z_CN)*(ZC_MODE_4 - ZC_MODE_0) )^2
      B_S4 = E_ld_S4 + R_Shell_S4_eff
      E_exc_S4_prov = E_Exc_S0_prov - B_S4
    Else
      B_S4 = 9999
      E_exc_S4_prov = - 9999  
    End If

    /' Mode 11 (overlap of channel 1 in light and heavy fragment '/
    /' Potential depth with respect to liquid-drop potential: B_S11 '/
    B_S11 = 2.E0 * (R_Shell_S1_eff + De_Defo_S1 _
             + P_Z_Curv_S1 * (ZC_Mode_1 - ZC_Mode_0)^2 ) - De_Defo_S1 
             
  
    /' Lowering of effective barrier by lower ZPM due to larger width in
       partial overlap region (shells in light and heavy fragment) '/
    DES11ZPM = Level_S11 * Min(Abs(ZC_Mode_1 - ZC_Mode_0),4.E0*P_Z_Curv_S1)
' Print B_S11,DES11ZPM,ZC_Mode_1-ZC_Mode_0    

    B_S11 = B_S11 + DES11ZPM
    
 '  If B_S11 > R_Shell_S1_eff + 0.5E0 Then 
 '   If B_S11 > R_Shell_S1_eff + Level_S11 Then
 '     B_S11 = 100   ' S1 and S11 are exclusive
 '   Else
 '     B_S11 = Min(B_S11,R_Shell_S1_eff)  
 '   End If  
    

    E_exc_S11_prov = E_Exc_S0_prov - B_S11

    /' Mode 22 (overlap of channel 2 in light and heavy fragment '/
    /' Potential depth with respect to liquid-drop potential: B_S22 '/

 '   B_S22 = 2.E0 * (E_ld_S2 + P_Shell_S2) _
 '       + 2.E0 * P_Z_Curv_S2 * (ZC_Mode_2 - ZC_Mode_0)^2   /' Parabola '/
'Print E_ld_S2,P_Shell_S2,P_Z_Curv_S2,ZC_Mode_2,ZC_Mode_0   
    B_S22 = 2.E0 * R_Shell_S2_eff  * _
             U_Box(Csng(P_A_CN)/2.0 - AC_Mode_2, _
             SigA_Mode_2,P_A_Width_S2) * P_A_Width_S2
             ' The integral of U_Box is normalized, not the height! 
'    If Abs((P_A_CN/2.E0) - AC_Mode_2) > P_A_Width_S2 Then B_S22 = 9999   
    If P_A_CN < 226 Then B_S22 = 9999 

    E_exc_S22_prov = E_Exc_S0_prov - B_S22

    
    E_Min_Barr = Min(0.0,B_S1)
    E_Min_Barr = Min(E_Min_Barr,B_S2)
    E_Min_Barr = Min(E_Min_Barr,B_S3)
    E_Min_Barr = Min(E_Min_Barr,B_S4)
    E_Min_Barr = Min(E_Min_Barr,B_S11)
    E_Min_Barr = Min(E_Min_Barr,B_S22)
    
    /' Energy minus the height of the respective fission saddle '/
    E_exc_S0 = E_exc_S0_prov + E_Min_Barr - Delta_S0
    E_exc_S1 = E_exc_S1_prov + E_Min_Barr
    E_exc_S2 = E_exc_S2_prov + E_Min_Barr
    E_exc_S3 = E_exc_S3_prov + E_Min_Barr
    E_exc_S4 = E_exc_S4_prov + E_Min_Barr
    E_exc_S11 = E_exc_S11_prov + E_Min_Barr
    E_exc_S22 = E_exc_S22_prov + E_Min_Barr

    /' Energy above the lowest fission saddle '/
    E_exc_Barr = Max(E_Exc_S0,E_Exc_S1)
    E_exc_Barr = Max(E_exc_Barr,E_Exc_S2)
    E_exc_Barr = Max(E_exc_Barr,E_Exc_S3)
    E_exc_Barr = Max(E_exc_Barr,E_Exc_S4)
    E_exc_Barr = Max(E_exc_Barr,E_exc_S11)
    E_exc_Barr = Max(E_exc_Barr,E_exc_S22)
    

    /' Collective temperature used for calculating the widths
       in mass asymmetry and charge polarization '/

    If E_Exc_S0 < 0 Then E_tunn = -E_Exc_S0 Else E_tunn = 0
    R_E_exc_eff = Max(0.1,E_Exc_S0)
  '  T_Coll_Mode_0 = TFCOLL * R_E_exc_eff + _  /' empirical, replaced by TRusanov '/
    T_Coll_Mode_0 = TCOLLFRAC * (De_Saddle_Scission(Csng(I_Z_CN)^2 / _ 
           Csng(I_A_CN)^0.33333E0,ESHIFTSASCI_coll) - E_tunn)
    T_Coll_Mode_0 = Max(T_Coll_Mode_0,0.0)

' Print "De_SS, E_tunn, T_Coll ";De_Saddle_Scission(I_Z_CN^2/I_A_CN^0.3333,ESHIFTSASCI_coll),E_tunn,T_Coll_Mode_0    
    
    /' Temperature description fitting to the empirical systematics of Rusanov et al. '/
    /' Here from Ye. N. Gruzintsev et al., Z. Phys. A 323 (1986) 307 '/    
    /' Empirical description of the nuclear temperature according to the '/
    /' Fermi-gas description. Should be valid at higher excitation energies '/
      Dim As Single T_Rusanov
    T_Rusanov = TRusanov(R_E_exc_eff,Csng(I_A_CN)) 
  '  Print "Temperatures, (GEF, Total, Rusanov): ", T_Coll_Mode_0, TFCOLL * R_E_exc_eff, T_Rusanov
    T_Coll_Mode_0 = Max(T_Coll_Mode_0,T_Rusanov)
    /' Transition vom const. temp. to Fermi gas occurs around 20 MeV by MAX function '/
'    T_Pol_Mode_0 = T_Pol_Red * T_Coll_Mode_0

    ' Application of the statistical model, intrinsic temperature at saddle
T_Pol_Mode_0 = U_Temp(0.5 * Csng(I_Z_CN),0.5 *Csng(I_A_CN), R_E_exc_eff, 0, 0, Tscale, Econd)
    T_Asym_Mode_0 = Sqr(T_Coll_Mode_0^2 + (6E0*TCOLLMIN)^2)  
    
    E_pot_scission = (De_Saddle_Scission(Csng(I_Z_CN)^2 / _ 
               Csng(I_A_CN)^0.33333E0,ESHIFTSASCI_intr) - E_tunn)               

    /' Suppression of S1 fission channel due to reduced pairing in 132Sn '/
    /' At very low excitation energy on the fission path, the binding energy at the
       S1 fission channel does not profit as much from pairing as SL and S2,
       because pairing is reduced in magic nuclei. This leads to a reduction of
       the yield in S1 in the case that the fully paired ground-state configuration
       is populated on the fission path with a considerable probability. '/
 '   EeffS2 = Max(E_exc_S2,0.0) + EDISSFRAC * E_pot_scission - 2.3E0
 '   EeffS2 = Max(0.0,EeffS2)
       /' -2.3 MeV, because fission channels are assumed to be chosen before scission '/

 '   If EeffS2 < ETHRESHSUPPS1 + 2.E0 * ESIGSUPPS1 Then
 '     E_exc_S1 = E_exc_S1 - _
 '        0.5E0 * 4.E0 * 12.E0 / Sqr(132.E0) * Gaussintegral(ETHRESHSUPPS1 - EeffS2,ESIGSUPPS1)
 '   EndIf

    T_low_S1_used = T_low_S1
    
    T_Coll_Mode_1 = TFCOLL * Max(E_exc_S1,0.E0) + _
          TCOLLFRAC * (De_Saddle_Scission(I_Z_CN^2 / I_A_CN^0.33333E0,ESHIFTSASCI_coll) - E_tunn)
    T_Coll_Mode_1 = Max(T_Coll_mode_1,0.0)
'    T_Pol_Mode_1 = T_Pol_Red * T_Coll_Mode_1
T_Pol_Mode_1 = T_Pol_Mode_0
    T_Asym_Mode_1 = Sqr(T_Coll_Mode_1^2 + (4.0*TCOLLMIN)^2)  ' TCOLLMIN for ZPM

    T_Coll_Mode_2 = TFCOLL * Max(E_exc_S2,0.E0) + _
          TCOLLFRAC * (De_Saddle_Scission(Csng(I_Z_CN)^2 / _ 
          Csng(I_A_CN)^0.33333E0,ESHIFTSASCI_coll) - E_tunn)
    T_Coll_Mode_2 = Max(T_Coll_mode_2,0.0)
'    T_Pol_Mode_2 = T_Pol_Red * T_Coll_Mode_2
T_Pol_Mode_2 = T_Pol_Mode_0
    T_Asym_Mode_2 = Sqr(T_Coll_Mode_2^2 + TCOLLMIN^2)

    T_Coll_Mode_3 = TFCOLL * Max(E_exc_S3,0.E0) + _
          TCOLLFRAC * (De_Saddle_Scission(Csng(I_Z_CN)^2 / _ 
            Csng(I_A_CN)^0.33333E0,ESHIFTSASCI_coll) - E_tunn)
    T_Coll_Mode_3 = Max(T_Coll_mode_3,0.0)
'    T_Pol_Mode_3 = T_Pol_Red * T_Coll_Mode_3
T_Pol_Mode_3 = T_Pol_Mode_0
    T_Asym_Mode_3 = Sqr(T_Coll_Mode_3^2 + TCOLLMIN^2)

    T_Coll_Mode_4 = TFCOLL * Max(E_exc_S4,0.E0) + _
          TCOLLFRAC * (De_Saddle_Scission(Csng(I_Z_CN)^2 / _
             Csng(I_A_CN)^0.33333E0,ESHIFTSASCI_coll) - E_tunn)
    T_Coll_Mode_4 = Max(T_Coll_mode_4,0.0)
'    T_Pol_Mode_4 = T_Pol_Red * T_Coll_Mode_4
T_Pol_Mode_4 = T_Pol_Mode_0
    T_Asym_Mode_4 = Sqr(T_Coll_Mode_4^2 + 4.0*TCOLLMIN^2)  ' ZPM like S1

    /' Stiffness in polarization '/

    RZ = Csng(I_Z_CN) * 0.5E0
    RA = Csng(I_A_CN) * 0.5E0
    beta1 = Beta(0,1,CInt(RZ))
    beta2 = Beta(0,2,CInt(RZ))
    R_Pol_Curv_S0 = ( LyMass( RZ - 1.E0, RA, beta1 ) + _
             LyMass( RZ + 1.0E0, RA, beta2 ) + _
             LyMass( RZ + 1.0E0, RA, beta1 ) + _
             LyMass( RZ - 1.0E0, RA, beta2 ) + _
             ecoul( RZ - 1.0E0, RA, beta1, _
                    RZ + 1.0E0, RA, beta2, dneck) + _
             ecoul( RZ + 1.0E0, RA, beta1, _
                    RZ - 1.0E0, RA, beta2, dneck) - _
         2.0E0*ecoul( RZ, RA, beta1, RZ, RA, beta2, dneck) - _
         2.0E0*LyMass( RZ, RA, beta1 ) - _
         2.0E0*LyMass( RZ, RA, beta2) ) * 0.5E0

    P_Pol_Curv_S0 = R_Pol_Curv_S0

    R_Pol_Curv_S1 = R_Pol_Curv_S0
    R_Pol_Curv_S2 = R_Pol_Curv_S0
    R_Pol_Curv_S3 = R_Pol_Curv_S0
    R_Pol_Curv_S4 = R_Pol_Curv_S0



    /' Mean values and standard deviations of fission modes '/
    
    Dim As Single R_E_intr_S1, R_E_intr_S2, R_E_intr_S3   ' intrinsic exc. energies at barrier
    Dim As Single R_E_intr_S4
    ReDim As Single R_Att(6)                              ' attenuation of shell
    ReDim As Single R_Att_Sad(6)     
  '  Dim As Single E_backshift 
  '  E_backshift = -3
  

    SIGZ_Mode_0 = Sqr(0.5E0 * T_Asym_Mode_0/R_Z_Curv_S0)
    If T_Pol_Mode_0 > 1.E-2 Then
      SigPol_Mode_0 = Sqr(0.25E0 * HOMPOL / R_Pol_Curv_S0 / _
                     Tanh(HOMPOL/(2.E0 * T_Pol_Mode_0)))
    Else
      SigPol_Mode_0 = Sqr(0.25E0 * HOMPOL / R_Pol_Curv_S0)
        /' including influence of zero-point motion '/
    Endif


    R_E_intr_S1 = Max(E_Exc_S1+Lypair(I_Z_CN,I_A_CN),0.0)
    R_Att(1) = exp(-R_E_intr_S1/Shell_fading)
    R_Att(5) = R_Att(1)
    R_Att_Sad(1) = exp(-R_E_intr_S1/Shell_fading)
    R_Att_Sad(5) = R_Att_Sad(1)
    SIGZ_Mode_1 = Sqr(0.5E0 * T_Asym_Mode_1/(P_Z_Curv_S1*Sqr(R_Att(1))))
    If T_Pol_Mode_1 > 1.E-2 Then
      SigPol_Mode_1 = Sqr(0.25E0 * HOMPOL / R_Pol_Curv_S1 / _
                     Tanh(HOMPOL/(2.E0 * T_Pol_Mode_1)))
    Else
      SigPol_Mode_1 = Sqr(0.25E0 * HOMPOL / R_Pol_Curv_S1)
    Endif

    R_E_intr_S2 = Max(E_Exc_S2+Lypair(I_Z_CN,I_A_CN),0.0)
    R_Att(2) = exp(-R_E_intr_S2/Shell_fading)
    R_Att(6) = R_Att(2)
    R_Att_Sad(2) = exp(-R_E_intr_S2/Shell_fading)
    R_Att_Sad(6) = R_Att_Sad(2)
    SIGZ_Mode_2 = Sqr(0.5E0 * T_Asym_Mode_2/(P_Z_Curv_S2*Sqr(R_Att(2))))
    If T_Pol_Mode_2 > 1.E-2 Then
      SigPol_Mode_2 = Sqr(0.25E0 * HOMPOL / R_Pol_Curv_S2 / _
                     Tanh(HOMPOL/(2.E0 * T_Pol_Mode_2)))
    Else
      SigPol_Mode_2 = Sqr(0.25E0 * HOMPOL / R_Pol_Curv_S2)
    End If

    R_E_intr_S3 = Max(E_exc_S3+Lypair(I_Z_CN,I_A_CN),0.0)
    R_Att(3) = exp(-R_E_intr_S3/Shell_fading)
    R_Att_Sad(3) = exp(-R_E_intr_S3/Shell_fading)
    SIGZ_Mode_3 = Sqr(0.5E0 * T_Asym_Mode_3/(P_Z_Curv_S3*Sqr(R_Att(3))))
    If T_Pol_Mode_3 > 1.E-2 Then
      SigPol_Mode_3 = Sqr(0.25E0 * HOMPOL / R_Pol_Curv_S3 / _
                     Tanh(HOMPOL/(2.E0 * T_Pol_Mode_3)))
    Else
      SigPol_Mode_3 = Sqr(0.25E0 * HOMPOL / R_Pol_Curv_S3)
    End if

    R_E_intr_S4 = Max(E_exc_S4+Lypair(I_Z_CN,I_A_CN),0.0)
    R_Att(4) = exp(-R_E_intr_S4/Shell_fading)
    R_Att_Sad(4) = exp(-R_E_intr_S4/Shell_fading)
    SIGZ_Mode_4 = Sqr(0.5E0 * T_Asym_Mode_4/(P_Z_Curv_S4*Sqr(R_Att(4))))
    If T_Pol_Mode_4 > 1.E-2 Then
      SigPol_Mode_4 = Sqr(0.25E0 * HOMPOL / R_Pol_Curv_S4 / _
                     Tanh(HOMPOL/(2.E0 * T_Pol_Mode_4)))
    Else
      SigPol_Mode_4 = Sqr(0.25E0 * HOMPOL / R_Pol_Curv_S4)
    End if



    /' Energy-dependent shift of fission channels '/
    Scope
      Dim As Single DZ_S1,DZ_S2,DZ_S3,DZ_S4
      Dim As Single EtotS2
      Dim As Single P_Z_Curv_S1_eff
      P_Z_Curv_S1_eff = P_Z_Curv_S1 * P_Z_Curvmod_S1
      Dim AS Single P_Z_Curv_S2_eff
      P_Z_Curv_S2_eff = P_Z_Curv_S2 * P_Z_Curvmod_S2     
      Dim As Single P_Z_Curv_S3_eff
      P_Z_Curv_S3_eff = P_Z_Curv_S3 * P_Z_Curvmod_S3     
      Dim As Single P_Z_Curv_S4_eff
      P_Z_Curv_S4_eff = P_Z_Curv_S4 * P_Z_Curvmod_S4     

      DZ_S1 = ZC_Mode_1 * _
              (P_Z_Curv_S1_eff*R_Att(1) / (R_Z_Curv_S0 + P_Z_Curv_S1_eff*R_Att(1)) _
            - (P_Z_Curv_S1_eff / (R_Z_Curv_S0 + P_Z_Curv_S1_eff) ) )
      DZ_S2 =  ZC_Mode_2 * _
               (P_Z_Curv_S2_eff*R_Att(2) / (R_Z_Curv_S0 + P_Z_Curv_S2_eff*R_Att(2)) _
             - (P_Z_Curv_S2_eff / (R_Z_Curv_S0 + P_Z_Curv_S2_eff) ) )  
      DZ_S3 =  ZC_Mode_3 * _
               (P_Z_Curv_S3_eff*R_Att(3) / (R_Z_Curv_S0 + P_Z_Curv_S3_eff*R_Att(3)) _
             - (P_Z_Curv_S3_eff / (R_Z_Curv_S0 + P_Z_Curv_S3_eff) ) )
      DZ_S4 = Sgn(ZC_Mode_4 - ZC_Mode_0) * ZC_Mode_4 * _
               (P_Z_Curv_S4_eff*R_Att(4) / (R_Z_Curv_S0 + P_Z_Curv_S4_eff*R_Att(4)) _
             - (P_Z_Curv_S4_eff / (R_Z_Curv_S0 + P_Z_Curv_S4_eff) ) )  
 
     ' Empirical shift of S2 channel at low excitation energy at scission 
     ' for better reproduction of 238U(s,f) and some data for Th isotopes.
     ' Does not solve the problem of 229Th(nth,f).    
     EtotS2 = Max(E_Exc_S2 + EDISSFRAC * E_pot_scission,0.0)
     If EtotS2 < 5.E0 Then
       DZ_S2 = DZ_S2 + (5.E0 - EtotS2) * 0.1
     End If             

 '   DZ_S1 = 0
 '   DZ_S2 = 0
 '   DZ_S3 = 0
 '   DZ_S4 = 0
   

      P_Z_Mean_S0 = ZC_Mode_0
      ZC_Mode_1 = ZC_Mode_1 + DZ_S1  
      P_Z_Mean_S1 = ZC_Mode_1          /' Copy to global parameter '/
      ZC_Mode_2 = ZC_Mode_2 + DZ_S2  
      P_Z_Mean_S2 = ZC_Mode_2          /'             "            '/
      ZC_Mode_3 = ZC_Mode_3 + DZ_S3
      P_Z_Mean_S3 = ZC_Mode_3
   '   ZC_Mode_4 = ZC_Mode_4 + DZ_S4  
           ' shift is very small, because S4 exists only close to symmetry
      P_Z_Mean_S4 = ZC_Mode_4 
    End Scope

    /' Energy dependence of charge polarization '/
    /' Due to washing out of shells '/
    
    For I = 10 To I_A_CN - 10   ' mass number
      For J = 1 To 4    ' fission channel
        For K = 1 To 2    ' light - heavy group
          Zshift(J,K,I) = Zshift(0,K,I) + (Zshift(J,K,I) - Zshift(0,K,I))*R_Att(J)
        Next
      Next    
    Next    
    
     
    /' Energy dependence of shell-induced deformation '/
    /' Due to washing out of shells '/
    /' (Under development) '/
  /'For I = 10 To I_Z_CN - 10  ' mass number
      For J = 1 To 4           ' fission channel
        For K = 1 To 2         ' light - heavy group
          beta(J,K,I) = beta(0,K,I) + (beta(J,K,I) - beta(0,K,I))*R_Att_Sad(J)
          if beta(J,K,I) < 0 Then 
            beta(J,K,I) = 0
          End If  
          Z1 = I
          Z2 = I_Z_CN - Z1
          A1 = Z1 / Csng(I_Z_CN) * Csng(I_A_CN)
          A2 = I_A_CN - A1
          E_defo = Lymass(Z1,A1,beta(J,K,I)) - Lymass(Z1,A1,0.0)
          Edefo(J,K,I) = E_defo
        Next
      Next    
    Next  '/  
    
    


    /' General relations between Z and A of fission channels '/  
    /' 2nd iteration '/

    RZpol = 0
    For I = 1 To 3
      RA = (ZC_Mode_0 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN)
      RZpol = Zshift(0,2,CInt(RA))
    Next
    AC_Mode_0 = (ZC_Mode_0 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN) /' mean position in mass '/
    NC_Mode_0 = AC_Mode_0 - ZC_Mode_0

    RZpol = 0
    For I = 1 To 3
      RA = (ZC_Mode_1 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN)
      RZpol = Zshift(1,2,CInt(RA))
    Next
    AC_Mode_1 = (ZC_Mode_1 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN)
    NC_Mode_1 = AC_Mode_1 - ZC_Mode_1

    RZpol = 0
    For I = 1 To 3
      RA = (ZC_Mode_2 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN)
      RZpol = Zshift(2,2,CInt(RA))
    Next
    AC_Mode_2 = (ZC_Mode_2 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN)
    NC_Mode_2 = AC_Mode_2 - ZC_Mode_2

    RZpol = 0
    For I = 1 To 3
      RA = (ZC_Mode_3 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN)
      RZpol = Zshift(3,2,CInt(RA))
    Next
    AC_Mode_3 = (ZC_Mode_3 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN)
    NC_Mode_3 = AC_Mode_3 - ZC_Mode_3

    RZpol = 0
    For I = 1 To 3
      RA = (ZC_Mode_4 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN)
      RZpol = Zshift(4,2,CInt(RA))
    Next
    AC_Mode_4 = (ZC_Mode_4 - RZPol) * Csng(I_A_CN) / Csng(I_Z_CN)
    NC_Mode_4 = AC_Mode_4 - ZC_Mode_4



   /' Yields of the fission modes '/
   
    Yield_Mode_0 = Getyield(E_exc_S0,E_exc_S0,T_low_SL,TEgidy(Csng(I_A_CN),0.E0,Tscale))

    Yield_Mode_1 = _
          Getyield(E_exc_S1,E_exc_S0,T_low_S1_used,TEgidy(Csng(I_A_CN),R_Shell_S1_eff + dE_Defo_S1,Tscale))
                  /'  - Getyield(E_exc_S0 - E_ld_S1,T_low,T_high) '/

    Yield_Mode_2 = Getyield(E_exc_S2,E_exc_S0,T_low_S2,TEgidy(Csng(I_A_CN),R_Shell_S2_eff + dE_Defo_S2,Tscale))
                  /'  - Getyield(E_exc_S0 - E_ld_S2,T_low,T_high) '/

    Yield_Mode_3 = Getyield(E_exc_S3,E_exc_S0,T_low_S3,TEgidy(Csng(I_A_CN),R_Shell_S3_eff + dE_Defo_S3,Tscale))
                  /'  - Getyield(E_exc_S0 - E_ld_S3,T_low,T_high) '/

    Yield_Mode_4 = Getyield(E_exc_S4,E_exc_S0,T_low_S4,TEgidy(Csng(I_A_CN),R_Shell_S4_eff + dE_Defo_S4,Tscale))  
                  /'   - Getyield(E_exc_S0 - E_ld_S4,T_low,T_high) '/ 
    
'Print TEgidy(Csng(I_A_CN),0.E0,Tscale), TEgidy(Csng(I_A_CN),R_Shell_S2_eff + dE_Defo_S2,Tscale), de_Defo_S2 
'sleep

    If B_S11 > B_S1 Then 
      Yield_Mode_11 = 0.0
    Else
      Yield_Mode_11 = Getyield(E_exc_S11,E_exc_S0, T_low_S11, _
          TEgidy(Csng(I_A_CN),R_Shell_S1_eff + 2.E0 * dE_Defo_S1,Tscale))
    End If      

    If B_S22 > B_S2 Then 
      Yield_Mode_22 = 0.0
    Else
      Yield_Mode_22 = Getyield(E_exc_S22,E_exc_S0, T_low_S2, _
          TEgidy(Csng(I_A_CN),R_Shell_S2_eff,Tscale))
    End If     
    

    Yield_Norm = Yield_Mode_0 + Yield_Mode_1 + Yield_Mode_2 + Yield_Mode_3 _
                 + Yield_Mode_4 + Yield_Mode_11 + Yield_Mode_22
    Yield_Mode_0 = Yield_Mode_0 / Yield_Norm
    Yield_Mode_1 = Yield_Mode_1 / Yield_Norm
    Yield_Mode_2 = Yield_Mode_2 / Yield_Norm
    Yield_Mode_3 = Yield_Mode_3 / Yield_Norm
    Yield_Mode_4 = Yield_Mode_4 / Yield_Norm
    Yield_Mode_11 = Yield_Mode_11 / Yield_Norm
    Yield_Mode_22 = Yield_Mode_22 / Yield_Norm


    /' Mass widhts of the fission channels '/

    SigA_Mode_0 = SigZ_Mode_0 * Csng(I_A_CN) / Csng(I_Z_CN) /' width in mass '/
    SigA_Mode_1 = SigZ_Mode_1 * Csng(I_A_CN) / Csng(I_Z_CN)
    SigA_Mode_1 = Min(SigA_Mode_1,SigA_Mode_0)  ' not broader than liquid-drop
    SigA_Mode_2 = SigZ_Mode_2 * Csng(I_A_CN) / Csng(I_Z_CN)
    SigA_Mode_2 = Min(SigA_Mode_2,SigA_Mode_0)  ' not broader than liquid-drop
    SigA_Mode_3 = SigZ_Mode_3 * Csng(I_A_CN) / Csng(I_Z_CN)
    SigA_Mode_3 = Min(SigA_Mode_3,SigA_Mode_0)
    SigA_Mode_4 = SigZ_mode_4 * Csng(I_A_CN) / Csng(I_Z_CN)
    SigA_Mode_4 = Min(SigA_Mode_4,SigA_Mode_0)
    SigA_Mode_11 = SigZ_Mode_1 * sqr(2.E0) * Csng(I_A_CN) / Csng(I_Z_CN)
    SigA_Mode_11 = Min(SigA_Mode_11,SigA_Mode_0)
    SigA_Mode_22 = SigZ_Mode_2 * sqr(2.E0) * Csng(I_A_CN) / Csng(I_Z_CN)
    SigA_Mode_22 = Min(SigA_Mode_22,SigA_Mode_0)



    /' Shell effects of different fission channels '/
    /' This is the "real" microscopic shell effect, not the effective shell-correction energy '/
    /' EShell acts on the level density and determines the T parameter '/

    For I = 1 To I_A_CN - 1
      For J = 0 To 4
        EShell(J,1,I) = 0   /' Shells in "light" fragment assumed to be zero '/
      Next
      DU0 = 0
      EShell(0,2,I) = 0 /' Shell = 0 in symmetric mode '/
      DU1 = R_Shell_S1_eff + dE_Defo_S1 /' + R_A_Curv1_S1 * (AC_Mode_1 - Float(I,6))**2; '/
      DU1 = MIN(DU1,0.E0)  /' Technical limit '/
      EShell(1,2,I) = DU1

      DU2 = R_Shell_S2_eff + dE_Defo_S2 /' + R_A_Curv1_S2 * (AC_Mode_2 - Float(I,6))**2; '/
      DU2 = Min(DU2,0.E0)  /' Technical limit '/
      EShell(2,2,I) = DU2

      DU3 = R_Shell_S3_eff + dE_Defo_S3 /' + R_A_Curv1_S3 * (AC_Mode_3 - Float(I,6))**2; '/
      DU3 = Min(DU3,0.E0)  /' Technical limit '/
      EShell(3,2,I) = DU3

      DU4 = R_Shell_S4_eff + dE_Defo_S4 /' + R_A_Curv1_S4 * (AC_Mode_4 - Float(I,6))**2; '/
      DU4 = Min(DU4,0.E0)  /' Technical limit '/
      EShell(4,2,I) = DU4

    Next


    /' Intrinsic temperatures of fragments at scission '/

    /' Mean values '/
    T_intr_Mode_0 = TEgidy(AC_Mode_0,0.0,0.8)
    T_intr_Mode_1_heavy = TEgidy(AC_Mode_1,R_Shell_S1_eff + dE_Defo_S1,Tscale)
    T_intr_Mode_1_light = TEgidy(Csng(I_A_CN) - AC_Mode_1,0.0,Tscale)
    T_intr_Mode_2_heavy = TEgidy(AC_Mode_2,R_Shell_S2_eff + dE_Defo_S2,Tscale)
    T_intr_Mode_2_light = TEgidy(Csng(I_A_CN) - AC_Mode_2,0.0,Tscale)
    T_intr_Mode_3_heavy = TEgidy(AC_Mode_3,R_Shell_S3_eff + dE_Defo_S3,Tscale)
    T_intr_Mode_3_light = TEgidy(Csng(I_A_CN) - AC_Mode_3,0.0,Tscale)
    T_intr_Mode_4_heavy = TEgidy(AC_Mode_4,R_Shell_S4_eff + dE_Defo_S4,Tscale)
    T_intr_Mode_4_light = TEgidy(Csng(I_A_CN) - AC_Mode_4,0.0,Tscale)


    /' Mass-dependent values of individual fragments '/
    /' Mode 0 '/
    For I = 1 To I_A_CN - 1
      T = TEgidy(Csng(I),EShell(0,1,I),Tscale)
      Temp(0,1,I) = T /' "light" fragment at freeze-out (somewhere before scission) '/
      T = TEgidy(Csng(I),EShell(0,2,I),Tscale)
      Temp(0,2,I) = T /' "heavy" fragment at freeze-out (somewhere before scission) '/

      T = TEgidy(Csng(I),0.0,1.0)
      TempFF(0,1,I) = T       ' FF in their ground state
      TempFF(0,2,I) = T       ' FF in their ground state 
    Next

    /' Mode 1 '/
    For I = 1 To I_A_CN - 1
      T = TEgidy(Csng(I),EShell(1,1,I),Tscale)
      Temp(1,1,I) = T  /' "light" fragment '/
      T = TEgidy(Csng(I),EShell(1,2,I),Tscale)
      Temp(1,2,I) = T  /' "heavy" fragment '/

      T = TEgidy(Csng(I),0.0,1.0)
      TempFF(1,1,I) = T       ' FF in their ground state
      TempFF(1,2,I) = T       ' FF in their ground state
    Next

    /' Mode 2 '/
    For I = 1 To I_A_CN - 1
      T = TEgidy(Csng(I),EShell(2,1,I),Tscale)
      Temp(2,1,I) = T /' "light" fragment '/
      T = TEgidy(Csng(I),EShell(2,2,I),Tscale)
      Temp(2,2,I) = T /' "heavy" fragment '/

   /' The next section is introduced, because energy sorting is not strong enough,
      when shells are only introduced in the heavy fragment.
      Ad hoc assumption: For Mode 2 there are shells in both fragments of about
      equal size. Technically, we neglect the shells in both fragments.
      This has about the same effect for the energy sorting. '/
      T = TEgidy(Csng(I),0.0,Tscale)   ' FF at scssion
      Temp(2,1,I) = T /' "light" fragment '/
      T = TEgidy(Csng(I),0.0,Tscale)   ' FF at scission
      Temp(2,2,I) = T /' "heavy" fragment '/

      T = TEgidy(Csng(I),0.0,1.0)    ' shell effect neglected
      TempFF(2,1,I) = T    ' FFs in their ground state
      TempFF(2,2,I) = T    ' FFs in their ground state
    Next
    
    /' Mode 3 '/
    For I = 1 To I_A_CN -1
      T = TEgidy(Csng(I),0.0,Tscale)
      Temp(3,1,I) = T
      T = TEgidy(Csng(I),0.0,Tscale)
      Temp(3,2,I) = T
      
      T = TEgidy(Csng(I),0.0,1.0)
      TempFF(3,1,I) = T       ' FF in their ground state
      TempFF(3,2,I) = T       ' FF in their ground state
    Next

    /' Mode 4 '/
    For I = 1 To I_A_CN -1
      T = TEgidy(Csng(I),0.0,Tscale)
      Temp(4,1,I) = T
      T = TEgidy(Csng(I),0.0,Tscale)
      Temp(4,2,I) = T
      
      T = TEgidy(Csng(I),0.0,1.0)
      TempFF(4,1,I) = T       ' FF in their ground state
      TempFF(4,2,I) = T       ' FF in their ground state
    Next


    /'** Intrinsic excitation energy at saddle and at scission as well as   **'/
    /'** Even-odd effect in proton and neutron number for each fission mode **'/
    Dim As Single Etot,E1FG,E1ES
    Dim As Single Rincr1P,Rincr1N,Rincr2,Rincr2P,Rincr2N
    Dim As Single T1,T2,E1,E2
    Redim As Single E_coll_saddle(0 To 6)
    Dim As Single Ediff


    For I_Mode = 0 To 6
      E_coll_saddle(I_Mode) = 0
      If I_Mode = 0 Then Etot = E_exc_S0
      If I_Mode = 1 Then Etot = E_exc_S1
      If I_Mode = 2 Then Etot = E_exc_S2
      If I_Mode = 3 Then Etot = E_exc_S3
      If I_Mode = 4 Then Etot = E_exc_S4
      If I_Mode = 5 Then Etot = E_exc_S11
      If I_Mode = 6 Then Etot = E_exc_S22

      If I_Z_CN Mod 2 + I_N_CN Mod 2 = 0 Then  /' Even-even CN '/      
        If Etot > 0 And Etot < 2.E0 * 14.E0/Sqr(Csng(I_A_CN)) Then
          E_coll_saddle(I_Mode) = Etot
          Etot = 0
         /' Excitation below the pairing gap in even-even CN goes into collective excitations '/
        End If
      End If

  '    If I_Z_CN Mod 2 + I_N_CN Mod 2 = 0 Then    ' even-even
  '      Ediff = Min(Etot, 14.0/sqr(Csng(I_A_CN)))
  '    End If
  '    If I_Z_CN Mod 2 + I_N_CN Mod 2 = 1 Then    ' even-odd or odd-even
  '       Ediff = Min(Etot, 2.0 * 14.0/sqr(Csng(I_A_CN)))
  '    End If
  '    Ediff = Max(Ediff,0.0) 
  '    Etot = Etot - Ediff
 
      
      If Etot < 0 Then E_tunn = -Etot Else E_tunn = 0
      Etot = Max(Etot,0.0)
       
      E_pot_scission = (De_Saddle_Scission(Csng(I_Z_CN)^2 / _ 
               Csng(I_A_CN)^0.33333E0,ESHIFTSASCI_intr) )  
      Etot = Etot + EDISSFRAC * (E_pot_scission - E_tunn)
      /' All excitation energy at saddle and part of the potential-energy gain to scission
         go into intrinsic excitation energy at scission '/
 

  

      If I_Mode = 2 Then
        EINTR_SCISSION = Etot /' (For Mode 2) Global parameter '/
      End If

      Dim As Single DT

      For IA1 = 40 To I_A_CN - 40

        IA2 = I_A_CN - IA1
        If I_Mode <= 4 Then
          T1 = Temp(I_Mode,1,IA1)
          T2 = Temp(I_Mode,2,IA2)
        End If
        If I_Mode = 5 Then
          T1 = Temp(1,2,IA1)
          T2 = Temp(1,2,IA2)
        End If  
        If I_Mode = 6 Then
          T1 = Temp(2,2,IA1)
          T2 = Temp(2,2,IA2)
        End If
        DT = ABS(T2 - T1)
        
          /' Even-odd effect '/
        IF I_Z_CN Mod 2 = 0 Then
           Rincr1P = Exp(-Etot/PZ_EO_symm)
        Else
           Rincr1P = 0
        End If
        If I_N_CN Mod 2 = 0 Then
           Rincr1N = Exp(-Etot/PN_EO_symm)
        Else
           Rincr1N = 0
        End If
        PEOZ(I_Mode,1,IA1) = Rincr1P
        PEOZ(I_Mode,2,IA2) = Rincr1P
        PEON(I_Mode,1,IA1) = Rincr1N
        PEON(I_Mode,2,IA2) = Rincr1N

        Rincr2 = Gaussintegral(DT/Etot-R_EO_Thresh, _
                 R_EO_Sigma*(DT+0.0001))
                  /' even-odd effect due to asymmetry '/
        Rincr2P = (R_EO_MAX - Rincr1P) * Rincr2
        Rincr2N = (R_EO_MAX - Rincr1N) * Rincr2        

        If IA1 < IA2 Then  ' A1 is lighter
          PEOZ(I_Mode,1,IA1) = _
               PEOZ(I_Mode,1,IA1) + Rincr2P
          IF I_Z_CN Mod 2 = 0 Then
             PEOZ(I_Mode,2,IA2) = _
                PEOZ(I_Mode,2,IA2) + Rincr2P
          Else
             PEOZ(I_Mode,2,IA2) = _
                PEOZ(I_Mode,2,IA2) - Rincr2P
          End if
          PEON(I_Mode,1,IA1) = _
             PEON(I_Mode,1,IA1) + Rincr2N
          IF I_N_CN Mod 2 = 0 Then
             PEON(I_Mode,2,IA2) = _
                PEON(I_Mode,2,IA2) + Rincr2N
          Else
             PEON(I_Mode,2,IA2) = _
                PEON(I_Mode,2,IA2) - Rincr2N
          End if
        Else   
          PEOZ(I_Mode,1,IA1) = PEOZ(I_Mode,2,IA1)
          PEON(I_Mode,1,IA1) = PEON(I_Mode,2,IA1)
          PEOZ(I_Mode,2,IA2) = PEOZ(I_Mode,1,IA2)
          PEON(I_Mode,2,IA2) = PEON(I_Mode,1,IA2)
        End If            
          
          
    /'  Else
          PEOZ(I_Mode,2,IA2) = _
               PEOZ(I_Mode,1,IA2) + Rincr2P
          IF I_Z_CN Mod 2 = 0 Then
             PEOZ(I_Mode,1,IA1) = _
                PEOZ(I_Mode,1,IA1) + Rincr2P
          Else
             PEOZ(I_Mode,1,IA1) = _
                PEOZ(I_Mode,1,IA1) - Rincr2P
          End if
          PEON(I_Mode,2,IA2) = _
             PEON(I_Mode,2,IA2) + Rincr2N
          IF I_N_CN Mod 1 = 0 Then
             PEON(I_Mode,1,IA1) = _
                PEON(I_Mode,1,IA1) + Rincr2N
          Else
             PEON(I_Mode,1,IA1) = _
                PEON(I_Mode,1,IA1) - Rincr2N
          End if
        End If  '/
        
        PEOZ(I_Mode,1,IA1) = PEOZ(I_Mode,1,IA1) * EOscale
        PEOZ(I_Mode,2,IA2) = PEOZ(I_Mode,2,IA2) * EOscale
        PEON(I_Mode,1,IA1) = PEON(I_Mode,1,IA1) * EOscale
        PEON(I_Mode,2,IA2) = PEON(I_Mode,2,IA2) * EOscale

          /' Energy sorting '/
     /' E1 = Etot * Gaussintegral(T2-T1,0.03); '/
        If Abs(T1-T2) < 1.E-6 Then
          E1 = 0.5E0 * Etot
        Else
          E1ES = Csort * T1 * T2 / ( Abs(T1 - T2) )
          E1ES = Min(E1ES,0.5E0*Etot)
           /' Asymptotic value after "complete" energy sorting '/
          E1FG = Etot * IA1 / I_A_CN  /' in Fermi-gas regime '/
          If Etot < 13 Then E1 = E1ES  ' complete energy sorting
          If Etot >= 13 and Etot <= 20 Then  ' transition region
            E1 = E1ES + (Etot-13)/7*(E1FG-E1ES)
          End If
          If Etot > 20 Then E1 = E1FG   ' Fermi-gas regime
        End If
        E2 = Etot - E1
        EPART(I_Mode,1,IA1) = Max(E1,0.0)  /' Mean E* in light fragment '/
        EPART(I_Mode,2,IA2) = Max(E2,0.0)  /' Mean E* in heavy fragment '/
      Next
    Next

  
   /'** RMS angular momentum of fission fragments **'/
   /' Following Naik et al., EPJ A 31 (2007) 195 and  '/
   /' S. G. Kadmensky, Phys. At. Nucl. 71 (2008) 1193 '/ 

   Scope
    Dim As Single AUCD   /' UCD fragment mass '/
    Dim As Single I_rigid_spher  /' I rigid for spherical shape '/
    Dim As Single I_rigid        /' I rigid for deformed scission shape '/
    Dim As Single I_eff          /' I with reduction due to pairing '/
    Dim As Single alph           /' deformation parameter '/
    Dim As Single E_exc          /' Excitation energy '/
    Dim As Single J_rms          /' rms angular momentum '/
/'>'/
    Dim As Integer ZT,AT         /' Z and A of target nucleus '/
    Dim As Single I_ISO          /' ISO number '/
    Dim As Integer I_MAT

    /' CN spin '/
    ZT = P_Z_CN
  '  AT = I_A_CN
    AT = P_A_CN
    If Emode = 2 Then AT = AT -1
    If Emode = 12 Then
      AT = AT -1
      ZT = ZT -1
    End If    
    
/'
/'<'/
    Spin_CN = P_J_CN
    P_I_rms_CN = P_J_CN
/'>'/
'/    
    
    I_MAT = I_MAT_ENDF(ZT,AT)
    IF I_E_iso = 0 Then ' fissioning nucleus or target in ground state
      If P_I_rms_CN = 0 Then
        Spin_CN =  NucTab(I_MAT).R_SPI
      Else
        Spin_CN = P_I_rms_CN
      End If    
    Else ' fissioning nucleus or target in isomeric state
      IF N_ISO_MAT(I_MAT) < I_E_iso Then
         Print "The isomer is not in the table of nuclear properties."
         Print "Z, A, #iso ",ZT,AT,I_E_iso
         Print "Please restart GEF."
         End
      End If
      Spin_CN = NucTab(I_MAT + I_E_iso).R_SPI
    End If   
 '   Print "ZT, AT, I_MAT",ZT, AT, I_MAT
 '   Print "SPIN_CN",Spin_CN
 '   Sleep
/'<'/    
    Spin_pre_fission = SPIN_CN  ' target or CN ground-state spin
/'>'/    
    /' Incoming neutron (spin + orbital) '/
    If Emode = 2 Then
       ' 2/3 * 1.16 * sqr(2 * 939.65) / 197.33 = 0.1699 
      If R_E_exc_used > 0 Then 
        Spin_pre_fission = sqr(Spin_pre_fission^2 + _ 
         + 0.5^2 + (0.1699 * AT^0.333333 * sqr(R_E_exc_used))^2)
      End If   
    End If  
    If Emode = 12 Then  /' preliminary (because Coulomb barrier neglected) '/
      If R_E_exc_used > 0 Then
        Spin_pre_fission = sqr(Spin_pre_fission^2 + _ 
         + 0.5^2 + (0.1699 * AT^0.333333 * sqr(R_E_exc_used))^2)
      End If   
    End If    
/'<'/    
    
    For IZ1 = 10 To I_Z_CN - 10
      AUCD = Int(Csng(IZ1) * Csng(I_A_CN) / Csng(I_Z_CN))
      For IA1 = Int(AUCD - 15) To Int(AUCD + 15)
       IN1 = IA1 - IZ1
       If IA1 - IZ1 >= 10 Then
        /' Rigid momentum of inertia for spherical nucleus '/
        I_rigid_spher = 1.16E0^2 * Csng(IA1)^1.6667E0 / 103.8415E0
                /' unit: hbar^2/MeV '/
        For I_Mode = 0 To 6  
          
          /' First (normally light) fission fragment: '/
          
          beta1 = Beta(I_Mode,1,IZ1)
          alph = beta1 / sqr(4.E0 * pi / 5.E0)
          I_rigid = I_rigid_spher * (1.E0 + 0.5E0*alph + 9.E0/7.E0*alph^2)
                  /' From Hasse & Myers, Geometrical Relationships ... '/
          E_exc = EPART(I_Mode,1,IA1)
          If E_exc < 0 Then E_exc = 0
          T = U_Temp(Csng(IZ1),Csng(IA1),E_exc,1,1,Tscale,Econd)          
       '   T = sqr(T^2 + 0.8^2)       ' For ZPM
       '   T = T_orbital
       '   T =  sqr(T^2 + T_orbital^2)
          If T_orbital > 0.1 Then
            T = T_orbital / tanh(T_orbital/T)  ' T_orbital represents the ZPM
          End If  
          I_eff = I_rigid * (1.E0 - 0.8E0 * exp(-0.693E0 * E_exc / 5.E0))
          J_rms = sqr(2.E0 * I_eff * T)  
          
          J_rms = J_rms * Jscaling 

          If IZ1 Mod 2 = 1 Or IN1 Mod 2 = 1 Then _ 
              J_rms = J_rms + Spin_odd * (Csng(IA1)/140.0)^0.66667 
       '                * Max(0,1 - (E_exc-1)/9) /' empirical '/
           /' Additional angular momentum of unpaired proton. '/ 
           /' See also Tomar et al., Pramana 68 (2007) 111 '/
           
' Print Z1,I_Mode,beta1,T,E_exc,Spin_CN         
' Print " ",I_rigid_spher,I_rigid,I_eff,J_rms

          J_rms = sqr(J_rms^2 + (IA1/I_A_CN * Spin_pre_fission)^2)
            
          SpinRMSNZ(I_Mode,1,IA1-IZ1,IZ1) = J_rms
          
    '     Print
    '     Print IA1,T,E_exc,I_rigid_spher,I_rigid,I_eff,J_rms

          /' Second (normally heavy) fission fragment: '/

          beta2 = Beta(I_Mode,2,IZ1)
          alph = beta2 / sqr(4.E0 * pi / 5.E0)
          I_rigid = I_rigid_spher * (1.E0 + 0.5E0*alph + 9.E0/7.E0*alph^2)
                  /' From Hasse & Myers, Geometrical Relationships ... '/
          E_exc = EPART(I_Mode,2,IA1)
          If E_exc < 0 Then E_exc = 0
          T = U_Temp(Csng(IZ1),Csng(IA1),E_exc,1,1,Tscale,Econd)          
      '    T = sqr(T^2 + 0.8^2)       ' For ZPM
      '    T = T_orbital
      '    T =  sqr(T^2 + T_orbital^2)
          If T_orbital > 0.1 Then
            T = T_orbital / tanh(T_orbital/T)  ' T_orbital represents the ZPM
          End If
          I_eff = I_rigid * (1.E0 - 0.8E0 * exp(-0.693E0 * E_exc / 5.E0))
          J_rms = sqr(2.E0 * I_eff * T)

          J_rms = J_rms * Jscaling 

          If IZ1 Mod 2 = 1 Or IN1 Mod 2 = 1 Then _ 
              J_rms = J_rms + Spin_odd * (Csng(IA1)/140.0)^0.66667  
      '                 * Max(0,1 - (E_exc-1)/9) /' empirical '/
           /' Additional angular momentum of unpaired proton. '/ 
           /' See also Tomar et al., Pramana 68 (2007) 111 '/
           
          J_rms = sqr(J_rms^2 + (IA1/I_A_CN * Spin_pre_fission)^2)
          
          SpinRMSNZ(I_Mode,2,IA1-IZ1,IZ1) = J_rms
          
   '      Print IA1,T,E_exc,I_rigid_spher,I_rigid,I_eff,J_rms          

        Next
       ENd If 
      Next
    Next
   End Scope
/'>'/   
    

   ' Print " "
   ' Print "Pre-routines finished at ";time;"."

/'
/'<'/

' ****************************************************************
' *** Filling arrays with results in the folding mode (GEFSUB) *** 
' ****************************************************************

 Dim As Integer Ic, Jc
 Dim As Single R_Help,Zs,R_Sum
 
 For I = 10 To I_A_CN - P_Z_CN - 10
   For J = 10 To P_Z_CN - 10
     For K = 0 To 6
       NZMPRE(K,I,J) = 0.0
     Next
   Next
 Next
 
 ' Mode 0
 For I = 20 To I_A_CN - 20
   Ic = I_A_CN - I
   R_Help = Yield_Mode_0 * (U_Gauss_mod(AC_Mode_0 - Csng(I), SigA_Mode_0) _ 
                 + U_Gauss_mod(AC_Mode_0 - Csng(Ic), SigA_Mode_0)) ' Mass yield
   If I < Ic Then
     Zs = ZShift(0,1,I)
   Else
     Zs = -ZShift(0,1,Ic)
   End If
   For J = 10 To P_Z_CN - 10 
     Jc = P_Z_CN - J
     If I-J >= 0 And Ic-Jc >= 0 And I-J <= 200 And Ic-Jc <= 200 Then
       NZMPRE(0,I-J,J) = R_Help * _ 
          U_Gauss_mod(Csng(P_Z_CN)/Csng(I_A_CN)*Csng(I) + Zs - Csng(J),SigPol_Mode_0) * _
          U_Even_Odd(J,PEOZ(0,1,I)) * U_Even_Odd(I-J,PEON(0,1,I))   
     End If     
   Next
 Next

 ' Mode 1
 For I = 20 To I_A_CN - 20
   Ic = I_A_CN - I
   R_Help = Yield_Mode_1 * (U_Gauss_mod(AC_Mode_1 - Csng(I), SigA_Mode_1) _
               + U_Gauss_mod(AC_Mode_1 - Csng(Ic), SigA_Mode_1)) ' Mass yield
   If I < Ic Then
     Zs = ZShift(1,1,I)
   Else
     Zs = -ZShift(1,1,Ic)
   End If  
   For J = 10 To P_Z_CN - 10 
     Jc = P_Z_CN - J
     If I-J >= 0 And Ic-Jc >= 0 And I-J <= 200 And Ic-Jc <= 200 Then
       NZMPRE(1,I-J,J) = R_Help * _ 
          U_Gauss_mod(Csng(P_Z_CN)/Csng(I_A_CN)*Csng(I) + Zs - Csng(J),SigPol_Mode_1)* _
          U_Even_Odd(J,PEOZ(1,1,I)) * U_Even_Odd(I-J,PEON(1,1,I))   
     End If    
   Next
 Next 
 
 ' Mode 2
 Dim As Single R_Cut1, R_Cut2
 For I = 20 To I_A_CN - 20
   Ic = I_A_CN - I
   R_Help = Yield_Mode_2 * (U_Box2(AC_Mode_2 - Csng(I), _
               sqr(2.0)*S2leftmod*SigA_Mode_2, _
               sqr(2.0)*SigA_Mode_2,P_A_Width_S2) + _
            U_Box2(AC_Mode_2 - Csng(Ic), _
               sqr(2.0)*S2leftmod*SigA_Mode_2, _
               sqr(2.0)*SigA_Mode_2,P_A_Width_S2))
   If I < Ic Then
     Zs = ZShift(2,1,I)
   Else
     Zs = -ZShift(2,1,Ic)
   End If   
   For J = 10 To P_Z_CN - 10
     Jc = P_Z_CN - J 
     If I-J >= 0 And Ic-Jc >= 0 And I-J <= 200 And Ic-Jc <= 200 Then
       R_Cut1 = R_Help
       R_Cut2 = R_Help
       If J > Jc Then
         R_Cut1 = R_Help * Gaussintegral(Csng(J)-ZTRUNC50,FTRUNC50*SigZ_Mode_2)
       Else 
         R_Cut2 = R_Help * Gaussintegral(Csng(J)-ZTRUNC50,FTRUNC50*SigZ_Mode_2)
       End If     
       NZMPRE(2,I-J,J) = R_Help * _ 
          U_Gauss_mod(Csng(P_Z_CN)/Csng(I_A_CN)*Csng(I) + Zs - Csng(J),SigPol_Mode_2) * _
         U_Even_Odd(J,PEOZ(2,1,I)) * U_Even_Odd(I-J,PEON(2,1,I))  
     End If     
   Next
 Next

 ' Mode 3
 For I = 20 To I_A_CN - 20
   Ic = I_A_CN - I
   R_Help = Yield_Mode_3 * (U_Gauss_mod(AC_Mode_3 - Csng(I), SigA_Mode_3) + _
                    U_Gauss_mod(AC_Mode_3 - Csng(Ic), SigA_Mode_3)) ' Mass yield   
   If I < Ic Then
     Zs = ZShift(3,1,I)
   Else
     Zs = -ZShift(3,1,Ic)
   End If   
   For J = 10 To P_Z_CN - 10 
     Jc = P_Z_CN - J
     If I-J >= 0 And Ic-Jc >= 0 And I-J <= 200 And Ic-Jc <= 200 Then
       NZMPRE(3,I-J,J) = R_Help * _ 
          U_Gauss_mod(Csng(P_Z_CN)/Csng(I_A_CN)*Csng(I) + Zs - Csng(J),SigPol_Mode_3) * _
         U_Even_Odd(J,PEOZ(3,1,I)) * U_Even_Odd(I-J,PEON(3,1,I))         
     End If     
   Next
 Next  
 
 ' Mode 4
 For I = 20 To I_A_CN - 20
   Ic = I_A_CN - I
   R_Help = Yield_Mode_4 * (U_Gauss_mod(AC_Mode_4 - Csng(I), SigA_Mode_4) + _
                    U_Gauss_mod(AC_Mode_4 - Csng(Ic), SigA_Mode_4)) ' Mass yield   
   If I < Ic Then
     Zs = ZShift(3,1,I)
   Else
     Zs = -ZShift(3,1,Ic)
   End If   
   For J = 10 To P_Z_CN - 10 
     Jc = P_Z_CN - J
     If I-J >= 0 And Ic-Jc >= 0 And I-J <= 200 And Ic-Jc <= 200 Then
       NZMPRE(4,I-J,J) = R_Help * _ 
          U_Gauss_mod(Csng(P_Z_CN)/Csng(I_A_CN)*Csng(I) + Zs - Csng(J),SigPol_Mode_4) * _
          U_Even_Odd(J,PEOZ(4,1,I)) * U_Even_Odd(I-J,PEON(4,1,I))         
     End If           
   Next
 Next   

 ' Mode 11
 For I = 20 To I_A_CN - 20
   Ic = I_A_CN - I
   R_Help = Yield_Mode_11 * (U_Gauss_mod(AC_Mode_0 - Csng(I), SigA_Mode_11) + _
                    U_Gauss_mod(AC_Mode_0 - Csng(Ic), SigA_Mode_11)) ' Mass yield   
   For J = 10 To P_Z_CN - 10 
     Jc = P_Z_CN - J
     If I-J >= 0 And Ic-Jc >= 0 And I-J <= 200 And Ic-Jc <= 200 Then
       NZMPRE(5,I-J,J) = R_Help * _ 
          U_Gauss_mod(Csng(P_Z_CN)/Csng(I_A_CN)*Csng(I) - Csng(J),SigPol_Mode_0) * _
          U_Even_Odd(J,PEOZ(5,1,I)) * U_Even_Odd(I-J,PEON(5,1,I))         
     End If    
   Next
 Next 
 
 ' Mode 22
 For I = 20 To I_A_CN - 20
   Ic = I_A_CN - I
   R_Help = Yield_Mode_22 * (U_Gauss_mod(AC_Mode_0 - Csng(I), SigA_Mode_22) + _
                    U_Gauss_mod(AC_Mode_0 - Csng(Ic), SigA_Mode_22)) ' Mass yield   
   For J = 10 To P_Z_CN - 10 
     Jc = P_Z_CN - J
     If I-J >= 0 And Ic-Jc >= 0 And I-J <= 200 And Ic-Jc <= 200 Then
       NZMPRE(6,I-J,J) = R_Help * _ 
          U_Gauss_mod(Csng(P_Z_CN)/Csng(I_A_CN)*Csng(I) - Csng(J),SigPol_Mode_0) * _
          U_Even_Odd(J,PEOZ(6,1,I)) * U_Even_Odd(I-J,PEON(6,1,I))        
     End If            
   Next
 Next  
 

 ' Normalization 
 R_Sum = 0
 For I = 10 To (I_A_CN - P_Z_CN) - 10
   For J = 10 To P_Z_CN - 10
     NZPRE(I,J) = 0
     For K = 0 To 6
       If NZMPRE(K,I,J) > 0 Then
         R_Sum = R_Sum + NZMPRE(K,I,J) 
         NZPRE(I,J) = NZPRE(I,J) + NZMPRE(K,I,J)  ' sum of all modes
       End If
     Next
   Next
 Next
 Print R_Sum
 For I = 10 To (I_A_CN - P_Z_CN) - 10
   For J = 10 To P_Z_CN - 10
     NZPRE(I,J) = NZPRE(I,J) / R_Sum
     For K = 0 To 6
       NZMPRE(K,I,J) = NZMPRE(K,I,J) / R_Sum
     Next
   Next
 Next    
 
 ' Calculate and store distributions of fragment excitation energy and spin
 
 Dim As Integer N_index,Z_index,A_index,M_index 
 Dim As Single Ymin = 1.E-7           ' Minimum yield to be stored
 Dim As Single Eexc_mean, Eexc_sigma
 Dim As Single Eexc_intr, Eexc_coll

 /' ***************** Begin Module GEFRESULTS ********************* '/
 Dim As Integer N_cases            ' Number of cases in NZMkey, Etab, Jtab and Ytab
 ' (First dimension of NZMkey, Etab, Jtab and Ytab)
 ReDim NZMkey(10000,3) As Integer  ' Key (Mode,N,Z) for E*, spin and yield distr. of fragments 
 ReDim Etab(10000,1000) As Single  ' Distribution of E*(exc. energy above yrast line 
    ' of fragments at scission (0.1 MeV bins).
    ' Note that E* = Etab + Erot_fragment with
    '      Erot_fragment =  Jtab * (Jtab + 1) / (2.0 * IfragEff),
    '      IfragEff = U_Ired(I_Z_fragment,I_A_fragment).
    '      Erot and Jtab are correlated!
 Redim Jtab(10000,100) As Single   ' Spin distribution of fragments
 ' (0 to 100 hbar for even-A or 1/2 to 201/2 hbar for odd-A nuclei)
 Redim Ytab(10000) As Single       ' Yield of fragments
 /' ****************** End Module GEFRESULTS ********************* '/
 
  
 N_cases = 0 
 For N_index = 10 To (I_A_CN - P_Z_CN) - 10   ' Neutron number
   For Z_index = 10 To P_Z_CN - 10            ' Atomic number
     For M_index = 0 To 6                     ' Fission channel
       If NZMPRE(M_index,N_index,Z_index) > Ymin Then
         N_cases = N_cases + 1
         If N_cases = Ubound(NZMkey,1) Then
           Print "Upper bound of NZkey reached"
           Print "Result will be incomplete"
           Exit For, For, For 
         End If
         NZMkey(N_cases,1) = M_index  ' Fission mode
         NZMkey(N_cases,2) = N_index  ' Neutron number of fragment
         NZMkey(N_cases,3) = Z_index  ' Atomic number of fragment
       End If
     Next  
   Next
 Next
 Print "N_cases  ",N_cases
 /'<FO WRITE (*,*) "N_cases ",N_cases FO>'/
 
 For K = 1 To N_cases
   M_index = NZMkey(K,1)   ' fission mode
   N_index = NZMkey(K,2)   ' neutron number
   Z_index = NZMkey(K,3)   ' atomic number 
   A_index = N_index + Z_index

   ' Yield
   Ytab(K) = NZMpre(M_index,N_index,Z_index)
   
   ' Angular momentum:
   For I = 1 To 100
     If M_index <= 4 Then
       If Z_index < 0.5 * P_Z_CN Then
         Jtab(K,I) = _
           U_LinGauss(Csng(I),SpinRMSNZ(M_index,1,N_index,Z_index)/sqr(2.0))
       Else
         Jtab(K,I) = _
           U_LinGauss(Csng(I),SpinRMSNZ(M_index,2,N_index,Z_index)/sqr(2.0))
       End If  
     End If
     If M_index = 5 Then
       Jtab(K,I) = _
         U_LinGauss(Csng(I),SpinRMSNZ(1,2,N_index,Z_index)/sqr(2.0))
     End If
     If M_index = 6 Then
       Jtab(K,I) = _
         U_LinGauss(Csng(I),SpinRMSNZ(2,2,N_index,Z_index)/sqr(2.0))
     End If
   Next 
   
   ' Normalize numerically (due to non-continuous values) 
   Scope
     Dim As Single Rint
     Rint = 0
     For I = 1 To 100
       Rint = Rint + Jtab(K,I)
     Next   
     If Rint > 0 Then
       For I = 1 To 100
         Jtab(K,I) = Jtab(K,I) / Rint
       Next  
     End If
   End Scope  
   
   
   ' Excitation energy:
   ' 1. Deformation energy at scission
   Dim As Single RS
   If M_index = 0 Then
     If Z_index < 0.5 * P_Z_CN Then
       Eexc_mean = Edefo(M_index,1,Z_index)
       Eexc_sigma = _
          ( Lymass(Csng(Z_index),Csng(A_index),beta(M_index,1,Z_index) + SIGDEFO_0) - _
            Lymass(Csng(Z_index),Csng(A_index),beta(M_index,1,Z_index) ))       
     Else
       Eexc_mean = Edefo(M_index,2,Z_index)
       Eexc_sigma = _ 
          ( Lymass(Csng(Z_index),Csng(A_index),beta(M_index,2,Z_index) + SIGDEFO_0) - _
            Lymass(Csng(Z_index),Csng(A_index),beta(M_index,2,Z_index) ))
     End If 
   End If
   If M_index > 0 And M_index <= 4 Then
     If Z_index < 0.5 * P_Z_CN Then
       Eexc_mean = Edefo(M_index,1,Z_index)
       RS = SIGDEFO/Sqr(R_Att_Sad(M_index))
       Eexc_sigma = _
          ( Lymass(Csng(Z_index),Csng(A_index),beta(M_index,1,Z_index) + RS) - _
            Lymass(Csng(Z_index),Csng(A_index),beta(M_index,1,Z_index) ))       
     Else
       Eexc_mean = Edefo(M_index,2,Z_index)
       RS = SIGDEFO/Sqr(R_Att_Sad(M_index))       
       Eexc_sigma = _ 
          ( Lymass(Csng(Z_index),Csng(A_index),beta(M_index,2,Z_index) + RS) - _
            Lymass(Csng(Z_index),Csng(A_index),beta(M_index,2,Z_index) ))
     End If
   End If    
   If M_index = 5 Then
     Eexc_mean = Edefo(1,2,Z_index)
     RS = SIGDEFO/Sqr(R_Att_Sad(M_index))       
     Eexc_sigma = _
          ( Lymass(Csng(Z_index),Csng(A_index),beta(1,2,Z_index) + RS) - _
            Lymass(Csng(Z_index),Csng(A_index),beta(1,2,Z_index) ))  
   End If
   If M_index = 6 Then
     Eexc_mean = Edefo(2,2,Z_index)  
     RS = SIGDEFO/Sqr(R_Att_Sad(M_index))       
     Eexc_sigma = _ 
          ( Lymass(Csng(Z_index),Csng(A_index),beta(2,2,Z_index) + RS) - _
            Lymass(Csng(Z_index),Csng(A_index),beta(2,2,Z_index) ))
   End If
   Eexc_mean = Max(Eexc_mean,0.0)
   
   ' 2. Intrinsic excitation energy at scission
   If Z_index < 0.5 * Csng(P_Z_CN) Then   
     Eexc_intr = EPART(M_index,1,A_index)
   Else
     Eexc_intr = EPART(M_index,2,A_index) 
   End If  
   If  M_index = 0 Then  ' add shell and pairing of final fragment
     Eexc_intr = Eexc_intr - _
      AME2012(Z_index,A_index) + LDMass(Csng(Z_index),Csng(A_index),0.) _  
             - 2.0 * 12.0 / sqr(Csng(A_index))     ' general shift  
   End If
   Eexc_intr = Max(Eexc_intr,0.0)
   Eexc_mean = Eexc_mean + Eexc_intr
   Eexc_sigma = sqr(Eexc_sigma^2 + (EexcSIGrel * Eexc_intr)^2)
   
   ' 3. Pairing staggering
   Eexc_mean = Eexc_mean - Lypair(Z_index,A_index)
   
   ' 4. Collective energy
   Eexc_coll = 0.5 * ECOLLFRAC * (De_Saddle_Scission(Csng(P_Z_CN)^2 / _
     Csng(I_A_CN)^0.33333E0,ESHIFTSASCI_coll) - E_tunn)
   Eexc_coll = Max(Eexc_coll,0.0)
   Eexc_sigma = sqr(Eexc_sigma^2 + 0.5*(EexcSIGrel*Eexc_coll)^2)
   Eexc_mean = Eexc_mean + Eexc_coll + 0.5 * E_coll_saddle(M_index)

   ' 5. Total excitation energy distribution of fragments (all contributions summed up)
        ' This is the value above the yrast line. Erot must be added!
   For I = 0 To 1000  ' 100 keV bins up to 100 MeV
     Etab(K,I) = exp(-(0.1*Csng(I)-Eexc_mean)^2/(2.0 * Eexc_sigma))
   Next
   
 ' Normalize excitation-energy distribution
   Scope
     Dim As Single RintE 
     RintE = 0
     For I = 0 To 1000
       RintE = RintE + Etab(K,I)
     Next   
     If RintE > 0 Then
       For I = 0 To 1000
         Etab(K,I) = Etab(K,I) / RintE
       Next  
     End If
   End Scope  
   
 Next



/'>'/
 '/
  For ILoop = 1 To NEVTused /' Loop for events with same parameters '/
  
  
      /' Chosing fission mode'/

     Scope
      Dim As Single R_Sum_0,R_Sum_1,R_Sum_2,R_Sum_3,R_Sum_4,R_Sum_5,R_Sum_6
      Dim As Single R_Choice,Rincr

      R_Choice = Rnd  
      R_Sum_0 = Yield_Mode_0
      R_Sum_1 = R_Sum_0 + Yield_Mode_1
      R_Sum_2 = R_Sum_1 + Yield_Mode_2
      R_Sum_3 = R_Sum_2 + Yield_Mode_3
      R_Sum_4 = R_Sum_3 + Yield_Mode_4
      R_Sum_5 = R_Sum_4 + Yield_Mode_11
      R_Sum_6 = R_Sum_5 + Yield_Mode_22   
      I_Mode = 6
      IF R_Choice < R_Sum_0 Then I_Mode = 0
      If R_Choice >= R_Sum_0 And R_Choice < R_Sum_1 Then I_Mode = 1
      If R_Choice >= R_Sum_1 And R_Choice < R_Sum_2 Then I_Mode = 2
      If R_Choice >= R_Sum_2 And R_Choice < R_Sum_3 Then I_Mode = 3
      If R_Choice >= R_Sum_3 And R_Choice < R_Sum_4 Then I_Mode = 4
      If R_Choice >= R_Sum_4 And R_Choice < R_Sum_5 Then I_Mode = 5
      If R_Choice >= R_Sum_5 And R_Choice < R_Sum_6 Then I_Mode = 6
      Mode_Events(I_Mode) = Mode_Events(I_Mode) + 1  
      Mode_Events(10) = Mode_Events(10) + 1  ' For normalization
     End Scope
     

    /' Chosing Z and A values '/
    Dim As Single R_A_help,RN
    Dim As Integer Iguess 
  DiceA:  
    Select Case I_Mode
      Case 0        
        R_A_heavy = PGauss(AC_Mode_0,SigA_Mode_0) /' random choice of mass '/
        RZpol = Zshift(0,2,CInt(R_A_heavy)) /' local polarization '/
        RZ = R_A_heavy * I_Z_CN / I_A_CN + RZpol /' mean position in Z for given mass '/
        R_Z_heavy = PGauss(RZ,SigPol_Mode_0) /' random choice of Z '/
      Case 1
        R_A_heavy = PGauss(AC_Mode_1,SigA_Mode_1)
        RZpol = Zshift(1,2,CInt(R_A_heavy)) /' local polarization '/
        RZ = R_A_heavy * I_Z_CN / I_A_CN + RZpol          
        R_Z_heavy = PGauss(RZ,SigPol_Mode_1)
      Case 2
        Iguess = 0
       Next2:
        Iguess = Iguess + 1
        R_A_heavy = PBox2(AC_Mode_2,SigA_Mode_2*S2leftmod,SigA_Mode_2,P_A_Width_S2)
        RZpol = Zshift(2,2,CInt(R_A_heavy))
        RZ = R_A_heavy * I_Z_CN / I_A_CN + RZpol       
        RN = R_A_heavy - RZ   
        Rtest = RND
    '    If Iguess < 1.E3 Then
    '      If Rtest > Gaussintegral(RN-82,1.5*SigZ_Mode_2) Then
    '        Goto Next2
    '      End If   
    '    End If
      '  If Iguess < 1.E3 Then
      '   If Rtest > Gaussintegral(RZ-ZTRUNC50,FTRUNC50*SigZ_Mode_2) Then 
      '    If  Rtest > 0.5 * ERF((RZ-ZTRUNC50)/(FTRUNC50*SigZ_Mode_2)) + 0.5E0 Or _
      '       Rtest > 0.5 * ERF((I_Z_CN - RZ - ZTRUNC28)/(FTRUNC28*SigZ_Mode_2)) + 0.5 Then 
      '      Goto Next2
      '    End If
      '  End If     
        /' truncation below Z = 35 and below Z = 50 due to properties of deformed shells '/
        R_Z_heavy = PGauss(RZ,SigPol_Mode_2)   
      Case 3
        R_A_heavy = PGauss(AC_Mode_3,SigA_Mode_3)
        RZpol = Zshift(3,2,R_A_heavy)
        RZ = R_A_heavy * I_Z_CN / I_A_CN + RZpol  
        R_Z_heavy = PGauss(RZ,SigPol_Mode_3)
      Case 4  
        If ZC_Mode_4 > ZC_Mode_0 Then
          R_A_heavy = PGauss(AC_Mode_4,SigA_Mode_4)
        Else
          R_A_heavy = I_A_CN - PGauss(AC_Mode_4,SigA_Mode_4)
              'AC_Mode_4 refers to the light fragment
        End If  
        RZpol = Zshift(4,2,R_A_heavy)
        RZ = R_A_heavy * I_Z_CN / I_A_CN + RZpol  
        R_Z_heavy = PGauss(RZ,SigPol_Mode_4)
      Case 5
        R_A_heavy = PGauss(AC_Mode_0,SigA_Mode_11)
        RZ = R_A_heavy * I_Z_CN / I_A_CN
        R_Z_heavy = PGAUSS(RZ,SigPol_Mode_0)
      Case 6
        R_A_heavy = PGauss(AC_Mode_0,SigA_Mode_22)
        RZ = R_A_heavy * I_Z_CN / I_A_CN
        R_Z_heavy = PGauss(RZ,SigPol_Mode_0)
      Case Else
    End Select

    R_Z_light = I_Z_CN - R_Z_heavy
    R_A_light = I_A_CN - R_A_heavy

    If R_A_heavy < 1 Or R_A_light < 1 Then Goto DiceA
    If R_A_heavy < R_A_light Then Goto DiceA

    /' Pre-neutron Z distribution, without even-odd effect '/
    I = CInt(R_Z_heavy)
    If I >= Lbound(ZPROV) and I <= Ubound(Zprov) Then
      ZPROV(I) = ZPROV(I) + Racc
      ZMPROV(I_Mode,I) = ZMPROV(I_Mode,I) + Racc
    End If 
    I  = CInt(R_Z_light)
    If I >= Lbound(ZPROV) and I <= Ubound(Zprov) Then 
      ZPROV(I) = ZPROV(I) + Racc
      ZMPROV(I_Mode,I) = ZMPROV(I_Mode,I) + Racc
    End If


    /' Provisional mass distribution, pre-neutron,
       directly deduced from Z distribution with UCD assumption '/

    APROV(CInt(R_Z_heavy * I_A_CN / I_Z_CN + 0.5)) = _
           APROV(CInt(R_Z_heavy * I_A_CN / I_Z_CN + 0.5)) + Racc
    APROV(CInt(R_Z_light * I_A_CN / I_Z_CN + 0.5)) = _
           APROV(CInt(R_Z_light * I_A_CN / I_Z_CN + 0.5)) + Racc
    AMPROV(I_Mode,CInt(R_Z_heavy * I_A_CN / I_Z_CN + 0.5)) = _
           AMPROV(I_Mode,CInt(R_Z_heavy * I_A_CN / I_Z_CN + 0.5)) + Racc
    AMPROV(I_Mode,CInt(R_Z_light * I_A_CN / I_Z_CN + 0.5)) = _
           AMPROV(I_Mode,CInt(R_Z_light * I_A_CN / I_Z_CN + 0.5)) + Racc


    /' Round on integer values with even-odd effect '/
    Dim As Single R_N_heavy
    Dim As Integer I_Z_heavy, I_A_heavy
    Dim As Integer I_Z_light, I_A_light
    Dim As Integer I_N_heavy, I_N_light
    Dim As Single ESIGDEFOlight,ESIGDEFOheavy
    Dim As Single RS

    R_N_heavy = R_A_heavy - R_Z_heavy

    I_N_heavy = Even_odd(R_N_heavy,PEON(I_Mode,2,CInt(R_A_heavy)))
    I_Z_heavy = Even_odd(R_Z_heavy,PEOZ(I_Mode,2,CInt(R_A_heavy)))
    I_A_heavy = I_N_heavy + I_Z_heavy
    
    I_N_light = I_N_CN - I_N_heavy
    I_Z_light = I_Z_CN - I_Z_heavy
    I_A_light = I_A_CN - I_A_heavy


    /' Correct mass distribution, pre-neutron '/

    APRE(I_A_heavy) = APRE(I_A_heavy) + Racc
    APRE(I_A_light) = APRE(I_A_light) + Racc
    AMPRE(I_Mode,I_A_heavy) = AMPRE(I_Mode,I_A_heavy) + Racc
    AMPRE(I_Mode,I_A_light) = AMPRE(I_Mode,I_A_light) + Racc
    
'    Q value from TF masses (M&S) + shell + pairing      
 '   Qvalue = U_Mass(I_Z_CN,I_A_CN) + Lypair(I_Z_CN,I_A_CN) - _
 '           (U_Mass(I_Z_heavy,I_A_heavy) + Lypair(I_Z_heavy,I_A_heavy) _
 '          + U_Mass(I_Z_light,I_A_light) + Lypair(I_Z_light,I_A_light) )    

'    Q value from 2012 mass evaluation 
    Qvalue = AME2012(I_Z_CN,I_A_CN) - _
              (AME2012(I_Z_heavy,I_A_heavy)  _
             + AME2012(I_Z_light,I_A_light))

    /' Excitation energy of fragments '/

    If I_Mode = 0 Then
      Eexc_light_mean = Edefo(I_Mode,1,I_Z_light) /' Only deformation energy '/
      Eexc_heavy_mean = Edefo(I_Mode,2,I_Z_heavy) /' Only deformation energy '/
      RS = SIGDEFO_0 
 '     RS = SIGDEFO_0 * Sqr(T_Pol_Mode_0 / TEgidy(P_A_CN,0.0,1.0))
         ' Scaling with Sqr(intrinsic temperature / const. T value)    
      ESIGDEFOlight = _
             ( Lymass(I_Z_light,I_A_light,beta(I_Mode,1,I_Z_light) + RS) - _
               Lymass(I_Z_light,I_A_light,beta(I_Mode,1,I_Z_light) ))    
      ESIGDEFOheavy = _
             ( Lymass(I_Z_heavy,I_A_heavy,beta(I_Mode,2,I_Z_heavy) + RS) - _
               Lymass(I_Z_heavy,I_A_heavy,beta(I_Mode,2,I_Z_heavy) ))
    End If
    If I_Mode > 0 And I_Mode <= 4 Then
      Eexc_light_mean = Edefo(I_Mode,1,I_Z_light) /' Only deformation energy '/
      Eexc_heavy_mean = Edefo(I_Mode,2,I_Z_heavy) /' Only deformation energy '/
      RS = SIGDEFO/Sqr(R_Att_Sad(I_Mode))
      ESIGDEFOlight = _
             ( Lymass(I_Z_light,I_A_light,beta(I_Mode,1,I_Z_light) + RS) - _
               Lymass(I_Z_light,I_A_light,beta(I_Mode,1,I_Z_light) ))
      ESIGDEFOheavy = _
             ( Lymass(I_Z_heavy,I_A_heavy,beta(I_Mode,2,I_Z_heavy) + RS) - _
               Lymass(I_Z_heavy,I_A_heavy,beta(I_Mode,2,I_Z_heavy) ))
               
' If beta(I_MOde,1,I_Z_light) > 0.68 Then
'   ESIGDEFOlight = 0.4*ESIGDEFOlight
' End If              
               
    End If  
    If I_Mode = 5 Then
      Eexc_heavy_mean = Edefo(1,2,I_Z_heavy)
      Eexc_light_mean = Edefo(1,2,I_Z_light)  /' Shell effect stored for "heavy" fragment '/
      RS = SIGDEFO/Sqr(R_Att_Sad(I_Mode))
      ESIGDEFOheavy = _
             ( Lymass(I_Z_heavy,I_A_heavy,beta(1,2,I_Z_heavy) + RS) - _
               Lymass(I_Z_heavy,I_A_heavy,beta(1,2,I_Z_heavy) ))
      ESIGDEFOlight = _
             ( Lymass(I_Z_light,I_A_light,beta(2,2,I_Z_light) + RS) - _
               Lymass(I_Z_light,I_A_light,beta(2,2,I_Z_light) ))
    End If
    If I_Mode = 6 Then
      Eexc_heavy_mean = Edefo(2,2,I_Z_heavy)
      Eexc_light_mean = Edefo(2,2,I_Z_light)  /' Shell effect stored for "heavy" fragment '/
      RS = SIGDEFO/Sqr(R_Att_Sad(I_Mode))
      ESIGDEFOheavy = _
             ( Lymass(I_Z_heavy,I_A_heavy,beta(2,2,I_Z_heavy) + RS) - _
               Lymass(I_Z_heavy,I_A_heavy,beta(2,2,I_Z_heavy) ))
      ESIGDEFOlight = _
             ( Lymass(I_Z_light,I_A_light,beta(2,2,I_Z_light) + RS) - _
               Lymass(I_Z_light,I_A_light,beta(2,2,I_Z_light) ))
    End If
    If Eexc_heavy_mean < 0 Then Eexc_heavy_mean = 0
    If Eexc_light_mean < 0 Then Eexc_light_mean = 0
    
    /' Deformation of heavy and light fragment assumed to be uncorrelated '/
    TKEmin = 1.44 * I_Z_light * I_Z_heavy / (3.0 * (I_A_light^0.33333 + I_A_heavy^0.3333))
    /' TKEmin for limiting the excitation energies of the fragments '/
    If Qvalue-TKEmin < 0 Then
      Print "<W> Estimated TKEmin > Qvalue: TKEmin = ";TKEmin;", Qvalue = ";Qvalue
      Print "    A_heavy = ";I_A_heavy;", A_light;";I_A_light;", Mode = ";I_Mode
      Print "    Z_heavy = ";I_Z_heavy;", Z_light;";I_Z_light;", Mode = ";I_Mode
      TKEmin = Qvalue - 1  
    End If
    Eexc_light = -1.
    While Eexc_light < 0. or Eexc_light > (Qvalue-TKEmin) * I_A_heavy / I_A_CN
      Eexc_light = PGAUSS(Eexc_light_mean,ESIGDEFOlight)
    Wend
    Eexc_heavy = -1.
    While Eexc_heavy < 0. or Eexc_heavy > (Qvalue-TKEmin) * I_A_light / I_A_CN 
      Eexc_heavy = PGAUSS(Eexc_heavy_mean,ESIGDEFOheavy)
    Wend
' If Eexc_light_mean > 50 or Eexc_heavy_mean > 50 Then    
' Print "Mode,Zlight,Alight,Eexc_light_mean",I_Mode,I_Z_light,I_A_light,Eexc_light_mean
' Print "Mode,Zheavy,Aheavy,Eexc_heavy_mean",I_Mode,I_Z_heavy,I_A_heavy,Eexc_heavy_mean
' End If
      
    /' Assumption: width in TKE is the '/
    /' quadratic sum of widths in defo and coll. '/
    /' Remark of caution: The width of the TKE for fixed mass contains often
       several fission modes. The width in Lang et al. for fixed Z contains several A,
       which contributes already with about 3% to the width. Therefore, the
       width in TXE (or TKE) for fixed A and fixed mode may be much smaller than
       4 or 5 percent! '/

    If Cint(10.0*Eexc_heavy) <= Ubound(Edefo2d,2) Then
      Edefo2d(I_A_heavy,CInt(10.0*Eexc_heavy))= _
                    Edefo2d(I_A_heavy,CInt(10*Eexc_heavy))+Racc
    End If                
    If Cint(10.0*Eexc_light) <= Ubound(Edefo2d,2) Then
      Edefo2d(I_A_light,CInt(10.0*Eexc_light))= _
                    Edefo2d(I_A_light,CInt(10*Eexc_light))+Racc
    End If                


    /' Temperatures of fragments '/

    If I_Mode <= 4 Then
      Tlight = Temp(I_Mode,1,I_A_light)
      Theavy = Temp(I_Mode,2,I_A_heavy)
    End If
    If I_Mode = 5 Then
      Tlight = Temp(1,2,I_A_light)
      Theavy = Temp(1,2,I_A_heavy)
    End If
    If I_Mode = 6 Then
      Tlight = Temp(2,2,I_A_light)
      Theavy = Temp(2,2,I_A_heavy)
    End If 


    /' Intrinsic excitation energies of fragments '/
    
    Dim As Single Delta_E_Q
    Dim As Integer I_E_Q
    Dim As Single E_intr_tot, Sigma_E_intr
    
    E_intr_light_mean = EPART(I_Mode,1,I_A_light)
    E_intr_heavy_mean = EPART(I_Mode,2,I_A_heavy)

    If I_Mode = 0 Then  
      ' At high E*(CN), where the SL (S0) mode is dominant,
      ' TKE is only a macroscopic quantity, microscopic contributions
      ' must be suppressed.
      ' TKE is derived from empirical formula.

      I_E_Q = 0
    Repeat_E_Q:
      I_E_Q = I_E_Q + 1
      E_intr_light = -1.
      E_intr_tot = E_intr_light_mean + E_intr_heavy_mean
      If E_intr_tot < 10 Then
            ' Fit function: Fluctuation of energy division from
            ' width of phase-space factor with Fermi-gas level density 
            ' (below 10 MeV: constant-T level density) 
        Sigma_E_intr = E_intr_tot * 0.47 * Exp(-Sqr(10/160)) / I_E_Q / 2.35
      Else
        Sigma_E_intr = E_intr_tot * 0.47 * Exp(-Sqr(E_intr_tot/160)) / I_E_Q / 2.35
      End If
      E_intr_light = PGauss(E_intr_light_mean, Sigma_E_intr)
      E_intr_heavy = E_intr_tot - E_intr_light 
' Print
' Print "At scission ";I_A_light;I_A_heavy;E_intr_light,E_intr_heavy     
  
      ' Add E* gain after scission due to shell and pairing of fragments.
      ' This way, they are removed from TKE (further down in the code).
      ' This is reasonable, because fragment shell and pairing energies appear after scisssion.
      ' Thus, they do not appear in the TKE.
      E_intr_light = E_intr_light - _
        AME2012(I_Z_light,I_A_light) + LDMass(I_Z_light,I_A_light,0.) _  ' shell and pairing
             - 2.0 * 12.0 / sqr(I_A_light)     ' general shift    
      E_intr_heavy = E_intr_heavy - _
        AME2012(I_Z_heavy,I_A_heavy) + LDMass(I_Z_heavy,I_A_heavy,0.) _  ' shell ande pairing
             - 2.0 * 12.0 / sqr(I_A_heavy)     ' general shift    
    Dim As Single Epre_mean,TKE_mac,TKE1,TKE2,dmod
      Epre_mean = 0.4 * De_Saddle_Scission(I_Z_CN^2/I_A_CN^0.333333,ESHIFTSASCI_coll)  
      ' Mean pre-scission kinetic energy 
      dmod = (dneck + 1) '  adjusted to TKE of 250Fm(E*=45 MeV) 
      TKE_mac = 1.44*I_Z_light*I_Z_heavy/ _  
          (1.16*(I_A_light^0.33333*(1.0+0.66667*Beta(0,1,I_Z_light)) + _
                 I_A_heavy^0.33333*(1.0+0.66667*Beta(0,2,I_Z_heavy))) +  dmod) _
              - 1.44*(I_Z_CN/2)^2 / _   
          (1.16*( (I_A_CN/2)^0.33333*(1.0+0.66667*Beta(0,1,I_Z_CN/2)) + _
                  (I_A_CN/2)^0.33333*(1.0+0.66667*Beta(0,2,I_Z_CN/2))) + dneck)
             ' (Variation of TKE from macroscopic formula of Wilkins et al.
             '  relative to value at symmetry.)
      Delta_E_Q = - AME2012(I_Z_CN,I_A_CN) + LDMass(I_Z_CN,I_A_CN,0.) _
                + LYMass(I_Z_light,I_A_light,0.) + LYMass(I_Z_heavy,I_A_heavy,0.) _
                - LYMass(I_Z_CN,I_A_CN,0) _
                - (LYMass(I_Z_CN/2,I_A_CN/2,0.) + LYMass(I_Z_CN/2,I_A_CN/2,0.) _
                - LYMass(I_Z_CN,I_A_CN,0)) _  
                + TKE_mac _  ' replace macr. variation of Q value by Wilkins formula              
                + Epre_mean  ' add pre-scission TKE  
      ' positives Delta_E_Q (ohne TKE_mac) erhöht TKE in den Flanken (mehr als TKE_mac)   
      ' positives Epre_mean erhöht TKE
      ' positives TKE_mac vermindert TKE in den Flanken   
                
      E_intr_light = E_intr_light - I_A_light/I_A_CN * (Delta_E_Q)
      E_intr_heavy = E_intr_heavy - I_A_heavy/I_A_CN * (Delta_E_Q) 
      
' Print, "After fission: ";E_intr_light,E_intr_heavy
      
      If E_intr_light < 0 or E_intr_heavy < 0 Then 
        If I_E_Q < 3 Then 
          Goto Repeat_E_Q
        End If  
 '   print "E_intr_light/heavy = ";E_intr_light; E_intr_heavy
 '   print "Mean values: ";E_intr_light_mean,E_intr_heavy_mean
        E_intr_light = Max(0.0,E_intr_light)
        E_intr_heavy = Max(0.0,E_intr_heavy)
      End If    
If E_intr_light < 0 or E_intr_heavy < 0 Then _ 
      Print "E_intr < 0",E_intr_light,E_intr_heavy        

    Else 

      Dim As Single TXE_shift
      If I_Z_CN Mod 2 = 0 Then  
        ' Even-odd fluctuation of TXE acc. to Lang et al. NPA 345 (1980) 34
        ' Lower TXE for completely paired configuration at scission.
        ' Only the even-odd effect at symmetry is considered, because the
        ' energy gain by the asymmetry-driven even-odd effect goes to E_intr
        ' of the heavy fragment due to energy sorting.
        If I_Z_light Mod 2 = 0 Then
          If RND < PEOZ(I_Mode,1,0.5E0*I_A_CN) Then
            TXE_shift = -12.E0/sqr(I_A_CN)
          End If   
        End If
      End If

      E_intr_light_mean = E_intr_light_mean + TXE_shift * _
                 E_intr_light_mean / (E_intr_light_mean + E_intr_heavy_mean)
      If E_intr_light_mean < 0. Then E_intr_light_mean = 0.
      E_intr_heavy_mean = E_intr_heavy_mean + TXE_shift * _
                   E_intr_heavy_mean / (E_intr_heavy_mean + E_intr_light_mean)             
      If E_intr_heavy_mean < 0. Then E_intr_heavy_mean = 0.

      E_intr_light = -1.
      While E_intr_light < 0.  
        E_intr_light = PGauss(E_intr_light_mean,EexcSIGrel* E_intr_light_mean+0.5)
      Wend
      E_intr_heavy = -1.
      While E_intr_heavy < 0.
        E_intr_heavy = PGauss(E_intr_heavy_mean,EexcSIGrel* E_intr_heavy_mean+0.5)
      Wend  
    
      E_intr_light = E_intr_light - Lypair(I_Z_light,I_A_light)
      E_intr_heavy = E_intr_heavy - Lypair(I_Z_heavy,I_A_heavy)  
          /' Staggering of BE of fragments by pairing '/   
          /' Assumption: pairing only felt in the lowest nuclear levels, '/
          /' at the end of the evaporation cascade '/
          /' (This should be a good assumption for higher excitation energies. '/
          /' Some deviations occur due to the even-odd effect at low exc. energies. '/
    

    
    End If ' If I_Mode <> 0
    
    
    /'*** Neutron emission between saddle and scission ***'/
    Dim As Integer I_nu_ss
    Scope
      Dim As Integer I
      Dim As Single E_Final_Light,J_Frag_light,TlightFF,R_Z_light_sci,R_A_light_sci
      Dim As Single E_Final_heavy,J_Frag_heavy,TheavyFF,R_Z_heavy_sci,R_A_heavy_sci
      
      I_nu_ss = 0
      If E_intr_light + E_intr_heavy > Escission_lim Then
        For I = 1 To UBound(Array_En_light)
          Array_En_light(I) = 0
          Array_Tn(I) = 0
        Next I 
        E_Final_Light = Escission_lim * Csng(I_A_light) / Csng(I_A_CN) - 9
        J_Frag_light = 0
        Eva(0,I_Z_light,I_A_light,E_intr_light,TlightFF,J_Frag_light,R_Z_Light_sci, _
                  R_A_Light_sci,E_Final_Light,Array_En_light(),Array_Tn(), _
                  Array_Eg0_light())
        For I = 1 To Ubound(Array_En_light)
          If Array_En_light(I) = 0 Then Exit For
          ENsci(Cint(Array_En_light(I)*1000)) = ENsci(Cint(Array_En_light(I)*1000)) + 1
          I_nu_ss = I_nu_ss + 1
        Next I
        I_A_light = R_A_light_sci
        I_Z_light = R_Z_light_sci
        I_N_light = I_A_light - I_Z_light
        E_intr_light =  E_Final_light

        For I = 1 To UBound(Array_En_heavy)
          Array_En_heavy(I) = 0
          Array_Tn(I) = 0
        Next 
        E_Final_Heavy = Escission_lim * Csng(I_A_heavy) / Csng(I_A_CN) - 9
        J_Frag_heavy = 0
        Eva(0,I_Z_heavy,I_A_heavy,E_intr_heavy,TheavyFF,J_Frag_heavy,R_Z_Heavy_sci, _
                  R_A_Heavy_sci,E_Final_Heavy,Array_En_heavy(),Array_Tn(), _
                  Array_Eg0_heavy()) 
        For I = 1 To Ubound(Array_En_heavy)
          If Array_En_heavy(I) = 0 Then Exit For
          ENsci(Cint(Array_En_heavy(I)*1000)) = ENsci(Cint(Array_En_heavy(I)*1000)) + 1
          I_nu_ss = I_nu_ss + 1
        Next I
        I_A_heavy = R_A_heavy_sci
        I_Z_heavy = R_Z_heavy_sci
        I_N_heavy = I_A_heavy - I_Z_heavy
        E_intr_heavy =  E_Final_heavy
      End If
      NNsci(I_nu_ss) = NNsci(I_nu_ss) + 1
      
    End Scope

    
    /' Nuclide distribution, pre-neutron '/

    NZPRE(I_N_heavy,I_Z_heavy) = NZPRE(I_N_heavy,I_Z_heavy) + 1
    NZPRE(I_N_light,I_Z_light) = NZPRE(I_N_light,I_Z_light) + 1
    NPRE(I_N_heavy) = NPRE(I_N_heavy) + Racc
    NPRE(I_N_light) = NPRE(I_N_light) + Racc
    NMPRE(I_Mode,I_N_heavy) = NMPRE(I_Mode,I_N_heavy) + Racc
    NMPRE(I_Mode,I_N_light) = NMPRE(I_Mode,I_N_light) + Racc

    ZISOPRE(I_A_Heavy,I_Z_Heavy) = ZISOPRE(I_A_Heavy,I_Z_Heavy) + Racc
    ZISOPRE(I_A_Light,I_Z_Light) = ZISOPRE(I_A_Light,I_Z_Light) + Racc



    If E_intr_light <= Ubound(Eintr2d,2) Then
      Eintr2d(I_A_light,CInt(10*E_intr_light))= _
                    Eintr2d(I_A_light,CInt(10*E_intr_light))+Racc
    End If
    If E_intr_heavy <= Ubound(Eintr2d,2) Then
      Eintr2d(I_A_heavy,CInt(10*E_intr_heavy))= _
                    Eintr2d(I_A_heavy,CInt(10*E_intr_heavy))+Racc
    End If

    Eexc_heavy = Eexc_heavy + E_intr_heavy
    Eexc_light = Eexc_light + E_intr_light
    /' Now: deformation + intrinsic excitation energy '/


    /' Collective energy of fragments '/
    
    Ecoll_mean = ECOLLFRAC * _ 
      (De_Saddle_Scission(I_Z_CN^2 / I_A_CN^0.33333E0,ESHIFTSASCI_coll) - E_tunn)

    If Ecoll_mean < 0. Then Ecoll_mean = 0.
    
    /' Experimental data of prompt neutron yields show an enhancement of the '/  
    /' neutron yield for odd-Z CN, corresponding to an enhanced E* by about 1.6 MeV '/
    /' The enhancement is equally shared to the light and the heavy fragment. '/
    /' The neutron number of the CN has no influence. '/ 
    /' The origin of this effect is not clear. '/
    /' By technical reasons, this additional energy is introduced here into the '/
    /' collective energy at scission, because this energy is divided equally between both fragments. '/
    /'  KHS, 31. Jan. 2012 '/
  '  If  (I_Z_CN Mod 2) = 1 Then Ecoll_mean = Ecoll_mean + 1.6
  ' This is not used any more. It seems to be wrong or to be replaced by something else.

'  Assuming positive correlation of Ecoll_heavy and Ecoll_light.
'  This is probably not realistic!
'  See Nix & Swiatecki, Nucl. Phys. 71 (1965) 1    
/'  Ecoll = -1.
    While Ecoll < 0.
      Ecoll = PGauss(Ecoll_mean,EexcSIGrel* Ecoll_mean)
    Wend  
    Ecoll = Ecoll + E_coll_saddle(I_Mode)
    Ecoll_heavy = 0.5E0 * Ecoll
    Ecoll_light = 0.5E0 * Ecoll '/
    
'   Assuming no correlation (partly positive, partly negative correlation)
    Scope
      Dim As Single Rmean,Rsigma 
      Rmean = 0.5 * Ecoll_mean
      Rsigma = sqr(0.5) * EexcSIGrel * Ecoll_mean
      Ecoll_heavy = -1.
      While Ecoll_heavy < 0. 
        Ecoll_heavy = PGauss(Rmean,Rsigma)
      Wend
      Ecoll_heavy = Ecoll_heavy + 0.5E0 * E_coll_saddle(I_Mode)
      Ecoll_light = -1    
      While Ecoll_light < 0. 
        Ecoll_light = PGauss(Rmean,Rsigma)
      Wend
      Ecoll_light = Ecoll_light + 0.5E0 * E_coll_saddle(I_Mode)
      Ecoll = Ecoll_heavy + Ecoll_light
    End Scope    
    
    Ecoll2d(I_A_heavy,CInt(10*Ecoll_heavy)) = _
        Ecoll2d(I_A_heavy,CInt(10*Ecoll_heavy)) + Racc
    Ecoll2d(I_A_light,CInt(10*Ecoll_light)) = _
        Ecoll2d(I_A_light,CInt(10*Ecoll_light)) + Racc

    Eexc_heavy = Eexc_heavy + Ecoll_heavy
    Eexc_light = Eexc_light + Ecoll_light
    /' Now: also collective excitation energy added '/
    
    
   /' Excitation energy not including the rotational energy at scission: '/
    If Eexc_heavy < 100 Then
      Eexc2d(I_A_heavy,CInt(10*Eexc_heavy))= _
                Eexc2d(I_A_heavy,CInt(Eexc_heavy))+Racc
    End If
    If Eexc_light < 100 Then            
      Eexc2d(I_A_light,CInt(10*Eexc_light))= _
                Eexc2d(I_A_light,CInt(Eexc_light))+Racc
    End If            

    /'** Angular momentum of fragments **'/

    Dim As Single J_Frag_light,J_Frag_heavy
    Dim As Single J_Frag_rot_light,J_Frag_rot_heavy ' collective spin
    Dim As Single N_J_attempt
    N_J_attempt = 0
 J_attempt:
    N_J_attempt = N_J_attempt + 1
    If I_Mode <= 4 Then
      J_Frag_light = PLinGauss(SpinRMSNZ(I_Mode,1,I_N_Light,I_Z_light)/sqr(2.0)) - 0.5
      J_Frag_heavy = PLinGauss(SpinRMSNZ(I_Mode,2,I_N_Heavy,I_Z_heavy)/sqr(2.0)) - 0.5
    End If
    If I_Mode = 5 Then
      J_Frag_light = PLinGauss(SpinRMSNZ(1,2,I_N_Light,I_Z_light)/sqr(2.0)) - 0.5
      J_Frag_heavy = PLinGauss(SpinRMSNZ(1,2,I_N_Heavy,I_Z_heavy)/sqr(2.0)) - 0.5
    End If
    If I_Mode = 6 Then
      J_Frag_light = PLinGauss(SpinRMSNZ(2,2,I_N_Light,I_Z_light)/sqr(2.0)) - 0.5
      J_Frag_heavy = PLinGauss(SpinRMSNZ(2,2,I_N_Heavy,I_Z_heavy)/sqr(2.0)) - 0.5
    End If  

    If J_Frag_light < 0 Then J_Frag_light = 0
    If J_Frag_light <= Ubound(JFRAGpre,3) Then
      JFRAGpre(I_N_light,I_Z_light,J_Frag_light) = JFRAGpre(I_N_light,I_Z_light,J_Frag_light) + 1
    End If
    
    If J_Frag_heavy < 0 Then J_Frag_heavy = 0
    If J_Frag_heavy <=  Ubound(JFRAGpre,3) Then
      JFRAGpre(I_N_heavy,I_Z_heavy,J_Frag_heavy) = JFRAGpre(I_N_heavy,I_Z_heavy,J_Frag_heavy) + 1
    End If
    
    Dim As Single Erotlight,Erotheavy,TXElight,TXEheavy
  

      Dim As Single IfragEff_light,IfragEff_heavy
    Scope
      Dim As Single TXE, E_total
  
      IfragEff_light = U_Ired(I_Z_light,I_A_light)  
      Erotlight =  J_Frag_light*(J_Frag_light+1)/(2*IfragEff_light)
         ' rotational energy at scission
      
      IfragEff_heavy = U_Ired(I_Z_heavy,I_A_heavy)
      Erotheavy =  J_Frag_heavy*(J_Frag_heavy+1)/(2*IfragEff_heavy)
         ' rotational energy at scission

  /' Kinetic energies of fragments '/
  
      ' TXE includes E*_CN 

      TXE = Eexc_light + Eexc_heavy + Erotlight + Erotheavy ' provisional value   
  
      If I_A_heavy >= Lbound(AQpre,1) and I_A_heavy <= Ubound(AQpre,1) and _
         Qvalue >= Lbound(AQpre,2) and Qvalue <= Ubound(AQpre,2) Then
        AQpre(I_A_heavy,Qvalue) = AQpre(I_A_heavy,Qvalue) + 1
      End If
      If I_A_light >= Lbound(AQpre,1) and I_A_light <= Ubound(AQpre,1) and _
         Qvalue >= Lbound(AQpre,2) and Qvalue <= Ubound(AQpre,2) Then
        AQpre(I_A_light,Qvalue) = AQpre(I_A_light,Qvalue) + 1
      End If
      If Qvalue < Ubound(Qvalues,1) and Qvalue > Lbound(Qvalues,1) Then
         Qvalues(Qvalue) = Qvalues(Qvalue) + 1
      End If
      E_total = Qvalue + R_E_exc_GS
      TKE = E_total - TXE
      If TKE < 0 Then
        If N_J_attempt <= 3 Then Goto J_attempt
        Dim As Single TXEcorr
        Print "<E> Event with excessive excitation energy found."
        Print "I_A_light,I_Z_light,I_A_heavy,I_Z_heavy",I_A_light,I_Z_light,I_A_heavy,I_Z_heavy
        Print "Q value,TKE",Qvalue,TKE
        Print "Eexc_light,Eexc_heavy,Erot_light,Erot_heavy",Eexc_light,Eexc_heavy,Erotlight,Erotheavy
        Print "J_rms_light,J_rms_heavy",SpinRMSNZ(I_Mode,1,I_N_Light,I_Z_light), _
                     SpinRMSNZ(I_Mode,2,I_N_Heavy,I_Z_heavy)
        Print "J_light,J_heavy",J_Frag_light,J_Frag_heavy
        Print "Ecoll_light,Eintr_light",Ecoll_light,E_intr_light
        Print "Ecoll_heavy,Eintr_heavy",Ecoll_heavy,E_intr_heavy
        Print "I_Mode,Edefo(light),Edefo(heavy)",I_Mode, _
                   Edefo(I_Mode,1,I_Z_light),Edefo(I_Mode,2,I_Z_heavy) 
        TXEcorr = TXE + TKE - 1
        TKE = 1
        Eexc_light = Eexc_light * TXEcorr/TXE
        Eexc_heavy = Eexc_heavy * TXEcorr/TXE
        TXE = TXEcorr
      End If
      
      TXElight = Eexc_light  + Erotlight    ' final values
      TXEheavy = Eexc_heavy  + Erotheavy    '  "      " 
      
    ' Print Erotlight,Erotlight_FF,Erotheavy,Erotheavy_FF      
      
      
      TXE = TXElight + TXEheavy            '  "      "  
      
      If TXE < Ubound(TotXE,1) and TXE > Lbound(TotXE,1) Then
         TotXE(CInt(TXE)) = TotXE(CInt(TXE)) + 1
      End If

      Ekinlight = TKE * I_A_heavy / I_A_CN
      Ekinheavy = TKE * I_A_light / I_A_CN

      If Ekinlight < Ubound(Ekinpre,1) and Ekinlight > Lbound(EKinpre,1) Then
         Ekinpre(CInt(Ekinlight)) = Ekinpre(CInt(Ekinlight)) + 1
         EkinpreM(I_Mode,CInt(Ekinlight)) = EkinpreM(I_Mode,CInt(Ekinlight)) + 1
      End If
      If Ekinheavy < Ubound(Ekinpre,1) and Ekinheavy > Lbound(EKinpre,1) Then
         Ekinpre(CInt(Ekinheavy)) = Ekinpre(CInt(Ekinheavy)) + 1
         EkinpreM(I_Mode,CInt(Ekinheavy)) = EkinpreM(I_Mode,CInt(Ekinheavy)) + 1
      End If
      If I_A_light < Ubound(AEkinpre,1) and I_A_light > Lbound(AEkinpre,1) and _
         Ekinlight+1 < Ubound(AEkinpre,2) and Ekinlight > Lbound(AEkinpre,2) Then
        AEkinpre(I_A_light,CInt(Ekinlight)) = AEkinpre(I_A_light,CInt(Ekinlight)) + 1
      End If    
      If I_A_heavy < Ubound(AEkinpre,1) and I_A_heavy > Lbound(AEkinpre,1) and _
         Ekinheavy+1 < Ubound(AEkinpre,2) and Ekinheavy > Lbound(AEkinpre,2) Then
        AEkinpre(I_A_heavy,CInt(Ekinheavy)) = AEkinpre(I_A_heavy,CInt(Ekinheavy)) + 1
      End If
      If TKE < Ubound(TKEpre,1) and TKE > Lbound(TKEpre,1) Then
         TKEpre(CInt(TKE)) = TKEpre(CInt(TKE)) + 1
         TKEpreM(I_Mode,CInt(TKE)) = TKEpreM(I_Mode,CInt(TKE)) + 1
      End If
      If I_A_light < Ubound(ATKEpre,1) and I_A_light > Lbound(ATKEpre,1) and _
         TKE + 1 < Ubound(ATKEpre,2) and TKE > Lbound(ATKEpre,2) Then
        ATKEpre(I_A_light,Cint(TKE)) = ATKEpre(I_A_light,Cint(TKE)) + 1 
      End If    
      If I_A_heavy < Ubound(ATKEpre,1) and I_A_heavy > Lbound(ATKEpre,1) and _
         TKE + 1 < Ubound(ATKEpre,2) and TKE > Lbound(ATKEpre,2) Then
        ATKEpre(I_A_heavy,Cint(TKE)) = ATKEpre(I_A_heavy,Cint(TKE)) + 1 
      End If    


    End Scope
    
 ' Print " "
    /'** Neutron evaporation from fragments **'/
    
    Ngtot = 0
    Nglight = 0
    Ngheavy = 0
    Egtot1000 = 0
    
    Dim As Single R_Z_Heavy_Post,R_A_Heavy_Post,R_N_Heavy_Post
    Dim As Single R_Z_Light_Post,R_A_Light_Post,R_N_Light_Post
    Dim As Single TLIGHTFF,THEAVYFF
    Dim As Single E_Final,E_Kin,E_Final_Light,E_Final_Heavy
    Dim As Single I_nu,I_nu_light,I_nu_heavy,I_nu_fr
    Dim As Integer I_Z_Heavy_Post,I_A_Heavy_Post,I_N_Heavy_Post
    Dim As Integer I_Z_Light_Post,I_A_Light_Post,I_N_Light_Post
    Dim As Single v_N,v_F,v_N_long,v_N_perp,v_N_cm,E_N_cm,E_N_cm_eV
    Dim As Single Epre,Epre_mean
    Dim As Single cos_alpha   
    Redim As Single Array_cos_alpha(100)
    Redim As Single Array_phi(100) 
    
     ' Pre-scission kinetic energy
    Epre = E_pot_scission + E_exc_Barr - Ecoll - E_intr_light - E_intr_heavy - Erotlight - Erotheavy
Epre_again:    
' Epre = Pgauss(5,5)
' Epre = Pgauss(7, 6.E0 * U_temp(I_Z_CN,I_A_CN,E_exc_Barr,1,1,Tscale,Econd))
' Epre = Pgauss(4, 6.E0 * U_temp(I_Z_CN,I_A_CN,E_exc_Barr,1,1,Tscale,Econd))
 Epre_mean = 0.4 * De_Saddle_Scission(I_Z_CN^2/I_A_CN^0.333333,ESHIFTSASCI_coll)
'Epre = Pgauss(Epre_mean, 6.E0 * U_temp(I_Z_CN,I_A_CN,E_exc_Barr,1,1,Tscale,Econd))
Epre = Pgauss(Epre_mean, Epre_mean)

 If Epre < 0 Then Goto Epre_again    

    If Epre < 0 Then Epre = 0

    If I_Mode <= 4 Then
      TlightFF = TempFF(I_Mode,1,I_A_light)
      TheavyFF = TempFF(I_Mode,2,I_A_heavy)
    End If
    If I_Mode = 5 Then  ' S11
      TlightFF = TempFF(1,2,I_A_light)
      TheavyFF = TempFF(1,2,I_A_heavy)
    End If   
    If I_Mode = 6 Then  ' S22
      TlightFF = TempFF(2,2,I_A_light)
      TheavyFF = TempFF(2,2,I_A_heavy)
    End If


    '*** Neutron evaporation from light fragment ***   

    In_post = 0

    For I = 1 To UBound(Array_En_light)
      Array_En_light(I) = 0
      Array_Vn_light_long(I) = 0
      Array_Vn_light_perp(I) = 0
      Array_Tn(I) = 0
    Next I 
    For I = 1 To UBound(Array_Eg0_light)
      Array_Eg0_light(I) = 0
    Next I  
    E_Final_Light = 0
    Eva(1,I_Z_light,I_A_light,Eexc_light,TlightFF,J_Frag_light,R_Z_Light_Post, _
                  R_A_Light_Post,E_Final_Light,Array_En_light(),Array_Tn(), _
                  Array_Eg0_light())
                  
 '  Print "E_Final_Light",E_Final_light   
 '  Print "E_Final_tot  ",E_Final_light + E_FInal_heavy      
 
    R_N_Light_Post = R_A_Light_Post - R_Z_Light_Post
    I_Z_Light_Post = CInt(R_Z_Light_Post)
    I_N_Light_Post = CInt(R_N_Light_Post)
    I_A_Light_Post = I_Z_Light_Post + I_N_Light_Post

    For I = 1 To UBound(Array_En_light)
      If Array_En_light(I) = 0 Then Exit For
      v_N = sqr(Array_En_light(I))    ' units of velocity sqrt(E/A), E in MeV
      If Array_Tn(I) > 100 Then
        v_F = sqr(Ekinlight/R_A_light)
      Else
        v_F = u_accel(I_A_light,I_Z_light,I_A_heavy,I_Z_heavy,TKE,Epre,Array_Tn(I))  
      End If  
      v_N_long = v_N * (2.E0*rnd - 1.E0)
      Array_Vn_light_long(I) = v_N_long
      v_N_perp = sqr(v_N^2 - v_N_long^2)
      Array_Vn_light_perp(I) = v_N_perp
      v_N_cm = sqr( (v_F+v_N_long)^2 + v_N_perp^2 )
      E_N_cm = v_N_cm^2

      In_post = In_post + 1
      Array_En_post(In_post) = E_N_cm
     
      ' cosine of angle between light fragment and neutron 
      ' (approximation: recoil of additional neutrons neglected
      cos_alpha = (v_F + v_N_long) / v_N_cm
      Array_cos_alpha(In_post) = cos_alpha
      Array_phi(In_post) = Rnd * 360
      J = Int(100*cos_alpha)
      If E_N_cm > 0.4 Then Ndirlight(J) = Ndirlight(J) + 1
      
   '   If E_N_cm > 0 and E_N_cm*10 < UBOUND(EN) Then
   '     EN(Int(E_N_cm*10)) = EN(Int(E_N_cm*10)) + 1 
   '       ' neutron spectrum in cm of CN, 100 keV steps
   '   End If
      For J  = 1 To Ubound(ENfrvar) ' Spectrum with variable bin size
        E_N_cm_eV = 1.E6 * E_N_cm
        If E_N_cm_eV >= ENfrvar_lim(J-1) and E_N_cm_eV < ENfrvar_lim(J) Then
          ENfrvar(J) = ENfrvar(J) + 1
          Exit For
        End If
      Next J
      If E_N_cm > 0 and E_N_cm*1000 < UBOUND(ENfr) Then
        ENfr(Int(E_N_cm*1000)) = ENfr(Int(E_N_cm*1000)) + 1 
          ' neutron spectrum in cm of CN, 1 keV steps
        If I_N_Multi < Ubound(ENfrC,1) And _
          I_Z_Multi < Ubound(ENfrC,2) Then
          ENfrC(I_N_Multi,I_Z_Multi,Int(E_N_cm*1000)) = _
               EnfrC(I_N_Multi,I_Z_Multi,Int(E_N_cm*1000)) + 1
        End If       
        ENM(I_Mode, Int(E_N_cm*1000)) = ENM(I_Mode, Int(E_N_cm*1000)) + 1
        ENlight(Int(E_N_cm*1000)) = ENlight(Int(E_N_cm*1000)) + 1
        ENApre2d(I_A_Light,Cint(E_N_cm*10)) = ENApre2d(I_A_Light,Cint(E_N_cm*10)) + 1
        ENApost2d(I_A_Light_Post,Cint(E_N_cm*10)) = _
               ENApost2d(I_A_Light_Post,Cint(E_N_cm*10)) + 1
        ENApre2dfs(I_A_Light,Cint(Array_En_light(I)*10)) = _
                    ENApre2dfs(I_A_Light,Cint(Array_En_light(I)*10)) + 1
        ENApost2dfs(I_A_Light_Post,Cint(Array_En_light(I)*10)) = _
               ENApost2dfs(I_A_Light_Post,Cint(Array_En_light(I)*10)) + 1
        ENfrfs(Cint(Array_En_light(I)*1000)) = ENfrfs(Cint(Array_En_light(I)*1000)) + 1         
      Else 
        Print "<W> Neutron kinetic energy outside spectrum range:"
        Print Ekinlight,TKE,R_A_light,I_A_light,Array_Tn(I)
        Print I,Array_En_light(I),v_N,v_F,v_N_long,v_N_cm,E_N_cm
        Print I_Z_light,I_N_light,Eexc_light,TlightFF,E_Final_light
      End If        
    Next I    

    APOST(I_A_light_Post)=APOST(I_A_light_Post)+Racc
    AMPOST(I_MODE,I_A_light_Post)=AMPOST(I_MODE,I_A_light_Post)+Racc

    ZPOST(I_Z_light_Post)=ZPOST(I_Z_light_Post)+Racc
    ZMPOST(I_Mode,I_Z_light_Post)=ZMPOST(I_Mode,I_Z_light_Post)+Racc

    NPOST(I_N_Light_Post)=NPOST(I_N_Light_Post)+Racc
    NMPOST(I_Mode,I_N_Light_Post)=NMPOST(I_Mode,I_N_Light_Post)+Racc

    NZPOST(I_N_light_Post,I_Z_light_post)= _
                   NZPOST(I_N_light_Post,I_Z_light_post)+Racc
                   
    I_nu_light = I_A_Light - I_A_Light_Post               
    If I_nu_light >= 0 and I_nu_light <= 50 Then
      NNlight(I_nu_light) = NNlight(I_nu_light) + 1
    End If
    If I_nu_light >= 0 and I_nu_light <= 20 Then
      N2dpost(I_A_Light_Post,I_nu_light)=N2dpost(I_A_Light_post,I_nu_light)+1
      N2dpre(I_A_Light,I_nu_light)=N2dpre(I_A_Light,I_nu_light)+1
    End If  


    '*** Neutron evaporation from heavy fragment ***   
    
    For I = 1 To UBound(Array_En_heavy)
      Array_En_heavy(I) = 0
      Array_Vn_heavy_long(I) = 0
      Array_Vn_heavy_perp(I) = 0
      Array_Tn(I) = 0
    Next 
    For I = 1 To UBound(Array_Eg0_heavy)
      Array_Eg0_heavy(I) = 0
    Next I       
    E_Final_Heavy = 0
    Eva(2,I_Z_heavy,I_A_heavy,Eexc_heavy,TheavyFF,J_Frag_heavy,R_Z_Heavy_Post, _
                  R_A_Heavy_Post,E_Final_Heavy,Array_En_heavy(),Array_Tn(), _
                  Array_Eg0_heavy()) 
                  
 '  Print "E_Final_Heavy",E_Final_Heavy   
 
    R_N_Heavy_Post = R_A_Heavy_Post - R_Z_Heavy_Post
    I_Z_Heavy_Post = CInt(R_Z_Heavy_Post)
    I_N_Heavy_Post = CInt(R_N_Heavy_Post)
    I_A_Heavy_Post = I_Z_Heavy_Post + I_N_Heavy_Post

    For I = 1 To UBound(Array_En_heavy)
      If Array_En_heavy(I) = 0 Then Exit For
      v_N = sqr(Array_En_heavy(I))    ' units of velocity sqrt(E/A), E in MeV
      If Array_Tn(I) > 100 Then
        v_F = sqr(Ekinheavy/R_A_heavy)
      Else
        v_F = u_accel(I_A_heavy,I_Z_heavy,I_A_light,I_Z_light,TKE,Epre,Array_Tn(I))  
      End If  
      v_N_long = v_N * (2.E0*rnd - 1.E0)
      Array_vN_heavy_long(I) = v_N_long
      v_N_perp = sqr(v_N^2 - v_N_long^2)
      Array_vN_heavy_perp(I) = v_N_perp
      v_N_cm = sqr( (v_F+v_N_long)^2 + v_N_perp^2 )
      E_N_cm = v_N_cm^2
      
      In_post = In_post + 1
      Array_En_post(In_post) = E_N_cm
      
      ' cosine of angle between light fragment and neutron 
      ' (approximation: recoil of additional neutrons neglected)
      cos_alpha = -(v_F + v_N_long) / v_n_cm
      Array_cos_alpha(In_post) = cos_alpha
      Array_phi(In_post) = Rnd * 360
      J = Int(100*cos_alpha)
      If E_N_cm > 0.4 Then Ndirlight(J) = Ndirlight(J) + 1            
      
  '    If E_N_cm > 0 and E_N_cm*10 < UBOUND(EN) Then
  '      EN(Int(E_N_cm*10)) = EN(Int(E_N_cm*10)) + 1 
  '        ' neutron spectrum in cm of CN, 100 keV steps
  '    End If   
      For J  = 1 To Ubound(ENfrvar) ' Spectrum with variable bin size
        E_N_cm_eV = 1.E6 * E_N_cm
        If E_N_cm_eV >= ENfrvar_lim(J-1) and E_N_cm_eV < ENfrvar_lim(J) Then
          ENfrvar(J) = ENfrvar(J) + 1
          Exit For
        End If
      Next J   
      If E_N_cm > 0 and E_N_cm*1000 < UBOUND(ENfr) Then
        ENfr(Int(E_N_cm*1000)) = ENfr(Int(E_N_cm*1000)) + 1 
          ' neutron spectrum in cm of CN, 1 keV steps
        If I_N_Multi < Ubound(ENfrC,1) And _
          I_Z_Multi < Ubound(ENfrC,2) Then
          ENfrC(I_N_Multi,I_Z_Multi,Int(E_N_cm*1000)) = _
               EnfrC(I_N_Multi,I_Z_Multi,Int(E_N_cm*1000)) + 1
        End If       
        ENM(I_Mode,Int(E_N_cm*1000))= ENM(I_Mode,Int(E_N_cm*1000)) + 1
        ENheavy(Int(E_N_cm*1000)) = ENheavy(Int(E_N_cm*1000)) + 1 
        ENApre2d(I_A_Heavy,Cint(E_N_cm*10)) = ENApre2d(I_A_Heavy,Cint(E_N_cm*10)) + 1
        ENApost2d(I_A_Heavy_Post,Cint(E_N_cm*10)) = _
               ENApost2d(I_A_Heavy_Post,Cint(E_N_cm*10)) + 1
        ENApre2dfs(I_A_Heavy,Cint(Array_En_heavy(I)*10)) = _
                    ENApre2dfs(I_A_Heavy,Cint(Array_En_heavy(I)*10)) + 1
        ENApost2dfs(I_A_Heavy_Post,Cint(Array_En_heavy(I)*10)) = _
               ENApost2dfs(I_A_Heavy_Post,Cint(Array_En_heavy(I)*10)) + 1
        ENfrfs(Cint(Array_En_heavy(I)*1000)) = ENfrfs(Cint(Array_En_heavy(I)*1000)) + 1         
      Else 
        Print "<W> Neutron kinetic energy outside spectrum range:"
        Print Ekinheavy,TKE,R_A_heavy,I_A_heavy,Array_Tn(I)
        Print I,Array_En_heavy(I),v_N,v_F,v_N_long,v_N_cm,E_N_cm
        Print I_Z_heavy,I_N_heavy,Eexc_heavy,TheavyFF,E_Final_heavy
      End If     
    Next              

    APOST(I_A_heavy_Post)=APOST(I_A_heavy_Post)+Racc
    AMPOST(I_MODE,I_A_heavy_Post)=AMPOST(I_MODE,I_A_heavy_Post)+Racc

    ZPOST(I_Z_heavy_Post)= ZPOST(I_Z_heavy_Post)+Racc
    ZMPOST(I_Mode,I_Z_heavy_post)=ZMPOST(I_Mode,I_Z_heavy_post)+Racc

    NPOST(I_N_Heavy_Post)=NPOST(I_N_Heavy_Post)+Racc
    NMPOST(I_Mode,I_N_Heavy_Post)=NMPOST(I_Mode,I_N_Heavy_Post)+Racc

    NZPOST(I_N_heavy_Post,I_Z_heavy_post)= _
        NZPOST(I_N_heavy_Post,I_Z_heavy_post)+Racc
    I_nu_heavy = I_A_heavy - I_A_Heavy_Post
    If I_nu_heavy >= 0 and I_nu_heavy <= 50 Then
      NNheavy(I_nu_heavy) = NNheavy(I_nu_heavy) + 1
    End If    
    If I_nu_heavy >= 0 and I_nu_heavy <= 20 Then
      N2dpost(I_A_Heavy_Post,I_nu_heavy)=N2dpost(I_A_Heavy_Post,I_nu_heavy)+1
      N2dpre(I_A_Heavy,I_nu_heavy)=N2dpre(I_A_Heavy,I_nu_heavy)+1
    End If  
    I_nu_fr = I_nu_heavy + I_nu_light
    If I_nu_fr >= 0 and I_nu_fr <= 50 Then
      NNfr(I_nu_fr) = NNfr(I_nu_fr) + 1
    End If
    


    '*** Post-neutron energies

    If TKE >= 0 and TKE <= 250 and I_nu_fr >=0 and I_nu_fr <=20 Then
      nuTKEpre(TKE,I_nu_fr) = nuTKEpre(TKE,I_nu_fr) + 1
    End If
 '   Ekinlight_post = Ekinlight * (I_A_light_post/I_A_Light)  ' not exact!
    vkinlight = sqr(Ekinlight/I_A_Light)   ' velocity in units of sqr(MeV/A)
    J = I_A_light
    For I = 1 To Ubound(Array_Vn_light_long)
      If Array_Vn_light_long(I) <> 0 Then
        J = J - 1
        vkinlight = vkinlight - Array_Vn_light_long(I) / J
        vkinlight = sqr(vkinlight^2 + (Array_Vn_light_perp(I)/J)^2 )
      End If 
    Next I
    Ekinlight_post = vkinlight^2 * I_A_light_post

 '   Ekinheavy_post = Ekinheavy * (I_A_heavy_post/I_A_heavy)  '  "    "
    vkinheavy = sqr(Ekinheavy/I_A_heavy)   ' velocity in units of sqr(MeV/A)
    J = I_A_heavy
    For I = 1 To Ubound(Array_Vn_heavy_long)
      If Array_Vn_heavy_long(I) <> 0 Then
        J = J - 1
        vkinheavy = vkinheavy - Array_Vn_heavy_long(I) / J
        vkinheavy = sqr(vkinheavy^2 + (Array_Vn_heavy_perp(I)/J)^2 )
      End If
    Next I
    Ekinheavy_post = vkinheavy^2 * I_A_heavy_post

    TKE_post = Ekinlight_post + Ekinheavy_post          
    If TKE_post >= 0 and TKE_post <= 250 and I_nu_fr >=0 and I_nu_fr <=20 Then
      nuTKEpost(TKE_post,I_nu_fr) = nuTKEpost(TKE_post,I_nu_fr) + 1
    End If    
    
    If Ekinlight_post < Ubound(Ekinpost,1) and Ekinlight_post > Lbound(EKinpost,1) Then
       Ekinpost(CInt(Ekinlight_post)) = Ekinpost(CInt(Ekinlight_post)) + 1
       EkinpostM(I_Mode,CInt(Ekinlight_post)) = EkinpostM(I_Mode,CInt(Ekinlight_post)) + 1
    End If
    If Ekinheavy_post < Ubound(Ekinpost,1) and Ekinheavy_post > Lbound(EKinpost,1) Then
       Ekinpost(CInt(Ekinheavy_post)) = Ekinpost(CInt(Ekinheavy_post)) + 1
       EkinpostM(I_Mode,CInt(Ekinheavy_post)) = EkinpostM(I_Mode,CInt(Ekinheavy_post)) + 1
    End If

    If TKE_post < Ubound(TKEpost,1) and TKE_post > Lbound(TKEpost,1) Then
       TKEpost(CInt(TKE_post)) = TKEpost(CInt(TKE_post)) + 1
       TKEpostM(I_Mode,CInt(TKE_post)) = TKEpostM(I_Mode,CInt(TKE_post)) + 1
    End If    
    If I_A_light_post < Ubound(AEkinpost,1) and I_A_light_post > Lbound(AEkinpost,1) and _
       Ekinlight_post+1 < Ubound(AEkinpost,2) and Ekinlight_post > Lbound(AEkinpost,2) Then
      AEkinpost(I_A_light_post,CInt(Ekinlight_post)) = AEkinpost(I_A_light_post,CInt(Ekinlight_post)) + 1
    End If      
    If I_A_heavy_post < Ubound(AEkinpost,1) and I_A_heavy_post > Lbound(AEkinpost,1) and _
       Ekinheavy_post+1 < Ubound(AEkinpost,2) and Ekinheavy_post > Lbound(AEkinpost,2) Then
      AEkinpost(I_A_heavy_post,CInt(Ekinheavy_post)) = AEkinpost(I_A_heavy_post,CInt(Ekinheavy_post)) + 1
    End If
    If I_A_light_post < Ubound(ATKEpost,1) and I_A_light_post > Lbound(ATKEpost,1) and _
       TKE_post + 1 < Ubound(ATKEpost,2) and TKE_post > Lbound(ATKEpost,2) Then
      ATKEpost(I_A_light_post,Cint(TKE_post)) = ATKEpost(I_A_light_post,Cint(TKE_post)) + 1 
    End If                 
    If I_A_heavy_post < Ubound(ATKEpost,1) and I_A_heavy_post > Lbound(ATKEpost,1) and _
       TKE_post + 1 < Ubound(ATKEpost,2) and TKE_post > Lbound(ATKEpost,2) Then
      ATKEpost(I_A_heavy_post,Cint(TKE_post)) = ATKEpost(I_A_heavy_post,Cint(TKE_post)) + 1 
    End If    

    ZISOPOST(I_A_Heavy_Post,I_Z_Heavy_post)= _
                 ZISOPOST(I_A_Heavy_Post,I_Z_Heavy_post)+Racc
    ZISOPOST(I_A_Light_Post,I_Z_Light_post)= _
                 ZISOPOST(I_A_Light_Post,I_Z_Light_post)+Racc

    If J_Frag_light <= Ubound(JFRAGpost,3) Then
      JFRAGpost(I_N_light_post,I_Z_light_post,J_Frag_light) = _
           JFRAGpost(I_N_light_post,I_Z_light_post,J_Frag_light) + 1
    End If
    If J_Frag_heavy <= Ubound(Jfragpost,3) Then
      JFRAGpost(I_N_heavy_post,I_Z_heavy_post,J_Frag_heavy) = _
           JFRAGpost(I_N_heavy_post,I_Z_heavy_post,J_Frag_heavy) + 1
    End If

  /' Scission neutrons (for testing only, not considered in energy balance!) '/
  /'If Rnd < 0.3 Then
      E_N_cm = PMaxwell(0.5)
      EN(Int(E_N_cm*1000)) = EN(Int(E_N_cm*1000)) + 1
    End If '/


  /' Prompt gamma emission '/

    Scope
      Dim As Single Einit,Eg,Erest
      Dim As Single Jfrag,Ifrag  ' angular momentum, inertia
    '  Dim As Single I_rigid_spher_yrast,I_eff
    '  Dim As Integer Nspectrum,N,I,Icount
      Dim As Single RJeff
      Dim As Integer Nspectrum
      Dim As Single xran,yran
      Dim As Integer I_MAT,N_iso,I_iso
      Dim As Single alph
      Dim As Single J,K

  ' 1st (light) fragment    
  
' I_Z_light = 36
' I_A_light_post = 92    
' Print "92Kr"
  
      I_MAT = I_MAT_ENDF(I_Z_light,I_A_light_post)
      Spin_gs_light =  NucTab(I_MAT).R_SPI  ' ground.state spin of light fragment
      J_frag_rot_light = ( (J_frag_light - Spin_gs_light) \ 2 ) * 2  ' collective spin
      Jfrag = J_frag_rot_light
' Jfrag = 10      

      Einit = E_Final_Light 
 '    Entrance energy above yrast line (energy after last prompt neutron)
      Nspectrum = Einit*1000 
      If Nspectrum > 0 and Nspectrum < Ubound(Eentrance) Then
        Eentrance(Nspectrum) = Eentrance(Nspectrum) + 1
      End If
      
  '   E1 gammas
      Erest = E_Final_Light  
      
      Ig1_light = 0
      While Erest >= 0.1   
        Eg = P_Egamma_low(I_Z_light, I_A_light_post, Erest)  ' in MeV
        Ig1_light = Ig1_light + 1 
        Array_Eg1_light(Ig1_light) = Eg
        
        xran = 1000.*Eg  ' in keV

     '  Accumulate E1 gammas
        Nspectrum = CInt(xran)
        If Nspectrum > 0 Then
          If Nspectrum <= Ubound(Egamma) Then
            Egamma(Nspectrum) = Egamma(Nspectrum) + 1         ' 1 keV units
           #Ifdef B_EgammaA 
            StoreEgammaA(Nspectrum,I_A_light)
           #EndIf 
            EgammaL(Nspectrum) = EgammaL(Nspectrum) + 1
          End If
          Nglight = Nglight + 1
          Ngtot = Ngtot + 1
          Egtot1000 = Egtot1000 + xran    ' in 1 keV units
        End If  
        Erest = Erest - Eg  '  in MeV units
      Wend
 
      
      ' Rotational band (note that Erest is above the yrast line!)
      IfragEff_light = U_IredFF(I_Z_light,I_A_light_post) 
      beta1 = DEFOtab(I_A_light_post - I_Z_light, I_Z_light)
      alph = beta1 / sqr(4.E0 * pi / 5.E0)
      IfragEff_light = IfragEff_light * (1.E0 + 0.5E0*alph + 9.E0/7.E0*alph^2)
                  /' From Hasse & Myers, Geometrical Relationships ... '/      
      Ig2_light = 0
      N_iso = N_ISO_MAT(I_MAT)
      If N_iso > 0 Then
        I_iso = ISO_for_MAT(I_MAT)
      End If    
      For J = Jfrag to 2 step -2
           ' Stop gamma cascade at next isomer:
        If N_iso > 0 And I_DelGam = 0 Then 
'Print        
'Print "Z,A",I_Z_light,I_A_light_post
'Print "N_iso,N_states",N_iso,Isotab(I_iso).N_STATES        
          For K = 1 To Isotab(I_iso).N_STATES
            If J = Jfrag Then
              If Isotab(I_iso).R_SPI(K) = Cint(J_frag_light + Spin_gs_light) Then Exit For, For
            End If  
'Print "K,Spin",K,Isotab(I_iso).R_SPI(K)          
            If Isotab(I_iso).R_SPI(K) <= Jfrag + Spin_gs_light + 0.01 Then
              If Isotab(I_iso).R_SPI(K) >= J + Spin_gs_light - 0.01 Then Exit For, For
            End If  
          Next K   
'Print "Jfrag,J",Jfrag,J          
        End If
           ' Influence of shell effect: transition from rotation to vibration:
        RJeff = 2 + (J - 2) * ( U_I_Shell(I_Z_light,I_A_light_post) )^2 
           ' Influence of pairing: transition from irrotational flow to rigid body:
        RJeff = 0.45 * RJeff + 0.65 * RJeff * Max( (12.0 - RJeff)/12.0 , 0.0)     
        Eg = RJeff*(RJeff+1)/(2*IfragEff_light) - (RJeff-2)*(RJeff-1)/(2*IfragEff_light)
'Print J,Eg        
        Ig2_light = Ig2_light + 1
        Array_Eg2_light(Ig2_light) = Eg        
        Nspectrum = Int(Eg*1000 + 0.5)   ' 1 keV units
        If Nspectrum > 0 Then
          Nglight = Nglight + 1
          Ngtot = Ngtot + 1
          Egtot1000 = Egtot1000 + Eg * 1000
          If Nspectrum <= Ubound(Egamma) Then 
            Egamma(Nspectrum) = Egamma(Nspectrum) + 1
           #Ifdef B_EgammaA 
            StoreEgammaA(Nspectrum,I_A_light)
           #EndIf  
            EgammaL(Nspectrum) = EgammaL(Nspectrum) + 1
          End If 
          If Nspectrum <= Ubound(EgammaE2) Then 
            EgammaE2(Nspectrum) = EgammaE2(Nspectrum) + 1
          End If 
        End If      
      Next   

  ' 2nd (heavy) fragment   
  
' Input I_Z_Heavy, I_A_heavy_post, J_frag_heavy
  
' I_Z_heavy = 58
' I_A_heavy_post = 140     
' Print "58,140"
  
  
      I_MAT = I_MAT_ENDF(I_Z_heavy,I_A_heavy_post)      
      Spin_gs_heavy =  NucTab(I_MAT).R_SPI  ' ground.state spin of light fragment
      J_frag_rot_heavy = ( (J_frag_heavy - Spin_gs_heavy) \ 2 ) * 2   ' collective spin
      Jfrag = J_frag_rot_heavy

      Einit = E_Final_Heavy
 '    Entrance energy above yrast line (energy after last prompt neutron)
      Nspectrum = Einit*1000 
      If Nspectrum > 0 and Nspectrum < Ubound(Eentrance) Then
        Eentrance(Nspectrum) = Eentrance(Nspectrum) + 1
      End If
      
    ' E1 gammas
      Erest = E_Final_Heavy   
       
      Ig1_heavy = 0 
      While  Erest >= 0.1
        Eg = P_Egamma_low(I_Z_heavy, I_A_heavy_post, Erest)   ' in MeV
        Ig1_heavy = Ig1_heavy + 1 
        Array_Eg1_heavy(Ig1_heavy) = Eg   
             
        xran = 1000 * Eg  
      
      ' Accumulate E1 gammas
        Nspectrum = CInt(xran)
        If Nspectrum > 0 Then
          If Nspectrum <= Ubound(Egamma) Then
            Egamma(Nspectrum) = Egamma(Nspectrum) + 1         ' 1 keV units
           #If EgammaA 
            StoreEgammaA(Nspectrum,I_A_heavy)
           #EndIf 
            EgammaH(Nspectrum) = EgammaH(Nspectrum) + 1
          End If
          Ngheavy = Ngheavy + 1
          Ngtot = Ngtot + 1
          Egtot1000 = Egtot1000 + xran    ' in 1 keV units
        End If  
        Erest = Erest - Eg  '  in MeV units
      Wend 

      ' Rotational band (note that Erest is above the yrast line!)
      IfragEff_heavy = U_IredFF(I_Z_heavy,I_A_heavy_post) 
      beta1 = DEFOtab(I_A_heavy_post - I_Z_heavy, I_Z_heavy)
      alph = beta1 / sqr(4.E0 * pi / 5.E0)
      IfragEff_heavy = IfragEff_heavy * (1.E0 + 0.5E0*alph + 9.E0/7.E0*alph^2)
                  /' From Hasse & Myers, Geometrical Relationships ... '/   
      Ig2_heavy = 0
      N_iso = N_ISO_MAT(I_MAT)
      If N_iso > 0 Then
        I_iso = ISO_for_MAT(I_MAT)
      End If       
      For J = Jfrag to 2 step -2
           ' Stop gamma cascade at next isomer:
        If N_iso > 0 And I_DelGam = 0 Then 
          For K = 1 To Isotab(I_iso).N_STATES
            If J = Jfrag Then
              If Isotab(I_iso).R_SPI(K) = Cint(J_frag_heavy + Spin_gs_heavy) Then Exit For, For
            End If  
            If Isotab(I_iso).R_SPI(K) <= Jfrag + Spin_gs_heavy + 0.01 Then
              If Isotab(I_iso).R_SPI(K) >= J + Spin_gs_heavy - 0.01 Then Exit For, For
            End If  
          Next K   
        End If      
           ' Influence of shell effect: transition from rotation to vibration:
        RJeff = 2 + (J - 2) * ( U_I_Shell(I_Z_heavy,I_A_heavy_post) )^2   
           ' Influence of pairing: transition from irrotational flow to rigid body:
        RJeff = 0.45 * RJeff + 0.65 * RJeff * Max( (12.0 - RJeff)/12.0 , 0.0)     
        Eg = RJeff*(RJeff+1)/(2*IfragEff_heavy) - (RJeff-2)*(RJeff-1)/(2*IfragEff_heavy)
'Print J,Eg        
        Ig2_heavy = Ig2_heavy + 1
        Array_Eg2_heavy(Ig2_heavy) = Eg        
        Nspectrum = Int(Eg*1000 + 0.5)   ' 1 keV units
        If Nspectrum > 0 Then
          Ngheavy = Ngheavy + 1        
          Ngtot = Ngtot + 1
          Egtot1000 = Egtot1000 + Eg * 1000
          If Nspectrum <= Ubound(Egamma) Then 
            Egamma(Nspectrum) = Egamma(Nspectrum) + 1
           #Ifdef B_EgammaA 
            StoreEgammaA(Nspectrum,I_A_heavy)
           #EndIf  
            EgammaH(Nspectrum) = EgammaH(Nspectrum) + 1
          End If 
          If Nspectrum <= Ubound(EgammaE2) Then 
            EgammaE2(Nspectrum) = EgammaE2(Nspectrum) + 1
          End If 
        End If      
      Next 
    
    End Scope
' sleep    

    
    If Nglight <= Ubound(NgammaA,2) Then _
    NgammaA(I_A_light,Nglight) = NgammaA(I_A_light,Nglight) + 1
    If Ngheavy <= Ubound(NgammaA,2) Then _
    NgammaA(I_A_heavy,Ngheavy) = NgammaA(I_A_heavy,Ngheavy) + 1
    If Ngtot <= Ubound(Ngammatot) Then Ngammatot(Ngtot)=Ngammatot(Ngtot) + 1  
    If CInt(Egtot1000) <= Ubound(Egammatot,1) Then _
    Egammatot(CInt(Egtot1000))=Egammatot(CInt(Egtot1000)) + 1  


    Nglight = 0
    Ngheavy = 0
    Ngtot = 0
    Egtot1000 = 0


    /' Keeping track of pre-fission particle emission '/
    Scope 
      Dim As String C_test
      Dim As ULongint Ilong
      Redim As Integer IE_array(10)
      
      ' 1. Multiplicities
      NP(I_Z_Multi) = NP(I_Z_Multi) + 1 
      NNCN(I_N_Multi) = NNCN(I_N_Multi) + 1
      I_nu = I_nu_fr + I_nu_ss + I_N_Multi
      NN(I_nu) = NN(I_nu) + 1

      ' 2. Energies  
      ' (This is also a preparation for the list-mode output.)
      Select Case I_A_multi
        Case 1
          For J = J_multi_last(1) To 2 * Imulti
            K = J Mod Ubound(En_multi_1) + 1
            If En_multi_1(K) Mod 2^10 = I_E_Multi And _
                      PLoss(I_emit_1(K)) = I_Z_Multi Then 
              J_multi_last(1) = K 
              C_test = Oct(I_emit_1(K))
           '   Ilong = Fix(En_multi_1(K)*2^-10)
              Ilong = En_multi_1(K) SHR 10
              IE_array(1) = Modulo(Ilong,2^10)
              Exit For
            End If
          Next
        Case 2
          For J = J_multi_last(2) To 2 * Imulti
            K = J Mod Ubound(En_multi_2) + 1
            If En_multi_2(K) Mod 2^10 = I_E_Multi And _
                  PLoss(I_emit_2(K)) = I_Z_Multi Then 
              J_multi_last(2) = K
              C_test = Oct(I_emit_2(K))
          '    Ilong = Fix(En_multi_2(K)*2^-10)
              Ilong = En_multi_2(K) SHR 10
              IE_array(1) = Modulo(Ilong,2^10)
          '    Ilong = Fix(En_multi_2(K)*2^-20)
              Ilong = En_multi_2(K) SHR 20
              IE_array(2) = Modulo(Ilong,2^10)
              Exit For
            End If
          Next            
        Case 3
          For J = J_multi_last(3) To 2 * Imulti
            K = J Mod Ubound(En_multi_3) + 1
            If En_multi_3(K) Mod 2^10 = I_E_Multi And _
                  PLoss(I_emit_3(K)) = I_Z_Multi Then 
              J_multi_last(3) = K
              C_test = Oct(I_emit_3(K))
             ' Ilong = Fix(En_multi_3(K)*2^-10)
              Ilong = En_multi_3(K) SHR 10
              IE_array(1) = Modulo(Ilong,2^10)
             ' Ilong = Fix(En_multi_3(K)*2^-20)
              Ilong = En_multi_3(K) SHR 20
              IE_array(2) = Modulo(Ilong,2^10)
             ' Ilong = Fix(En_multi_3(K)*2^-30)
              Ilong = En_multi_3(K) SHR 30
              IE_array(3) = Modulo(Ilong,2^10)
              Exit For
            End If
          Next            
        Case 4
          For J = J_multi_last(4) To 2 * Imulti
            K = J Mod Ubound(En_multi_4) + 1 
            If En_multi_4(K) Mod 2^10 = I_E_Multi And _
                  PLoss(I_emit_4(K)) = I_Z_Multi Then 
              J_multi_last(4) = K
              C_test = Oct(I_emit_4(K))
             ' Ilong = Fix(En_multi_4(K)*2^-10)
              Ilong = En_multi_4(K) SHR 10
              IE_array(1) = Modulo(Ilong,2^10)
             ' Ilong = Fix(En_multi_4(K)*2^-20)
              Ilong = En_multi_4(K) SHR 20
              IE_array(2) = Modulo(Ilong,2^10)
             ' Ilong = Fix(En_multi_4(K)*2^-30)
              Ilong = En_multi_4(K) SHR 30
              IE_array(3) = Modulo(Ilong,2^10)
             ' Ilong = Fix(En_multi_4(K)*2^-40)
              Ilong = En_multi_4(K) SHR 40
              IE_array(4) = Modulo(Ilong,2^10)
              Exit For
            End If
          Next            
        Case 5 
          For J = J_multi_last(5) To 2 * Imulti
            K = J Mod Ubound(En_multi_5) + 1
            If En_multi_5(K) Mod 2^10 = I_E_Multi And _
                  PLoss(I_emit_5(K)) = I_Z_Multi Then 
              J_multi_last(5) = K
              C_test = Oct(I_emit_5(K))
             ' Ilong = Fix(En_multi_5(K)*2^-10) 
              Ilong = En_multi_5(K) SHR 10
              IE_array(1) = Modulo(Ilong,2^10)
             ' Ilong = Fix(En_multi_5(K)*2^-20)
              Ilong = En_multi_5(K) SHR 20
              IE_array(2) = Modulo(Ilong,2^10)
             ' Ilong = Fix(En_multi_5(K)*2^-30)
              Ilong = En_multi_5(K) SHR 30
              IE_array(3) = Modulo(Ilong,2^10)
             ' Ilong = Fix(En_multi_5(K)*2^-40)
              Ilong = En_multi_5(K) SHR 40
              IE_array(4) = Modulo(Ilong,2^10)
             ' Ilong = Fix(En_multi_5(K)*2^-50)
              Ilong = En_multi_5(K) SHR 50
              IE_array(5) = Modulo(Ilong,2^10)
              Exit For
            End If
          Next            
        Case 6  
          For J = J_multi_last(6) To 2 * Imulti
            K = J Mod Ubound(En_multi_6) + 1
            If En_multi_6(K) Mod 2^10 = I_E_Multi And _
                  PLoss(I_emit_6(K)) = I_Z_Multi Then 
              J_multi_last(6) = K
              C_test = Oct(I_emit_6(K))
             ' Ilong = Fix(En_multi_6(K)*2^-10)
              Ilong = En_multi_6(K) SHR 10
              IE_array(1) = Modulo(Ilong,2^10)
             ' Ilong = Fix(En_multi_6(K)*2^-20)
              Ilong = En_multi_6(K) SHR 20
              IE_array(2) = Modulo(Ilong,2^10)
             ' Ilong = Fix(En_multi_6(K)*2^-30)
              Ilong = En_multi_6(K) SHR 30
              IE_array(3) = Modulo(Ilong,2^10)
             ' Ilong = Fix(En_multi_6(K)*2^-40)
              Ilong = En_multi_6(K) SHR 40
              IE_array(4) = Modulo(Ilong,2^10)
             ' Ilong = Fix(En_multi_6(K)*2^-50)
              Ilong = En_multi_6(K) SHR 50
              IE_array(5) = Modulo(Ilong,2^8)
             ' Ilong = Fix(En_multi_6(K)*2^-58)
              Ilong = En_multi_6(K) SHR 58
              IE_array(6) = Modulo(Ilong,2^8)
              Exit For
            End If
          Next  
        Case Else
      End Select    

   /' For J = 1 To I_A_Multi
        If Mid(C_Test,J,1) = "1" or Mid(C_Test,J,1) = "3" Then
      '    ENCNtest(IE_array(J)) = ENCNtest(IE_array(J)) + 1
      '    EN(IE_array(J)) = EN(IE_array(J)) + 1
        End If
        If Mid(C_Test,J,1) = "2" or Mid(C_test,J,1) = "4" Then
      '    EP(IE_array(J)) = EP(IE_array(J)) + 1
        End If
      Next  '/    


    /' List-mode output '/
      Dim As Single J_frag_light_q, J_frag_heavy_q
      Dim As Integer J_max
      If CFileoutlmd <> "" Then
        If B_Error_Analysis  = 0 Or Brec = 1 Then
          If PZ1 = 1 Then Print #foutlmd,Using "###";I_Z_Light_Post;" "; 
          If PZ2 = 1 Then Print #foutlmd,Using "###";I_Z_Heavy_Post;" ";
          If PA1pre = 1 Then Print #foutlmd,Using "####";I_A_light;" ";
          If PA2pre = 1 Then Print #foutlmd,Using "####";I_A_heavy;" ";
          If PA1post = 1 Then Print #foutlmd,Using "####";I_A_Light_Post;" ";
          If PA2post = 1 Then Print #foutlmd,Using "####";I_A_Heavy_Post;" ";
          If I_A_light Mod 2 = 0 Then
            J_frag_light_q = Int(J_frag_light + 0.5)
          Else 
            J_frag_light_q = Int(J_frag_light) + 0.5
          End If  
          IF PI1 = 1 Then Print #foutlmd,Using "###.#";J_frag_light_q;" ";
          If I_A_heavy Mod 2 = 0 Then
            J_frag_heavy_q = Int(J_frag_heavy + 0.5)
          Else
            J_frag_heavy_q = Int(J_frag_heavy) + 0.5
          End If   
          If PI2 = 1 Then Print #foutlmd,Using "###.#";J_frag_heavy_q;" ";
          If PI1 = 1 Then Print #foutlmd,Using "###.#";Spin_gs_light;" ";
          If PI2 = 1 Then Print #foutlmd,Using "###.#";Spin_gs_heavy;" ";
          If PXE = 1 Then 
            Print #foutlmd,Using "####.##";TXElight;" ";
            Print #foutlmd,Using "####.##";TXEheavy;" ";
          End If
          If Pn1 = 1 Then Print #foutlmd,Using "###";I_nu_light;" ";
          If Pn2 = 1 Then Print #foutlmd,Using "###";I_nu_heavy;" ";
          If PTKEpre = 1 Then Print #foutlmd,Using "####.##";TKE;" ";
          If PTKEpost = 1 Then Print #foutlmd,Using "####.##";TKE_post;" ";
          If PnCN = 1 And Inofirst > 0 Then
            Print #foutlmd,Using "###.# ";0.1*I_E_Multi;" ";
            If I_A_multi <= 6 Then   ' Restriction to 6 first energies!
              Print #foutlmd,C_test;" ";
              For J = 1 To I_A_multi   
                Print #foutlmd,Using "###.# ";0.1*IE_array(J);
              Next
            End If  
            If I_A_multi > 6 Then
              Print #foutlmd," Multiplicity of pre-scission particles > 6!";     
            End If
          End If
          If PEnpost = 1 Then
            Print #foutlmd," "
            Print #foutlmd,"  0";
            For J = 1 To In_post
              Print #foutlmd,Using "####.##"; Array_En_post(J);
              Print #foutlmd,Using "####.##"; Array_cos_alpha(J);
              Print #foutlmd,Using "#####.#"; Array_phi(J);
            Next
            Print #foutlmd," "
            Print #foutlmd,"  1";
            For J = 1 To UBound(Array_En_light)
              If Array_En_light(J) = 0 Then Exit For
              Print #foutlmd,Using "####.##"; Array_En_light(J);
            Next
            Print #foutlmd," "
            Print #foutlmd,"  2";
            For J = 1 To UBound(Array_En_heavy)
              If Array_En_heavy(J) = 0 Then Exit For
              Print #foutlmd,Using "####.##"; Array_En_heavy(J);
            Next
          End If
          If PEgpost = 1 Then
       '    If Array_Eg0_light(1) <> 0 Then
              Print #foutlmd," "
              Print #foutlmd,"  3";
              For J = 1 To UBound(Array_Eg0_light)
                If Array_Eg0_light(J) = 0 Then Exit For
                Print #foutlmd,Using "####.###"; Array_Eg0_light(J);
              Next 
       '    End If             
            Print #foutlmd," "
            Print #foutlmd,"  4";
            For J = 1 To Ig1_light
              If Array_Eg1_light(J) = 0 Then Exit For
              Print #foutlmd,Using "####.###"; Array_Eg1_light(J);
            Next            
            Print #foutlmd," "
            Print #foutlmd,"  5";
            For J = 1 To Ig2_light
              If Array_Eg2_light(J) = 0 Then Exit For
              Print #foutlmd,Using "####.###"; Array_Eg2_light(J);
            Next  
       '    If Array_Eg0_heavy(1) <> 0 Then          
              Print #foutlmd," "
              Print #foutlmd,"  6";
              For J = 1 To UBound(Array_Eg0_heavy)
                If Array_Eg0_heavy(J) = 0 Then Exit For
                Print #foutlmd,Using "####.###"; Array_Eg0_heavy(J);
              Next            
       '    End If
            Print #foutlmd," "
            Print #foutlmd,"  7"; 
            For J = 1 To Ig1_heavy
              If Array_Eg1_heavy(J) = 0 Then Exit For
              Print #foutlmd,Using "####.###"; Array_Eg1_heavy(J);
            Next            
            Print #foutlmd," "
            Print #foutlmd,"  8";
            For J = 1 To Ig2_heavy
              If Array_Eg2_heavy(J) = 0 Then Exit For
              Print #foutlmd,Using "####.###"; Array_Eg2_heavy(J);
            Next            
          End If
          Print #foutlmd," "
        End If  
      End If
    End Scope
    
    
  SkipEvent:

 
  IEVTtot = IEVTtot + 1
  Scope
   If (NEVTtot >= 1.E6 And (IEVTtot * 100) = NEVTtot)  Or _
      (NEVTtot >= 1.E5 And (IEVTtot * 10) Mod NEVTtot = 0) Or _
      (NEVTtot >= 1.E4 And (IEVTtot * 2) Mod NEVTtot = 0) Then  
     Dim As Double Rdatetime
     Rdatetime = Now
     Print IEVTtot;" Events (";Cint(100*IEVTtot/NEVTtot);"% ) of "; _
                  NEVTtot;" events processed at ";
     Print Format( Rdatetime, "dd.mm.yyyy, hh:mm:ss")
   End If
  End Scope 
  
  Next ILoop  ' For ILoop = 1 To NEVTused

  End If  ' If NEVTused  (from W_spectrum(I_E_distr)) > 0     
  ' End of loop over energy spectrum from file   
  Next I_E_multi  ' For I_E_multi
  Next I_Z_multi  ' For I_Z_multi
  Next I_N_multi  ' For I_N_multi   
  Next I_E_distr  ' For I_E_distr   

   
   /' Output  '/ 
   
   Print " "
   ' Print "Calculation finished at ";time;"."
   
   IF B_Error_Analysis = 0 Then  
     Print "Output is written to file ";
     Print CFileout_full;"."
     Print " "
   End If  
   If CFileoutlmd <> "" Then
     If B_Error_Analysis = 0 Or Brec Then   
       Print "List-mode data are written to file ";
       Print CFileoutlmd_full;"."  
       Print
'      Close #foutlmd
     End If
   End If
   

'   If Bfilein = 0 And BGUI = 0 And B_Error_Analysis = 0 Then
'     Print "Press 'ENTER' to start the code again or 'Q' to quit!" 
'     Print "(Focus must be on graphic window.)"
'   End If
   

    ' Synchronize output (allow only one process to print on file at a time)
   If Bfilein Then 
     Do
     '  await(I_thread,I_thread_max)    ' avoid collisions in writing to sync.ctl (may be not necessary) 
       Sleep 100
       fsync = freefile
       CHDIR("ctl")
         If Fileexists("sync.ctl") = 0 Then
           Open "sync.ctl" For Output As fsync
             Print #fsync,0
           Close fsync
         End If
       CHDIR("..")
       Open "ctl\sync.ctl"  For Input As fsync
         Input #fsync, Isync
       Close fsync  
     Loop Until Isync = 0
     Open "ctl\sync.ctl" For Output As fsync
       Print #fsync,1
     Close fsync
   End If 
   

   f = freefile
   If B_Error_Analysis = 1 Then
     If Brec = 1 Then 
       Open "out\"+Cfileout+".ptb" For Append As #f   ' for results with perturbed parameters
     Else 
       Open "tmp\"+Cfileout+".ptb" For Output As #f               ' overwrite previous content
       Print #f," Note: This file only keeps the last calculation."
       Print #f," Choose the appropriate output option:" 
       Print #f," 'extended output of perturbed calculations'" 
       Print #f," to provide full information!"
     End If 
     Print #f," "
     Print #f,"*******************************************"
     Print #f,"***  Results of perturbed calculations  ***"
     Print #f,"          Parameter set #"+Str(I_Error+1)
     Print #f,"*******************************************"
     Print #f," "
   Else
     Open Cfileout_full For Append As #f   ' for final result, with error bars if available 
   End If  

   /' Calculation of mean neutron yield per mass '/
   Scope
     Dim As Integer I,J
     Dim As Double Zaehler, Nenner
     For I = Lbound(N2dpre,1) To Ubound(N2dpre,1)
       Zaehler = 0
       Nenner = 0
       For J = Lbound(N2dpre,2) To Ubound(N2dpre,2)
         Zaehler = Zaehler + J * N2Dpre(I,J)
         Nenner = Nenner + N2Dpre(I,J)
       Next
       If Nenner > 0 Then
         NApre(I) = Zaehler / Nenner
       Else
         NApre(I) = 0
       End If    
     Next
     For I = Lbound(N2dpost,1) To Ubound(N2dpost,1)
       Zaehler = 0
       Nenner = 0
       For J = Lbound(N2dpost,2) To Ubound(N2dpost,2)
         Zaehler = Zaehler + J * N2Dpost(I,J)
         Nenner = Nenner + N2Dpost(I,J)
       Next
       If Nenner > 0 Then
         NApost(I) = Zaehler / Nenner
       Else
         NApost(I) = 0
       End If    
     Next
   End Scope

   /' Calculation of mean neutron energy per mass '/
   Scope
     Dim As Integer I,J
     Dim As Double Zaehler, Nenner
     For I = 0 To Ubound(ENApre2d,1)
       Zaehler = 0
       Nenner = 0
       For J = 0 To Ubound(ENApre2d,2)
         Zaehler = Zaehler + 0.1 * J * ENApre2d(I,J)
         Nenner = Nenner + ENApre2d(I,J)
       Next
       If Nenner > 0 Then
         ENApre(I) = Zaehler / Nenner
       Else
         ENApre(I) = 0
       End If    
     Next
     For I = 1 To Ubound(ENApost2d,1)
       Zaehler = 0
       Nenner = 0
       For J = 0 To Ubound(ENApost2d,2)
         Zaehler = Zaehler + 0.1 * J * ENApost2d(I,J)
         Nenner = Nenner + ENApost2d(I,J)
       Next
       If Nenner > 0 Then
         ENApost(I) = Zaehler / Nenner
       Else
         ENApost(I) = 0
       End If    
     Next   
     For I = 0 To Ubound(ENApre2dfs,1)    ' fs = fragment system
       Zaehler = 0
       Nenner = 0
       For J = 0 To Ubound(ENApre2dfs,2)
         Zaehler = Zaehler + 0.1 * J * ENApre2dfs(I,J)
         Nenner = Nenner + ENApre2dfs(I,J)
       Next
       If Nenner > 0 Then
         ENAprefs(I) = Zaehler / Nenner
       Else
         ENAprefs(I) = 0
       End If    
     Next
     For I = 1 To Ubound(ENApost2dfs,1)
       Zaehler = 0
       Nenner = 0
       For J = 0 To Ubound(ENApost2dfs,2)
         Zaehler = Zaehler + 0.1 * J * ENApost2dfs(I,J)
         Nenner = Nenner + ENApost2dfs(I,J)
       Next
       If Nenner > 0 Then
         ENApostfs(I) = Zaehler / Nenner
       Else
         ENApostfs(I) = 0
       End If    
     Next   
   End Scope 
   
   
       ' Projections of 2-dimensional arrays
   Scope
    ' EdefoA
    Dim As Double Zaehler, Nenner
    For I = Lbound(Edefo2d,1) To Ubound(Edefo2d,1)
      Zaehler = 0
      Nenner = 0
      For J = Lbound(Edefo2d,2) To Ubound(Edefo2d,2)  ' 100 kev bin
        Zaehler = Zaehler + (0.1*J+0.5) * Edefo2d(I,J)
        Nenner = Nenner + Edefo2d(I,J)
      Next J 
      If Nenner > 0 Then
        EdefoA(I) = Zaehler / Nenner
      End If 
    Next I
    ' EintrA
    For I = Lbound(Eintr2d,1) To Ubound(Eintr2d,1)
      Zaehler = 0
      Nenner = 0
      For J = Lbound(Eintr2d,2) To Ubound(Eintr2d,2)  ' 100 kev bin
        Zaehler = Zaehler + (0.1*J+0.5) * Eintr2d(I,J)
        Nenner = Nenner + Eintr2d(I,J)
      Next J 
      If Nenner > 0 Then
        EintrA(I) = Zaehler / Nenner
      End If 
    Next I
    ' EcollA
    For I = Lbound(Ecoll2d,1) To Ubound(Ecoll2d,1)
      Zaehler = 0
      Nenner = 0
      For J = Lbound(Ecoll2d,2) To Ubound(Ecoll2d,2)  ' 100 keV bin
        Zaehler = Zaehler + (J*0.1+0.5) * Ecoll2d(I,J)
        Nenner = Nenner + Ecoll2d(I,J)
      Next J 
      If Nenner > 0 Then
        EcollA(I) = Zaehler / Nenner
      End If 
    Next I
    ' EkinApre
    For I = Lbound(AEkinpre,1) To Ubound(AEkinpre,1)
      Zaehler = 0
      Nenner = 0
      For J = Lbound(AEkinpre,2) To Ubound(AEkinpre,2)
        Zaehler = Zaehler + (J+0.5) * AEkinpre(I,J)
        Nenner = Nenner + AEkinpre(I,J)
      Next J
      If Nenner > 0 Then
        EkinApre(I) = Zaehler / Nenner
      End If
    Next I 
    ' EkinApost
    For I = Lbound(AEkinpost,1) To Ubound(AEkinpost,1)
      Zaehler = 0
      Nenner = 0
      For J = Lbound(AEkinpost,2) To Ubound(AEkinpost,2)
        Zaehler = Zaehler + (J+0.5) * AEkinpost(I,J)
        Nenner = Nenner + AEkinpost(I,J)
      Next J
      If Nenner > 0 Then
        EkinApost(I) = Zaehler / Nenner
      End If
    Next I 
    ' TKEApre
    For I = Lbound(ATKEpre,1) To Ubound(ATKEpre,1)
      Zaehler = 0
      Nenner = 0
      For J = Lbound(ATKEpre,2) To Ubound(ATKEpre,2)
        Zaehler = Zaehler + (J+0.5) * ATKEpre(I,J)
        Nenner = Nenner + ATKEpre(I,J)
      Next J
      If Nenner > 0 Then
        TKEApre(I) = Zaehler / Nenner
      End If
    Next I 
    ' TKEApost
    For I = Lbound(ATKEpost,1) To Ubound(ATKEpost,1)
      Zaehler = 0
      Nenner = 0
      For J = Lbound(ATKEpost,2) To Ubound(ATKEpost,2)
        Zaehler = Zaehler + (J+0.5) * ATKEpost(I,J)
        Nenner = Nenner + ATKEpost(I,J)
      Next J
      If Nenner > 0 Then
        TKEApost(I) = Zaehler / Nenner
      End If
    Next I 
    ' QA
    For I = Lbound(AQpre,1) To Ubound(AQpre,1)
      Zaehler = 0
      Nenner = 0
      For J = Lbound(AQpre,2) To Ubound(AQpre,2)
        Zaehler = Zaehler + (J+0.5) * AQpre(I,J)
        Nenner = Nenner + AQpre(I,J)
      Next J
      If Nenner > 0 Then
        QA(I) = Zaehler / Nenner
      End If
    Next I 
    
    ' Filling spectrum EN (Sum of EnCN and Enfr)
  /'If NEVTtot = Imulti Then 
      For I = 1 To Ubound(En)
        En(I) = EnCN(I) + Enfr(I)
      Next I
    End If'/  
    
    
   End Scope


   /' Analysis of uncertainties and covariances '/
   Static As Integer fmvd_single, fmvd  'files
   Dim As Integer NAptb,NZptb,NZAptb
      
   Redim SZCovar(1,1) As OCovar
   Redim SZCorr(1,1) As OCorr
   Redim SApreCovar(1,1) As OCovar
   Redim SApreCorr(1,1) As OCorr
   Redim SApostCovar(1,1) As OCovar
   Redim SApostCorr(1,1) As OCorr
   Redim SZApreCovar(1,1,1,1) As OCovar
   Redim SZApreCorr(1,1,1,1) As OCorr
   Redim SZApostCovar(1,1,1,1) As OCovar
   Redim SZApostCorr(1,1,1,1) As Ocorr
   Dim As Integer Zmin,Zmax
   Dim As Integer Apremin,Apremax
   Dim As Integer Apostmin,Apostmax
   Dim As Integer dApremax
   Dim As Integer dApostmax
   Redim VAprelim(1,2) As Integer   ' Amin, Amax
   Redim VApostlim(1,2) As Integer   ' Amin, Amax
   Dim As Integer L

   Redim SZ_Double_Covar(1,1) As OCovar
   Redim SZ_Double_Corr(1,1) As OCorr
   Redim SApre_Double_Covar(1,1) As OCovar
   Redim SApre_Double_Corr(1,1) As OCorr
   Redim SApost_Double_Covar(1,1) As OCovar
   Redim SApost_Double_Corr(1,1) As OCorr
   Redim SZApre_Double_Covar(1,1,1,1) As OCovar
   Redim SZApre_Double_Corr(1,1,1,1) As OCorr
   Redim SZApost_Double_Covar(1,1,1,1) As OCovar
   Redim SZApost_Double_Corr(1,1,1,1) As OCorr
   Dim As Integer Z_Double_min,Z_Double_max
   Dim As Integer Apre_Double_min,Apre_Double_max
   Dim As Integer Apost_Double_min,Apost_Double_max
   Dim As Integer dApre_Double_max
   Dim As Integer dApost_Double_max
   Redim VApre_Double_lim(1,2) As Integer   ' Amin, Amax
   Redim VApost_Double_lim(1,2) As Integer   ' Amin, Amax
   Redim rms_Double(1,2) As Double
   Redim rms_ZA_Double(1,1,2) As Double


   If B_Error_On = 1 Then
     If B_Error_Analysis = 1 Then

       ' Mass yields
       For I = 20 To P_A_CN - 20
         If APost(I) > 0 Then
           d_APost(1,I) = d_APost(1,I) + APost(I)
           d_APost(2,I) = d_APost(2,I) + (APost(I))^2
         End If
       Next     
       
       ' Z yields
       For I = 10 To P_Z_CN - 10
         If ZPOST(I) > 0 Then 
           d_ZPost(1,I) = d_ZPost(1,I) + ZPost(I)
           d_ZPost(2,I) = d_ZPost(2,I) + (ZPost(I))^2
         End If  
       Next       
     
       ' Nuclide yields
       For I = 20 To P_A_CN - 20
         For J = 10 To P_Z_CN - 10
           If ZISOPOST(I,J) > 0 Then
             d_ZISOPOST(1,I,J) = d_ZISOPOST(1,I,J) + ZISOPOST(I,J)
       	     d_ZISOPOST(2,I,J) = d_ZISOPOST(2,I,J) + (ZISOPOST(I,J))^2
           End If
         Next
       Next
       
       
       ' Uncertainties of specific fission quantities
       Scope
       
         ' prompt-neutron multiplicities
         Dim As Double Zaehler,Nenner
         Dim As Double NCN_mean
         Zaehler = 0
         Nenner = 0  
         For I = 0 To Ubound(NNCN,1)
           Zaehler = Zaehler + I * NNCN(I)
           Nenner = Nenner + NNCN(I)
         Next I       
         NCN_mean = Zaehler / Nenner
         d_NCN(1) = d_NCN(1) + NCN_mean
         d_NCN(2) = d_NCN(2) + NCN_mean^2      
       
         Dim As Double Nsci_mean
         Zaehler = 0
         Nenner = 0  
         For I = 0 To Ubound(NNsci,1)
           Zaehler = Zaehler + I * NNsci(I)
           Nenner = Nenner + NNsci(I) 
         Next I
         Nsci_mean = Zaehler / Nenner
         d_Nsci(1) = d_Nsci(1) + Nsci_mean
         d_Nsci(2) = d_Nsci(2) + Nsci_mean^2
       
         Dim As Double Nfr_mean
         Zaehler = 0
         Nenner = 0
         For I = 0 To Ubound(NNfr,1)
           Zaehler = Zaehler + I * NNfr(I)
           Nenner = Nenner + NNfr(I)
         Next I
         Nfr_mean = Zaehler / Nenner
         d_Nfr(1) = d_Nfr(1) + Nfr_mean
         d_Nfr(2) = d_Nfr(2) + Nfr_mean^2

         Dim As Double Nlight_mean
         Zaehler = 0
         Nenner = 0
         For I = 0 To Ubound(NNlight,1)
           Zaehler = Zaehler + I * NNlight(I)
           Nenner = Nenner + NNlight(I)
         Next I
         Nlight_mean = Zaehler / Nenner
         d_Nlight(1) = d_Nlight(1) + Nlight_mean
         d_Nlight(2) = d_Nlight(2) + Nlight_mean^2

         Dim As Double Nheavy_mean 
         Zaehler = 0
         Nenner = 0
         For I = 0 To Ubound(NNheavy,1)
           Zaehler = Zaehler + I * NNheavy(I)
           Nenner = Nenner + NNheavy(I)
         Next I
         Nheavy_mean = Zaehler / Nenner
         d_Nheavy(1) = d_Nheavy(1) + Nheavy_mean
         d_Nheavy(2) = d_Nheavy(2) + Nheavy_mean^2
         
         Dim As Double Ntot_mean
         Ntot_mean = NCN_mean + Nsci_mean + Nfr_mean
         d_Ntot(1) = d_Ntot(1) + Ntot_mean
         d_Ntot(2) = d_Ntot(2) + Ntot_mean^2

       ' Mean neutron energy (from fragments)
         Dim As Double ENfr_mean
         Zaehler = 0
         Nenner = 0
         For I = 0 To Ubound(ENfr)
           Zaehler = Zaehler + Csng(I)/1000.0 * ENfr(I)   ' kev -> MeV
           Nenner = Nenner + ENfr(I)
         Next I
         ENfr_mean = Zaehler/Nenner
         d_ENfr(1) = d_ENfr(1) + ENfr_mean
         d_ENFr(2) = d_ENfr(2) + ENfr_mean^2
         
       ' Gamma multiplitiy
         Dim As Double Ng_mean
         Zaehler = 0
         Nenner = 0
         For I = 0 To Ubound(Ngammatot)
           Zaehler = Zaehler + I * Ngammatot(I)
           Nenner = Nenner + Ngammatot(I)
         Next I
         Ng_mean = Zaehler/Nenner
         d_Ng(1) = d_Ng(1) + Ng_mean
         d_Ng(2) = d_NG(2) + Ng_mean^2
         
       ' Mean gamma energy
         Dim As Double Eg_mean
         Zaehler = 0
         Nenner = 0
         For I = 1 To Ubound(Egamma)
           Zaehler = Zaehler + Csng(I)/1000.0 * Egamma(I)
           Nenner = Nenner + Egamma(I)
         Next I
         Eg_mean = Zaehler / Nenner
         d_Eg(1) = d_Eg(1) + Eg_mean
         d_Eg(2) = d_Eg(2) + Eg_mean^2
         
       ' Total gamma energy (sum of all gammas in one fission event)
         Dim As Double Egtot_mean
         Zaehler = 0
         Nenner = 0
         For I = 1 To Ubound(Egammatot)
           Zaehler = Zaehler + Csng(I)/1000.0 * Egammatot(I)
           Nenner = Nenner + Egammatot(I)
         Next I
         Egtot_mean = Zaehler / Nenner
         d_Egtot(1) = d_Egtot(1) + Egtot_mean
         d_Egtot(2) = d_Egtot(2) + Egtot_mean^2
         
       ' Mean total kinetic energy
         Dim As Double TKEpre_mean
         Zaehler = 0
         Nenner = 0
         For I = 1 To Ubound(TKEpre)
           Zaehler = Zaehler + Csng(I) * TKEpre(I)
           Nenner = Nenner + TKEpre(I)
         Next I
         TKEpre_mean = Zaehler / Nenner
         d_TKEpre(1) = d_TKEpre(1) + TKEpre_mean
         d_TKEpre(2) = d_TKEpre(2) + TKEpre_mean^2

         Dim As Double TKEpost_mean
         Zaehler = 0
         Nenner = 0
         For I = 1 To Ubound(TKEpost)
           Zaehler = Zaehler + Csng(I) * TKEpost(I)
           Nenner = Nenner + TKEpost(I)
         Next I
         TKEpost_mean = Zaehler / Nenner
         d_TKEpost(1) = d_TKEpost(1) + TKEpost_mean
         d_TKEpost(2) = d_TKEpost(2) + TKEpost_mean^2
         
       ' Mean total kinetic energy
         Dim As Double TotXE_mean
         Zaehler = 0
         Nenner = 0
         For I = 1 To Ubound(TotXE)
           Zaehler = Zaehler + Csng(I) * TotXE(I)
           Nenner = Nenner + TotXE(I)
         Next I
         TotXE_mean = Zaehler / Nenner
         d_TotXE(1) = d_TotXE(1) + TotXE_mean
         d_TotXE(2) = d_TotXE(2) + TotXE_mean^2
  
         
       End Scope
       
       
       
       Cfilemvd_single = "tmp\"+Cfileout_single+".mvd"
       fmvd_single = freefile
       If I_Error = 0 Then
         Open Cfilemvd_Single For Output As #fmvd_single
         Print #fmvd_single,"* This file provides the multi-variant distributions of GEF" 
         Print #fmvd_single,"* yields, which form the raw data for determining covariance data"
         Print #fmvd_single,"* for fission yields by establishing correlations of the results" 
         Print #fmvd_single,"* of several perturbed calculations."
       Else 
         Open Cfilemvd_single For Append As #fmvd_single
       End If  
       Print #fmvd_single,"*"
       Dim As Double Rdatetime
       Rdatetime = Now
       Print #fmvd_single,"* Output written on ";
       Print #fmvd_single, Format( Rdatetime, "dd.mm.yyyy, hh:mm:ss")
       Print #fmvd_single,"* "
       Print #fmvd_single, Using "& ### & ###";"* Calculation for the nucleus Z = ";P_Z_CN;", A = ";P_A_CN

       Select Case Emode
         Case 0
           Print #fmvd_single,"* at E* = ";P_E_exc;" MeV above the outer saddle."
           Print #fmvd_single,"* Spin = ";Spin_CN
         Case 1
           Print #fmvd_single,"* at E* = ";P_E_exc;" MeV above the ground state."
           Print #fmvd_single,"* Spin = ";Spin_CN
         Case -1
           Print #fmvd_single,"* at E* = ";P_E_exc;" MeV above the ground state."
           Print #fmvd_single,"* Spin = ";Spin_CN
           Print #fmvd_single,"* Only first-chance fission!"
         Case 2
           Print #fmvd_single,"* formed by (n,f) with En = ";P_E_exc- E_EXC_ISO;" MeV."
           Print #fmvd_single,"* Spin of target nucleus = ";Spin_CN
         Case 12
           Print #fmvd_single,"* formed by (p,f) with Ep = ";P_E_exc- E_EXC_ISO;" MeV."
           Print #fmvd_single,"* Spin of target nucleus = ";Spin_CN           
         Case 3
           Print #fmvd_single,"* with user-defined excitation-energy distribution."
           Print #fmvd_single,"* Only first-chance fission."
           Print #fmvd_single,"* Spin = ";Spin_CN
       End Select
       If I_E_iso > 0 Then
         Print #fmvd_single, "* Isomer # ";I_E_iso;", at E* = ";E_EXC_ISO;" MeV" 
       End If          
       Print #fmvd_single,"*"
       Print #fmvd_single,"* Perturbed parameter set #"+Str(I_Error+1)
       Print #fmvd_single,"*"
       Print #fmvd_single,"* Set     Z         Y(Z)"
       Print #fmvd_single,"*Z*"
       For I = 10 To P_Z_CN - 10
         If ZPost(I) > 0 Then
     	   Print #fmvd_single,Using "####    ####    ###.#####";I_Error+1;I;ZPOST(I)
   	     End If
       Next      
       Print #fmvd_single,"*" 
       Print #fmvd_single,"* Set","A"," Y(A) ","    Y(A) "
       Print #fmvd_single,"*"," "," pre-neutron","    post-neutron"
       Print #fmvd_single,"*A*"
       For I = 20 To P_A_CN - 20
         If APost(I) > 0 Or APre(I) > 0 Then 
     	   Print #fmvd_single,Using "####        ####         ###.#####        ###.#####";_
   	              I_Error+1;I;APre(I);APost(I)
   	     End If         
   	   Next 
   	   Print #fmvd_single,"*"
       Print #fmvd_single,"* Set          A             Z            Y(A,Z)pre        Y(A,Z)post"  
       Print #fmvd_single,"*AZ*"
       For I = 20 To P_A_CN - 20
         For J = 10 To P_Z_CN - 10
           If ZISOPRE(I,J) > 0 Or ZISOPOST(I,J) > 0 Then
       	     Print #fmvd_single,I_Error+1,I,J,Csng(ZISOPRE(I,J));Tab(60);Csng(ZISOPOST(I,J))
       	   End If   
         Next
       Next
       If I_Error + 1 = N_Error_Max Then
     '    Print #fmvd_single,"***********************END****************************"
       End If  
       Close #fmvd_single
       
       
       For I = 10 To P_A_CN - P_Z_CN - 10
         For J = 10 To P_Z_CN -10
           If NZPost(I,J) > 0 Then
             d_NZPost(1,I,J) = d_NZPost(1,I,J) + NZPost(I,J) ' simple sum
             d_NZPost(2,I,J) = d_NZPost(2,I,J) + (NZPost(I,J))^2 ' quadratic sum
           End If
         Next
       Next
       
     Else  ' Last passage, calculate final uncertainties and covariances
     
       ' Open file for values needed for covariances between different fissioning systems     
       If B_Double_Covar = 1 Then
         Cfilemvd = "tmp\"+Cfileout+".mvd"
         fmvd = freefile
         If I_Double_Covar = 1 Then
           Open Cfilemvd For Output As #fmvd
         Else
           Open Cfilemvd For Append As #fmvd
         End If
       End If

     
       ' --- Covariances and uncertainties for single systems ---

       ' Covariances
       
       Dim As String Coption
       Dim As String Cread
       
       /' First step: Read and count event data from file '/
       fmvd_single = freefile       
       Open Cfilemvd_single For Input As #fmvd_single
       Coption = "None"
       NZptb = 0
       NAptb = 0
       NZAptb = 0
       
       Do Until EOF(fmvd_single)
         Line Input #fmvd_single, Cread
         If Left(Cread,1) = "*" Then Coption = "None"
         If Cread = "*Z*" Then 
           Coption = "Z"
           Line Input #fmvd_single, Cread
         End If  
         If Cread = "*A*" Then 
           Coption = "A"
           Line Input #fmvd_single, Cread
         End If  
         If Cread = "*AZ*" Then 
           Coption = "AZ"
           Line Input #fmvd_single, Cread
         End If  
         Select Case Coption
           Case "Z"
             NZptb = NZptb + 1
           Case "A"
             NAptb = NAptb + 1
           Case "AZ" 
             NZAptb = NZAptb + 1
           Case Else
         End Select
       Loop
       Close #fmvd_single    
       

       Redim VApreptb(NAptb) As OCovarEvt1d
       Redim VApostptb(NAptb) As OCovarEvt1d
       Redim VZptb(NZptb) As OCovarEvt1d
       Redim VZApreptb(NZAptb) As OCovarEvt2d
       Redim VZApostptb(NZAptb) As OCovarEvt2d
       
       Dim As Integer IcntA,IcntZ,IcntZA
       Dim CVsplit(5) As String
       Dim Nsplit As Integer
       IcntZ=0
       IcntA=0
       IcntZA=0


       /' Calculation of covariance matrices '/
       
       /' Second step: Read event data from file '/
       fmvd_single = freefile       
       Open Cfilemvd_single For Input As #fmvd_single
       Coption = "None"
       Do Until EOF(fmvd_single)
         Line Input #fmvd_single, Cread
         If Left(Cread,1) = "*" Then Coption = "None"
         If Cread = "*Z*" Then 
           Coption = "Z"
           Line Input #fmvd_single, Cread
         End If  
         If Cread = "*A*" Then 
           Coption = "A"
           Line Input #fmvd_single, Cread
         End If  
         If Cread = "*AZ*" Then 
           Coption = "AZ"
           Line Input #fmvd_single, Cread
         End If  
         Select Case Coption
           Case "Z"
             CC_cut Cread," ",CVsplit(),Nsplit
             IcntZ = IcntZ + 1
             VZptb(IcntZ).IParameter = Cast(Integer,CVsplit(1))
             VZptb(IcntZ).ICoordinate = Cast(Integer,CVsplit(2))
             VZptb(IcntZ).Ryield = Cast(Single,CVsplit(3))
           Case "A"
             CC_cut Cread," ",CVsplit(),Nsplit
             IcntA = IcntA + 1
             VApreptb(IcntA).IParameter = Cast(Integer,CVsplit(1))
             VApreptb(IcntA).ICoordinate = Cast(Integer,CVsplit(2))
             VApreptb(IcntA).RYield = Cast(Single,CVsplit(3))
             VApostptb(IcntA).IParameter = Cast(Integer,CVsplit(1))
             VApostptb(IcntA).ICoordinate = Cast(Integer,CVsplit(2))
             VApostptb(IcntA).Ryield = Cast(Single,CVsplit(4))
           Case "AZ" 
             CC_cut Cread," ",CVsplit(),Nsplit
             IcntZA = IcntZA + 1  
             VZApreptb(IcntZA).IParameter = Cast(Integer,CVsplit(1))
             VZApreptb(IcntZA).ICoordinate1 = Cast(Integer,CVsplit(2))
             VZApreptb(IcntZA).ICoordinate2 = Cast(Integer,CVsplit(3)) 
             VZApreptb(IcntZA).Ryield = Cast(Single,CVsplit(4))
             VZApostptb(IcntZA).IParameter = Cast(Integer,CVsplit(1))
             VZApostptb(IcntZA).ICoordinate1 = Cast(Integer,CVsplit(2))
             VZApostptb(IcntZA).ICoordinate2 = Cast(Integer,CVsplit(3))
             VZApostptb(IcntZA).Ryield = Cast(Single,CVsplit(5))
           Case Else
         End Select
       Loop
       Close #fmvd_single
         
       /' Step 3, calculate covariances '/  
       Dim As Integer IParset, IMatrix

       Dim Bfound As Byte
       Dim As Integer Nsum
       Dim As Double Rsum,Rsquare
  
  
       
       ' Covariances for Z distribution
       Zmax = 0
       Zmin = 100   
       For I = 1 To IcntZ
         Zmax = Max(Zmax,VZptb(I).ICoordinate)
         Zmin = Min(Zmin,VZptb(I).ICoordinate)
       Next I   
       
       Redim VZmean(Zmin to Zmax) As Single
       Redim VZrms(Zmin to Zmax) As Single
       Redim SZCovar(Zmin to Zmax,Zmin to Zmax) As OCovar
       Redim SZCorr(Zmin to Zmax,Zmin to Zmax) As OCorr
       
       ' Determine mean values and variances
       For I = Zmin To Zmax
         Rsum = 0
         Rsquare = 0
         Nsum = 0
         For J = 1 To IcntZ
           If VZptb(J).ICoordinate = I Then   
             RSum = RSum + VZptb(J).Ryield
             Rsquare = Rsquare + (VZptb(J).Ryield)^2
             Nsum = Nsum + 1
           End If
         Next  
         If Nsum > 0 Then
           VZmean(I) = RSum / Nsum
           VZrms(I) = sqr((Rsquare - Rsum^2/Nsum)/(Nsum-1))
         Else
           VZmean(I) = 0
           VZrms(I) = 0
         End If   
       Next
       
       'Determine covariance matrix
       For I = 1 To IcntZ
         For J = 1 To IcntZ
           If VZptb(I).IParameter = Vzptb(J).IParameter Then 
             SZCovar(VZptb(I).ICoordinate,VZptb(J).ICoordinate).Rval = _
                  SZCovar(VZptb(I).ICoordinate,VZptb(J).ICoordinate).Rval + _
             (VZptb(I).Ryield - VZmean(VZptb(I).ICoordinate)) * _
             (VZptb(J).Ryield - VZmean(VZptb(J).ICoordinate))
             SZCovar(VZptb(I).ICoordinate,VZptb(J).ICoordinate).Nval = _
             SZCovar(VZptb(I).ICoordinate,VZptb(J).ICoordinate).Nval + 1             
           End If  
         Next      
       Next
       For I = Zmin To Zmax
         For J = Zmin To Zmax
'          If SZCovar(I,J).Nval > 1 Then   ' Leads to unrealistic correlation coefficients
           If SZCovar(I,J).Nval = N_Error_Max Then
             SZCovar(I,J).Rval = SZCovar(I,J).Rval / (SZCovar(I,J).Nval-1)
             If VZrms(I) <> 0 And VZrms(J) <> 0 Then
               SZCorr(I,J).Rval = SZCovar(I,J).Rval / (VZrms(I) * VZrms(J))
             End If
           Else
             SZCovar(I,J).Rval = 0
           End If  
         Next
       Next
       
       If B_Double_Covar = 1 Then  ' Store values for covariances between different systems
         Print #fmvd, "* System Nr.   Parameterset  Observable     Number (1. 2. dim)        Deviation"
         For I = 1 To IcntZ
           Print #fmvd, I_Double_Covar, VZptb(I).IParameter, "Z", VZptb(I).ICoordinate, 0, _
                        VZptb(I).Ryield - VZmean(VZptb(I).ICoordinate)
         Next I
       End If



       ' Covariances for A_pre Distribution
       Apremax = 0
       Apremin = 300   
       For I = 1 To IcntA
         Apremax = Max(Apremax,VApreptb(I).ICoordinate)
         Apremin = Min(Apremin,VApreptb(I).ICoordinate)
       Next I   
       
       Redim VApremean(Apremin to Apremax) As Single
       Redim VAprerms(Apremin to Apremax) As Single
       Redim SApreCovar(Apremin to Apremax,Apremin to Apremax) As OCovar
       Redim SApreCorr(Apremin to Apremax,Apremin to Apremax) As OCorr
       
       ' Determine mean values and variances
       For I = Apremin To Apremax
         Rsum = 0
         Rsquare = 0
         Nsum = 0
         For J = 1 To IcntA
           If VApreptb(J).ICoordinate = I Then   
             RSum = RSum + VApreptb(J).Ryield
             Rsquare = Rsquare + (VApreptb(J).Ryield)^2
             Nsum = Nsum + 1
           End If
         Next  
         If Nsum > 0 Then
           VApremean(I) = RSum / Nsum
           VAprerms(I) = sqr((Rsquare - Rsum^2/Nsum)/(Nsum-1))
         Else
           VApremean(I) = 0
           VAprerms(I) = 0
         End If            
       Next

       'Determine covariance matrix
       For I = 1 To IcntA
         For J = 1 To IcntA
           If VApreptb(I).IParameter = VApreptb(J).IParameter Then 
             SApreCovar(VApreptb(I).ICoordinate,VApreptb(J).ICoordinate).Rval = _
                  SApreCovar(VApreptb(I).ICoordinate,VApreptb(J).ICoordinate).Rval + _
             (VApreptb(I).Ryield - VApremean(VApreptb(I).ICoordinate)) * _
             (VApreptb(J).Ryield - VApremean(VApreptb(J).ICoordinate))
             SApreCovar(VApreptb(I).ICoordinate,VApreptb(J).ICoordinate).Nval = _
             SApreCovar(VApreptb(I).ICoordinate,VApreptb(J).ICoordinate).Nval + 1             
           End If  
         Next      
       Next
       For I = Apremin To Apremax
         For J = Apremin To Apremax
'          If SApreCovar(I,J).Nval > 1 Then
           If SApreCovar(I,J).Nval = N_Error_Max Then
             SApreCovar(I,J).Rval = SApreCovar(I,J).Rval / (SApreCovar(I,J).Nval-1)
             If VAprerms(I) <> 0 And VAprerms(J) <> 0 Then
               SApreCorr(I,J).Rval = SApreCovar(I,J).Rval / (VAprerms(I) * Vaprerms(J))
             End If
           Else
             SApreCovar(I,J).Rval = 0  
           End If  
         Next
       Next

       If B_Double_Covar = 1 Then  ' Store values for covariances between different systems
         Print #fmvd, "* System Nr.   Parameterset  Observable     Number (1. 2. dim)        Deviation"
         For I = 1 To IcntA
           Print #fmvd, I_Double_Covar, VApreptb(I).IParameter, "Apre", VApreptb(I).ICoordinate, 0, _
                        VApreptb(I).Ryield - VApremean(VApreptb(I).ICoordinate)
         Next I
       End If

       
       ' Covariances for A_post distribution       
       Apostmax = 0
       Apostmin = 300   
       For I = 1 To IcntA
         Apostmax = Max(Apostmax,VApostptb(I).ICoordinate)
         Apostmin = Min(Apostmin,VApostptb(I).ICoordinate)
       Next I   
       
       Redim VApostmean(Apostmin to Apostmax) As Single
       Redim VApostrms(Apostmin to Apostmax) As Single
       Redim SApostCovar(Apostmin to Apostmax,Apostmin to Apostmax) As OCovar
       Redim SApostCorr(Apostmin to Apostmax,Apostmin to Apostmax) As OCorr
       
       ' Determine mean values and variances
       For I = Apostmin To Apostmax
         Rsum = 0
         Rsquare = 0
         Nsum = 0
         For J = 1 To IcntA
           If VApostptb(J).ICoordinate = I Then   
             RSum = RSum + VApostptb(J).Ryield
             Rsquare = Rsquare + (VApostptb(J).Ryield)^2
             Nsum = Nsum + 1
           End If
         Next  
         If Nsum > 0 Then
           VApostmean(I) = RSum / Nsum
           VApostrms(I) = sqr((Rsquare - Rsum^2/Nsum)/(Nsum-1))         
         Else
           VApostmean(I) = 0
           VApostrms(I) = 0
         End If  
       Next

       'Determine covariance matrix
       For I = 1 To IcntA
         For J = 1 To IcntA
           If VApostptb(I).IParameter = VApostptb(J).IParameter Then 
             SApostCovar(VApostptb(I).ICoordinate,VApostptb(J).ICoordinate).Rval = _
                  SApostCovar(VApostptb(I).ICoordinate,VApostptb(J).ICoordinate).Rval + _
             (VApostptb(I).Ryield - VApostmean(VApostptb(I).ICoordinate)) * _
             (VApostptb(J).Ryield - VApostmean(VApostptb(J).ICoordinate))
             SApostCovar(VApostptb(I).ICoordinate,VApostptb(J).ICoordinate).Nval = _
             SApostCovar(VApostptb(I).ICoordinate,VApostptb(J).ICoordinate).Nval + 1             
           End If  
         Next      
       Next
       For I = Apostmin To Apostmax
         For J = Apostmin To Apostmax
'          If SApostCovar(I,J).Nval > 1 Then
           If SApostCovar(I,J).Nval = N_Error_Max Then
             SApostCovar(I,J).Rval = SApostCovar(I,J).Rval / (SApostCovar(I,J).Nval-1)
             If VApostrms(I) <> 0 And VApostrms(J) <> 0 Then
               SApostCorr(I,J).Rval = SApostCovar(I,J).Rval / (VApostrms(I) * Vapostrms(J))
             End If
           Else
             SApostCovar(I,J).Rval = 0
           End If  
         Next
       Next
       
       If B_Double_Covar = 1 Then  ' Store values for covariances between different systems
         Print #fmvd, "* System Nr.   Parameterset  Observable     Number (1. 2. dim)        Deviation"
         For I = 1 To IcntA
           Print #fmvd, I_Double_Covar, VApostptb(I).IParameter, "Apost", VApostptb(I).ICoordinate, 0, _
                        VApostptb(I).Ryield - VApostmean(VApostptb(I).ICoordinate)
         Next I
       End If

        


       ' Covariance for pre-neutron nuclide distribution 
       ' Determine dimension of covariance matrix
       Redim VAprelim(Zmin to Zmax ,2) As Integer
       Dim As Integer A1full,A1red,A2full,A2red,Z1,Z2
       For I = Zmin to Zmax
         VAprelim(I,1) = 1000
         VAprelim(I,2) = 0
       Next
       dApremax = 0
       For I = 1 To IcntZA
         If I <= Ubound(VZApreptb) Then
           Z1 = VZApreptb(I).Icoordinate2
           VAprelim(Z1,1) = Min(VAprelim(Z1,1),VZApreptb(I).Icoordinate1)
           VAprelim(Z1,2) = Max(VAprelim(Z1,2),VZApreptb(I).Icoordinate1)
         Else
           Print "Internal error 1: I,IcntZA",I,IcntZA
           sleep
         End If 
       Next
       For I = Zmin to Zmax
         dApremax = Max(dApremax, VAprelim(I,2) - VAprelim(I,1))
       Next
       dApremax = dApremax + 1
       Redim VZApremean(Zmin to Zmax,1 to dApremax) As Single
       Redim VZAprerms(Zmin to Zmax,1 to dApremax) As Single       
       Redim SZApreCovar(Zmin to Zmax,1 to dApremax, _
                          Zmin to Zmax,1 to dApremax) As OCovar 
       Redim SZApreCorr(Zmin to Zmax,1 to dApremax, _
                          Zmin to Zmax,1 to dApremax) As OCorr 
                          
       ' Determine mean values and variances
       For I = Zmin To Zmax
         For J = VAprelim(I,1) To VAprelim(I,2) 
           Rsum = 0
           Rsquare = 0
           Nsum = 0
           For K = 1 To IcntZA
             If K <= Ubound(VZApreptb) Then
               If VZApreptb(K).ICoordinate2 = I And _
                  VZApreptb(K).ICoordinate1 = J Then
                 RSum = RSum + VZApreptb(K).Ryield
                 Rsquare = Rsquare + (VZApreptb(K).Ryield)^2
                 Nsum = Nsum + 1
               End If
             Else
               Print "Internal error 2: I,J,K,IcntZA=",I,J,K,IcntZA
               sleep
             End If
           Next  
           A1red = J-VAprelim(I,1)+1
           If Nsum > 0 Then
             VZApremean(I,A1red) = RSum / Nsum
             VZAprerms(I,A1red) = sqr((Rsquare - Rsum^2/Nsum)/(Nsum-1))
           Else
             VZApremean(I,A1red) = 0
             VZAprerms(I,A1red) = 0
           End If       
         Next 
       Next
       
       'Determine covariance matrix
       For I = 1 To IcntZA
         If I <= Ubound(VZApreptb) Then
           Z1 = VZApreptb(I).ICoordinate2
           A1full = VZApreptb(I).ICoordinate1
           A1red = A1full - VAprelim(Z1,1) + 1
           For J = 1 To IcntZA
             If J <= Ubound(VZApreptb) Then
               Z2 = VZApreptb(J).ICoordinate2
               A2full = VZApreptb(J).ICoordinate1
               A2red = A2full - VAprelim(Z2,1) + 1
               If VZApreptb(I).IParameter = VZApreptb(J).IParameter Then 
                 SZApreCovar(Z1,A1red,Z2,A2red).Rval = SZApreCovar(Z1,A1red,Z2,A2red).Rval + _
                   (VZApreptb(I).Ryield - VZApremean(Z1,A1red)) * _
                   (VZApreptb(J).Ryield - VZApremean(Z2,A2red)) 
                 SZApreCovar(Z1,A1red,Z2,A2red).Nval = SZApreCovar(Z1,A1red,Z2,A2red).Nval + 1                       
               End If       
             Else
               Print "Internal error 3: I,J,IcntZA=",I,J,IcntZA
               sleep
             End If    
           Next
         Else
           Print "Internal error 4: I,IcountZA=",I,IcntZA
           sleep
         End If
       Next      
       
       For I = Zmin To Zmax
         For J = VAprelim(I,1) To VAprelim(I,2) 
           A1red = J - VAprelim(I,1) + 1
           For K = Zmin To Zmax
             For L = VAprelim(K,1) To VAprelim(K,2)
               A2red = L - VAprelim(K,1) + 1 
'              If SZApreCovar(I,A1red,K,A2red).Nval > 1 Then
               If SZApreCovar(I,A1red,K,A2red).Nval = N_Error_Max Then
                 SZApreCovar(I,A1red,K,A2red).Rval = SZApreCovar(I,A1red,K,A2red).Rval / _
                                          (SZApreCovar(I,A1red,K,A2red).Nval-1)
                 If VZAprerms(I,A1red) <> 0 And VZAprerms(K,A2red) <> 0 Then
                   SZApreCorr(I,A1red,K,A2red).Rval = SZApreCovar(I,A1red,K,A2red).Rval / _
                          (VZAprerms(I,A1red) * VZAprerms(K,A2red))                   
                 End If                         
               Else
                 SZApreCovar(I,A1red,K,A2red).Rval = 0
               End If                           
             Next
           Next
         Next
       Next       
       
       If B_Double_Covar = 1 Then  ' Store values for covariances between different systems
         Print #fmvd, "* System Nr.   Parameterset  Observable     Number (1. 2. dim)        Deviation"
         For I = 1 To IcntZA
           Print #fmvd, I_Double_Covar, VZApreptb(I).IParameter, "ZApre", _ 
                        VZApreptb(I).ICoordinate1, _
                        VZApreptb(I).ICoordinate2, _
                        VZApreptb(I).Ryield  - _
                            VZApremean(VZApreptb(I).ICoordinate2,VZApreptb(I).ICoordinate1 + 1 - _ 
                            VAprelim(VZApreptb(I).ICoordinate2,1)) 
         Next I
       End If



       ' Covariance for post-neutron nuclide distribution 
       ' Determine dimension of covariance matrix
       Redim VApostlim(Zmin to Zmax ,2) As Integer
       For I = Zmin to Zmax
         VApostlim(I,1) = 1000
         VApostlim(I,2) = 0
       Next
       dApostmax = 0
       For I = 1 To IcntZA
         If I <= Ubound(VZApostptb) Then
           Z1 = VZApostptb(I).Icoordinate2
           VApostlim(Z1,1) = Min(VApostlim(Z1,1),VZApostptb(I).Icoordinate1)
           VApostlim(Z1,2) = Max(VApostlim(Z1,2),VZApostptb(I).Icoordinate1)
         Else
           Print "Internal error 5: I,IcntZA",I,IcntZA
           sleep
         End If  
       Next
       For I = Zmin to Zmax
         dApostmax = Max(dApostmax, VApostlim(I,2) - VApostlim(I,1))
       Next
       dApostmax = dApostmax + 1
       Redim VZApostmean(Zmin to Zmax,1 to dApostmax) As Single
       Redim VZApostrms(Zmin to Zmax,1 to dApostmax) As Single       
       Redim SZApostCovar(Zmin to Zmax,1 to dApostmax, _
                          Zmin to Zmax,1 to dApostmax) As OCovar 
       Redim SZApostCorr(Zmin to Zmax,1 to dApostmax, _
                          Zmin to Zmax,1 to dApostmax) As OCorr 

       ' Determine mean values and variances
       For I = Zmin To Zmax
         For J = VApostlim(I,1) To VApostlim(I,2) 
           Rsum = 0
           Rsquare = 0
           Nsum = 0
           For K = 1 To IcntZA
             If K <= Ubound(VZApostptb) Then
               If VZApostptb(K).ICoordinate2 = I And _
                  VZApostptb(K).ICoordinate1 = J Then
                 RSum = RSum + VZApostptb(K).Ryield
                 Rsquare = Rsquare + (VZApostptb(K).Ryield)^2
                 Nsum = Nsum + 1
               End If
             Else  
               Print "Internal error 6: I,J,K,IcntZA=",I,J,K,IcntZA  
               sleep
             End If
           Next  
           A1red = J-VApostlim(I,1)+1     
           If Nsum > 0 Then     
             VZApostmean(I,A1red) = RSum / Nsum
             VZApostrms(I,A1red) = sqr((Rsquare - Rsum^2/Nsum)/(Nsum-1)) 
           Else
             VZApostmean(I,A1red) = 0
             VZApostrms(I,A1red) = 0 
           End If           
         Next 
       Next
     
       
       'Determine covariance matrix
       For I = 1 To IcntZA
         If I <= Ubound(VZApostptb) Then
           Z1 = VZApostptb(I).ICoordinate2
           A1full = VZApostptb(I).ICoordinate1
           A1red = A1full - VApostlim(Z1,1) + 1
           For J = 1 To IcntZA
             If J <= Ubound(VZApostptb) Then
               Z2 = VZApostptb(J).ICoordinate2
               A2full = VZApostptb(J).ICoordinate1
               A2red = A2full - VApostlim(Z2,1) + 1
               If VZApostptb(I).IParameter = VZApostptb(J).IParameter Then 
                 SZApostCovar(Z1,A1red,Z2,A2red).Rval = SZApostCovar(Z1,A1red,Z2,A2red).Rval + _
                   (VZApostptb(I).Ryield - VZApostmean(Z1,A1red)) * _
                   (VZApostptb(J).Ryield - VZApostmean(Z2,A2red)) 
                 SZApostCovar(Z1,A1red,Z2,A2red).Nval = SZApostCovar(Z1,A1red,Z2,A2red).Nval + 1    

               End If
             Else
               Print "Internal error 7: I,J,IcntZA",I,J,IcntZA
               sleep
             End If             
           Next
         Else
           Print "Internal error 8: I,J,IcntZA",I,J,IcntZA
           sleep
         End If  
       Next     
       
       For I = Zmin To Zmax
         For J = VApostlim(I,1) To VApostlim(I,2) 
           A1red = J - VApostlim(I,1) + 1
           For K = Zmin To Zmax
             For L = VApostlim(K,1) To VApostlim(K,2)
               A2red = L - VApostlim(K,1) + 1 
'              If SZApostCovar(I,A1red,K,A2red).Nval > 1 Then
               If SZApostCovar(I,A1red,K,A2red).Nval = N_Error_Max Then
                 SZApostCovar(I,A1red,K,A2red).Rval = SZApostCovar(I,A1red,K,A2red).Rval / _
                                          (SZApostCovar(I,A1red,K,A2red).Nval-1)
                 If VZApostrms(I,A1red) <> 0 And VZApostrms(K,A2red) <> 0 Then
                   SZApostCorr(I,A1red,K,A2red).Rval = SZApostCovar(I,A1red,K,A2red).Rval / _
                          (VZApostrms(I,A1red) * VZApostrms(K,A2red))     
                 End If                         
               Else
                 SZApostCovar(I,A1red,K,A2red).Rval = 0
               End If                           
             Next
           Next
         Next
       Next       
       
       If B_Double_Covar = 1 Then  ' Store values for covariances between different systems
         Print #fmvd, "* System Nr.   Parameterset  Observable     Number (1. 2. dim)        Deviation"
         For I = 1 To IcntZA
           Print #fmvd, I_Double_Covar, VZApostptb(I).IParameter, "ZApost", _ 
                        VZApostptb(I).ICoordinate1, _
                        VZApostptb(I).ICoordinate2, _
                        VZApostptb(I).Ryield  - _
                            VZApostmean(VZApostptb(I).ICoordinate2,VZApostptb(I).ICoordinate1 + 1 - _ 
                            VApostlim(VZApostptb(I).ICoordinate2,1)) 
         Next I
         Close #fmvd
       End If
       
       

     ' Uncertainties

       For I = 20 To 190
         If d_APost(1,I) > 0 Then
           d_APost(0,I) = sqr ( (d_APost(2,I) - _
                                 d_APost(1,I)^2/N_Error_Max ) /(N_Error_Max-1.0) )
         Else
           d_APost(0,I) = APost(I)                       
         End If
         If d_APost(0,I) > APost(I) Or d_APost(0,I) = 0 Then 
            d_APost(0,I) = APost(I)
         End If   
       Next            

       For I = 20 To 70
         If d_ZPost(1,I) > 0 Then
           d_ZPost(0,I) = sqr ( (d_ZPost(2,I) - _
                                 d_ZPost(1,I)^2/N_Error_Max ) /(N_Error_Max-1.0) )
         Else
           d_ZPost(0,I) = ZPost(I)                       
         End If
         If d_ZPost(0,I) > ZPost(I) Or d_ZPost(0,I) = 0 Then 
           d_ZPost(0,I) = ZPost(I)
         End If        
       Next I

       For I = 20 To P_A_CN - 20
         For J = 10 To P_Z_CN - 10
           If ZISOPOST(I,J) > 0 Then
             d_ZISOPOST(0,I,J) = sqr ( (d_ZISOPOST(2,I,J) - _
                                        d_ZISOPOST(1,I,J)^2/N_Error_Max ) /(N_Error_Max-1.0) )
           Else
             d_ZISOPOST(0,I,J) = ZISOPOST(I,J)      
       	   End If
       	   If d_ZISOPOST(0,I,J) > ZISOPOST(I,J) or d_ZISOPOST(0,I,J) = 0 Then
       	                d_ZISOPOST(0,I,J) = ZISOPOST(I,J)
       	   End If             
         Next
       Next
       For I = 10 To P_A_CN - P_Z_CN -10
         For J = 10 To P_Z_CN - 10
           If NZPost(I,J) > 0 Then
             d_NZPOST(0,I,J) = sqr ( (d_NZPOST(2,I,J) - _
                                      d_NZPOST(1,I,J)^2/N_Error_Max ) /(N_Error_Max-1.0) )
           Else
             d_NZPost(0,I,J) = NZPost(I,J)
                   ' relative error set to 100%
           End If
           If d_NZPost(0,I,J) > NZPost(I,J) Or d_NZPost(0,I,J) = 0 Then 
                    d_NZPost(0,I,J) = NZPost(I,J) 
                   ' relative error limited to 100% 
           End If         
         Next
       Next
       d_NCN(0) = sqr( (d_NCN(2) - d_NCN(1)^2/N_Error_Max) / (N_Error_Max - 1.0) )
       d_Nsci(0) = sqr( (d_Nsci(2) - d_Nsci(1)^2/N_Error_Max) / (N_Error_Max - 1.0) ) 
       d_Nfr(0) = sqr( (d_Nfr(2) - d_Nfr(1)^2/N_Error_Max) / (N_Error_Max - 1.0) )
       d_Nlight(0) = sqr( (d_Nlight(2) - d_Nlight(1)^2/N_Error_Max) / (N_Error_Max - 1.0) )
       d_Nheavy(0) = sqr( (d_Nheavy(2) - d_Nheavy(1)^2/N_Error_Max) / (N_Error_Max - 1.0) )
       d_Ntot(0) = sqr( (d_Ntot(2) - d_Ntot(1)^2/N_Error_Max) / (N_Error_Max - 1.0) )
       d_ENfr(0) = sqr( (d_ENfr(2) - d_ENfr(1)^2/N_Error_Max) / (N_Error_Max - 1.0) )
       
       d_Ng(0) = sqr( (d_Ng(2) - d_Ng(1)^2/N_Error_Max) / (N_Error_Max - 1.0) )
       d_Eg(0) = sqr( (d_Eg(2) - d_Eg(1)^2/N_Error_Max) / (N_Error_Max - 1.0) )
       d_Egtot(0) = sqr( (d_Egtot(2) - d_Egtot(1)^2/N_Error_Max) / (N_Error_Max - 1.0) )
       d_TKEpre(0) = sqr( (d_TKEpre(2) - d_TKEpre(1)^2/N_Error_Max) / (N_Error_Max - 1.0) )
       d_TKEpost(0) = sqr( (d_TKEpost(2) - d_TKEpost(1)^2/N_Error_Max) / (N_Error_max - 1.0) )
       d_TotXE(0) = sqr( (d_TotXE(2) - d_TotXE(1)^2/N_Error_Max) / (N_Error_Max - 1.0) )
    
    
       ' --- Covariances between the yields of two fissioning systems ---
       
       If B_Double_Covar = 1 And I_Double_Covar = 2 Then
         Dim As String C_dummy
         Redim As String C_part(1) 
         Dim As Integer Nstored, Istored, Nvalues
         fmvd = freefile       
         Open Cfilemvd For Input As #fmvd
         Nstored = 0
         Do 
           Line Input #fmvd, C_dummy
           If Left(Ltrim(C_dummy),1) <> "*" Then Nstored = Nstored + 1
         Loop Until EOF(fmvd)
         Close #fmvd

         Redim As Integer VIstored(4,Nstored)
           ' 1: System, 2: Parset, 3: Dim. 1, 4: Dim. 2
         Redim As Single VRstored(Nstored)  
         Redim As String VCstored(Nstored) 

         fmvd = freefile       
         Open Cfilemvd For Input As #fmvd
         Istored = 0
         Do 
           Line Input #fmvd, C_dummy
           If Left(Ltrim(C_dummy),1) <> "*" Then 
             Istored = Istored + 1
             Nvalues = CC_Count(C_dummy," ")
             Redim C_part(Nvalues)
             CC_Cut(C_dummy," ",C_part(),Nvalues)
             VIstored(1,Istored) = Val(C_part(1))  ' Fissioning system (1 or 2)
             VIstored(2,Istored) = Val(C_part(2))  ' Parameter set (1 To N_Error_Max)
             VIstored(3,Istored) = Val(C_part(4))  ' Dimension 1 (e.g. Z or A)
             VIstored(4,Istored) = Val(C_part(5))  ' Dimension 2 (e.g. Z)
             VRstored(Istored) = Val(C_part(6))    ' Deviation from mean value
             VCstored(Istored) = C_part(3)         ' Kind of data (Z, Apre, Apost, ZApre, ZApost)
           End If  
         Loop Until EOF(fmvd) 
         Close #fmvd                  
         
    
          
         ' Covariances between the Z distributions of the two systems
         ' Determine dimension of the covariance matrix.
         Z_Double_max = 0
         Z_Double_min = 1000
         For I = 1 To Nstored
           If VCstored(I) = "Z" Then
             Z_Double_min = Min(Z_Double_min,VIstored(3,I))
             Z_Double_max = Max(Z_Double_max,VIstored(3,I))
           End If
         Next I
         
         ' Reorder deviations for faster access
         Redim As Single VRZ_Double(2,N_Error_Max,Z_Double_min To Z_Double_max)
         For I = 1 To Nstored
           If VCstored(I) = "Z" Then
             VRZ_Double(VIstored(1,I),VIstored(2,I),VIstored(3,I)) = VRstored(I)
           End If  
         Next I

         ' Calculate covariances
         Redim SZ_Double_Covar(Z_Double_min to Z_Double_max, _
                               Z_Double_min to Z_Double_max) As OCovar
         Redim SZ_Double_Corr(Z_Double_min to Z_Double_max, _
                              Z_Double_min to Z_Double_max) As OCorr
         For I = Z_Double_min To Z_Double_max
           For J = Z_Double_min To Z_Double_max
             For K = 1 To N_Error_Max
               If VRZ_Double(1,K,I) <> 0 And VRZ_Double(2,K,J) <> 0 Then
                 SZ_Double_Covar(I,J).Rval = _
                      SZ_Double_Covar(I,J).Rval + VRZ_Double(1,K,I) * VRZ_Double(2,K,J)
                      SZ_Double_Covar(I,J).Nval = SZ_Double_Covar(I,J).Nval + 1
               End If       
             Next K
           Next J
         Next I

         ' Normalize and remove incomplete results         
         For I = Z_Double_min To Z_Double_max
           For J = Z_Double_min To Z_Double_max
             If SZ_Double_Covar(I,J).Nval = N_Error_Max Then 
               SZ_Double_Covar(I,J).Rval = SZ_Double_Covar(I,J).Rval _
                                        / (SZ_Double_Covar(I,J).Nval-1)
             Else
               SZ_Double_Covar(I,J).Rval = 0
             End If                           
           Next J
         Next I  
         
         Redim rms_Double(Z_Double_min to Z_Double_max,2)
         For I = Z_Double_min To Z_Double_max
           For J = 1 To N_Error_Max
             rms_Double(I,1) = rms_Double(I,1) + VRZ_Double(1,J,I)^2
             rms_Double(I,2) = rms_Double(I,2) + VRZ_Double(2,J,I)^2
           Next J
         Next I
         For I = Z_Double_min To Z_Double_max
           rms_Double(I,1) = sqr(rms_Double(I,1) / (N_Error_Max - 1))
           rms_Double(I,2) = sqr(rms_Double(I,2) / (N_Error_Max - 1))
         Next I
         For I = Z_Double_min To Z_Double_max
           For J = Z_Double_min To Z_Double_max
             If rms_Double(I,1) <> 0 And rms_Double(J,2) <> 0 Then                          
                 SZ_Double_Corr(I,J).Rval = SZ_Double_Covar(I,J).Rval / _
                        (rms_Double(I,1) * rms_Double(J,2))          
             End If                       
           Next J
         Next I  
         Erase VRZ_Double
   
         

         ' Covariances between the Apre distributions of the two systems
         ' Determine dimensions of the covariance matrix
         Apre_Double_max = 0
         Apre_Double_min = 1000
         For I = 1 To Nstored
           If VCstored(I) = "Apre" Then
             Apre_Double_min = Min(Apre_Double_min,VIstored(3,I))
             Apre_Double_max = Max(Apre_Double_max,VIstored(3,I))
           End If
         Next I
         
         ' Reorder deviations for faster access         
         Redim As Single VRApre_Double(2,N_Error_Max,Apre_Double_min To Apre_Double_max)
         For I = 1 To Nstored
           If VCstored(I) = "Apre" Then
             VRApre_Double(VIstored(1,I),VIstored(2,I),VIstored(3,I)) = VRstored(I)
           End If  
         Next I         

         ' Calculate covariances
         Redim SApre_Double_Covar(Apre_Double_min to Apre_Double_max, _
                               Apre_Double_min to Apre_Double_max) As OCovar
         Redim SApre_Double_Corr(Apre_Double_min to Apre_Double_max, _
                               Apre_Double_min to Apre_Double_max) As OCorr
         For I = Apre_Double_min To Apre_Double_max
           For J = Apre_Double_min To Apre_Double_max
             For K = 1 To N_Error_Max
               If VRApre_Double(1,K,I) <> 0 And VRApre_Double(2,K,J) <> 0 Then
                 SApre_Double_Covar(I,J).Rval = _
                      SApre_Double_Covar(I,J).Rval + VRApre_Double(1,K,I) * VRApre_Double(2,K,J)
                      SApre_Double_Covar(I,J).Nval = SApre_Double_Covar(I,J).Nval + 1
               End If       
             Next K
           Next J
         Next I

         ' Normalize and remove incomplete results         
         For I = Apre_Double_min To Apre_Double_max
           For J = Apre_Double_min To Apre_Double_max
             If SApre_Double_Covar(I,J).Nval = N_Error_Max Then 
               SApre_Double_Covar(I,J).Rval = SApre_Double_Covar(I,J).Rval _
                                        / (SApre_Double_Covar(I,J).Nval-1)
             Else
               SApre_Double_Covar(I,J).Rval = 0
             End If                              
           Next J
         Next I  
         
         Redim rms_Double(Apre_Double_min to Apre_Double_max,2)
         For I = Apre_Double_min To Apre_Double_max
           For J = 1 To N_Error_Max
             rms_Double(I,1) = rms_Double(I,1) + VRApre_Double(1,J,I)^2
             rms_Double(I,2) = rms_Double(I,2) + VRApre_Double(2,J,I)^2
           Next J
         Next I
         For I = Apre_Double_min To Apre_Double_max
           rms_Double(I,1) = sqr(rms_Double(I,1) / (N_Error_Max - 1))
           rms_Double(I,2) = sqr(rms_Double(I,2) / (N_Error_Max - 1))
         Next I
         For I = Apre_Double_min To Apre_Double_max
           For J = Apre_Double_min To Apre_Double_max
             If rms_Double(I,1) <> 0 And rms_Double(J,2) <> 0 Then                          
                 SApre_Double_Corr(I,J).Rval = SApre_Double_Covar(I,J).Rval / _
                        (rms_Double(I,1) * rms_Double(J,2))          
             End If                       
           Next J
         Next I  
         Erase VRApre_Double

        
   
         ' Covariances between the Apost distributions of the two systems
         ' Determine dimensions of the covariance matrix
         Apost_Double_max = 0
         Apost_Double_min = 1000
         For I = 1 To Nstored
           If VCstored(I) = "Apost" Then
             Apost_Double_min = Min(Apost_Double_min,VIstored(3,I))
             Apost_Double_max = Max(Apost_Double_max,VIstored(3,I))
           End If
         Next I
         
         ' Reorder deviations for faster access         
         Redim As Single VRApost_Double(2,N_Error_Max,Apost_Double_min To Apost_Double_max)
         For I = 1 To Nstored
           If VCstored(I) = "Apost" Then
             VRApost_Double(VIstored(1,I),VIstored(2,I),VIstored(3,I)) = VRstored(I)
           End If  
         Next I         

         ' Calculate covariances
         Redim SApost_Double_Covar(Apost_Double_min to Apost_Double_max, _
                               Apost_Double_min to Apost_Double_max) As OCovar
         Redim SApost_Double_Corr(Apost_Double_min to Apost_Double_max, _
                               Apost_Double_min to Apost_Double_max) As OCorr
         For I = Apost_Double_min To Apost_Double_max
           For J = Apost_Double_min To Apost_Double_max
             For K = 1 To N_Error_Max
               If VRApost_Double(1,K,I) <> 0 And VRApost_Double(2,K,J) <> 0 Then
                 SApost_Double_Covar(I,J).Rval = _
                      SApost_Double_Covar(I,J).Rval + VRApost_Double(1,K,I) * VRApost_Double(2,K,J)
                      SApost_Double_Covar(I,J).Nval = SApost_Double_Covar(I,J).Nval + 1
               End If       
             Next K
           Next J
         Next I

         ' Normalize and remove incomplete results         
         For I = Apost_Double_min To Apre_Double_max
           For J = Apost_Double_min To Apre_Double_max
             If SApost_Double_Covar(I,J).Nval = N_Error_Max Then 
               SApost_Double_Covar(I,J).Rval = SApost_Double_Covar(I,J).Rval _
                                        / (SApost_Double_Covar(I,J).Nval-1)
             Else
               SApost_Double_Covar(I,J).Rval = 0
             End If                           
           Next J
         Next I  

         Redim rms_Double(Apost_Double_min to Apost_Double_max,2)
         For I = Apost_Double_min To Apost_Double_max
           For J = 1 To N_Error_Max
             rms_Double(I,1) = rms_Double(I,1) + VRApost_Double(1,J,I)^2
             rms_Double(I,2) = rms_Double(I,2) + VRApost_Double(2,J,I)^2
           Next J
         Next I
         For I = Apost_Double_min To Apost_Double_max
           rms_Double(I,1) = sqr(rms_Double(I,1) / (N_Error_Max - 1))
           rms_Double(I,2) = sqr(rms_Double(I,2) / (N_Error_Max - 1))
         Next I
         For I = Apost_Double_min To Apost_Double_max
           For J = Apost_Double_min To Apost_Double_max
             If rms_Double(I,1) <> 0 And rms_Double(J,2) <> 0 Then                          
                 SApost_Double_Corr(I,J).Rval = SApost_Double_Covar(I,J).Rval / _
                        (rms_Double(I,1) * rms_Double(J,2))          
             End If                       
           Next J
         Next I  
         Erase VRApost_Double            
         Erase rms_Double
         


         ' Covariance between the pre-neutron nuclide distributions of the two systems 
         ' Determine dimension of covariance matrix
         Redim VApre_Double_lim(Z_Double_min to Z_Double_max,2) As Integer
         Dim As Integer M
         For I = Z_Double_min To Z_Double_max
           VApre_Double_lim(I,1) = 1000
           VApre_Double_lim(I,2) = 0
         Next I
         dApre_Double_max = 0 
         For I = 1 To Nstored
           If VCstored(I) = "ZApre" Then
             Z1 = VIstored(4,I)
             VApre_Double_lim(Z1,1) = Min(VApre_Double_lim(Z1,1),VIstored(3,I))
             VApre_Double_lim(Z1,2) = Max(VApre_Double_lim(Z1,2),VIstored(3,I))
           End If         
         Next I
         For I = Z_Double_min To Z_Double_max
           If VApre_Double_lim(I,1) = 1000 Then 
             VApre_Double_lim(I,1) = 0
           Else 
             dApre_Double_max = Max(dApre_Double_max, VApre_Double_lim(I,2) - VApre_Double_lim(I,1) + 1) 
           End If  
         Next I
         
         ' Reorder deviations for faster access
         Redim As Single VRZApre_Double(2,N_Error_Max,Z_Double_min To Z_Double_max,dApre_Double_max)
         For I = 1 To Nstored
           If VCstored(I) = "ZApre" Then
             Z1 = VIstored(4,I)
             A1 = VIstored(3,I)
             A1red = A1 - VApre_Double_lim(Z1,1) + 1
             VRZApre_Double(VIstored(1,I),VIstored(2,I),A1red,Z1) = _
                     VRstored(I)
           End If
         Next I
         
         
         ' Calculate covariances
         Redim SZApre_Double_Covar(Z_Double_min to Z_Double_max, 1 to dApre_Double_max,_
                                   Z_Double_min to Z_Double_max, 1 to dApre_Double_max) As OCovar
         Redim SZApre_Double_Corr(Z_Double_min to Z_Double_max, 1 to dApre_Double_max,_
                                   Z_Double_min to Z_Double_max, 1 to dApre_Double_max) As OCorr
         For I = Z_Double_min to Z_Double_max
           For J = VApre_Double_lim(I,1) To VApre_Double_lim(I,2)
             A1full = J
             A1red = J - VApre_Double_lim(I,1) + 1
             For K = Z_Double_min To Z_Double_max
               For L = VApre_Double_lim(K,1) To VApre_Double_lim(K,2)
                 A2full = L
                 A2red = L - VApre_Double_lim(K,1) + 1
                 For M = 1 To N_Error_Max
                   If VRZApre_Double(1,M,A1red,I) <> 0 And VRZApre_Double(2,M,A2red,K) <> 0 Then    
 '  Print M,I,J,VRZApre_Double(1,M,A1red,I),VRZApre_Double(2,M,A2red,K)                              
                     SZApre_Double_Covar(I,A1red,K,A2red).Rval = _
                                SZApre_Double_Covar(I,A1red,K,A2red).Rval _ 
                                + VRZApre_Double(1,M,A1red,I) * VRZApre_Double(2,M,A2red,K) 
                     SZApre_Double_Covar(I,A1red,K,A2red).Nval = _
                                SZApre_Double_Covar(I,A1red,K,A2red).Nval + 1
                   End If              
                 Next M       
               Next L
             Next K
           Next J    
         Next I          
         
         ' Normalize and remove incomplete results   
         For I = Z_Double_min to Z_Double_max
           For J = 1 To dApre_Double_max
             For K = Z_Double_min To Z_Double_max
               For L = 1 To dApre_Double_max
                 If SZApre_Double_Covar(I,J,K,L).Nval = N_Error_Max Then 
                   SZApre_Double_Covar(I,J,K,L).Rval = SZApre_Double_Covar(I,J,K,L).Rval / _
                                  (SZApre_Double_Covar(I,J,K,L).Nval - 1) 
                 Else
                   SZApre_Double_Covar(I,J,K,L).Rval = 0
                 End If  
               Next L
             Next K
           Next J
         Next I       
         
         Redim rms_ZA_Double(Z_Double_min to Z_Double_max, 1 to dApre_Double_max,2)
         For I = Z_Double_min to Z_Double_max
           For J = 1 To dApre_Double_max
             For K = 1 To N_Error_Max 
               rms_ZA_Double(I,J,1) = rms_ZA_Double(I,J,1) + VRZApre_Double(1,K,J,I)^2
               rms_ZA_Double(I,J,2) = rms_ZA_Double(I,J,2) + VRZApre_Double(2,K,J,I)^2   
             Next K     
           Next J
         Next I
         For I = Z_Double_min to Z_Double_max
           For J = 1 To dApre_Double_max
             rms_ZA_Double(I,J,1) = sqr(rms_ZA_Double(I,J,1) / (N_Error_Max - 1))
             rms_ZA_Double(I,J,2) = sqr(rms_ZA_Double(I,J,2) / (N_Error_Max - 1))
           Next J
         Next I
         For I = Z_Double_min to Z_Double_max
           For J = 1 To dApre_Double_max
             For K = Z_Double_min to Z_Double_max
               For L = 1 To dApre_Double_max
                 If rms_ZA_Double(I,J,1) <> 0 And rms_ZA_Double(K,L,2) <> 0 Then
                   SZApre_Double_Corr(I,J,K,L).Rval = SZApre_Double_Covar(I,J,K,L).Rval / _
                     (rms_ZA_Double(I,J,1) * rms_ZA_Double(K,L,2))
                 End If
               Next L
             Next K
           Next J
         Next I
         Erase VRZApre_Double
         
         
         
         ' Covariance between the post-neutron nuclide distributions of the two systems 
         ' Determine dimension of covariance matrix
         Redim VApost_Double_lim(Z_Double_min to Z_Double_max,2) As Integer
         For I = Z_Double_min To Z_Double_max
           VApost_Double_lim(I,1) = 1000
           VApost_Double_lim(I,2) = 0
         Next I
         dApost_Double_max = 0 
         For I = 1 To Nstored
           If VCstored(I) = "ZApost" Then
             Z1 = VIstored(4,I)
             VApost_Double_lim(Z1,1) = Min(VApost_Double_lim(Z1,1),VIstored(3,I))
             VApost_Double_lim(Z1,2) = Max(VApost_Double_lim(Z1,2),VIstored(3,I))
           End If         
         Next I
         For I = Z_Double_min To Z_Double_max
           If VApost_Double_lim(I,1) = 1000 Then 
             VApost_Double_lim(I,1) = 0
           Else 
             dApost_Double_max = Max(dApost_Double_max, VApost_Double_lim(I,2) - VApost_Double_lim(I,1) + 1) 
           End If  
         Next I
         
         ' Reorder deviations for faster access
         Redim As Single VRZApost_Double(2,N_Error_Max,Z_Double_min To Z_Double_max,dApost_Double_max)
         For I = 1 To Nstored
           If VCstored(I) = "ZApost" Then
             Z1 = VIstored(4,I)
             A1 = VIstored(3,I)
             A1red = A1 - VApost_Double_lim(Z1,1) + 1
             VRZApost_Double(VIstored(1,I),VIstored(2,I),A1red,Z1) = _
                     VRstored(I)
           End If
         Next I
         
         
         ' Calculate covariances
         Redim SZApost_Double_Covar(Z_Double_min to Z_Double_max, 1 to dApost_Double_max,_
                                   Z_Double_min to Z_Double_max, 1 to dApost_Double_max) As OCovar
         Redim SZApost_Double_Corr(Z_Double_min to Z_Double_max, 1 to dApost_Double_max,_
                                   Z_Double_min to Z_Double_max, 1 to dApost_Double_max) As OCorr
         For I = Z_Double_min to Z_Double_max
           For J = VApost_Double_lim(I,1) To VApost_Double_lim(I,2)
             A1full = J
             A1red = J - VApost_Double_lim(I,1) + 1
             For K = Z_Double_min To Z_Double_max
               For L = VApost_Double_lim(K,1) To VApost_Double_lim(K,2)
                 A2full = L
                 A2red = L - VApost_Double_lim(K,1) + 1
                 For M = 1 To N_Error_Max
                   If VRZApost_Double(1,M,A1red,I) <> 0 And VRZApost_Double(2,M,A2red,K) <> 0 Then    
                     SZApost_Double_Covar(I,A1red,K,A2red).Rval = _
                                SZApost_Double_Covar(I,A1red,K,A2red).Rval _ 
                                + VRZApost_Double(1,M,A1red,I) * VRZApost_Double(2,M,A2red,K) 
                     SZApost_Double_Covar(I,A1red,K,A2red).Nval = _
                                SZApost_Double_Covar(I,A1red,K,A2red).Nval + 1
                   End If              
                 Next M       
               Next L
             Next K
           Next J    
         Next I          
         
         ' Normalize and remove incomplete results   
         For I = Z_Double_min to Z_Double_max
           For J = 1 To dApost_Double_max
             For K = Z_Double_min To Z_Double_max
               For L = 1 To dApost_Double_max
                 If SZApost_Double_Covar(I,J,K,L).Nval = N_Error_Max Then 
                  SZApost_Double_Covar(I,J,K,L).Rval = SZApost_Double_Covar(I,J,K,L).Rval / _
                                  (SZApost_Double_Covar(I,J,K,L).Nval - 1)
                 Else                  
                  SZApost_Double_Covar(I,J,K,L).Rval = 0
                 End If  
               Next L
             Next K
           Next J
         Next I                              

         Redim rms_ZA_Double(Z_Double_min to Z_Double_max, 1 to dApre_Double_max,2)
         For I = Z_Double_min to Z_Double_max
           For J = 1 To dApost_Double_max
             For K = 1 To N_Error_Max 
               rms_ZA_Double(I,J,1) = rms_ZA_Double(I,J,1) + VRZApost_Double(1,K,J,I)^2
               rms_ZA_Double(I,J,2) = rms_ZA_Double(I,J,2) + VRZApost_Double(2,K,J,I)^2   
             Next K          
           Next J
         Next I
         For I = Z_Double_min to Z_Double_max
           For J = 1 To dApost_Double_max
             rms_ZA_Double(I,J,1) = sqr(rms_ZA_Double(I,J,1) / (N_Error_Max - 1))
             rms_ZA_Double(I,J,2) = sqr(rms_ZA_Double(I,J,2) / (N_Error_Max - 1))
           Next J
         Next I
         For I = Z_Double_min to Z_Double_max
           For J = 1 To dApost_Double_max
             For K = Z_Double_min to Z_Double_max
               For L = 1 To dApost_Double_max
                 If rms_ZA_Double(I,J,1) <> 0 And rms_ZA_Double(K,L,2) <> 0 Then
                   SZApost_Double_Corr(I,J,K,L).Rval = SZApost_Double_Covar(I,J,K,L).Rval / _
                     (rms_ZA_Double(I,J,1) * rms_ZA_Double(K,L,2))
                 End If
               Next L
             Next K
           Next J
         Next I
         Erase VRZApre_Double
         Erase rms_ZA_Double
         
         
         
       End If  ' If B_Double_Covar ...
                
     End If
            
   End If
   
   
   If B_Double_Covar = 1 And I_Double_Covar = 2 Then
     Print #f," "
     Print #f," "
   Else  
     Print #f,"<?xml version=""1.0"" encoding=""ISO-8859-1""?>"
   End If  
   Print #f,"<GEF>"
   Print #f,"  <Title>"
   Print #f,"***********************************************************"
   Print #f," "
   Print #f,"       *****************************"
   Print #f,"       *    GEF, Version "+C_GEF_Version+"  *"
   Print #f,"       * K.-H. Schmidt, B. Jurado  *"
   Print #f,"       *****************************"
   Print #f," "
   Dim As Double Rdatetime
   Rdatetime = Now
   Print #f,"Output written on ";
   Print #f, Format( Rdatetime, "dd.mm.yyyy, hh:mm:ss")
   Print #f,"Calculation with ";NEVTtot;" events."
   Print #f," "
   Print #f, Using "& ### & ###";"Calculated yields for the nucleus Z = ";P_Z_CN;", A = ";P_A_CN

   Select Case Emode
      Case 0
        Print #f," at E* = ";P_E_exc;" MeV above the outer saddle."
        Print #f," Spin = ";Spin_CN
      Case 1
        Print #f," at E* = ";P_E_exc;" MeV above the ground state."
        Print #f," Spin = ";Spin_CN
      Case -1
        Print #f," at E* = ";P_E_exc;" MeV above the ground state."
        Print #f," Spin = ";Spin_CN
        Print #f," Only first-chance fission!"
      Case 2
        Print #f," formed by (n,f) with En = ";P_E_exc- E_EXC_ISO;" MeV."
        Print #f," Spin of target nucleus = ";Spin_CN
      Case 12
        Print #f," formed by (p,f) with Ep = ";P_E_exc- E_EXC_ISO;" MeV."
        Print #f," Spin of target nucleus = ";Spin_CN        
      Case 3
        Print #f," with user-defined excitation-energy distribution."
        Print #f," Only first-chance fission."
        Print #f," Spin = ";Spin_CN
   End Select
   If I_E_iso > 0 Then
     Print #f, " Isomer # ";I_E_iso;", at E* = ";E_EXC_ISO;" MeV" 
   End If   
   If Bmulti = 1 And Imulti = 0 Then
     Print #f, " "
     Print #f, "******************************************************************"
     Print #f, " Due to low fission probability, the fission chances could not be determined."
     Print #f, " Therefore, only first-chance fission is calculated!"
     Print #f, " (The validity of this approximation should be checked.)"
     Print #f, "******************************************************************"
   End If
   If EMode = 3 Then
     If I_Warning = 1 Then
       Print #f, " "
       Print #f, "****************************************************************"
       Print #f, "Note: Only first-chance fission is calculated "
       Print #f, "      when an excitation-energy distribution is given on input."
       Print #f, "****************************************************************"
     End If
   End If


   Print #f," "

   Print #f," Shell effect in symmetric channel: ";Delta_S0;" MeV."

   Print #f,"  </Title>"
   
   Print #f,"  <Prompt_results>"
   
   If Inofirst = 0 Then
     Print #f, "    <Multi_chance>" 
     Print #f, "Probability for first-chance fission: 100%." 
     Print #f, "    </Multi_chance>"
   End If
   
   If Inofirst > 0 Then
     Print #f, "    <Multi_chance>"  
     If Emode = 2 Then
       Print #f, "Pre-compound neutron emission is considered."
     Else
       Print #f, "Calculation starts with the assumed formation of a compound nucleus." 
       Print #f, "(Pre-compound processes are not considered.)" 
     End If
     Print #f, "Fission after eventual emission of neutrons"
     Print #f, "(multi-chance fission) is included."
     Print #f, " "
     Print #f, "---------------------------------------------------------------"
     Print #f, "--- Relative contributions of the different fission chances ---"
     Print #f, "---------------------------------------------------------------"
     Print #f, " "
     Print #f, " pre-fission neutrons      protons      contribution" 
     For I = 0 To 10
       For J = 0 To 2
         If W_chances(I,J) > 0 Then
           Print #f, "      ",I,J,W_chances(I,J)
         End If
       Next J  
     Next I 
     Print #f, " "
     Print #f, " "
     Dim As Integer Imax,Jmax,Kmin,Kmax
     
   /'For J = 0 To 400
       Print Using "####.#";J*0.1;
       For I = 0 To 5
         Print Using " #.#####";E_multi_chance(I,J);
       Next
       Print " "
     Next'/ 
     
     For I = 0 To Ubound(E_multi_chance,1)
       For J = 0 To Ubound(E_multi_chance,2)
         For K = 0 To Ubound(E_multi_chance,3) 
           If E_multi_chance(I,J,K) > 0 Then 
             Imax = I
           End If
         Next    
       Next
     Next
     For J = 0 To Ubound(E_multi_chance,2) 
       For I = 0 To Ubound(E_multi_chance,1)
         For K = 0 To Ubound(E_multi_chance,3)
           If E_multi_chance(I,J,K) > 0 Then 
             Jmax = J
           End If 
         Next  
       Next
     Next
     For K = 0 To Ubound(E_multi_chance,3) 
       For I = 0 To Ubound(E_multi_chance,1) 
         For J = 0 To Ubound(E_multi_chance,2) 
           If E_multi_chance(I,J,K) > 0 Then 
             Kmax = K
           End If  
         Next  
       Next
     Next
     For K = Ubound(E_multi_chance,3) To 0 Step -1 
       For I = 0 To Ubound(E_multi_chance,1) 
         For J = 0 To Ubound(E_multi_chance,2) 
           If E_multi_chance(I,J,K) > 0 Then 
             Kmin = K
           End If  
         Next  
       Next
     Next
     
     If Imax > 0 Or Jmax > 0 Then
       Print #f, "--- Number of events with E* ---"
       Print #f, "(E* = excitation energy above nuclear ground state at fission)"
       Print #f, "Separately given for # of pre-fission neutrons / protons"
       If Imax + Jmax > 11 Then
         Print #f, "Only 11 first chances given."
       End If
       Print #f, " "
       Print #f, " E*/MeV ";
       For I = 0 To Min(10,Imax)
         For J = 0 To Min(2,Jmax)
           If Imax + Jmax <= 10 Then
             Print #f, "  ";I;" /";J;"   ";
           End If  
         Next  
       Next
       Print #f, " "
       For K = Kmin To Kmax
         Print #f, Using "####.#";K * 0.1;" ";
         For I = 0 To Min(10,Imax)
           For J = 0 To Min(2,Jmax)
             If Imax + Jmax <= 10 Then
               Print #f, Using "    #.#####";E_multi_chance(I,J,K);
             End If   
           Next  
         Next  
         Print #f, " "
       Next
       Print #f, " "
       Print #f, " "
       Print #f, "The energies of the pre-fission neutrons are included in the"
       Print #f, "list-mode output file, if the list-mode option is activated."
     End If
     Print #f, "    </Multi_chance>"  
   End If

   If Emode < 3 And Inofirst = 0 Then
     Print #f,"    <Fission_channels>"
     
     Print #f,"----------------------------------------------------------"
     Print #f,"--- Relative yields of fission channels (exact values) ---"
     Print #f,"----------------------------------------------------------"
     Print #f," "
     Print #f,"Yield of SL (super long) Channel = ", 100*Round(Yield_Mode_0,4) ;" %"         /' Relative yield of SL '/
     Print #f,"Yield of S1 (standard I) Channel = ", 100*Round(Yield_Mode_1,4) ;" %"         /' Relative yield of S1 '/
     Print #f,"Yield of S2 (standard II) Channel = ", 100*Round(Yield_Mode_2,4) ;" %"        /' Relative yield of S2 '/
     Print #f,"Yield of SA (superasymmetric) Channel = ", 100*Round(Yield_Mode_3,4) ;" %"    /' Relative yield of S3 '/
     If P_A_CN < 220 Then _
       Print #f,"Yield of Z=38 channel = "," ", 100*Round(Yield_Mode_4,4) ;" %"                /' Relative yield of S4 '/
     Print #f,"Yield of S1 Channel in both fragments = ", 100*Round(Yield_Mode_11,4) ;" %"   /' Relative yield of S11 '/
     Print #f,"Yield of S2 Channel in both fragments = ", 100*Round(Yield_Mode_22,4) ;" %"   /' Relative yield of S22 '/

     Print #f,"    </Fission_channels>"
   Else
     Print #f,"    <Fission_channels>"

     Print #f,"------------------------------------------------------------------"
     Print #f,"--- Relative yields of fission channels (Monte-Carlo sampling) ---"
     Print #f,"------------------------------------------------------------------"
     Print #f," "
     Print #f,"Yield of SL (super long) Channel = ", 100*Round(Mode_Events(0)/Mode_Events(10),4) ;" %"         /' Relative yield of SL '/
     Print #f,"Yield of S1 (standard I) Channel = ", 100*Round(Mode_Events(1)/Mode_Events(10),4) ;" %"         /' Relative yield of S1 '/
     Print #f,"Yield of S2 (standard II) Channel = ", 100*Round(Mode_Events(2)/Mode_Events(10),4) ;" %"        /' Relative yield of S2 '/
     Print #f,"Yield of SA (superasymmetric) Channel = ", 100*Round(Mode_Events(3)/Mode_Events(10),4) ;" %"    /' Relative yield of S3 '/
     If P_A_CN < 220 Then _
       Print #f,"Yield of Z=38 channel = "," ", 100*Round(Mode_Events(4)/Mode_Events(10),4) ;" %"                /' Relative yield of S4 '/
     If Mode_Events(5) > 0 Then _
       Print #f,"Yield of S1 Channel in both fragments = ", 100*Round(Mode_Events(5)/Mode_Events(10),4) ;" %"   /' Relative yield of S11 '/
     If Mode_Events(6) > 0 Then _
       Print #f,"Yield of S2 Channel in both fragments = ", 100*Round(Mode_Events(6)/Mode_Events(10),4) ;" %"   /' Relative yield of S22 '/

     Print #f,"    </Fission_channels>"
   End If
     


   Dim As Single Dplus,Dminus
   
   Scope
   Dim As Single YZeven,YZodd

   Print #f,"    <FF>"
   Print #f,"      <FF_Z>"
   Print #f,"        <Element_yields>"
   Print #f,"----------------------------------"
   Print #f,"--- Element-yield distribution ---"
   Print #f,"----------------------------------"
   Print #f," "
   If B_Error_On = 1 And B_Error_Analysis = 0 Then
     Print #f,"  Z         Yield          Uncertainty(+-)"
   Else
     Print #f,"  Z         Yield"
   End If  
   Print #f," "
   For I = 20 To 70
     If ZPost(I) > 0 Then
       If B_Error_On = 1 And B_Error_Analysis = 0 Then
         Dplus = d_ZPost(0,I) 
         Dminus = -d_ZPost(0,I)
         Print #f,Using "####    ###.#####   &###.#####   &###.#####";I;ZPost(I);"  ";Dplus;" ";Dminus
       Else   
   	     Print #f,Using "####    ###.#####";I;ZPOST(I)
   	   End If
   	 End If    
   Next
   Print #f,"        </Element_yields>"
   
   YZeven = 0
   YZodd = 0
   For I = 20 To 70 Step 2
     YZeven = YZeven + ZPost(I)
   Next
   For I = 21 To 69 Step 2
     YZodd = YZodd + ZPost(I)
   Next 
   Print #f,"        <Z_even_odd>"
   Print #f,"Global even-odd effect in Z yields: "; Round((YZeven-YZodd) / (YZeven + YZodd) * 100,3);" %"
   Print #f,"        </Z_even_odd>"
    
    
   If B_Error_On = 1 And B_Error_Analysis = 0 Then    
     Dim As Integer Inl
     If Bcov = 1 Then
       Print #f,"        <Z_covariances>"
       Print #f,"--- Covariance matrix of Z yields ---"
       Print #f," "
       Print #f,"  Header for loop structure: (For Z1 =";Zmin;" to";Zmax;") (For Z2 =";Zmin;" to";Zmax;")" 
       Print #f," "
       Inl = 0  
       For I = Zmin To Zmax 
         For J = Zmin To Zmax
           Inl = Inl + 1
           Print #f, Round(SZCovar(I,J).Rval,4);" ";
           If Inl = 20 Then
             Inl = 0
             Print #f," "
           End If  
         Next
       Next
       Print #f," "
       Print #f,"        </Z_covariances>"
     End If
     If Bcor = 1 Then  
       Print #f,"        <Z_correlations>"
       Print #f,"--- Corrrelation matrix of Z yields ---"
       Print #f," "
       Print #f,"  Header for loop structure: (For Z1 =";Zmin;" to";Zmax;") (For Z2 =";Zmin;" to";Zmax;")" 
       Print #f," "
       Inl = 0  
       For I = Zmin To Zmax 
         For J = Zmin To Zmax
           Inl = Inl + 1
           Print #f, Round(SZCorr(I,J).Rval,4);" ";
           If Inl = 20 Then
             Inl = 0
             Print #f," "
           End If  
         Next
       Next
       Print #f," "
       Print #f,"        </Z_correlations>"
     End If  
     
     If B_Double_Covar = 1 And I_Double_Covar = 2 Then
       If Bcov = 1 Then
         Print #f,"        <Z_covariances_2_systems>"
         Print #f,"--- Covariance matrix of Z yields for the two fissioning systems ---"
         Print #f," "
         Print #f,"  Header for loop structure: (For Z1 (first system) =";Z_Double_min;" to";Z_Double_max; _
                       ") (For Z2 (second system) =";Z_Double_min;" to";Z_Double_max;")" 
         Print #f," "
         Dim As Integer Inl 
         Inl = 0  
         For I = Z_Double_min To Z_Double_max 
           For J = Z_Double_min To Z_Double_max
             Inl = Inl + 1
             Print #f, Round(SZ_Double_Covar(I,J).Rval,4);" "; 
             If Inl = 20 Then
               Inl = 0
               Print #f," "
             End If  
           Next
         Next
         Print #f," "
         Print #f,"        </Z_covariances_2_systems>"
       End If
       If Bcor = 1 Then  
         Print #f,"        <Z_correlations_2_systems>"
         Print #f,"--- Correlation matrix of Z yields for the two fissioning systems ---"
         Print #f," "
         Print #f,"  Header for loop structure: (For Z1 (first system) =";Z_Double_min;" to";Z_Double_max; _
                     ") (For Z2 (second system) =";Z_Double_min;" to";Z_Double_max;")" 
         Print #f," "
         Inl = 0  
         For I = Z_Double_min To Z_Double_max 
           For J = Z_Double_min To Z_Double_max
             Inl = Inl + 1
             Print #f, Round(SZ_Double_Corr(I,J).Rval,4);" "; 
             If Inl = 20 Then
               Inl = 0
               Print #f," "
             End If  
           Next
         Next
         Print #f," "
         Print #f,"        </Z_correlations_2_systems>"
       End If
     End If  
   End If  
   Print #f,"      </FF_Z>"     
   
   Dim As Single NYsum,Ysum,NoverZ
   Print #f,"      <N_over_Z>"
   Print #f,"--------------------------------------------------------"
   Print #f,"--- N_mean/Z over Z (before prompt-neutron emission) ---"
   Print #f,"--------------------------------------------------------"
   Print #f," "
   Print #f,"  Z         N_mean/Z (pre-neutron)"
   For I = 20 To P_Z_CN - 20
     Nysum = 0
     Ysum = 0
     If ZPost(I) > 0 Then
        For J = 30 to 120
          Nysum = Nysum + J * NZpre(J,I)
          Ysum = Ysum + NZpre(J,I)
        Next  
        NoverZ = (Nysum/Ysum) / I
        Print #f, I, NoverZ
     End If
   Next        

   
   Print #f," "
   Print #f," "
   Print #f,"-------------------------------------------------------"
   Print #f,"--- N_mean/Z over Z (after prompt-neutron emission) ---"
   Print #f,"-------------------------------------------------------"
   Print #f," "
   Print #f,"  Z         N_mean/Z (post-neutron)"
   For I = 20 To P_Z_CN - 20
     Nysum = 0
     Ysum = 0
     If ZPost(I) > 0 Then
        For J = 30 to 120
          Nysum = Nysum + J * NZpost(J,I)
          Ysum = Ysum + NZpost(J,I)
        Next  
        NoverZ = (Nysum/Ysum) / I
        Print #f, I, NoverZ
     End If
   Next    
   Print #f,"      </N_over_Z>"    

   
   End Scope
   
   Print #f,"      <FF_N>"
   Print #f,"        <Isotonic_yields>"
   Print #f,"----------------------------------------------------------------------------"
   Print #f,"--- Isotonic yield distribution before and after prompt-neutron emission ---"
   Print #f,"----------------------------------------------------------------------------"
   Print #f," "
   Print #f,"  N         Yield(pre-neutron)     Yield(post-neutron)"
   Print #f," "
   For I = 20 To 120
     If Npre(I) > 0 Or Npost(I) > 0 Then
'       Print #f, I, Npre(I),, Npost(I)
   	    Print #f,Using "####         ###.#####              ###.#####";_
   	              I;NPre(I);NPost(I)        
     End If
   Next
   Print #f,"        </Isotonic_yields>"
   Print #f,"      </FF_N>"

   Print #f,"      <FF_A>"
   Print #f,"        <Mass_yields>"
   Print #f,"------------------------------------------------------------------------"
   Print #f,"--- Mass-yield distribution before and after prompt-neutron emission ---"
   Print #f,"------------------------------------------------------------------------"
   Print #f," "
   If B_Error_On = 1 And B_Error_Analysis = 0 Then
     Print #f,"  A"," Yield","    Yield","     Uncertainty(+-)"
   Else
     Print #f,"  A"," Yield","    Yield"
   End If  
   Print #f," "," pre-neutron","    post-neutron"
   Print #f," "
   For I = 20 To 190
     If APost(I) > 0 Or APre(I) > 0 Then 
       If B_Error_On = 1 And B_Error_Analysis = 0 Then
         Dplus = d_APost(0,I) 
         Dminus = -d_APost(0,I)
         Print #f,Using "####          ###.######       ###.######   &###.#####   &###.##### "; _
                  I;Apre(I);APost(I);"  ";Dplus;" ";Dminus
       Else
   	     Print #f,Using "####          ###.######       ###.######";_
   	              I;APre(I);APost(I)
   	   End If  
   	 End If  
   Next
   Print #f,"        </Mass_yields>"


   If B_Error_On = 1 And B_Error_Analysis = 0 Then   
     Dim As Integer Inl 
     If Bcov = 1 Then
       Print #f,"        <Apre_covariances>"
       Print #f,"--- Covariance matrix of A yields before prompt-neutron emission ---"
       Print #f," "
       Print #f,"  Header for loop structure: (For A1 =";Apremin;" to";Apremax; _
                                          ") (For A2 =";Apremin;" to";Apremax;")" 
       Print #f," "
       Inl = 0
       For I = Apremin To Apremax 
         For J = Apremin To Apremax
            Print #f, Round(SApreCovar(I,J).Rval,4);" ";
            Inl = Inl + 1
            If Inl >= 20 Then
              Inl = 0
              Print #f, " "
            End If
         Next
       Next
       Print #f," "
       Print #f,"        </Apre_covariances>"
     End If
     If BCor = 1 Then  
       Print #f,"        <Apre_correlations>"
       Print #f,"--- Correlation matrix of A yields before prompt-neutron emission ---"
       Print #f," "
       Print #f,"  Header for loop structure: (For A1 =";Apremin;" to";Apremax; _
                                          ") (For A2 =";Apremin;" to";Apremax;")" 
       Print #f," "
       Inl = 0
       For I = Apremin To Apremax 
         For J = Apremin To Apremax
            Print #f, Round(SApreCorr(I,J).Rval,4);" ";
            Inl = Inl + 1
            If Inl >= 20 Then
              Inl = 0
              Print #f, " "
            End If
         Next
       Next
       Print #f," "
       Print #f,"        </Apre_correlations>"
     End If
     If Bcov = 1 Then  
       Print #f,"        <Apost_covariances>"
       Print #f,"--- Covariance matrix of A yields after prompt-neutron emission ---"
       Print #f," "
       Print #f,"  Header for loop structure: (For A1 =";Apostmin;" to";Apostmax; _
                                          ") (For A2 =";Apostmin;" to";Apostmax;")" 
       Print #f," "
       Inl = 0
       For I = Apremin To Apremax 
         For J = Apremin To Apremax
           Inl = Inl + 1
           Print #f, Round(SApostCovar(I,J).Rval,4);" ";
           If Inl >= 20 Then
             Inl = 0
             Print #f," "
           End If  
         Next
       Next 
       Print #f," "
       Print #f,"        </Apost_covariances>"
     End If
     If Bcor = 1 Then       
       Print #f,"        <Apost_correlations>"
       Print #f,"--- Correlation matrix of A yields after prompt-neutron emission ---"
       Print #f," "
       Print #f,"  Header for loop structure: (For A1 =";Apostmin;" to";Apostmax; _
                                          ") (For A2 =";Apostmin;" to";Apostmax;")" 
       Print #f," "
       Inl = 0
       For I = Apremin To Apremax 
         For J = Apremin To Apremax
           Inl = Inl + 1
           Print #f, Round(SApostCorr(I,J).Rval,4);" ";
           If Inl >= 20 Then
             Inl = 0
             Print #f," "
           End If  
         Next
       Next
       Print #f," "
       Print #f,"        </Apost_correlations>"
     End If
     
     If B_Double_Covar = 1 And I_Double_Covar = 2 Then
       If Bcov = 1 Then
         Print #f," "
         Print #f,"        <Apre_covariances_2_systems>"
         Print #f,"--- Covariance matrix of A yields before neutron emission for the two fissioning systems ---"
         Print #f," "
         Print #f,"  Header for loop structure: (For A1 (first system) =";Apre_Double_min;" to";Apre_Double_max; _
                     ") (For A2 (second system) =";Apre_Double_min;" to";Apre_Double_max;")" 
         Print #f," "
         Dim As Integer Inl 
         Inl = 0  
         For I = Apre_Double_min To Apre_Double_max 
           For J = Apre_Double_min To Apre_Double_max
             Inl = Inl + 1
             Print #f, Round(SApre_Double_Covar(I,J).Rval,4);" "; 
             If Inl = 20 Then
               Inl = 0
               Print #f," "
             End If  
           Next
         Next
         Print #f," "
         Print #f,"        </Apre_covariances_2_systems>"
       End If
       If Bcor = 1 Then  
         Print #f,"        <Apre_correlations_2_systems>"
         Print #f,"--- Correlation matrix of A yields before neutron emission for the two fissioning systems ---"
         Print #f," "
         Print #f,"  Header for loop structure: (For A1 (first system) =";Apre_Double_min;" to";Apre_Double_max; _
                     ") (For A2 (second system) =";Apre_Double_min;" to";Apre_Double_max;")" 
         Print #f," "
         Inl = 0  
         For I = Apre_Double_min To Apre_Double_max 
           For J = Apre_Double_min To Apre_Double_max
             Inl = Inl + 1
             Print #f, Round(SApre_Double_Corr(I,J).Rval,4);" "; 
             If Inl = 20 Then
               Inl = 0
               Print #f," "
             End If  
           Next
         Next
         Print #f," "
         Print #f,"        </Apre_correlations_2_systems>"
       End If         
       If Bcov = 1 Then
         Print #f,"        <Apost_covariances_2_systems>"
         Print #f,"--- Covariance matrix of A yields after neutron emission for the two fissioning systems ---"
         Print #f," "
         Print #f,"  Header for loop structure: (For A1 (first system) =";Apost_Double_min;" to";Apost_Double_max; _
                     ") (For A2 (second system) =";Apost_Double_min;" to";Apost_Double_max;")" 
         Print #f," "
         Inl = 0  
         For I = Apost_Double_min To Apost_Double_max 
           For J = Apost_Double_min To Apost_Double_max
             Inl = Inl + 1
             Print #f, Round(SApost_Double_Covar(I,J).Rval,4);" "; 
             If Inl = 20 Then
               Inl = 0
               Print #f," "
             End If  
           Next
         Next  
         Print #f," "
         Print #f,"        </Apost_covariances_2_systems>"
       End If
       If Bcor = 1 Then
         Print #f,"        <Apost_correlations_2_systems>"
         Print #f," "
         Print #f,"--- Correlation matrix of A yields after neutron emission for the two fissioning systems ---"
         Print #f," "
         Print #f,"  Header for loop structure: (For A1 (first system) =";Apost_Double_min;" to";Apost_Double_max; _
                     ") (For A2 (second system) =";Apost_Double_min;" to";Apost_Double_max;")" 
         Print #f," "
         Inl = 0  
         For I = Apost_Double_min To Apost_Double_max 
           For J = Apost_Double_min To Apost_Double_max
             Inl = Inl + 1
             Print #f, Round(SApost_Double_Corr(I,J).Rval,4);" "; 
             If Inl = 20 Then
               Inl = 0
               Print #f," "
             End If  
           Next
         Next
         Print #f," "       
         Print #f,"        </Apost_correlations_2_systems>"
       End If       
     End If
   End If  
   Print #f,"      </FF_A>"    


   Dim As Single RS,RS0,RS1,RS2,RZUCD

    /' Normalization to 200 % '/
    RS0 = 0
    For I = 1 To 300
      For J = 1 To 100
        RS0 = RS0 + ZISOPRE(I,J)
      Next
    Next
    IF RS0 > 0 Then
      For I = 1 To 300
        For J = 1 To 100
          RS = ZISOPRE(I,J) / RS0 * 200.E0
          ZISOPRE(I,J) = RS
        Next
      Next
    EndIf

    /' Charge polarization '/
    For I = 1 To 300  /' Loop over masses '/
      RS0 = 0
      RS1 = 0
      For J = 1 To 100   /' Loop over Z '/
        RS0 = RS0 + ZISOPRE(I,J)
        RS1 = RS1 + J * ZISOPRE(I,J)
      Next
      RZUCD = I / P_A_CN * P_Z_CN
      IF RS0 > 0 Then
        RS = RS1 / RS0 - RZUCD
        ZPOLARPRE(I) = RS
      EndIf
    Next

    /' Width of isobaric charge distribution'/
    For I = 1 To 300
      RS0 = 0
      RS2 = 0
      RZUCD = I / P_A_CN * P_Z_CN
      For J = 1 To 100
        RS0 = RS0 + ZISOPRE(I,J)
        RS2 = RS2 + (J - ZPOLARPRE(I) - RZUCD)^2 * ZISOPRE(I,J)
      Next
      IF RS0 > 0 Then
        RS = RS2 / RS0
        SIGMAZPRE(I) = sqr(RS)
      EndIf
    Next


    /' Normalization to 200 % '/
    RS0 = 0
    For I = 1 To 300
      For J = 1 To 100
        RS0 = RS0 + ZISOPOST(I,J)
      Next
    Next
    IF RS0 > 0 Then
      For I = 1 To 300
        For J = 1 To 100
          RS = ZISOPOST(I,J) / RS0 * 200.E0
          ZISOPOST(I,J) = RS
        Next
      Next
    EndIf


    /' Charge polarization '/
    For I = 1 To 300  /' Loop over masses '/
      RS0 = 0
      RS1 = 0
      For J = 1 To 100   /' Loop over Z'/
        RS0 = RS0 + ZISOPOST(I,J)
        RS1 = RS1 + J * ZISOPOST(I,J)
      Next
      RZUCD = I / P_A_CN * P_Z_CN
      IF RS0 > 0 Then
        RS = RS1 / RS0 - RZUCD
        ZPOLARPOST(I) = RS
      EndIf
    Next

    /' Width of isobaric charge distribution'/
    For I = 1 To 300
      RS0 = 0
      RS2 = 0
      RZUCD = I / P_A_CN * P_Z_CN
      For J = 1 To 100
        RS0 = RS0 + ZISOPOST(I,J)
        RS2 = RS2 + (J - ZPOLARPOST(I) - RZUCD)^2 * ZISOPOST(I,J)
      Next
      IF RS0 > 0 Then
        RS = RS2 / RS0
        SIGMAZPOST(I) = sqr(RS)
      EndIf
    Next

    Dim As Single RL(5)
    Dim As Single REO
    Dim As Single SumEven,SumOdd
    Dim As Single EvenOdd
    SumEven = 0
    SumOdd = 0
    /' Local even-odd effect '/
    For I = 9 To 84
      For J = 1 To 5
        RL(J) = ZPOST(I+J)
        If RL(J) = 0 Then GOTO NEXTIP
        RL(J) = LOG(RL(J))
      Next
      REO = 0.125E0 * (-1)^(I+J+1) * (RL(1) - 3.E0*RL(2) + 3.E0*RL(3) - RL(4))
      DPLOCAL(I+2) = REO
    NEXTIP:
      If I Mod 2 = 0 Then
        SumEven = SumEven + ZPost(I)
      Else
        SumOdd = SumOdd + ZPost(I)
      EndIf
    Next
    EvenOdd = (SumEven-SumOdd) / (SumEven+SumOdd)

    For I = 9 To 150
      For J = 1 To 5
        RL(J) = NPOST(I+J)
        If RL(J) = 0 Then GOTO NEXTIN
        RL(J) = LOG(RL(J))
      Next
      REO = 0.125E0 * (-1)^(I+J+1) * (RL(1) - 3.E0*RL(2) + 3.E0*RL(3) - RL(4))
      DNLOCAL(I+2) = REO
    NEXTIN:
    Next
    
   Dim As Integer Ilast  ' For inserting blank line between different Z
   Print #f,"      <FF_AZ>"
   Print #f,"        <Independent_yields>"
   Print #f,"------------------------------------------------------------------------------"
   Print #f,"--- Independent mass-chain yields before and after prompt-neutron emission ---"
   Print #f,"------------------------------------------------------------------------------"
   Print #f,"    (Isomeric ratios are given below -> 'FF_spin -> Isomeric_yields'.)"
   Print #f," "
   If B_Error_On = 1 And B_Error_Analysis = 0 Then
     Print #f,"  A    Z        Yield              Yield          Uncertainty(+-)"
   Else
     Print #f,"  A    Z        Yield              Yield"
   End If     
   Print #f,"                pre-neutron        post-neutron"
   For I = 20 To P_A_CN - 20
     For J = 10 To P_Z_CN - 10
     	  If ZISOPRE(I,J) > 0 Or ZISOPOST(I,J) > 0 Then
     	     If I > Ilast Then
     	  	     Print #f," "
                 Ilast = I
     	     End If
     	     If B_Error_On = 1 And B_Error_Analysis = 0 Then
     	       Dplus = d_ZISOPOST(0,I,J)   
     	       Dminus = -d_ZISOPOST(0,I,J)
     	       Print #f,Using "####  ###     ###.######         ###.######   &###.######  &###.######";_
     	           I;J;ZISOPRE(I,J);ZISOPOST(I,J);"  ";Dplus;" ";Dminus
             Else
     	       Print #f,Using "####  ###     ###.######         ###.######";_
     	           I;J;ZISOPRE(I,J);ZISOPOST(I,J)
     	     End If  
     	  End If
     Next
   Next
   
'For I = 10 To P_Z_CN - 10
'  For J = 10 To P_A_CN - P_Z_CN - 10
'    If NZpre(J,I) > 0 Then
'      Print #f, I, I+J, NZpre(J,I) * 100 / 1.E5
'    End If 
'  Next J  
'Next I   
   
   Print #f,"        </Independent_yields>"

   Dim As Integer Inewline 
   Inewline = 0
   Dim As Single Rout
   If B_Error_On = 1 And B_Error_Analysis = 0 Then    
     If Bcov = 1 Then
       Print #f,"        <ZApre_covariances>"
       Print #f,"--- Covariance matrix of independent yields before prompt-neutron emission ---"
       Print #f," "
       Print #f,"  Key table of Amin and Amax values: (Z,Amin(Z),Amax(Z))"                                     
       For I = Zmin to Zmax
         If VAprelim(I,2) > 0 Then _
          Print #f, I;" ";VAprelim(I,1);" ";VAprelim(I,2)
       Next                                     
       Print #f," "                                      
       Print #f,"  Header for loop structure: (For Z1 =";Zmin;" to";Zmax; _
                                            ") (For A1 = Amin to Amax"; _
                                            ") (For Z2 =";Zmin;" to";Zmax; _
                                            ") (For A2 = Amin to Amax)"
       Print #f," "
       Print #f,"(The data are written according to the loop structure specified above."
       Print #f,"The last loop is the inner-most one. Line breaks are not related to the data structure!"
       Print #f," "
       Inewline = 0
       For I = Zmin To Zmax 
         For J = VAprelim(I,1) To VAprelim(I,2)
           For K = Zmin To Zmax
             For L = VAprelim(K,1) To VAprelim(K,2)
               Rout = SZApreCovar(I,J-VAprelim(I,1)+1,K,L-VAprelim(K,1)+1).Rval
               Print #f,Round(Rout,4);" ";
               Inewline = Inewline + 1
               If Inewline >= 500 Then
                 Print #f, " "
                 Inewline = 0
               End If
             Next
           Next    
         Next
       Next
       Print #f," "
       Print #f,"        </ZApre_covariances>"
     End If
     If Bcor = 1 Then
       Print #f,"        <ZApre_correlations>"
       Print #f,"--- Correlation matrix of independent yields before prompt-neutron emission ---"
       Print #f," "
       Print #f,"  Key table of Amin and Amax values: (Z,Amin(Z),Amax(Z))"                                     
       For I = Zmin to Zmax
         If VAprelim(I,2) > 0 Then _
          Print #f, I;" ";VAprelim(I,1);" ";VAprelim(I,2)
       Next                                     
       Print #f," "                                      
       Print #f,"  Header for loop structure: (For Z1 =";Zmin;" to";Zmax; _
                                            ") (For A1 = Amin to Amax"; _
                                            ") (For Z2 =";Zmin;" to";Zmax; _
                                            ") (For A2 = Amin to Amax)"
       Print #f," "
       Print #f,"(The data are written according to the loop structure specified above."
       Print #f,"The last loop is the inner-most one. Line breaks are not related to the data structure!"
       Print #f," "
       Inewline = 0
       For I = Zmin To Zmax 
         For J = VAprelim(I,1) To VAprelim(I,2)
           For K = Zmin To Zmax
             For L = VAprelim(K,1) To VAprelim(K,2)
               Rout = SZApreCorr(I,J-VAprelim(I,1)+1,K,L-VAprelim(K,1)+1).Rval
               Print #f,Round(Rout,4);" ";
               Inewline = Inewline + 1
               If Inewline >= 500 Then
                 Print #f, " "
                 Inewline = 0
               End If
             Next
           Next    
         Next
       Next
       Print #f," "
       Print #f,"        </ZApre_correlations>"
     End If
     If Bcov = 1 Then  
       Print #f,"        <ZApost_covariances>"
       Print #f,"--- Covariance matrix of independent yields after prompt-neutron emission ---"
       Print #f," "
       Print #f,"  Key table of Amin and Amax values: (Z,Amin(Z),Amax(Z))"                                     
       For I = Zmin to Zmax
         If VApostlim(I,2) > 0 Then _
          Print #f, I;" ";VApostlim(I,1);" ";VApostlim(I,2)
       Next                                     
       Print #f," "                                      
       Print #f,"  Header for loop structure: (For Z1 =";Zmin;" to";Zmax; _
                                            ") (For A1 = Amin to Amax"; _
                                            ") (For Z2 =";Zmin;" to";Zmax; _
                                            ") (For A2 = Amin to Amax)"
       Print #f," "
       Print #f,"(The data are written according to the loop structure specified above."
       Print #f,"The last loop is the inner-most one. Line breaks are not related to the data structure!"
       Print #f," "
       Inewline = 0
       For I = Zmin To Zmax 
         For J = VApostlim(I,1) To VApostlim(I,2)
           For K = Zmin To Zmax
             For L = VApostlim(K,1) To VApostlim(K,2)
               Rout = SZApostCovar(I,J-VApostlim(I,1)+1,K,L-VApostlim(K,1)+1).Rval
               Print #f,Round(Rout,4);" ";
               Inewline = Inewline + 1
               If Inewline >= 500 Then
                 Print #f, " "
                 Inewline = 0
               End If
             Next
           Next    
         Next
       Next 
       Print #f," "
       Print #f,"        </ZApost_covariances>"
     End If  
     If Bcor = 1 Then  
       Print #f,"        <ZApost_correlations>"
       Print #f,"--- Correlation matrix of independent yields after prompt-neutron emission ---"
       Print #f," "
       Print #f,"  Key table of Amin and Amax values: (Z,Amin(Z),Amax(Z))"                                     
       For I = Zmin to Zmax
         If VApostlim(I,2) > 0 Then _
          Print #f, I;" ";VApostlim(I,1);" ";VApostlim(I,2)
       Next                                     
       Print #f," "                                      
       Print #f,"  Header for loop structure: (For Z1 =";Zmin;" to";Zmax; _
                                            ") (For A1 = Amin to Amax"; _
                                            ") (For Z2 =";Zmin;" to";Zmax; _
                                            ") (For A2 = Amin to Amax)"
       Print #f," "
       Print #f,"(The data are written according to the loop structure specified above."
       Print #f,"The last loop is the inner-most one. Line breaks are not related to the data structure!"
       Print #f," "
       Inewline = 0
       For I = Zmin To Zmax 
         For J = VApostlim(I,1) To VApostlim(I,2)
           For K = Zmin To Zmax
             For L = VApostlim(K,1) To VApostlim(K,2)
               Rout = SZApostCorr(I,J-VApostlim(I,1)+1,K,L-VApostlim(K,1)+1).Rval
               Print #f,Round(Rout,4);" ";
               Inewline = Inewline + 1
               If Inewline >= 500 Then
                 Print #f, " "
                 Inewline = 0
               End If
             Next
           Next    
         Next
       Next 
       Print #f," "
       Print #f,"        </ZApost_correlations>"
     End If
     
     If B_Double_Covar = 1 And I_Double_Covar = 2 Then
       If Bcov = 1 Then
         Print #f,"        <ZApre_covariances_2_systems>"
         Print #f,"--- Covariance matrix of the nuclide pre-neutron yields for the two fissioning systems ---"
         Print #f," "     
         Print #f,"  Key table of Amin and Amax values: (Z,Amin(Z),Amax(Z))"                                     
         For I = Z_Double_min to Z_Double_max
           If VApre_Double_lim(I,2) > 0 Then _
            Print #f, I;" ";VApre_Double_lim(I,1);" ";VApre_Double_lim(I,2)
         Next                                     
         Print #f," "                                      
         Print #f,"  Header for loop structure: (For Z1 =";Z_Double_min;" to";Z_Double_max; _
                                              ") (For A1 = Amin to Amax"; _
                                              ") (For Z2 =";Z_Double_min;" to";Z_Double_max; _
                                              ") (For A2 = Amin to Amax)"
         Print #f," "
         Print #f,"(The data are written according to the loop structure specified above."
         Print #f,"The last loop is the inner-most one. Line breaks are not related to the data structure!"
         Print #f," "
         Inewline = 0
         For I = Z_Double_min To Z_Double_max 
           For J = VApre_Double_lim(I,1) To VApre_Double_lim(I,2)
             For K = Z_Double_min To Z_Double_max
               For L = VApre_Double_lim(K,1) To VApre_Double_lim(K,2)
                 Rout = SZApre_Double_Covar(I,J-VApre_Double_lim(I,1)+1,K,L-VApre_Double_lim(K,1)+1).Rval
                 Print #f,Round(Rout,4);" ";
                 Inewline = Inewline + 1
                 If Inewline >= 500 Then
                   Print #f, " "
                   Inewline = 0
                 End If
               Next
             Next    
           Next
         Next 
         Print #f," "
         Print #f,"        </ZApre_covariances_2_systems>"
       End If
       If Bcor = 1 Then  
         Print #f,"        <ZApre_correlations_2_systems>"
         Print #f,"--- Correlation matrix of the nuclide pre-neutron yields for the two fissioning systems ---"
         Print #f," "     
         Print #f,"  Key table of Amin and Amax values: (Z,Amin(Z),Amax(Z))"                                     
         For I = Z_Double_min to Z_Double_max
           If VApre_Double_lim(I,2) > 0 Then _
            Print #f, I;" ";VApre_Double_lim(I,1);" ";VApre_Double_lim(I,2)
         Next                                     
         Print #f," "                                      
         Print #f,"  Header for loop structure: (For Z1 =";Z_Double_min;" to";Z_Double_max; _
                                              ") (For A1 = Amin to Amax"; _
                                              ") (For Z2 =";Z_Double_min;" to";Z_Double_max; _
                                              ") (For A2 = Amin to Amax)"
         Print #f," "
         Print #f,"(The data are written according to the loop structure specified above."
         Print #f,"The last loop is the inner-most one. Line breaks are not related to the data structure!"
         Print #f," "
         Inewline = 0
         For I = Z_Double_min To Z_Double_max 
           For J = VApre_Double_lim(I,1) To VApre_Double_lim(I,2)
             For K = Z_Double_min To Z_Double_max
               For L = VApre_Double_lim(K,1) To VApre_Double_lim(K,2)
                 Rout = SZApre_Double_Corr(I,J-VApre_Double_lim(I,1)+1,K,L-VApre_Double_lim(K,1)+1).Rval
                 Print #f,Round(Rout,4);" ";
                 Inewline = Inewline + 1
                 If Inewline >= 500 Then
                   Print #f, " "
                   Inewline = 0
                 End If
               Next
             Next    
           Next
         Next 
         Print #f," "
         Print #f,"        </ZApre_correlations_2_systems>"
       End If
       If Bcov = 1 Then   
         Print #f,"        <ZApost_covariances_2_systems>"
         Print #f,"--- Covariance matrix of the nuclide post-neutron yields for the two fissioning systems ---"
         Print #f," "     
         Print #f,"  Key table of Amin and Amax values: (Z,Amin(Z),Amax(Z))"                                     
         For I = Z_Double_min to Z_Double_max
           If VApost_Double_lim(I,2) > 0 Then _
            Print #f, I;" ";VApost_Double_lim(I,1);" ";VApost_Double_lim(I,2)
         Next                                     
         Print #f," "                                      
         Print #f,"  Header for loop structure: (For Z1 =";Z_Double_min;" to";Z_Double_max; _
                                              ") (For A1 = Amin to Amax"; _
                                              ") (For Z2 =";Z_Double_min;" to";Z_Double_max; _
                                              ") (For A2 = Amin to Amax)"
         Print #f," "
         Print #f,"(The data are written according to the loop structure specified above."
         Print #f,"The last loop is the inner-most one. Line breaks are not related to the data structure!"
         Print #f," "
         Inewline = 0
         For I = Z_Double_min To Z_Double_max 
           For J = VApost_Double_lim(I,1) To VApost_Double_lim(I,2)
             For K = Z_Double_min To Z_Double_max
               For L = VApost_Double_lim(K,1) To VApost_Double_lim(K,2)
                 Rout = SZApost_Double_Covar(I,J-VApost_Double_lim(I,1)+1,K,L-VApost_Double_lim(K,1)+1).Rval
                 Print #f,Round(Rout,4);" ";
                 Inewline = Inewline + 1
                 If Inewline >= 500 Then
                   Print #f, " "
                   Inewline = 0
                 End If
               Next
             Next    
           Next
         Next 
         Print #f," "
         Print #f,"        </ZApost_covariances_2_systems>"
       End If
       If Bcor = 1 Then  
         Print #f,"        <ZApost_correlations_2_systems>"
         Print #f,"--- Correlation matrix of the nuclide post-neutron yields for the two fissioning systems ---"
         Print #f," "     
         Print #f,"  Key table of Amin and Amax values: (Z,Amin(Z),Amax(Z))"                                     
         For I = Z_Double_min to Z_Double_max
           If VApost_Double_lim(I,2) > 0 Then _
            Print #f, I;" ";VApost_Double_lim(I,1);" ";VApost_Double_lim(I,2)
         Next                                     
         Print #f," "                                      
         Print #f,"  Header for loop structure: (For Z1 =";Z_Double_min;" to";Z_Double_max; _
                                              ") (For A1 = Amin to Amax"; _
                                              ") (For Z2 =";Z_Double_min;" to";Z_Double_max; _
                                              ") (For A2 = Amin to Amax)"
         Print #f," "
         Print #f,"(The data are written according to the loop structure specified above."
         Print #f,"The last loop is the inner-most one. Line breaks are not related to the data structure!"
         Print #f," "
         Inewline = 0
         For I = Z_Double_min To Z_Double_max 
           For J = VApost_Double_lim(I,1) To VApost_Double_lim(I,2)
             For K = Z_Double_min To Z_Double_max
               For L = VApost_Double_lim(K,1) To VApost_Double_lim(K,2)
                 Rout = SZApost_Double_Corr(I,J-VApost_Double_lim(I,1)+1,K,L-VApost_Double_lim(K,1)+1).Rval
                 Print #f,Round(Rout,4);" ";
                 Inewline = Inewline + 1
                 If Inewline >= 500 Then
                   Print #f, " "
                   Inewline = 0
                 End If
               Next
             Next    
           Next
         Next 
         Print #f," "
         Print #f,"        <ZApost_correlations_2_systems>"
       End If     
     End If
   End If
   
   

   Print #f,"        <Z_mean>"
   Print #f,"-----------------------------------------------------------------"
   Print #f,"--- Moments of mass-chain Z-yield distributions (pre-neutron) ---"
   Print #f,"-----------------------------------------------------------------"
   Print #f," "
   Print #f,"  A_pre        Z_mean       Z_mean-Z_UCD    sigma_Z"
   Print #f," "
   For I = 20 To P_A_CN - 20
     If Apre(I) > 0 Then
       Print #f, Using " ####         ###.####     ###.####        ###.####"; _
        I; ZPOLARPRE(I)+I*P_Z_CN/P_A_CN; ZPOLARPRE(I); sigmaZPRE(I)
     End If
   Next
   

   Print #f," "
   Print #f," "
   Print #f,"------------------------------------------------------------------"
   Print #f,"--- Moments of mass-chain Z-yield distributions (post-neutron) ---"
   Print #f,"------------------------------------------------------------------"
   Print #f," "
   Print #f,"  A_post       Z_mean       Z_mean-Z_UCD    sigma_Z"
   Print #f," "
   For I = 20 To P_A_CN - 20
     If Apost(I) > 0 Then
       Print #f, Using " ####         ###.####     ###.####        ###.####"; _
        I; ZPOLARPOST(I)+I*P_Z_CN/P_A_CN; ZPOLARPOST(I); sigmaZPOST(I)
     End If
   Next
   Print #f,"        </Z_mean>"
   Print #f,"      </FF_AZ>"
   Print #f,"    </FF>"



'   Print #f," "
'   Print #f," "
'   Print #f,"--- Mean deformation of fragments from macroscopic model ---"
'   Print #f," "
'   Print #f,"   Z1          beta1     Edef1/MeV       Z2         beta2     Edef2/MeV"
'   Print #f," "
'   For I = 10 To P_Z_CN - 10
'     Print #f, Using " ####        ###.###      ####.##       ####       ###.###     ####.##"; _
'      I; Beta(0,1,I); Edefo(0,1,I); P_Z_CN-I; Beta(0,2,P_Z_CN-I); Edefo(0,2,P_Z_CN-I) 
'   Next   
   

   Scope
     Dim As Double Rmean,Zaehler,Nenner
   
     Print #f,"    <Gammas>"
     Print #f,"      <Gamma_multiplicity>"
     Print #f,"----------------------------------------------------------"
     Print #f,"--- Mass-dependent gamma multiplicity (from fragments) ---"
     Print #f,"----------------------------------------------------------"
     Print #f," "
     Print #f,"Apost  Nmean"," Multiplicity distribution, unnormalized (1 to 20)"
     For I = 20 To P_A_CN - 20
       If APost(I) > 0 Then
       Zaehler = 0
       Nenner = 0
       For J = 1 To 20
         Zaehler = Zaehler + J * NgammaA(I,J)
         Nenner = Nenner + NgammaA(I,J)
         Rmean = Zaehler / Nenner 
       Next
         Print #f, Using "#### ###.###";I;RMean;
         For J = 1 To 19
           Print #f, Using " #########";NgammaA(I,J);
         Next
         Print #f, Using " #########";NgammaA(I,20)
       End If
     Next  
     Print #f," "
   End Scope

   Scope
     Dim As Double Norm=0,Zaehler,Nenner
     Dim As Integer Imax
     Print #f, " "
     Print #f, " "
     Print #f, "-----------------------------------------------------------------------"
     Print #f, "--- Total gamma-multiplicity distribution (emission from fragments) ---"
     Print #f, "-----------------------------------------------------------------------"
     Print #f, " "
     Print #f, " N             Probability "
     
     For I = 0 To 50
       Norm = Norm + Ngammatot(I)
       If Ngammatot(I) > 0 Then Imax = I
       Zaehler = Zaehler + I * Ngammatot(I)
     Next  
     For I = 0 To Imax
       Print #f, I,Round(Ngammatot(I)/Norm,4)
     Next
     Print #f, " "
     If B_Error_On = 1 And B_Error_Analysis = 0 Then 
       Print #f, "Mean gamma multiplicity: ";Round(Zaehler/Norm,4);" +-";Round(d_Ng(0),4)
     Else     
       Print #f, "Mean gamma multiplicity: ";Round(Zaehler/Norm,4)
     End If
     Print #f,"      </Gamma_multiplicity>"
 
   

   ' Output of gamma spectrum
   Print #f,"      <E_entrance>"
   Print #f, " "
   Print #f, " "
   print #f, "---------------------------------------------------------------------------"
   print #f, "--- Distribution of E_entrance (E* after last neutron) above yrast line ---"
   print #f, "---------------------------------------------------------------------------"
   Print #f, " "
   Print #f, "Binsize = 100 keV."
   print #f, "The energy values mark the lower limit of the corresponding energy bins:"
   print #f, " e.g  channel '0.1 MeV' contains the spectrum between 0.1 and 0.2 MeV."
   Print #f, " "
   Print #f, " E / MeV","   Counts"
   Scope
     Redim Eentrance100keV(Ubound(Eentrance)/100) As Single
     For I = 1 To Ubound(Eentrance)
       Eentrance100keV(Int(I/100)) = Eentrance100keV(Int(I/100)) + Eentrance(I) 
     Next I
     For I = 1 To 200
       If Eentrance100keV(I) > 0 Then
         Print #f, Using "####.#    ############"; I*0.1; Eentrance100keV(I)  
       End If
     Next I
   End Scope  
   Print #f,"      </E_entrance>"
   Print #f,"      <E_gammas>"
   Print #f, " "
   Print #f, " "
   print #f, "---------------------------------------------------------------"
   print #f, "--- Spectrum of prompt individual gammas from the fragments ---"
   print #f, "---------------------------------------------------------------"
   Print #f, " "
   Print #f, "Binsize = 100 keV."
   print #f, "The energy values mark the lower limit of the corresponding energy bins:"
   print #f, " e.g  channel '0.1 MeV' contains the spectrum between 0.1 and 0.2 MeV."
   Print #f, "(The dmp folder provides spectra with 1 keV resolution."
   Print #f, " There is also a spectrum of pre-fission gammas.)"
   Print #f, " "
   Print #f, "E_gamma / MeV","  Counts(total)     Counts(E2 only)"

   Scope
     Redim Egamma100keV(Ubound(Egamma)/100) As Single
     For I = 0 To Ubound(Egamma)
       Egamma100keV(Int(I/100)) = Egamma100keV(Int(I/100)) + Egamma(I) 
     Next I     
     Redim EgammaE2100keV(Ubound(EgammaE2)/100) As Single
     For I = 0 To Ubound(EgammaE2)
       EgammaE2100keV(Int(I/100)) = EgammaE2100keV(Int(I/100)) + EgammaE2(I) 
     Next I     
     Dim As Double Norm,Zaehler,Nenner
     Dim As Integer Imax,I
     Norm = 0
     Zaehler = 0
     Nenner = 0
     For I = 0 To Ubound(Egamma)
       Zaehler = Zaehler + I*0.001*Egamma(I)
       Nenner = Nenner + Egamma(I)
     Next I
     For I = 0 To Ubound(Egamma100keV)
       If Egamma100keV(I) > 0 Then Imax = I
     Next
     For I = 0 To Imax
       If Egamma100keV(I) > 0 Then _
       Print #f,Using "####.#    ############      ############";I*0.1;Egamma100keV(I);EgammaE2100keV(I)
     Next
     Print #f," "
     If B_Error_On = 1 And B_Error_Analysis = 0 Then
       Print #f,"Mean gamma energy: (";Round(Zaehler/Nenner,4);" +-";Round(d_Eg(0),4);" ) MeV" 
     Else
       Print #f,"Mean gamma energy: ";Round(Zaehler/Nenner,4);" MeV"
     End If  
     Print #f,"(Calculated from a spectrum with 1 keV resolution.)"
   End Scope 
   Print #f,"      </E_gammas>"

 
     Print #f,"      <Sum_gamma_energy>"
     Print #f, " "
     Print #f, " "
     Print #f, "----------------------------------------------------------------------"
     Print #f, "--- Distribution of total gamma energy per fission (from fragments) --"
     Print #f, "----------------------------------------------------------------------"
     Print #f, " "
     Print #f, "Binsize = 100 keV; bins with zero content suppressed."
     print #f, "The energy values mark the lower limit of the corresponding energy bins:"
     print #f, " e.g  channel '0.1 MeV' contains the spectrum between 0.1 and 0.2 MeV."
     Print #f, "(The dmp folder provides a spectrum with 1 keV resolution.)"
     Print #f, " "
     Print #f, " Egamma / MeV     Counts "
     
     Redim Egammatot100keV(Ubound(Egammatot)/100) As Single
     For I = 1 To Ubound(Egammatot)
       Egammatot100keV(Int(I/100)) = Egammatot100keV(Int(I/100)) + Egammatot(I) 
     Next I  
     Norm = 0
     Zaehler = 0
     Nenner = 0
     For I = 0 To Ubound(Egammatot)
       Zaehler = Zaehler + Csng(I)*0.001*Egammatot(I)
       Nenner = Nenner + Egammatot(I)
     Next I
     For I = 0 To 1000
       If Egammatot100keV(I) > 0 Then Imax = I
     Next
     For I = 0 To Imax
       If Egammatot100keV(I) > 0 Then _
       Print #f,Using "####.#    ############";I*0.1;Egammatot100keV(I)
     Next I
     Print #f," "
     If B_Error_On = 1 And B_Error_Analysis = 0 Then 
       Print #f,"Mean total gamma energy per fission: (";Round(Zaehler/Nenner,4);" +-";Round(d_Egtot(0),4);" ) MeV"
     Else
       Print #f,"Mean total gamma energy per fission: ";Round(Zaehler/Nenner,4);" MeV"
     End If
     Print #f, "(Calculated from a spectrum with 1 keV resolution.)"
     Print #f,"      </Sum_gamma_energy>"
     Print #f,"    </Gammas>"
   End Scope  
   

   Scope
     Dim As Integer I,J
     Print #f,"    <Neutrons>"
     Print #f,"      <NmultA>"
     Print #f,"----------------------------------------------------------"
     Print #f,"---     Mass-dependent prompt-neutron multiplicity     ---"
     Print #f,"--- as a function of pre-neutron and post-neutron mass ---"
     Print #f,"---     (Only neutrons emitted from the fragments)     ---"
     Print #f,"----------------------------------------------------------"
     Print #f," "
     Print #f," Apre  Nmean"," Multiplicity distribution, unnormalized (0 to 16)"
     For I = 20 To P_A_CN - 20
       If NAPre(I) > 0 Then
         Print #f, Using "#### ###.###";I;NApre(I);
         For J = 0 To 15
           Print #f, Using " #########";N2dpre(I,J);
         Next
         Print #f, Using " #########";N2dpre(I,16)
       End If
     Next  
     Print #f," "
     Print #f," Apost Nmean"," Multiplicity distribution, unnormalized (0 to 16)"
     For I = 20 To P_A_CN - 20
       If NApost(I) > 0 Then
         Print #f, Using "#### ###.###";I;NApost(I);
         For J = 0 To 15
           Print #f, Using " #########";N2Dpost(I,J);
         Next
         Print #f, Using " #########";N2Dpost(I,16)
       End If
     Next  
     Print #f,"      </NmultA>"
   End Scope
 

   ' Calculate moments of neutron-multiplicity distributions
   Dim As Double Nmean, NCNmean, Nscimean, Nfrmean
   Scope
   Dim As Double Nsum, Nsigma2, NCNsum, NCNsigma2, Nscisum, Nscisigma2, Nfrsum, Nfrsigma2
   Dim As Double Nlightsum, Nlightmean, Nlightsigma2
   Dim As Double Nheavysum, Nheavymean, Nheavysigma2
     
   ' CN  
   NCNsum = 0
   For I = 0 To Ubound(NNCN)
      NCNsum = NCNsum + NNCN(I)
   Next
   For I = 0 To Ubound(NNCN)
      NNCN(I) = NNCN(I) / NCNsum
   Next
   NCNmean = 0
   For I = 0 To Ubound(NNCN)
     NCNmean = NCNmean + NNCN(I) * I
   Next
   NCNsigma2 = 0
   For I = 0 To Ubound(NNCN)
     NCNsigma2 = NCNsigma2 + NNCN(I) * (I - NCNmean)^2  
   Next  
     
   ' between saddle and scission
   Nscisum = 0
   For I = 0 To Ubound(NNsci)
      Nscisum = Nscisum + NNsci(I)
   Next
   If Nscisum = 0 Then
     Nscimean = 0
     Nscisigma2 = 0
   Else
     For I = 0 To Ubound(NNsci)
       NNsci(I) = NNsci(I) / Nscisum
     Next
     Nscimean = 0
     For I = 0 To Ubound(NNsci)
       Nscimean = Nscimean + NNsci(I) * I
     Next
     Nscisigma2 = 0
     For I = 0 To Ubound(NNsci)
       Nscisigma2 = Nscisigma2 + NNsci(I) * (I - Nscimean)^2  
     Next
   End If  
     
   ' fragments
   Nfrsum = 0
   For I = 0 To Ubound(NNfr)
      Nfrsum = Nfrsum + NNfr(I)
   Next
   For I = 0 To Ubound(NNfr)
      NNfr(I) = NNfr(I) / Nfrsum
   Next
   Nfrmean = 0
   For I = 0 To Ubound(NNfr)
     Nfrmean = Nfrmean + NNfr(I) * I
   Next
   Nfrsigma2 = 0
   For I = 0 To Ubound(NNfr)
     Nfrsigma2 = Nfrsigma2 + NNfr(I) * (I - Nfrmean)^2  
   Next  
     
   ' total  
   Nsum = 0
   For I = 0 To Ubound(NN)
      Nsum = Nsum + NN(I)
   Next
   For I = 0 To Ubound(NN)
      NN(I) = NN(I) / Nsum
   Next
   Nmean = 0
   For I = 0 To Ubound(NN)
     Nmean = Nmean + NN(I) * I
   Next
   Nsigma2 = 0
   For I = 0 To Ubound(NN)
     Nsigma2 = Nsigma2 + NN(I) * (I - Nmean)^2  
   Next  
   
   ' light fragments
   Nlightsum = 0
   For I = 0 To Ubound(NNlight)
      Nlightsum = Nlightsum + NNlight(I)
   Next
   For I = 0 To Ubound(NNlight)
      NNlight(I) = NNlight(I) / Nlightsum
   Next
   Nlightmean = 0
   For I = 0 To Ubound(NNlight)
     Nlightmean = Nlightmean + NNlight(I) * I
   Next
   Nlightsigma2 = 0
   For I = 0 To Ubound(NNlight)
     Nlightsigma2 = Nlightsigma2 + NNlight(I) * (I - Nlightmean)^2  
   Next     
   
   ' heavy fragments
   Nheavysum = 0
   For I = 0 To Ubound(NNheavy)
      Nheavysum = Nheavysum + NNheavy(I)
   Next
   For I = 0 To Ubound(NNheavy)
      NNheavy(I) = NNheavy(I) / Nheavysum
   Next
   Nheavymean = 0
   For I = 0 To Ubound(NNheavy)
     Nheavymean = Nheavymean + NNheavy(I) * I
   Next
   Nheavysigma2 = 0
   For I = 0 To Ubound(NNheavy)
     Nheavysigma2 = Nheavysigma2 + NNheavy(I) * (I - Nheavymean)^2  
   Next     

   Print #f,"      <Nmult>"
   Print #f," "
   Print #f,"---------------------------------------------------------------------"
   Print #f,"---         Multiplicity distribution of prompt neutrons          ---"
   Print #f,"---------------------------------------------------------------------"
   Print #f," "
   Print #f," N       before fission  saddle-scission from fragments     total"
   Scope
     Dim As Integer NNmax
     For I = Ubound(NN) To 0 Step -1
       If NN(I) > 0 Then
         NNmax = I
         Exit For
       End If
     Next 
     For I = 0 To NNmax
       Print #f, I, Csng(NNCN(I)), Csng(NNsci(I)), Csng(NNfr(I)), Csng(NN(I))
     Next
   End Scope  
   If B_Error_On = 1 And B_Error_Analysis = 0 Then 
     Print #f," "
     Print #f,"Mean value (pre-fission):            ",NCNmean;" +-";Csng(d_NCN(0))
     Print #f,"Width (standard deviation):    ",Csng(sqr(NCNsigma2))
     Print #f," "   
     Print #f,"Mean value (from saddle to scission):",Nscimean;" +-";Csng(d_Nsci(0))
   ' Print #f,"Width (standard deviation):",Csng(sqr(Nscisigma2))
     Print #f," "   
     Print #f,"Mean value (from fragments):         ",Nfrmean;" +-";Csng(d_Nfr(0))
     Print #f,"Width (standard deviation): ",Csng(sqr(Nfrsigma2))
     Print #f," "   
     Print #f,"Mean value (total):                  ",Nmean;" +-";Csng(d_Ntot(0))
     Print #f,"Width (standard deviation):          ",Csng(sqr(Nsigma2))
     Print #f," "   
   Else
     Print #f," "
     Print #f,"Mean value (pre-fission):            ",NCNmean
     Print #f,"Width (standard deviation):    ",Csng(sqr(NCNsigma2))
     Print #f," "   
     Print #f,"Mean value (from saddle to scission):",Nscimean
   ' Print #f,"Width (standard deviation):",Csng(sqr(Nscisigma2))
     Print #f," "   
     Print #f,"Mean value (from fragments):         ",Nfrmean
     Print #f,"Width (standard deviation): ",Csng(sqr(Nfrsigma2))
     Print #f," "   
     Print #f,"Mean value (total):                  ",Nmean
     Print #f,"Width (standard deviation):          ",Csng(sqr(Nsigma2))
     Print #f," "   
   End If

   
   Print #f," "
   Print #f," "
   Print #f,"---------------------------------------------------------------------"
   Print #f,"--- Multiplicity distribution of prompt neutrons (light fragment) ---"
   Print #f,"---------------------------------------------------------------------"
   Print #f," "
   For I = 0 To Ubound(NNlight)
     If NNlight(I) > 0 Then
        Print #f, I, Csng(NNlight(I))
     End If
   Next I
   Print #f," " 
   If B_Error_On = 1 And B_Error_Analysis = 0 Then 
     Print #f,"Mean value: ",Nlightmean;" +-";Csng(d_Nlight(0))
   Else 
     Print #f,"Mean value: ",Nlightmean
   End If  
   Print #f,"Width (standard deviation): ",Csng(Nlightsigma2)  
   
   
   Print #f," "
   Print #f," "
   Print #f,"---------------------------------------------------------------------"
   Print #f,"--- Multiplicity distribution of prompt neutrons (heavy-fragment) ---"
   Print #f,"---------------------------------------------------------------------"
   Print #f," "
   For I = 0 To Ubound(NNheavy)
     If NNheavy(I) > 0 Then
       Print #f, I, Csng(NNheavy(I))
     End If
   Next I
   Print #f," "
   If B_Error_On = 1 And B_Error_Analysis = 0 Then 
     Print #f,"Mean value: ",Nheavymean;" +-";Csng(d_Nheavy(0))
   Else  
     Print #f,"Mean value: ",Nheavymean
   End If  
   Print #f,"Width (standard deviation): ",Csng(Nheavysigma2)  
   End Scope
   
   
   Print #f," "
   Print #f," "
   Print #f,"---------------------------------------------------------------------"
   Print #f,"--- Neutron multiplicity versus neutron direction relative to LF  ---"
   Print #f,"--- (Only neutrons from fragments with En >= 0.4 MeV considered.) ---"
   Print #f,"---------------------------------------------------------------------"
   Print #f," "
   Print #f," Cos(alpha)             Counts"
   For I = -100 To 99
     Print #f, Using " ##.###        ##############"; I/100+0.005;Ndirlight(I)
   Next I

   Scope
   Dim As Integer First_output, Last_output
   Dim As Double Zaehler,Nenner
   Dim As Single nu_mean
   Dim As Single TKE_pre_vec(0 To 250)
   Dim As Single TKE_post_vec(0 To 250)
   Print #f," "
   Print #f," "
   Print #f,"---------------------------------------------------"
   Print #f,"--- Neutron multiplicity versus pre-neutron TKE ---"
   Print #f,"--        Only neutrons from fragments          ---"
   Print #f,"---------------------------------------------------"
   Print #f," "
   Print #f,"  TKE   events  nu_mean   nu distribution (nu = 0 to 20)"
   Print #f," "  
   For I = 0 to 250 
      For J = 0 to 20
         If nuTKEpre(I,J) > 0 Then
           First_output = I
           Exit For, For 
         End If
      Next J
   Next I    
   For I = 250 To 0 Step -1
      For J = 0 to 20
         If nuTKEpre(I,J) > 0 Then
           Last_output = I
           Exit For, For 
         End If
      Next
   Next
   For I = First_output To Last_output Step 1
      Zaehler = 0
      Nenner = 0
      For J = 0 To 20
          Zaehler = Zaehler + nuTKEpre(I,J)*J
          Nenner = Nenner + nuTKEpre(I,J)
          TKE_pre_vec(I) = TKE_pre_vec(I) + nuTKEpre(I,J)
      Next J
      If Zaehler  = 0 Then nu_mean = 0 Else _
      nu_mean = Zaehler/Nenner
      Print #f, Using "#####  #######  ###.##";I;TKE_pre_vec(I);nu_mean;
      For J = 0 To 19 
          Print #f, Using "#########"; nuTKEpre(I,J);  
      Next
      Print #f, Using "#########"; nuTKEpre(I,20)
   Next I
   
   Print #f," "
   Print #f," "
   Print #f,"----------------------------------------------------"
   Print #f,"--- Neutron multiplicity versus post-neutron TKE ---"
   Print #f,"---        Only neutrons from fragments          ---"
   Print #f,"----------------------------------------------------"
   Print #f," "
   Print #f,"  TKE   events  nu_mean   nu distribution (nu = 0 to 20)"
   Print #f," "     
   For I = 0 to 250 
      For J = 0 to 20
         If nuTKEpost(I,J) > 0 Then
           First_output = I
           Exit For, For 
         End If
      Next J
   Next I     
   For I = 250 To 0 Step -1
      For J = 0 to 20
         If nuTKEpost(I,J) > 0 Then
           Last_output = I
           Exit For, For 
         End If
      Next J
   Next I
   For I = First_output To Last_output Step 1
      Zaehler = 0
      Nenner = 0
      For J = 0 To 20
          Zaehler = Zaehler + nuTKEpost(I,J)*J
          Nenner = Nenner + nuTKEpost(I,J)
          TKE_post_vec(I) = TKE_post_vec(I) + nuTKEpost(I,J)
      Next
      If Zaehler  = 0 Then nu_mean = 0 Else _
      nu_mean = Zaehler/Nenner
      Print #f, Using "#####  #######  ###.##";I;TKE_post_vec(I);nu_mean;
      For J = 0 To 19 
          Print #f, Using "#########"; nuTKEpost(I,J);  
      Next
      Print #f, Using "#########"; nuTKEpost(I,20)
   Next I  
   Print #f,"      </Nmult>"
   End Scope 
 



   ' Rebin spectrum  1 keV -> 100 keV and more
   Scope
     Dim As Double Zaehler, Nenner
     Redim EnCN100keV(Ubound(EnCN)/100) As Single
     For I = 0 To Ubound(EnCN)
       EnCN100keV(Int(I/100)) = EnCN100keV(Int(I/100)) + EnCN(I) 
     Next I  
     If Imulti > 0 Then
       For I = 0 To Ubound(EnCN100keV)  ' normalize prefission spectrum to fission spectrum
         EnCN100keV(I) = EnCN100keV(I) * NEVTtot / Imulti
       Next I
     End If  
     Redim Ensci100keV(Ubound(Ensci)/100) As Single
     For I = 0 To Ubound(Enfr)
       Ensci100keV(Int(I/100)) = Ensci100keV(Int(I/100)) + Ensci(I) 
     Next I  
     Redim Enfr100keV(Ubound(Enfr)/100) As Single
     For I = 0 To Ubound(Enfr)
       Enfr100keV(Int(I/100)) = Enfr100keV(Int(I/100)) + Enfr(I) 
     Next I  
     Redim En100keV(Ubound(EnCN100keV)) As Single ' En100keV from sum of EnCN100keV and Enfr100keV
     For I = 0 To Ubound(En100keV)
       En100keV(I) = EnCN100keV(I) + ENsci100keV(i) + Enfr100keV(I)
     Next I 
     Redim EnfrC100keV(Ubound(ENfrC,1),Ubound(ENfrC,2),Ubound(ENfrC,3)/100) As Single
  '  For J = 0 To I_N_Multi_Max
  '    For K = 0 To I_Z_Multi_Max
     For J = 0 To Ubound(ENfrC,1)       ' limited by dimensions of ENfrC and ENfrC100keV
       For K = 0 To Ubound(ENfrC,2)
         For I = 0 To Ubound(EnfrC,3)
           ENfrC100keV(J,K,Int(I/100)) = ENfrC100keV(J,K,Int(I/100)) + ENfrC(J,K,I)
         Next I                          
       Next K
     Next J
     
   ' Normalize spectrum of prompt neutrons and divide by Maxwell distribution
     Dim As Integer I,J,K
     Dim As Double SumN = 0
     Dim As Double SumMaxwell = 0
     Dim As Double Norm
     Dim As Single YMaxwell, Energy
     Dim As Single Tnorm = 1.32
     For I = 1 To UBound(EN100keV)
       Energy = I * 0.1 + 0.05        
       SumN = SumN + EN100keV(I)
       SumMaxwell = SumMaxwell + Sqr(Energy) * Exp(-Energy/Tnorm)
     Next 
     Norm = SumMaxwell / SumN
     
   
     ' Output of overall neutron spectrum
     Print #f,"      <Nspectrum>"
     print #f, "-----------------------------------------------------------------------------------"
     print #f, "---      Spectrum of prompt neutrons in cm system of the fissioning nucleus     ---"
     print #f, "--- pre-fission and from fragments after emssion of DN neutrons and DZ protons. ---"
     print #f, "-----------------------------------------------------------------------------------"
     Print #f, " "
     print #f, "The energy values mark the lower limit of the corresponding energy bins:"
     print #f, "e.g  channel '0.1 MeV' contains the spectrum between 0.1 and 0.2 MeV."
     Print #f, "(The dmp folder provides spectra with 1 keV resolution.)"
     Print #f, " "

     Print #f, "E_neutron","   Counts","   Counts",
     If I_N_Multi_Max <= Ubound(ENfrC,1) And I_Z_Multi_Max <= Ubound(ENfrC,2) Then
       For I = 0 To I_N_Multi_Max
         For J = 0 To I_Z_Multi_Max
           Print #f, "    Counts",
         Next 
       Next  
     End If
     Print #f, "    Counts","    Counts","Yield norm. to Maxwellian"

     Print #f, "   MeV "," "," ",
     If I_N_Multi_Max <= Ubound(ENfrC,1) And I_Z_Multi_Max <= Ubound(ENfrC,2) Then
       For I = 0 To I_N_Multi_Max
         For J = 0 To I_Z_Multi_Max
           Print #f, "  DN / DZ ",
         Next
       Next
     End If
     Print #f, "    DN / DZ ","  ","Yield / (sqrt(En)*exp(-En/1.32MeV))"
     
     Print #f, " "," "," ",
     If I_N_Multi_Max <= Ubound(ENfrC,1) And I_Z_Multi_Max <= Ubound(ENfrC,2) Then
       For I = 0 To I_N_Multi_Max
         For J = 0 To I_Z_Multi_Max
           Print #f,"  ";I;" /";J,
         Next
       Next
     End If
     Print #f,"    all/all"

     Print #f, "       ","pre-fission","saddle-scission ";"from fragments";
     If I_N_Multi_Max <= Ubound(ENfrC,1) And I_Z_Multi_Max <= Ubound(ENfrC,2) Then
       Print #f," ...";
       For I = 0 To I_N_Multi_Max
         For J = 0 To I_Z_Multi_Max
           Print #f," ",
         Next
       Next
     End If  
     Print #f, "    total"

     For I = 0 To UBound(EN100keV)
       Energy = I * 0.1 + 0.05   
          ' "+ 0.05" is the centre of the energy bin. 
          ' This is the value to be compared with the Maxwellian!
          ' The spectrum EN itself is listed in SATAN convention (x value = lower bin limit)
       If EN100keV(I) >0 Then
         YMaxwell = sqr(Energy) * exp(-Energy/Tnorm)
         Print #f, Using "####.#      ##########      ##########"; _
                          I*0.1; ENCN100keV(I); ENsci100keV(I); 
         If I_N_Multi_Max <= Ubound(ENfrC,1) And I_Z_Multi_Max <= Ubound(ENfrC,2) Then
           For J = 0 To I_N_Multi_Max
             For K = 0 To I_Z_Multi_Max
               Print #f, Using "    ##########";ENfrC100keV(J,K,I);                          
             Next
           Next
         End If
         If YMaxwell > 1.E-10 Then 
           Print #f, Using "     ##########    ########## ########.######"; _
                            ENfr100keV(I); EN100keV(I); EN100keV(I) * Norm / YMaxwell
         Else                   
           Print #f, Using "     ##########    ##########      Overflow"; _
                            ENfr100keV(I); EN100keV(I)
         End If                 
       End If
     Next   
     ' Mean neutron energy
     Zaehler = 0
     Nenner = 0
     For I = 0 To UBound(ENfr)  
       Energy = I * 0.001 + 0.0005     ' in MeV  
       Zaehler = Zaehler + ENfr(I) * Energy
       Nenner = Nenner + ENfr(I)
     Next I
     Print #f, " "
     Print #f, " "
     Print #f, "---  Prompt-neutron spectrum (emission from fragments) with variable bin size  ---"
     Print #f, " "
     Print #f, "            E_neutrons            E_neutrons              Counts"
     Print #f, "                eV                     eV "
     Print #f, "        lower channel limit   upper channel limit"
     Print #f, " "
     For I = 1 To Ubound(ENfrvar)
       Print #f, Using "  ############.######    ############.######   ##################"; _
               ENfrvar_lim(I-1); ENfrvar_lim(I); ENfrvar(I)
     Next I
     Print #f, " "
     Print #f, " "
     If B_Error_On = 1 And B_Error_Analysis = 0 Then 
       Print #f, "Mean neutron energy  = (";Csng(Zaehler/Nenner);" +-";Csng(d_ENfr(0));" ) MeV"
     Else
       Print #f, "Mean neutron energy  = ";Csng(Zaehler/Nenner);" MeV"
     End If
     Print #f, "(Includes only neutrons emitted from fragments," 
     Print #f, " calculated from the neutron spectrum with 1 keV resolution)"
     Print #f,"      </Nspectrum>"
     Print #f,"    </Neutrons>"
   End Scope


   
/' special-purpuse file
Dim As Integer IFIAEA
Dim As Single Energy
If B_Error_Analysis = 0 Then
  IFIAEA = Freefile
  Open "out\Work.dat" For Append As #IFIAEA
  Print #IFIAEA, P_Z_CN, P_A_CN-1,Nmean 
  For I = 1 To 200
    Energy = I * 0.1  
    Print #IFIAEA, Round(Energy,4), EN(I)  
  Next
  Print #IFIAEA," "
  Close #IFIAEA
End If
'/   
   

   Dim As Single JNJ,NJ,Jmean 
   Dim As Single Jadd
   
   Print #f,"    <FF_spin>"
   Print #f,"      <A_Z_spin>"
   Print #f,"----------------------------------------------"
   Print #f,"--- Fragment angular-momentum distribution ---"
   Print #f,"---   before emission of prompt neutrons   ---"
   Print #f,"----------------------------------------------"
   Print #f," "
   Print #f," A  Z   Jmean  J distribution, unnormalized (0 to 25 (1/2 to 51/2) for even (odd) A)"
   ' JFRAGpre stores the angular momentum in steps of 1 hbar steps  
   For I = 20 To P_A_CN - 20
     For J = 10 To P_Z_CN - 10
          If I Mod 2 = 0 Then
             Jadd = 0
          Else
             Jadd = 1/2
          End If
          If ZISOPRE(I,J) > 0 Then
             NJ = 0
             JNJ = 0
             For K = 0 To 50
                NJ = NJ + JFRAGpre(I-J,J,K)
                JNJ = JNJ + (K+Jadd) * JFRAGpre(I-J,J,K)
             Next
             If NJ > 4 Then
                Jmean = JNJ / NJ
                Print #f, Using "### ## ##.## ########";I;J;Jmean;JFRAGpre(I-J,J,0);
                For K = 1 To 24
                   Print #f, Using "########";JFRAGpre(I-J,J,K);
                Next
                Print #f, Using "########";JFRAGpre(I-J,J,25)
             End If 
          End If
     Next J
   Next I


   Print #f," "
   Print #f," "
   Print #f,"-----------------------------------------------------"
   Print #f,"---    Fragment angular-momentum distribution     ---"
   Print #f,"--- after the emission of the last prompt neutron ---"
   Print #f,"-----------------------------------------------------"
   Print #f," "
   Print #f," A  Z   Jmean  J distribution, unnormalized (0 to 25 (1/2 to 51/2) for even (odd) A)"
   ' JFRAGpost stores the angular momentum in steps of 1 hbar  
   For I = 20 To P_A_CN - 20
     For J = 10 To P_Z_CN - 10
          If I Mod 2 = 0 Then
             Jadd = 0
          Else
             Jadd = 1/2
          End If
          If ZISOPOST(I,J) > 0 Then
             NJ = 0
             JNJ = 0
             For K = 0 To 50
                NJ = NJ + JFRAGpost(I-J,J,K)
                JNJ = JNJ + (K+Jadd) * JFRAGpost(I-J,J,K)
             Next
             If NJ > 4 Then
               Jmean = JNJ / NJ
               Print #f, Using "### ## ##.## ########";I;J;Jmean;JFRAGpost(I-J,J,0);
               For K = 1 To 24
                  Print #f, Using "#########";JFRAGpost(I-J,J,K);
               Next
               Print #f, Using "#########";JFRAGpost(I-J,J,25)
             End If 
          End If
     Next J
   Next I
   Print #f, "      </A_Z_spin>"

 
   Dim As Integer Niso,Iiso,MATnmbr,Itest
   Dim As Single RJ
   Dim As Single Rnorm
   
   Ilast = 0
   Print #f,"      <Isomeric_yields>"
   Print #f,"--------------------------------------------"
   Print #f,"--- Relative independent isomeric yields ---"
   Print #f,"--------------------------------------------"
   Print #f," "
   Print #f,"J and E* are the spin and the excitation energy of the state."
   Print #f,"Yield is the relative production yield of that state."
   Print #f,"The number of events allows to estimate the statistical uncertainty."
   Print #f,"The 'upper limit' is an internal parameter to calculate the yield."
   Print #f," "
   Print #f," A"," Z"," J"," E*","Yield","Events","Upper limit"
   For I = 20 To P_A_CN - 20
     For J = 10 To P_Z_CN - 10
      If I - J >= 10 And I - J <= I_N_CN - 10 Then
       Itest = 0
    '   If I = 120 And J = 47 Then Itest = 1
       If ZISOPOST(I,J) > 0 Then
     	 MATnmbr = I_MAT_ENDF(J,I) ' MAT number of nucleus in ground state 
     	 Niso = N_ISO_MAT(MATnmbr) ' Number of isomeric states
     	 Iiso = ISO_for_MAT(MATnmbr)
     	 If Niso > 0 Then 
'    	   If Itest = 1 Then Print
           Redim As Single R_yield_iso(Niso + 1)  ' Number of states
           For K = 1 To Niso + 1
             If Isotab(Iiso).R_lim(K) > 50 Then
               Isotab(Iiso).R_lim(K) = 50
             End If
           Next
           For RJ = 0 To Isotab(Iiso).R_lim(1) Step 1
             R_yield_iso(1) = R_yield_iso(1) + JFRAGpost(I-J,J,RJ)
      '      If Itest = 1 Then Print IR,JFRAGpost(I-J,J,RJ),R_yield_iso(1)
           Next
     	   For K = 2 To Niso + 1
     	     For RJ = Isotab(Iiso).R_lim(K-1) _
     	          To Isotab(Iiso).R_lim(K) Step 1
     	       R_yield_iso(K) = R_yield_iso(K) + JFRAGpost(I-J,J,RJ)
     '         If Itest = 1 Then Print IR,JFRAGpost(I-J,J,RJ),R_yield_iso(K)
     	     Next
     	   Next
     	   Rnorm = 0
     	   For K = 1 to Niso + 1
     	     Rnorm = Rnorm + R_yield_iso(K)
     	   Next
     	   For K = 1 To Niso + 1
       	     Isotab(Iiso).R_Prob(K) = R_yield_iso(K) / Rnorm
     	   Next
     	   For K = 1 to Niso + 1
             If I+J*100 <> Ilast Then Print #f," "
     	     Print #f,I,J, _
     	       Isotab(Iiso).R_SPI(K), _
     	       Isotab(Iiso).R_EXC(K), _
     	       Cast(Single, Isotab(Iiso).R_Prob(K) * 100.E0);" %", _
     	       R_yield_iso(K), _
     	       Isotab(Iiso).R_lim(K)
             Ilast = I+J*100
     	   Next
     	  End If
     	 End If   
       End If
     Next
   Next  
   Print #f,"      </Isomeric_yields>"
   Print #f,"    </FF_spin>"

  ' Output of Q-value spectrum
   Scope 
   Dim As Double Zaehler,Nenner
   Print #f,"    <Q_value>"
   Print #f,"------------------------------------------------------------"
   Print #f,"--- Q value spectrum (bins with zero content suppressed) ---"
   Print #f,"------------------------------------------------------------"
   Print #f," "
   Print #f,"   Q/MeV        Counts"
   Zaehler = 0
   Nenner = 0
   For I = Lbound(Qvalues) To Ubound(Qvalues)
      If Qvalues(I) > 0 Then
        Print #f,Using "########   ###########";I;Qvalues(I)
        Zaehler = Zaehler + I*Qvalues(I)
        Nenner = Nenner + Qvalues(I)
      End If 
   Next
   Print #f,"Mean value: Q-bar =";Zaehler/Nenner;" MeV" 
   Print #f,"    </Q_value>"
   End Scope
   


  ' Output of TKE spectrum
   Scope 
   Dim As Double Zaehler_pre,Zaehler_post,Nenner_pre,Nenner_post
   Print #f,"    <TKE>"
   Print #f,"--------------------------------------------------------------------------------"
   Print #f,"--- TKE spectrum (pre- and post-neutron) (bins with zero content suppressed) ---"
   Print #f,"--------------------------------------------------------------------------------"
   Print #f," "
   Print #f,"   TKE/MeV     Counts(pre)   Counts(post) "
   Zaehler_pre = 0
   Nenner_pre = 0
   Zaehler_post = 0
   Nenner_post = 0
   For I = lbound(TKEpre) To ubound(TKEpre)
      If TKEpre(I) > 0 or TKEpost(I) > 0 Then
        Print #f,Using "########   ###########   ###########";I;TKEpre(I);TKEpost(I)
        Zaehler_pre = Zaehler_pre + I*TKEpre(I)
        Nenner_pre = Nenner_pre + TKEpre(I)
        Zaehler_post = Zaehler_post + I*TKEpost(I)
        Nenner_post = Nenner_post + TKEpost(I)
      End If 
   Next
   If B_Error_On = 1 And B_Error_Analysis = 0 Then
     Print #f,"Mean values: TKE_pre =";Zaehler_pre/Nenner_pre; _
                          " MeV +-"; Csng(d_TKEpre(0));" MeV," 
     Print #f,"             TKE_post =";Zaehler_post/Nenner_post; _
                          " MeV +-";Csng(d_TKEpost(0));" MeV" 
   Else 
     Print #f,"Mean values: TKE_pre =";Zaehler_pre/Nenner_pre; _
                         " MeV, TKE_post =";Zaehler_post/Nenner_post;" MeV" 
   End If                      
   Print #f,"    </TKE>"                    
   End Scope


  ' Output of TXE spectrum
   Scope 
   Dim As Double Zaehler,Nenner
   Print #f,"    <TXE>"
   Print #f,"---------------------------------------------------------"
   Print #f,"--- TXE spectrum (bins with zero content suppressed) ---"
   Print #f,"---------------------------------------------------------"
   Print #f," "
   Print #f,"   TXE/MeV      Counts"
   Zaehler = 0
   Nenner = 0
   For I = lbound(TotXE) To ubound(TotXE)
      If TotXE(I) > 0 Then
        Print #f,Using "########   ###########";I;TotXE(I)
        Zaehler = Zaehler + I*TotXE(I)
        Nenner = Nenner + TotXE(I)
      End If 
   Next
   If B_Error_On = 1 And B_Error_Analysis = 0 Then
     Print #f,"Mean value: TXE =";Zaehler/Nenner; _
                         " MeV +-"; Csng(d_TKEpre(0));" MeV," 
   Else   
     Print #f,"Mean value: TXE =";Zaehler/Nenner;" MeV"
   End If   
   Print #f,"    </TXE>"
   End Scope
  
  ' Determine limits of AEkin
   Scope
   Dim As Integer Imin,Imax,Jmin,Jmax
   For I = 20 To 200
     For J = 1 To 300
        If AEkinpre(I,J) > 0 or AEkinpost(I,J) > 0 Then
          Imin = I
          Exit For, For
        End If
     Next
   Next 
   For I = 200 To 20 Step -1
     For J = 1 To 300
        If AEkinpre(I,J) > 0 or AEkinpost(I,J) > 0 Then
          Imax = I
          Exit For, For
        End If
     Next
   Next
   For J = 1 To 300
     For I = 20 To 200
        If AEkinpre(I,J) > 0 or AEkinpost(I,J) > 0 Then
          Jmin = J
          Exit For, For
        End If     
     Next
   Next 
   For J = 300 To 1 Step -1
     For I = 20 To 200 
        If AEkinpre(I,J) > 0 or AEkinpost(I,J) > 0 Then
          Jmax = J
          Exit For, For
        End If         
     Next
   Next    
    
  ' Output of AEkinpre
   Print #f,"    <A_Ekin>"
   Print #f,"------------------------------------"
   Print #f,"--- A-Ekin spectrum (pre-neutron)---"
   Print #f,"------------------------------------"
   Print #f," "
   Print #f,"2-dim. array: (A = ";Imin;" To ";Imax; _
                  " Step 1) (E = ";Jmin;" To ";Jmax;" Step 1)"  
   Print #f," "
   Print #f,"(The data are written according to the loop structure specified above."
   Print #f,"The last loop is the inner-most one. Line breaks are not related to the data structure!"
   Print #f," "
   K = 0
   For I = Imin To Imax
     For J = Jmin To Jmax
       K = K + 1
     '  IF AEkinpre(I,J) > 0 Then
         Print #f, AEkinpre(I,J);
         If K = 50 Then
              K = 0
              Print #f, " "
         ENd If
     '  End If 
     Next
   Next   

    
  ' Output of AEkinpost 
  ' (AEkinpost is not exact, neutron evaporation is only considered on average.)
   Print #f," "
   Print #f," "
   Print #f,"-------------------------------------"
   Print #f,"--- A-Ekin spectrum (post-neutron)---"
   Print #f,"-------------------------------------"
   Print #f," "
   Print #f,"2-dim. array: (A = ";Imin;" To ";Imax; _
                  " Step 1) (E = ";Jmin;" To ";Jmax;" Step 1)"  
   Print #f," "
   Print #f,"(The data are written according to the loop structure specified above."
   Print #f,"The last loop is the inner-most one. Line breaks are not related to the data structure!"
   Print #f," "
   K = 0
   For I = Imin To Imax
     For J = Jmin To Jmax
       K = K + 1
     '  IF AEkinpost(I,J) > 0 Then
         Print #f, AEkinpost(I,J);
         If K = 50 Then
              K = 0
              Print #f, " "
         ENd If
     '  End If 
     Next
   Next   
   Print #f," "
   Print #f,"    </A_Ekin>"


  ' Determine limits of ATKE
   For I = 20 To 300
     For J = 1 To 300
        If ATKEpre(I,J) > 0 or ATKEpost(I,J) > 0 Then
          Imin = I
          Exit For, For
        End If
     Next
   Next 
   For I = 300 To 20 Step -1
     For J = 1 To 300
        If ATKEpre(I,J) > 0 or ATKEpost(I,J) > 0 Then
          Imax = I
          Exit For, For
        End If
     Next
   Next
   For J = 1 To 300
     For I = 20 To 300
        If ATKEpre(I,J) > 0 or ATKEpost(I,J) > 0 Then
          Jmin = J
          Exit For, For
        End If     
     Next
   Next 
   For J = 300 To 1 Step -1
     For I = 20 To 300 
        If ATKEpre(I,J) > 0 or ATKEpost(I,J) > 0 Then
          Jmax = J
          Exit For, For
        End If         
     Next
   Next    
   
  ' Output of ATKEpre
   Print #f,"    <A_TKE>"
   Print #f,"------------------------------------"
   Print #f,"--- A-TKE spectrum (pre-neutron)---"
   Print #f,"------------------------------------"
   Print #f," "
   Print #f,"2-dim. array: (A = ";Imin;" To ";Imax; _
                  " Step 1) (E = ";Jmin;" To ";Jmax;" Step 1)"  
   Print #f," "
   Print #f,"(The data are written according to the loop structure specified above."
   Print #f,"The last loop is the inner-most one. Line breaks are not related to the data structure!"
   Print #f," "
   K = 0
   For I = Imin To Imax
     For J = Jmin To Jmax
       K = K + 1
     '  IF ATKEpre(I,J) > 0 Then
         Print #f, ATKEpre(I,J);
         If K = 50 Then
              K = 0
              Print #f, " "
         ENd If
     '  End If 
     Next J
   Next I   

    
  ' Output of ATKEpost 
  ' (ATKEpost is not exact, neutron evaporation is only considered on average.)
   Print #f," "
   Print #f," "
   Print #f,"-------------------------------------"
   Print #f,"--- A-TKE spectrum (post-neutron)---"
   Print #f,"-------------------------------------"
   Print #f," "
   Print #f,"2-dim. array: (A = ";Imin;" To ";Imax; _
                  " Step 1) (E = ";Jmin;" To ";Jmax;" Step 1)"  
   Print #f," "
   Print #f,"(The data are written according to the loop structure specified above."
   Print #f,"The last loop is the inner-most one. Line breaks are not related to the data structure!"
   Print #f," "
   K = 0
   For I = Imin To Imax
     For J = Jmin To Jmax
       K = K + 1
     '  IF ATKEpost(I,J) > 0 Then
         Print #f, ATKEpost(I,J);
         If K = 50 Then
              K = 0
              Print #f, " "
         ENd If
     '  End If 
     Next
   Next   
   Print #f," "   
   
   Print #f,"    </A_TKE>"
   
   Print #f,"  </Prompt_results>"
   End Scope



   Dim As Integer ICHECK = 1
   If ICHECK = 1 Then
 /' Control parameters '/
    Print #f, "  <Control>"
    Print #f, "Control parameters:"
    Print #f, "R_E_exc_Eb = ",R_E_exc_Eb      /' Energy above outer barrier '/
    Print #f, "Delta_S0 = ",Delta_S0          /' Shell at symmetry '/    
    Print #f, "P_Z_Mean_S1 = ",P_Z_Mean_S1    /' Mean Z of Mode 1 '/
    Print #f, "P_Z_Mean_S2 = ",P_Z_Mean_S2    /' Mean Z of Mode 2 '/
    Print #f, "P_Z_Mean_S3 = ",P_Z_Mean_S3    /' Mean Z of Mode 3 '/
    Print #f, "P_Z_Mean_S4 = ",P_Z_Mean_S4    /' Mean Z of Mode 4 '/
    Print #f, "SigZ_Mode_0 = ",SigZ_Mode_0
    Print #f, "SigZ_Mode_1 = ",SigZ_Mode_1
    Print #f, "SigZ_Mode_2 = ",SigZ_Mode_2
    Print #f, "SigZ_Mode_3 = ",SigZ_Mode_3
    Print #f, "SigZ_Mode_4 = ",SigZ_Mode_4
    Print #f, "NC_Mode_0 = ", NC_Mode_0       /' Mean N of symm. Mode '/
    Print #f, "NC_Mode_1 = ", NC_Mode_1       /' Mean N of Mode 1 '/
    Print #f, "NC_MODE_2 = ", NC_Mode_2       /' Mean N of Mode 2 '/
    Print #f, "NC_Mode_3 = ", NC_Mode_3       /' Mean N of Mode 3 '/
    Print #f, "NC_Mode_4 = ", NC_Mode_4       /' Mean N of Mode 4 '/
    Print #f, "AC_Mode_0 = ", AC_Mode_0
    Print #f, "AC_Mode_1 = ", AC_Mode_1
    Print #f, "AC_Mode_2 = ", AC_Mode_2
    Print #f, "AC_Mode_3 = ", AC_Mode_3
    Print #f, "AC_Mode_4 = ", AC_Mode_4
    Print #f, "B_S1 = ", B_S1                 /' Barrier S1, relative to SL '/
    Print #f, "B_S2 = ", B_S2                 /' Barrier S2, relative to SL '/
    Print #f, "B_S3 = ", B_S3                 /' Barrier S3, relative to SL '/
    Print #f, "B_S4 = ", B_S4                 /' Barrier S4, relative to SL '/
    Print #f, "B_S11 = ", B_S11               /' Barrier S11, relative to SL '/
    Print #f, "B_S22 = ", B_S22               /' Barrier S22, relative to SL '/
    Print #f, "DES11ZPM = ", DES11ZPM         /' Mod. of eff. barrier due to ZPM in overlap '/ 
    Print #f, "Delta_NZ_Pol = ", Delta_NZ_Pol      /' Polarization for 132Sn '/
    Print #f, "Yield_Mode_0 = ", Yield_Mode_0    /' Relative yield of SL '/
    Print #f, "Yield_Mode_1 = ", Yield_Mode_1    /' Relative yield of S1 '/
    Print #f, "Yield_Mode_2 = ", Yield_Mode_2    /' Relative yield of S2 '/
    Print #f, "Yield_Mode_3 = ", Yield_Mode_3    /' Relative yield of S3 '/
    Print #f, "Yield_Mode_4 = ", Yield_Mode_4    /' Relative yield of S4 '/
    Print #f, "Yield_Mode_11 = ", Yield_Mode_11  /' Relative yield of S11 '/
    Print #f, "Yield_Mode_22 = ", Yield_Mode_22  /' Relative yield of S22 '/
    Print #f, "P_Pol_Curv_S0 = ", P_Pol_Curv_S0  /' Stiffness in N/Z '/
    Print #f, "T_Coll_Mode_0 = ", T_Coll_Mode_0  /' Effective collective temperature '/
    Print #f, "E_Exc_S0 = ", E_Exc_S0            /' Energy over eff. barrier of symmetric channel '/
    Print #f, "E_Exc_S1 = ", E_Exc_S1            /' Energy over eff. barrier of S1 channel '/
    Print #f, "E_Exc_S2 = ", E_Exc_S2            /' Energy over eff. barrier of S2 channel '/
    Print #f, "E_Exc_S3 = ", E_Exc_S3            /' Energy over eff. barrier of S3 channel '/
    Print #f, "E_Exc_S4 = ", E_Exc_S4            /' Energy over eff. barrier of S4 channel '/
    Print #f, "E_exc_S11 = ", E_exc_S11          /' Energy over eff. barrier of S11 channel '/
    Print #f, "E_exc_S22 = ", E_exc_S22          /' Energy over eff. barrier of S22 channel '/
    Print #f, "E_POT_Scission = ", E_Pot_Scission /' Potential energy gain saddle-scission '/
    Print #f, "EINTR_Scission = ", EINTR_SCISSION   /' Intrinsic excitation energy at scission '/
    Print #f, "EEFFS2 = ", EEFFS2                /' Responsible for S1 reduction by pairing '/
    Print #f, "Sigpol_Mode_0 = ", Sigpol_Mode_0     /' Width of isobaric Z distribution '/
    Print #f, "Even-odd effect = ",EvenOdd       /' Enhanced production of even elements '/
    Print #f, "  </Control>"

   End If


/'
Scope
Restore PU240T
    Dim As Integer Afirst, Alast, I, N
    Dim As Single Diff,Logdiff , Chilin, Chilog
    Read Afirst
    Read Alast
    Redim As Single VYA(Afirst to Alast)
    Redim As Single VDYA(Afirst to Alast)
    For I = Afirst to Alast
       Read VYA(I)
    Next
    For I = Afirst to Alast
       Read VDYA(I)
    Next    
    Chilin = 0
    Chilog = 0
    N = 0
Print #f," A    ((Y_ENDF - Y_calc)/DY_ENDF)^2)"    
    For I = Afirst to Alast
      If Apost(I) > 0.01 And VYA(I) > 0.01 Then
        Diff = (VYA(I) - Apost(I)) / VDYA(I)
Print #f, I, Diff^2        
        Chilin = Chilin + Diff^2
        Logdiff = 0.5 * Log(VYA(I)/Apost(I)) / Log( (VYA(I) + VDYA(I)) / (VYA(I) - VDYA(I)) )
        Chilog = Chilog + Logdiff^2
        N = N + 1
      End If
    Next
Print #f, "Chi^2 = ", Chilin
Print #f, "Chi^2 / N = ", Chilin/N 
    Chilin = Chilin / N
    Chilog = Chilog / N
End Scope
 '/

   #ifdef B_ENDF  
     #Include "ENDF.bas" 
   #endif  
   
   Printcomments
   
   Print #f,"</GEF>"
   
   Close #f


   /' *** Filling dmp folder with SATAN analyzer dumps *** '/
   If B_Error_Analysis = 0 Then
     Dim As Integer I,J
     CdmpFolder = "Z"+Trim(Str(P_Z_CN))+"_A"+Trim(Str(P_A_CN))
     If I_E_iso > 0 Then
       If Abs(Emode) = 1 Then CdmpFolder = CdmpFolder + "f" + Trim(Str(I_E_iso))
       If Emode = 2 Then CdmpFolder = Cdmpfolder + "m" + Trim(Str(I_E_iso))
     End If  
     If Emode = 2 Then CdmpFolder = CdmpFolder + "_n" 
     If Emode = 12 Then CdmpFolder = CdmpFolder + "_p"     
     If Abs(Emode) = 1 Then
       If P_E_exc = 0 Then  
         CdmpFolder = Cdmpfolder + "_sf"
       Else
         If Emode = -1 Then
           CdmpFolder = Cdmpfolder + "_fc"
         Else
           CdmpFolder = Cdmpfolder + "_cf"
         End If  
       End If  
     End If  
     If Emode = 3 Then 
       CdmpFolder = CdmpFolder + "_ed"  ' energy distribution
     Else  
       CdmpFolder = CdmpFolder + "_E"+Trim(Str(P_E_exc))+"MeV"
     End If  
     CHDIR("dmp")
     If CHDIR(CdmpFolder) Then
       MKDIR(CdmpFolder) 
       Print "Subfolder \dmp\";CdmpFolder;" created."
     Else
       CHDIR("..")
     End If    
     CHDIR("..")

     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\Apre.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: pre-neutron mass distributions"
     U_DMP_1D(APRE(),"APRE","Pre-neutron mass distribution,"+Str(NEVTtot)+" fission events")
     Redim A_Work(Lbound(AMPRE,2) To Ubound(AMPRE,2)) As Double
     For I = Lbound(AMPRE,1) TO Ubound(AMPRE,1)
       For J = Lbound(AMPRE,2) To UBound(AMPRE,2)
         A_Work(J) = AMPRE(I,J)
       Next
       U_DMP_1D(A_Work(),"AMPRE("+Trim(Str(I))+")", _
             "Pre-neutron mass distribution per mode,"+Str(NEVTtot)+" fission events")
     Next     
     Close #DMPFile
     
     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\Apost.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: post-neutron mass distributions"
     U_DMP_1D(APOST(),"APOST","Post-neutron mass distribution, "+Str(NEVTtot)+" fission events")
  '   If B_Error_On Then U_DMP_1D(Err_APOST(),"Err_APOST","Post-neutron mass distribution, uncertainties")
     Redim A_Work(Lbound(AMPOST,2) To Ubound(AMPOST,2)) As Double
     For I = Lbound(AMPOST,1) TO Ubound(AMPOST,1)
       For J = Lbound(AMPOST,2) To UBound(AMPOST,2)
         A_Work(J) = AMPOST(I,J)
       Next
       U_DMP_1D(A_Work(),"AMPOST("+Trim(Str(I))+")", _
             "Post-neutron mass distribution per mode, "+Str(NEVTtot)+" fission events")
     Next     
     Close #DMPFile

     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\Ekin.dmp" For Append As #DMPfile
     
     Print #DMPFile,"GEF analyzer dump: Energies of fission fragments"
     U_DMP_1D(Ekinpre(),"Ekinpre","Pre-neutron fragment kinetic energy, "+ _
           Str(NEVTtot)+" fission events")
     Redim A_Work(Lbound(EkinpreM,2) To Ubound(EkinpreM,2)) As Double
     For I = Lbound(EkinpreM,1) TO Ubound(EkinpreM,1)
       For J = Lbound(EkinpreM,2) To UBound(EkinpreM,2)
         A_Work(J) = EkinpreM(I,J)
       Next
       U_DMP_1D(A_Work(),"EkinpreM("+Trim(Str(I))+")", _
             "Pre-neutron fragment kinetic energy per mode, "+Str(NEVTtot)+" fission events")
     Next 
     U_DMP_1D(EkinApre(),"EkinApre","Pre-neutron mean Ekin vs Apre, "+_    
           Str(NEVTtot)+" fission events")
     
     U_DMP_1D(Ekinpost(),"Ekinpost","Post-neutron fragment kinetic energy,"+ _
           Str(NEVTtot)+" fission events")
     Redim A_Work(Lbound(EkinpostM,2) To Ubound(EkinpostM,2)) As Double
     For I = Lbound(EkinpostM,1) TO Ubound(EkinpostM,1)
       For J = Lbound(EkinpostM,2) To UBound(EkinpostM,2)
         A_Work(J) = EkinpostM(I,J)
       Next
       U_DMP_1D(A_Work(),"EkinpostM("+Trim(Str(I))+")", _
             "Post-neutron fragment kinetic energy per mode, "+Str(NEVTtot)+" fission events")
     Next     
     U_DMP_1D(EkinApost(),"EkinApost","Post-neutron mean Ekin vs Apost, "+_    
           Str(NEVTtot)+" fission events")

     U_DMP_1D(TKEpre(),"TKEpre","Pre-neutron total-kinetic-energy distribution,"+ _
              Str(NEVTtot)+" fission events")
     Redim A_Work(Lbound(TKEpreM,2) To Ubound(TKEpreM,2)) As Double
     For I = Lbound(TKEpreM,1) TO Ubound(TKEpreM,1)
       For J = Lbound(TKEpreM,2) To UBound(TKEpreM,2)
         A_Work(J) = TKEpreM(I,J)
       Next
       U_DMP_1D(A_Work(),"TKEpreM("+Trim(Str(I))+")", _
             "Pre-neutron TKE distribution per mode, "+Str(NEVTtot)+" fission events")
     Next     
     
     U_DMP_1D(TKEpost(),"TKEpost","Post-neutron fragment total-kinetic-energy distribution,"+ _
           Str(NEVTtot)+" fission events")
     Redim A_Work(Lbound(TKEpostM,2) To Ubound(TKEpostM,2)) As Double
     For I = Lbound(TKEpostM,1) TO Ubound(TKEpostM,1)
       For J = Lbound(TKEpostM,2) To UBound(TKEpostM,2)
         A_Work(J) = TKEpostM(I,J)
       Next
       U_DMP_1D(A_Work(),"TKEpostM("+Trim(Str(I))+")", _
             "Post-neutron TKE distribution per mode, "+Str(NEVTtot)+" fission events")
     Next    

     U_DMP_1D(TKEApre(),"TKEApre","Mean TKE versus Af, pre-neutron, "+ _
           Str(NEVTtot)+" fission events")
     U_DMP_1D(TKEApost(),"TKEApost","Mean TKE versus Af, post-neutron, "+ _
           Str(NEVTtot)+" fission events")
     Close #DMPFile

     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\Zpost.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: element distributions"
     U_DMP_1D(ZPOST(),"ZPOST","Fragment element distribution, "+ _
           Str(NEVTtot)+" fission events,")
 '    If B_Error_On Then U_DMP_1D(Err_ZPOST(),"Err_ZPOST","Fragment element distribution, uncertainties")
     Redim A_Work(Lbound(ZMPOST,2) To Ubound(ZMPOST,2)) As Double
     For I = Lbound(ZMPOST,1) TO Ubound(ZMPOST,1)
       For J = Lbound(ZMPOST,2) To UBound(ZMPOST,2)
         A_Work(J) = ZMPOST(I,J)
       Next
       U_DMP_1D(A_Work(),"ZMPOST("+Trim(Str(I))+")", _
             "Fragment element distribution per mode, "+Str(NEVTtot)+" fission events")
     Next     
     Close #DMPFile
     
     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\Npre.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: pre-neutron neutron-number distributions"
     U_DMP_1D(NPRE(),"NPRE","Fragment neutron distribution before prompt-neutron emission, "+ _
           Str(NEVTtot)+" fission events")
     Redim A_Work(Lbound(NMPRE,2) To Ubound(NMPRE,2)) As Double
     For I = Lbound(NMPRE,1) TO Ubound(NMPRE,1)
       For J = Lbound(NMPRE,2) To UBound(NMPRE,2)
         A_Work(J) = NMPRE(I,J)
       Next
       U_DMP_1D(A_Work(),"NMPRE("+Trim(Str(I))+")", _
             "Fragment neutron distribution before prompt-neutron emission per mode, "+ _
                Str(NEVTtot)+" fission events")
     Next     
     Close #DMPFile
     
     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\Npost.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: post-neutron neutron-number distributions"
     U_DMP_1D(NPOST(),"NPOST","Fragment neutron distribution after prompt-neutron emission, "+ _
         Str(NEVTtot)+" fission events")
     Redim A_Work(Lbound(NMPOST,2) To Ubound(NMPOST,2)) As Double
     For I = Lbound(NMPOST,1) TO Ubound(NMPOST,1)
       For J = Lbound(NMPOST,2) To UBound(NMPOST,2)
         A_Work(J) = NMPOST(I,J)
       Next
       U_DMP_1D(A_Work(),"NMPOST("+Trim(Str(I))+")", _
             "Fragment neutron distribution after prompt-neutron emission per mode, "+ _
          Str(NEVTtot)+" fission events")
     Next     
     Close #DMPFile     
     
     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\ZPolarpre.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: pre-neutron charge polarization"
     U_DMP_1D(ZPolarpre(),"ZPolarpre","<Z|Apre> / Apre - ZCN / ACN, "+ _
           Str(NEVTtot)+" fission events")     
     Close #DMPFile     

     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\ZPolarpost.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: post-neutron charge polarization"
     U_DMP_1D(ZPolarpost(),"ZPolarpost", _
           "<Z|Apost> / Apost - ZCN/ACN, "+Str(NEVTtot)+" fission events")     
     Close #DMPFile     

     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\SigmaZpre.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: pre-neutron width of charge polarization"
     U_DMP_1D(SigmaZpre(),"SigmaZpre", _
           "Width (standard deviation) of Zpolarpre, "+Str(NEVTtot)+" fission events")     
     Close #DMPFile     

     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\SigmaZpost.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: post-neutron width of charge polarization"
     U_DMP_1D(SigmaZpost(),"SigmaZpost", _
           "Width (standard deviation) of Zpolarpost, "+Str(NEVTtot)+" fission events")     
     Close #DMPFile     

     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\NApre.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: neutron multiplicity vs. pre-neutron mass"
     U_DMP_1D(NApre(),"NApre","Prompt-neutron multiplicity vs. pre-neutron mass, "+ _
           Str(NEVTtot)+" fission events")     
     Close #DMPFile     

     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\NApost.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: neutron multiplicity vs. post-neutron mass"
     U_DMP_1D(NApost(),"NApost","Prompt-neutron multiplicity vs. post-neutron mass, "+ _
           Str(NEVTtot)+" fission events")     
     Close #DMPFile     

     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\EN.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: prompt-neutron energy spectra"
   /'If NEVTtot = Imulti Or Imulti = 0 Then 
       U_DMP_1D(EN(),"EN","Total spectrum of prompt neutrons, "+Str(NEVTtot)+" fission events")
     End If'/     
     If Imulti > 0 Then  
       U_DMP_1D(ENCN(),"ENCN","Spectrum of prompt neutrons before fission, "+ _
           Str(Imulti)+" fission events")     
       U_DMP_1D(EPCN(),"EPCN","Spectrum of prompt protons before fission, "+ _
           Str(Imulti)+" fission events") 
     End If          
     U_DMP_1D(ENsci(),"ENsci","Spectrum of prompt neutrons emitted between saddle and scission, "+ _
           Str(NEVTtot)+" fission events")
     U_DMP_1D(ENfr(),"ENfr","Spectrum of prompt neutrons from fragments, "+ _
           Str(NEVTtot)+" fission events")     
     U_DMP_1D(ENlight(),"ENlight","Spectrum of prompt neutrons from light fragments, "+ _
           Str(NEVTtot)+" fission events")     
     U_DMP_1D(ENheavy(),"ENheavy","Spectrum of prompt neutrons from heavy fragments, "+ _
           Str(NEVTtot)+" fission events")     
     U_DMP_1D(ENApre(),"ENApre","Mean energy per fragment mass, pre-neutron, "+ _
           Str(NEVTtot)+" fission events")     
     U_DMP_1D(ENApost(),"ENApost","Mean energy per fragment mass, post-neutron, "+ _
           Str(NEVTtot)+" fission events")
     U_DMP_1D(ENfrfs(),"ENfrfs","Spectrum of prompt neutrons from fragments, "+ _
           Str(NEVTtot)+" fission events")     
     U_DMP_1D(ENAprefs(),"ENAprefs","Mean energy per fragment mass, pre-neutron, "+ _
           Str(NEVTtot)+" fission events")
     U_DMP_1D(ENApostfs(),"ENApostfs","Mean energy per fragment mass, post-neutron, "+ _
           Str(NEVTtot)+" fission events")
     Close #DMPFile     

     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\NP.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: multiplicity of prompt protons"
     U_DMP_1D(NP(),"NP","Multiplicity distribution of prompt protons, "+ _
           Str(NEVTtot)+" fission events")     
     Close #DMPFile     

     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\NN.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: multiplicity of prompt neutrons"
     U_DMP_1D(NN(),"NN","Multiplicity distribution of prompt protons, "+ _
           Str(NEVTtot)+" fission events")     
     Close #DMPFile     

     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\DPlocal.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: proton even-odd effect"
     U_DMP_1D(DPlocal(),"DPlocal","Fission-fragment even-odd effect in Z distribution, "+ _
           Str(NEVTtot)+" fission events")     
     Close #DMPFile     

     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\DNlocal.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: neutron even-odd effect after prompt-neutron emission"
     U_DMP_1D(DNlocal(),"DNlocal","Fission-fragment even-odd effect in Z distribution, "+ _
           Str(NEVTtot)+" fission events")     
     Close #DMPFile     

     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\Egamma.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: energies of prompt gammas"
     If Imulti > 0 Then
       U_DMP_1D(EgammaCN(),"EgammaCN","Pre-fission prompt-gamma spectrum, "+ _
             Str(Imulti)+" fission events")
     End If        
     U_DMP_1D(Egamma(),"Egamma","Prompt-gamma spectrum from fragments, "+ _
           Str(NEVTtot)+" fission events")  
     U_DMP_1D(EgammaL(),"EgammaL","Prompt-gamma spectrum from light fragments, "+ _
           Str(NEVTtot)+" fission events")  
     U_DMP_1D(EgammaH(),"EgammaH","Prompt-gamma spectrum from heavy fragments, "+ _
           Str(NEVTtot)+" fission events")  
     U_DMP_1D(EgammaE2(),"EgammaE2","Non-statistical prompt-gamma spectrum, "+ _
           Str(NEVTtot)+" fission events")     
     U_DMP_1D(Egammatot(),"Egammatot","Spectrum of total gamma energy from fragments per fission, "+ _
           Str(NEVTtot)+" fission events")     
     Close #DMPFile     

     #Ifdef B_EgammaA 
       DMPFile = Freefile
       Open "dmp\"+CdmpFolder+"\EgammaA.dmp" For Append As #DMPFile
       Print #DMPFile,"GEF analyzer dump: energies of prompt gammas with conditions on fragment mass"
       Print #DMPFile,"C: The gamma spectra are divided in different sections with different binsizes."
       Redim A_Work(Lbound(EgammaA2,1) To Ubound(EgammaA2,1)) As Double   ' range in Egamma
       For I = Lbound(EgammaA2,2) TO Ubound(EgammaA2,2)  ' range in A-pre
         If Apre(I) > 0 Then 
           For J = Lbound(EgammaA2,1) To Ubound(EgammaA2,1)  ' range in Egamma
             A_Work(J) = EgammaA2(J,I)
           Next J   
           U_DMP_1D(A_Work(),"EgammaA2("+Trim(Str(I))+")", _
               "Prompt gamma spectrum emitted from fragments with mass A ="+Str(I)+", "+ _
           Str(NEVTtot)+" fission events")
         End If        
       Next I
       Redim A_Work(Lbound(EgammaA10,1) To Ubound(EgammaA10,1)) As Double
       For I = Lbound(EgammaA10,2) TO Ubound(EgammaA10,2)  ' range in A-pre 
         If Apre(I) > 0 Then 
           For J = Lbound(EgammaA10,1) To Ubound(EgammaA10,1)  ' range in Egamma
             A_Work(J) = EgammaA10(J,I)
           Next J   
           U_DMP_1D(A_Work(),"EgammaA10("+Trim(Str(I))+")", _
               "Prompt gamma spectrum emitted from fragments with mass A ="+Str(I)+", "+ _
            Str(NEVTtot)+" fission events")        
         End If
       Next I
       Redim A_Work(Lbound(EgammaA100,1) To Ubound(EgammaA100,1)) As Double
       For I = Lbound(EgammaA100,2) TO Ubound(EgammaA100,2)  ' range in A-pre 
         If Apre(I) > 0 Then 
           For J = Lbound(EgammaA100,1) To Ubound(EgammaA100,1)  ' range in Egamma
             A_Work(J) = EgammaA100(J,I)
           Next J   
           U_DMP_1D(A_Work(),"EgammaA100("+Trim(Str(I))+")", _
               "Prompt gamma spectrum emitted from fragments with mass A ="+Str(I)+", "+ _
            Str(NEVTtot)+" fission events")        
         End If
       Next I
       Redim A_Work(Lbound(EgammaA1000,1) To Ubound(EgammaA1000,1)) As Double
       For I = Lbound(EgammaA1000,2) TO Ubound(EgammaA1000,2)  ' range in A-pre 
         If Apre(I) > 0 Then 
           For J = Lbound(EgammaA1000,1) To Ubound(EgammaA1000,1)  ' range in Egamma
             A_Work(J) = EgammaA1000(J,I)
           Next J   
           U_DMP_1D(A_Work(),"EgammaA1000("+Trim(Str(I))+")", _
               "Prompt gamma spectrum emitted from fragments with mass A ="+Str(I)+", "+ _
            Str(NEVTtot)+" fission events")   
         End If      
       Next I
       Close #DMPFile     
     #EndIf   

     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\Ngammatot.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: gamma multiplicity per fission"
     U_DMP_1D(Ngammatot(),"Ngammatot","Gamma multiplicity per fission, "+ _
           Str(NEVTtot)+" fission events")     
     Close #DMPFile     

     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\Eentrance.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: entrance energy above yrast line"
     U_DMP_1D(Eentrance(),"Eentrance", _
           "Energy of entrance line for gamma emission minus E(yrast line), "+ _
           Str(NEVTtot)+" fission events")     
     Close #DMPFile     

     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\Qvalues.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: distribution of Q values"
     U_DMP_1D(Qvalues(),"Qvalues","Distribution of Q values,"+Str(NEVTtot)+" fission events")     
     U_DMP_1D(QA(),"QA","Mean Q value over Ap$r$e$,"+Str(NEVTtot)+" fission events")
     Close #DMPFile     

     DMPFile = Freefile
     Open "dmp\"+CdmpFolder+"\XE.dmp" For Append As #DMPFile
     Print #DMPFile,"GEF analyzer dump: excitation energies of final fragments"
     U_DMP_1D(TotXE(),"TotXE","Distribution of total excitation energies of final fragments, "+ _
           Str(NEVTtot)+" fission events")     
     U_DMP_1D(EintrA(),"EintrA","Mean intrinsic energy per fragment at scission over Apre, "+ _
           Str(NEVTtot)+" fission events")     
     U_DMP_1D(EdefoA(),"EdefoA","Mean deformation energy per fragment at scission over Apre, "+ _
           Str(NEVTtot)+" fission events")     
     U_DMP_1D(EcollA(),"EcollA","Mean collective energy per fragment at scission over Apre, "+ _      
           Str(NEVTtot)+" fission events")   
     Redim A_Work(Lbound(Epart,3) To Ubound(Epart,3)) As Double      
     For I = Lbound(Epart,1) To Ubound(Epart,1)  ' mode
       For J = Lbound(Epart,2) To Ubound(Epart,2)  ' light, heavy 
         For K = Lbound(Epart,3) To Ubound(Epart,3)  ' pre-neutron mass   
           A_Work(K) = Epart(I,J,K)
         Next K
         U_DMP_1D(A_Work(),"Epart("+Trim(Str(I))+","+Trim(Str(J))+")","Partition of excitation energy at scission, "+_
           "mode = "+Str(I)+", light(1)/heavy(2) fragment = "+Str(J)+" over Apre")
       Next J
     Next I            
     Close #DMPFile     
   End If  
   
   If Bfilein Then
  '   await(I_thread,I_thread_max)   
     Open "ctl\sync.ctl" For Output As fsync
       Print #fsync,0
     Close fsync
   End If   
 
   If Bfilein = 0 And BGUI = 0 And B_Error_Analysis = 0 Then
     Print
  '   Print "Press 'ENTER' to continue or 'Q' to quit!" 
     Print "Press 'ENTER' to continue!" 
     Print "(Focus must be on graphic window.)"
   End If

   #ifdef B_plotting 
     Dim As Integer Icontinue
     If Bplot Then
       P_Ylog = 1
       #Include "Plotting.bas"
       If BGUI Then 
       Else
         If B_Error_Analysis = 0 And I_E_Step = N_E_Steps Then
           Input " ",Icontinue
           P_Ylog = 0
           #Include "Plotting.bas"
         End If
       End If   
   '   Input " ",Icontinue
   '   Screen 0
     End If   
   #endif
      
  /' Loop for error analysis '/
  
   If B_error_On = 1 Then
     I_Error = I_Error + 1
     If I_Error = N_Error_Max Then
       B_Error_Analysis = 0
     End If
     If I_Error <= N_Error_Max Then
       Sleep 2000,1
       #ifdef B_plotting 
         IF BPlot Then
           SCREEN 0
         End If
       #endif  
       GoTo calcstart
     End If
   End If

   If CFileoutlmd <> "" Then
'     Print "List-mode data written to ";CFileoutlmd_full  
     Close #foutlmd
   End If

   If Bfilein = -1 Then
     Sleep 2000,1
     #ifdef B_plotting
       If Bplot Then
         Screen 0
       End If
     #endif
   End If  

   Next I_E_step  ' I_E_step

 /'#ifdef B_ENDF  
     If B_Error_Analysis = 1 Then Goto Skip4_ENDF_EOT
       #include "ENDF_EOT.bas"
     Skip4_ENDF_EOT:
   #endif '/
   Next Iline  ' Iline
   
   Next I_Double_Covar   ' I_Double_Covar
   
   Next Ifilein  'Ifilein

  /' Exit the code '/
 
   Dim As String kin
   
   If Bfilein Then
     kin = "Q"
   Else
     Do
       If BGUI = 0 Then
         kin = Inkey
       Else
          kin = "R"   
       End If
       If Ucase(kin) = "Q" Then Exit Do
       If kin <> "" Then Exit Do
       Sleep 1,1
     Loop
   End If
  
   
   #ifdef __FB_WIN32__
   If BGUI = 0 Then 
     SCREEN 0
   Else
     WaitSynch_GUI   ' Timing with GUI (wait for full second)
     fMutex = FreeFile
     Open "GUI\Mutex.ctl" For Output As #fMutex
     Print #fMutex, "0"    ' Allow input from GUI 
     Close #fMutex
   End If
   #else
     #ifdef B_Plotting
       If BPlot then
         SCREEN 0
       End If  
     #endif
   #endif
   
   If Ucase(kin) <> "Q" Then 
     If B_Error_Analysis = 1 Then
       B_Error_On = 1
     End If
     I_Error = 0  
     Goto StartAgain
   End If  
   
 
  End

/'
/'<'/
  End SUB
/'>'/  
'/
/'<'/



  /' Subroutines '/

/'<FO Include "BEexp.FOR" FO>'/
/'<FO Include "BEldmTF.FOR" FO>'/
/'<FO Include "ShellMO.FOR" FO>'/


/'>'/
     Function Find_IAnl(CAnl As String) As Integer
       Dim As Integer I
       Dim As Integer Ifound = 0
       Dim As String CAnl_short
       If Instr(CAnl,"(") > 0 Then
         CAnl_short = Left(CAnl,Instr(CAnl,"(")-1)
       Else
         CAnl_short = CAnl
       End If
       For I = 1 To N_Anl
         If Trim(Ucase(CAnl_short)) = Trim(Ucase(Anl_Par(I).C_Name)) Then
           Ifound = I
           Exit For
         End If 
       Next
       Find_IAnl = Ifound
     End Function

     Sub U_DMP_1D(Array() As Double,CName As String,Ccmt As String)
       Dim As Integer I_Anl
       Dim As String CLine
       Dim As String Cxloop
       Dim As Integer I,Imax
       Dim As Double Rdatetime
     
       Imax = Min(Lbound(Array)+10,Ubound(Array))
       For I = Ubound(Array) To Lbound(Array) + 1 Step -1
         If Array(I) <> 0 Then
           Imax = I
           Exit For
         End If  
       Next I
       If Imax > 0.8 * Ubound(Array) Then Imax = Ubound(Array)
       
       Rdatetime = Now
       Print #DMPFile,"C: Written on ";
       Print #DMPFile, Format( Rdatetime, "dd.mm.yyyy, hh:mm:ss")
       Print #DMPFile,"C: Calculation performed with GEF";C_GEF_Version 
       I_Anl = Find_IAnl(CName)
       Print #DMPFile,"S: ANALYZER(";Trim(CName);")"
       Print #DMPFile,"S: TITLE(";Trim(Anl_Par(I_Anl).C_Title);")"
       Print #DMPFile,"S: COMMENT(";CdmpFolder;")"
       Print #DMPFile,"S: COMMENT(";Ccmt;")"
       Print #DMPFile,"C: Y_{i}(X_{i}) contains the spectrum between X_{i} and X_{i+1}, when X continuous."
       Print #DMPFile,"X: ";Trim(Anl_Par(I_Anl).C_xaxis)
       Print #DMPFile,"Y: ";Trim(Anl_Par(I_Anl).C_yaxis)
       Cxloop = "(X = "+Str(Lbound(Array)*Anl_Par(I_Anl).R_ALim(1,3))+" TO " _
                +Str(Imax*Anl_Par(I_Anl).R_Alim(1,3))+" BY "_
                +Str(Anl_Par(I_Anl).R_ALim(1,3))+")"
       Print #DMPFile,"A: "+Cxloop+" Y,"+Trim(Anl_Par(I_Anl).C_Linesymbol) 
       CLine = Trim(Str(Array(Lbound(Array)))) + ","
       For I = Lbound(Array) + 1 To Imax
         CLine = CLine + Trim(Str(Csng(Array(I)))) 
         If I < Ubound(Array) Then CLine = CLine + ","  
         If Len(CLine) >=128 Then 
           Print #DMPFile,CLine
           CLine = ""
         End If  
       Next
       If CLine <> "" Then Print #DMPFile,CLine
       Print #DMPFile," "     
     End Sub
     
     Sub U_DMP_1D_S(Array() As Single,CName As String,Ccmt As String)
       Dim As Integer I_Anl
       Dim As String CLine
       Dim As String Cxloop
       Dim As Integer I,Imax
       Dim As Double Rdatetime
     
       Imax = Min(10,Ubound(Array))
       For I = Ubound(Array) To Lbound(Array) + 1 Step -1
         If Array(I) <> 0 Then
           Imax = I
           Exit For
         End If  
       Next I
       
       Rdatetime = Now
       Print #DMPFile,"C: Written on ";
       Print #DMPFile, Format( Rdatetime, "dd.mm.yyyy, hh:mm:ss")
       Print #DMPFile,"C: Calculation performed with GEF";C_GEF_Version 
       I_Anl = Find_IAnl(CName)
       Print #DMPFile,"S: ANALYZER(";Trim(CName);")"
       Print #DMPFile,"S: TITLE(";Trim(Anl_Par(I_Anl).C_Title);")"
       Print #DMPFile,"S: COMMENT(";CdmpFolder;")"
       Print #DMPFile,"S: COMMENT(";Ccmt;")"
       Print #DMPFile,"C: Y_{i}(X_{i}) contains the spectrum between X_{i} and X_{i+1}, when X continuous."
       Print #DMPFile,"X: ";Trim(Anl_Par(I_Anl).C_xaxis)
       Print #DMPFile,"Y: ";Trim(Anl_Par(I_Anl).C_yaxis)
       Cxloop = "(X = "+Str(Lbound(Array)*Anl_Par(I_Anl).R_ALim(1,3))+" TO " _
                +Str(Imax*Anl_Par(I_Anl).R_Alim(1,3))+" BY "+Str(Anl_Par(I_Anl).R_ALim(1,3))+")"
       Print #DMPFile,"A: "+Cxloop+" Y,"+Trim(Anl_Par(I_Anl).C_Linesymbol) 
       CLine = Trim(Str(Array(Lbound(Array)))) + ","
       For I = Lbound(Array) + 1 To Imax
         CLine = CLine + Trim(Str(Array(I))) 
         If I < Ubound(Array) Then CLine = CLine + ","  
         If Len(CLine) >=128 Then 
           Print #DMPFile,CLine
           CLine = ""
         End If  
       Next
       If CLine <> "" Then Print #DMPFile,CLine
       Print #DMPFile," "     
     End Sub

/'<'/
   Function U_Valid(I_Z As Integer,I_A As Integer) As Ubyte
     Dim As Ubyte Ivalid
     Ivalid = 1
 '   If I_A / I_Z < 210.E0/90.E0 
     If I_A / I_Z < 172.E0 / 80.E0 _
       Or I_A / I_Z > 250.E0/90.E0 _
       Then
       Ivalid = 0
     End If
     If I_Z < 76 Or I_Z > 120 Then
       Ivalid = 0  
     End If
     U_Valid = Ivalid  
   End Function     


   Function U_Delta_S0(I_Z As Integer,I_A As Integer) As Single
   ' I_Z and I_A refer to the fissioning nucleus90 22
     Dim As Single Delta
     Delta = 0.3
     If I_Z = 90 And I_A = 228 Then Delta = 0.70  'N
     If I_Z = 90 And I_A = 230 Then Delta = 0.6   'N
     If I_Z = 90 And I_A = 233 Then Delta = 0.3
     
     If I_Z = 91 And I_A = 228 Then Delta = 0.65
     
     If I_Z = 92 And I_A = 233 Then Delta = 0.5   'N
     If I_Z = 92 And I_A = 234 Then Delta = 0.6   'N
     If I_Z = 92 And I_A = 235 Then Delta = 0.3
     If I_Z = 92 And I_A = 236 Then Delta = 0.3   'N
     If I_Z = 92 And I_A = 237 Then Delta = 0.3
     If I_Z = 92 And I_A = 238 Then Delta = 0.3
     If I_Z = 92 And I_A = 239 Then Delta = 0.1
     
     If I_Z = 93 And I_A = 238 Then Delta = -0.1  'N
   
     If I_Z = 94 And I_A = 240 Then Delta = -0.1  'N
     If I_Z = 94 And I_A = 241 Then Delta = -0.5  'N
     If I_Z = 94 And I_A = 242 Then Delta = -0.15 'N
     If I_Z = 94 And I_A = 243 Then Delta = -0.45 'N
     If I_Z = 94 Then Delta = 0.25
   
     If I_Z = 95 And I_A = 242 Then Delta = -0.35 'N
   
     IF I_Z = 95 And I_A = 243 Then Delta = -0.1  'N
   
     If I_Z = 95 And I_A = 244 Then Delta = -0.1
   
     If I_Z = 96 And I_A = 244 Then Delta = 0     'N
     If I_Z = 96 And I_A = 246 Then Delta = -0.2  'N
     U_Delta_S0 = Delta    
   End Function       


  Function Getyield(E_rel As Single,E_ref As Single,T_low As Single,T_high As Single) As Single
         /' Erel: Energy relative to the barrier '/
         /' T_low: Effective temperature below barrier '/
         /' T_high: Effective temperature above barrier '/
         Dim As Single Exp1
         Dim As Single Yield

     Exp1 = E_rel/T_low - E_ref/0.4   ' energy far below barrier
                     ' Subtraction of E_ref/0.4 to prevent numerical problems.
     If Exp1 < -50 Then
       Yield = 0
     Else
       Yield = Exp(E_rel / T_high - E_ref/0.4) * 1.E0 / _
          (1.E0 + exp(-E_rel/ (T_high*T_low/(T_high-T_low) ) ) )
     End If
 '   print  E_rel,T_high,E_ref,Yield
     Getyield = Max(Yield,0.0)

  End Function


    Declare Function F1(Z_S_A As Single) As Single
    Function F1(Z_S_A As Single) As Single
      /' Fit to the lower part of the data '/
      Dim As Single Result
      Result = exp(-9.05E0 + 4.58E0 * Log(Z_S_A/2.3E0))
      F1 = Result
    End Function
    Declare Function F2(Z_S_A As Single) As Single
    Function F2(Z_S_A As Single) As Single
      /' Fit to the upper part of the data '/
      Dim As Single Result
      Result = exp(12.08E0 - 3.27E0 * Log(Z_S_A/2.3E0))
      F2 = Result
    End Function

  Function Masscurv(Z As Single, A As Single, RL As Single, kappa As Single) As Single
     /'  Fit to  Data of Fig. 7 of                                             '/
     /'  "Shell effect in the symmetric-modal fission of pre-actinide nuclei"  '/
     /'  S. I. Mulgin, K.-H. Schmidt, A. Grewe, S. V. Zhdanov                  '/
     /'  Nucl. Phys. A 640 (1998) 375 
     /' (From fit of the width of the mass distributions.) '/                                         '/
    Dim As Single RI, Result1, Result2, Result 
    Dim Z_square_over_A As Single
    Dim ZsqrA As Single
    Dim As Single c_rot = 600.0
    /'<FO REAL*4 F1 FO>'/
    /'<FO REAL*4 F2 FO>'/

    Z_square_over_A = Z^2/A
    RI = (A - 2*Z)/A
    ZsqrA = Z_square_over_A * (1.E0 - kappa * RI^2) / _
       (1.E0 - kappa * ((226.E0 - 2.E0*91.E0)/226.E0)^2) _
        + c_rot * RL^2 / A^(7.0/3.0)  ' Hasse & Myers
 '      + 0.0017 * RL^2

    Result1 = F1(ZsqrA)
    Result2 = F2(ZsqrA)
    Result = Min(Result1,Result2)
    Masscurv = Result
  
  End Function

  Function Masscurv1(Z As Single, A As Single, RL As Single, kappa As Single) As Single
     /'  Fit to  Data of Fig. 7 of                                             '/
     /'  "Shell effect in the symmetric-modal fission of pre-actinide nuclei"  '/
     /'  S. I. Mulgin, K.-H. Schmidt, A. Grewe, S. V. Zhdanov                  '/
     /'  Nucl. Phys. A 640 (1998) 375 
     /' (The left part assumed to be valid for the yields of the fission channels.) '/                                         '/
    Dim As Single RI,Result1, Result2, Result 
'    Dim As Single A,A_central,Z
    Dim Z_square_over_A As Single
    Dim ZsqrA As Single
    Dim As Single c_rot = 600.0
    /'<FO REAL*4 F1 FO>'/
    /'<FO REAL*4 F2 FO>'/

'A_central = -28.8156 + Z * 2.86587  ' Stability line for heavy nuclei        

    Z_square_over_A = Z^2/A
    RI = (A - 2*Z)/A
    ZsqrA = Z_square_over_A * (1.E0 - kappa * RI^2) / _
       (1.E0 - kappa * ((226.E0 - 2.E0*91.E0)/226.E0)^2) _
        + c_rot * RL^2 / A^(7.0/3.0)  ' Hasse & Myers
 '      + 0.0017 * RL^2
 
If ZsqrA < 36.0 Then   ' adjusted to Y(S2) in light nuclei (80<Z<92)
  ZsqrA = ZsqrA + 0.9 * (36.0 - ZsqrA)  
End If 

    Result1 = F1(ZsqrA)
  '  Result2 = F2(ZsqrA)
  '  Result = Min(Result1,Result2)
    Masscurv1 = Result1
  
  End Function


  Function De_Saddle_Scission(Z_square_over_Athird As Single, _
       ESHIFTSASCI As Single) As Single
    /' Energy release between saddle and scission '/
    /' M. Asghar, R. W. Hasse, J. Physique C 6 (1984) 455 '/
    Dim As Single Result
    Result = (31.E0 - 11.E0) / (1550.E0 - 1300.E0) * _
             (Z_square_over_Athird - 1300.E0 + ESHIFTSASCI) + 11.E0
       ' This formula with ESHIFTSASCI = 0 is the parameterisation of the results
       ' of Ashgar and Hasse, JPC 6 (1984) 455, see 
       ' F. Rejmund, A. V. Ignatyuk, A. R. Junghans, K.-H. Schmidt
       ' Nucl. Phys. A 678 (2000) 215     
    Result = max(Result,0.0)
    De_Saddle_Scission = Result
  End Function


  Function TEgidy(A As Single,DU As Single,Fred As Single) As Single
    /' Temperature parameter of the constant-temperature formula for the
       nuclear level density.
       Input parameters: A = Mass number of nucleus
                         DU = Shell effect (corrected for pairing:P=0 for odd-A nuclei)
       From "Correlations between the nuclear level density parameters"
         Dorel Bucurescu, Till von Egidy
         Phys. Rev. C 72 (2005) 067304    and
            "Systematics of nuclear level density parameters"
         Dorel Bucurescu, Till von Egidy
         J. Phys. G: Nucl. Part. Phys. 31 (2005) S1675 and
            "Systematics of nuclear level density parameters"
         Till von Egidy, Dorel Bucurescu
         Phys. Rev. C 72 (2005) 044311 '/
    Dim As Single Temp_smooth,Temp,T_Fac
  ' Temp_smooth = 17.45E0 / (A^0.666667E0)   
  ' Temp = (17.45E0 - 0.51E0 * DU + 0.051 * DU^2) / (A^0.666667E0)
    Temp_smooth = 1.0 / (0.0570 * A^0.6666667)
    Temp = 1.0 / ( (0.0570 + 0.00193*DU) * A^0.6666667)  ' from  PRC 80 (2009) 054310 
    T_Fac = Temp / Temp_smooth
    Temp = Temp * Fred  /' (For influence of deformation) '/
    TEgidy = Temp
  End Function


  Function TRusanov(E As Single, A As Single) As Single
     /' Fermi-gas level density, parameterisation of Rusanov et al. '/
     If E >0 Then 
       TRusanov = sqr(E / (0.094E0 * A) )
     Else
       TRusanov = 0.0
     End If   
  End Function

  Function LyMass(Z As Single,A As Single,beta As Single) As Single

     /' liquid-drop mass, Myers & Swiatecki, Lysekil, 1967  '/
     /' pure liquid drop, without pairing and shell effects '/

     /' On input:    Z     nuclear charge of nucleus        '/
     /'              N     number of neutrons in nucleus    '/
     /'              beta  deformation of nucleus           '/
     /' On output:   binding energy of nucleus              '/
 
     /'<FO Const As Single pi = 3.14159 FO>'/
     Dim As Single N
     Dim As Single alpha
     Dim As Single XCOM,XVS,XE,EL

     N = A - Z
     alpha = sqr(5.E0/(4.E0*pi)) * beta
     XCOM = 1.E0 - 1.7826E0 * ((A - 2.E0*Z)/A)^2
            /' factor for asymmetry dependence of surface and volume term '/
     XVS = - XCOM * (15.4941E0*A _
                   - 17.9439E0*A^(2.E0/3.E0)*(1.E0+0.4E0*Alpha^2))
            /' sum of volume and surface energy '/
     XE = Z^2 * (0.7053E0/A^(1.E0/3.E0)*(1.E0-0.2E0*Alpha^2) _
                  - 1.1529E0/A)
     EL = XVS + XE
  /'   EL = EL + LyPair(Z,A); '/
     LyMass = EL
   END Function


   Function LyPair(Z As Integer,A As Integer) As Single
     /' Calculates pairing energy '/
     /' odd-odd nucleus:   Lypair = 0 '/
     /' even-odd nucleus:  Lypair = -12/sqr(A) '/
     /' even-even nucleus: Lypair = -2*12/sqr(A) '/
    Dim As Single E_PAIR

     E_PAIR = - 12.E0 / sqr(Csng(A)) _
          * ( (Z+1) Mod 2 + (A-Z+1) Mod 2)

     Lypair = E_PAIR
   END Function


   Function TFPair(Z As Integer,A As Integer) As Single
     /' Pairing energy from Thomas-Fermi model of Myers and Swiatecki '/
     /' Shifted that TFPair is zero for odd-odd nuclei '/
     Dim As Integer N
     Dim As Single E_Pair
     N = A - Z
     IF Z Mod 2 = 0 And N Mod 2 = 0 Then /' even-even '/
        E_Pair = - 4.8E0 / Z^0.333333E0 - 4.8E0 / N^0.333333E0 + 6.6E0 / A^0.666666E0
     EndIf
     If Z Mod 2 = 0 And N Mod 2 = 1 Then /' even Z, odd N '/
        E_Pair = - 4.8E0 / Z^0.333333E0 + 6.6E0 / A^0.666666E0
     EndIf
     If Z Mod 2 = 1 And N Mod 2 = 0 Then /' odd Z, even N '/
        E_Pair = - 4.8E0 / N^0.333333E0 + 6.6E0 / A^0.666666E0
     EndIf
     If Z Mod 2 = 1 And N Mod 2 = 1 Then /' odd N, odd N '/
        E_Pair = 0.0
     EndIf
     TFPair = E_Pair
   End Function


   Function Pmass(Z As Single,A As Single,beta As Single) As Single
    /' Liquid-drop model of Pearson, 2001 '/
     Dim As Single N,EA,BE
     Dim As Single avol = -15.65
     Dim As Single asf = 17.63
     Dim As Single r0 = 1.233
     Dim As Single asym = 27.72
     Dim As Single ass = -25.60
     Dim As Single alpha
     /'<FO Const As Single pi = 3.14159 FO>'/     

      N = A - Z
      alpha = sqr(5.E0/(4.E0*pi)) * beta
      EA = avol + asf * A^(-0.333333)*(1.E0+0.4E0*Alpha^2) _
           + 0.6E0 * 1.44E0 * Z^2 / (A^1.333333 * r0 )*(1.E0-0.2E0*Alpha^2) _
           + (asym + ass * A^(-0.333333)) * (N-Z)^2 / A^2
      BE = EA * A
      Pmass = BE
   End Function


   Function FEDEFOP(Z As Single,A As Single,beta As Single) As Single
     /' According to liquid-drop model of Pearson 2001 '/
      Dim As Single asf = 17.63
      Dim As Single r0 = 1.233
      Dim As Single N,Alpha
     /'<FO Const As Single pi = 3.14159 FO>'/      

      N = A - Z
      alpha = sqr(5.E0/(4.E0*pi)) * beta
      FEDEFOP = asf * A^(0.666667)*(0.4E0*Alpha^2) _
              - 0.6E0 * 1.44E0 * Z^2 / (A^0.333333 * r0 )*(0.2E0*Alpha^2)
   End Function

   
   Function FEDEFOLys(Z As Single,A As Single,beta As Single) As Single
       /'<FO REAL*4 LYMASS FO>'/
       FEDEFOLys = Lymass(Z,A,beta) - Lymass(Z,A,0.0)
   End Function


   Function LDMass(Z As Single,A As Single,beta As Single) As Single
     Dim As Single N,BEtab
     /'<FO REAL*4 LYMASS FO>'/
     /'<FO REAL*4 FEDEFOLYS FO>'/
     /'<FO REAL*4 BEldmTF FO>'/
     /'<FO REAL*4 BEexp FO>'/
       N = A - Z
       BEtab = BEldmTF(CInt(N),CInt(Z)) + 2.0 * 12.0 / sqr(Csng(A)) _
                        - 0.00001433*Z^2.39
           ' The values in BEtab are the negative binding energies! 
           ' Pairing in Thomas Fermi masses is zero for Z,N even !        
       If BEtab = 0.0 Then
         BEtab = Lymass(Z,A,0.0) 
         Print "Warning: Binding energy of Z=";Z;", A=";A;" not in mass table,"; _
                        " replaced by LYMASS"
         Print "I_Mode = ";I_Mode               
       End If
       LDMASS = BEtab + FEDEFOLys(Z,A,beta)
   End Function

   Function AME2012(IZ As Integer,IA As Integer) As Single
      ' Masses from the 2003 mass evaluation, complemented by TF masses
      ' and Lysekil masses.
      Dim As Single BEexpval
      Dim As Single Z,A,N
      Dim As Integer INeu
      /'<FO REAL*4 LYPAIR FO>'/
      /'<FO REAL*4 U_SHELL FO>'/
      /'<FO REAL*4 LDMASS FO>'/
      /'<FO REAL*4 BEexp FO>'/
      INeu = IA - IZ
      A = Csng(IA)
      Z = Csng(IZ)
      N = A - Z
      BEexpval = BEexp(INeu,IZ) 
      If BEexpval > -1.E10 Then
        AME2012 = BEexpval
      Else
        AME2012 = Ldmass(Z,A,0.0) + U_SHELL(IZ,IA) + Lypair(IZ,IA)
      End If  
   End Function

   Function U_SHELL(Z As Integer,A As Integer) As Single
      Dim As Integer N
      Dim As Single Res
      /'<FO REAL*4 ShellMO FO>'/
      N = A - Z
      Res = ShellMO(N,Z)  
      If Res > 0.0 Then Res = 0.3 * Res     ' KHS (12. Feb. 2012)
     '      ' The positive shell effects for deformed nuclei seem to be too positive
            ' This gives too many high-energetic prompt neutrons.
     U_SHELL = Res
   End Function

   Function U_SHELL_exp(IZ As Integer, IA As Integer) As Single
      Dim Res As Single
      Dim As Single Z,A
      /'<FO REAL*4 LDMASS FO>'/
      /'<FO REAL*4 LYPAIR FO>'/
      /'<FO REAL*4 AME2012 FO>'/
      Z = Csng(IZ)
      A = Csng(IA)
   '   Res = 2.0 * ( AME2012(IZ,IA) - Lypair(IZ,IA) - LDMass(Z,A,0.0) ) _
   '          - 0.25 * ( AME2012(IZ,IA-1) - Lypair(IZ,IA-1) - LDMass(Z,A-1.0,0.0) ) _
   '          - 0.25 * ( AME2012(IZ,IA+1) - Lypair(IZ,IA+1) - LDMass(Z,A+1.0,0.0) ) _
   '          - 0.25 * ( AME2012(IZ+1,IA+1) - Lypair(IZ+1,IA+1) - LDMass(Z+1.0,A+1.0,0.0) ) _
   '          - 0.25 * ( AME2012(IZ-1,IA-1) - Lypair(IZ-1,IA-1) - LDMass(Z-1.0,A-1.0,0.0) )
      Res = 0.5 * ( AME2012(IZ,IA) - Lypair(IZ,IA) - LDMass(Z,A,0.0) ) _
             + 0.125 * ( AME2012(IZ,IA-1) - Lypair(IZ,IA-1) - LDMass(Z,A-1.0,0.0) ) _
             + 0.125 * ( AME2012(IZ,IA+1) - Lypair(IZ,IA+1) - LDMass(Z,A+1.0,0.0) ) _
             + 0.125 * ( AME2012(IZ+1,IA+1) - Lypair(IZ+1,IA+1) - LDMass(Z+1.0,A+1.0,0.0) ) _
             + 0.125 * ( AME2012(IZ-1,IA-1) - Lypair(IZ-1,IA-1) - LDMass(Z-1.0,A-1.0,0.0) )
      U_SHELL_exp = Res             
   End Function

 Function U_SHELL_EO_exp(IZ As Integer, IA As Integer) As Single
     ' Returns experimental shell and even-odd staggering
      Dim Res As Single
      Dim As Single Z,A
      /'<FO REAL*4 LDMASS FO>'/
      /'<FO REAL*4 LYPAIR FO>'/
      /'<FO REAL*4 AME2012 FO>'/
      Z = Csng(IZ)
      A = Csng(IA)
      Res = AME2012(IZ,IA) - LDMass(Z,A,0.0) 
      U_SHELL_EO_exp = Res             
   End Function



   Function U_MASS(Z As Single,A As Single) As Single
     /' LD + congruence energy + shell (no pairing) '/
     Dim As Single BE
     /'<FO REAL*4 U_SHELL FO>'/
     /'<FO REAL*4 LDMASS FO>'/
     If Z < 0 Or A < 0 Then
       Print "U_Mass: Z, A",Z,A
     End If
     BE = Ldmass(Z,A,0.0)  + U_SHELL(CInt(Z),CInt(A))
  '    BE = AME2012(Cint(Z),Cint(A)) - Lypair(Z,A)
  '    BE = Lymass(Z,A,0.0) + U_Shell(CInt(Z),CInt(A))     
  '    BE = Lymass(Z,A,0.0)  
     U_MASS = BE
   End Function


   Function ECOUL(Z1 As Single,A1 As Single,beta1 As Single,Z2 As Single,A2 As Single, _
                     beta2 As Single,d As Single) _
                     As Single

      /' Coulomb potential between two nuclei                    '/
      /' surfaces are in a distance of d                         '/
      /' in a tip to tip configuration                           '/

      /' approximate formulation                                 '/
      /' On input: Z1      nuclear charge of first nucleus       '/
      /'           A1      mass number of irst nucleus   '/
      /'           beta1   deformation of first nucleus          '/
      /'           Z2      nuclear charge of second nucleus      '/
      /'           A2      mass number of second nucleus  '/
      /'           beta2   deformation of second nucleus         '/
      /'           d       distance of surfaces of the nuclei    '/

       Dim As Single N1,N2,recoul
       Dim As Single dtot
       Dim As Single r0 = 1.16

      N1 = A1 - Z1
      N2 = A2 - Z2
      dtot = r0 *( (Z1+N1)^0.3333333E0 * (1.E0+0.6666667E0*beta1) _
             + (Z2+N2)^0.3333333E0 * (1.E0+0.6666667E0*beta2) ) _
             + d
      REcoul = Z1 * Z2 * 1.44E0 / dtot

      ECOUL = REcoul
   END Function


   Function beta_light(Z As Integer,betaL0 As Single,betaL1 As Single) As Single
      /' Deformation of light fission fragment for S1 and S2 '/
      /' Systematic correlation Z vs. beta for deformed shells '/
      /' Z of fission fragment '/
     Dim As Single beta
     beta = (Z - betaL0) * betaL1/20.E0 
     beta_light = beta
   End Function


   Function beta_heavy(Z As Integer,betaH0 As Single,betaH1 As Single) As Single
      /' Deformation of heavy fission fragment for S2 '/
      /' Systematic correlation Z vs. beta for deformed shells '/
      /' Z of fission fragment '/
     Dim As Single beta
     beta = (Z - betaH0) * betaH1/20.E0 
     beta_heavy = beta
   End Function



   Function Z_equi(ZCN As Integer,A1 As Integer,A2 As Integer, _
           beta1 As Single,beta2 As Single,d As Single,Imode As Integer, _
           POLARadd As Single, POLARfac As Single) _
           As Single
    /' Determines the minimum potential of the scission-point configuration
       represented by two deformed nuclei divided by a tip distance d.
       A1, A2, beta1, beta2, d are fixed, Z1 is searched for and returned on output.  '/

       /' ZCN: Z of fissioning nucleus '/
       /' A1: A of first fission fragment '/
       /' A2: A of second fission fragment '/
       /' beta1: deformation of first fission fragment '/
       /' beta2: deformation of second fission fragment '/
       /' d: tip distance '/

             Dim As Single RZ_equi
             Dim As Single RA1,RA2,RZCN,RACN
             Dim As Single Z1UCD,Z2UCD
             Dim As Single re1,re2,re3,eps1,eps2,DZ_Pol /' help variables '/
             /'<FO REAL*4 ECOUL FO>'/
             /'<FO REAL*4 LYMASS FO>'/

          RA1 = Csng(A1)
          RA2 = Csng(A2)
          RZCN = Csng(ZCN)       
          RACN = RA1 + RA2
          Z1UCD = RA1 / (RA1 + RA2) * RZCN
          Z2UCD = RZCN - Z1UCD
          re1 = LyMass( Z1UCD-1.E0, RA1, beta1 ) + _
                LyMass( Z2UCD+1.E0, RA2, beta2 ) + _
                ECoul( Z1UCD-1.E0, RA1, beta1, _
                       Z2UCD+1.E0, RA2, beta2, d )
          re2 = LyMass( Z1UCD, RA1, beta1) + _
                LyMass( Z2UCD, RA2, beta2) + _
                ECoul( Z1UCD, RA1, beta1, _
                       Z2UCD, RA2, beta2, d )
          re3 = LyMass( Z1UCD+1.E0, RA1, beta1 ) + _
                LyMass( Z2UCD-1.E0, RA2, beta2 ) + _
                ECoul( Z1UCD+1.E0, RA1, beta1, _
                       Z2UCD-1.E0, RA2, beta2, d )
          eps2 = ( re1 - 2.E0*re2 + re3 ) / 2.E0
          eps1 = ( re3 - re1 ) / 2.E0
          DZ_Pol = -eps1 / ( 2.E0 * eps2 )
          
          If DZ_Pol > 2 Or DZ_Pol < -2 Then DZ_Pol = 0

          If Imode > 0 Then
            /' Purely empirical enhancement of charge polarization '/
            DZ_POL = DZ_POL * POLARfac + POLARadd   
          End If           

          RZ_equi = Z1UCD + DZ_POL   
          Z_equi = RZ_equi
   End Function


   Sub Beta_opt_light(A1 As Single,A2 As Single,Z1 As Single,Z2 As Single, _
             d As Single,beta2_imposed As Single,ByRef beta1_opt As Single)
    /' Determines the optimum deformation of the light fragment when the deformation of the
       heavy fragment is imposed. '/

       Dim As Single beta1,dbeta1,beta1_prev,beta1_next
       Dim As Single Uguess,Uplus,Uminus,Uprev,Unext
       Dim As Integer I
       /'<FO REAL*4 ECOUL FO>'/
       /'<FO REAL*4 LYMASS FO>'/

    /' List('Beta_opt_light called with ');
       List(A1,A2,Z1,Z2,d,beta2_imposed,beta1_opt);
      DCL Byes Bit(1) aligned;
       Call GPYES('Continue',Byes); '/
       beta1 = 0.5
       dbeta1 = 0.01
       Uguess = LyMass(Z1, A1, beta1) + _
                Lymass(Z2, A2, beta2_imposed) + _
                ECoul(Z1, A1, beta1, Z2, A2, beta2_imposed, d)
       Uplus  = LyMass(Z1, A1, beta1 + dbeta1) + _
                Lymass(Z2, A2, beta2_imposed) + _
                ECoul(Z1, A1, beta1 + dbeta1, Z2, A2, beta2_imposed, d)
       Uminus = LyMass(Z1, A1, beta1 - dbeta1) + _
                Lymass(Z2, A2, beta2_imposed) + _
                ECoul(Z1, A1, beta1 - dbeta1, Z2, A2, beta2_imposed, d)
       If Uplus > Uguess And Uminus > Uguess then
         beta1_opt = beta1
       Else
         If Uplus < Uguess then dbeta1 = 0.01
         If Uminus < Uguess then dbeta1 = -0.01
         Unext = Uguess
         beta1_next = beta1
         For I = 1 to 10000
           beta1_prev = beta1_next
           Uprev = Unext
           beta1_next = beta1_prev + dbeta1
           Unext = LyMass(Z1, A1, beta1_next) + _
                   Lymass(Z2, A2, beta2_imposed) + _
                   ECoul(Z1, A1, beta1_next, Z2, A2, beta2_imposed, d)
           If Unext >= Uprev Then Exit For
/'>'/           
           If I = 10000 Then Print "Loop overflow in Beta_opt_light"
/'<'/           
         Next
         beta1_opt = beta1_prev
       EndIf

   End Sub


   Sub Beta_Equi(A1 As Single,A2 As Single,Z1 As Single,Z2 As Single,d As Single, _
                  beta1prev As Single,beta2prev As Single, _
                  ByRef beta1opt As Single,ByRef beta2opt As Single)
    /' Determines the minimum potential of the scission-point configuration
       represented by two deformed nuclei, divided by a tip distance d.
       A1, A2, Z1, Z2, d are fixed, beta1 and beta2 are searched for and returned on output '/

       Dim As Integer B_analytical = 0
        ' Switch to use the analytical approximation 
        ' that replaces the long numerical calculation.
       Dim As Single x,y,xcoul
       Dim As Single xcoul236U = 1369.64

       Dim As Single beta1,beta2
       
 '      Dim As Double U,Uprev,Ulast,Ubest,Uopt
       Dim As Single U,Uprev,Ulast,Ubest,Uopt

 '      Dim As Double sbeta1,sbeta2
       Dim As Single sbeta1 = 0
       Dim As Single sbeta2 = 0

       Dim As Integer N,N1,N2
       Dim As Integer Nopt = 0

 '      Dim As Double eps = 5.E-4
       Dim As Single eps = 5.E-4

       Dim As Integer I
       /'<FO REAL*4 LYMASS FO>'/       
       /'<FO REAL*4 ECOUL FO>'/       
       
       If B_analytical = 0 Then  ' Numerical algorithm

       beta1 = beta1prev
       beta2 = beta2prev
       Uprev = LyMass(Z1,A1,beta1) + LyMass(Z2,A2,beta2) + ECoul(Z1,A1,beta1,Z2,A2,beta2,d)
       Uopt = Uprev

       /' Test slope of variation of U '/
       beta1 = beta1prev + eps
       U = 1.E30

       beta2 = beta2prev
 '     For beta2 = beta2prev to 0 Step -eps
       For I = 1 To Int(beta2prev/eps)
         beta2 = beta2 - eps
         Ulast = U
         U = LyMass(Z1,A1,beta1) + LyMass(Z2,A2,beta2) + ECoul(Z1,A1,beta1,Z2,A2,beta2,d)
         If U > Ulast Then
           Exit For
         Else
           Ubest = U
         EndIf
       Next
       If Ubest < Uopt Then
         Uopt = Ubest
         sbeta1 = eps
         sbeta2 = -eps
       EndIf

       U = 1.E30
       beta2 = beta2prev
   '   For beta2 = beta2prev To 1 Step eps
       For I = 1 To Int((1 - beta2prev)/eps)
         beta2 = beta2 + eps
         Ulast = U
         U = LyMass(Z1,A1,beta1) + LyMass(Z2,A2,beta2) + ECoul(Z1,A1,beta1,Z2,A2,beta2,d)
         If U > Ulast Then
            Exit For
         Else
           Ubest = U
         EndIf
       Next
       If Ubest < Uopt Then
         Uopt = Ubest
         sbeta1 = eps
         sbeta2 = eps
       End If

       beta1 = beta1prev - eps
       U = 1.E30
       beta2 = beta2prev
   '   For beta2 = beta2prev To 0 Step -eps
       For I = 1 To Int(beta2prev/eps)
         beta2 = beta2 - eps
         Ulast = U
         U = LyMass(Z1,A1,beta1) + LyMass(Z2,A2,beta2) + ECoul(Z1,A1,beta1,Z2,A2,beta2,d)
         If U > Ulast Then
            Exit For
         Else
            Ubest = U
         End If
       Next
       If Ubest < Uopt Then
         Uopt = Ubest
         sbeta1 = -eps
         sbeta2 = -eps
       EndIf

       U = 1.E30
       beta2 = beta2prev
   '   For beta2 = beta2prev To 1 Step eps
       For I = 1 To Int((1-beta2prev)/eps)
         beta2 = beta2 + eps
         Ulast = U
         U = LyMass(Z1,A1,beta1) + LyMass(Z2,A2,beta2) + ECoul(Z1,A1,beta1,Z2,A2,beta2,d)
         If U > Ulast Then
            Exit For
         Else
           Ubest = U
         EndIf
       Next
       If Ubest < Uopt Then
         Uopt = Ubest
         sbeta1 = -eps
         sbeta2 = eps
       EndIf


      Ubest = Lymass(Z1,A1,beta1prev) + Lymass(Z2,A2,beta2prev) _
             + ECoul(Z1,A1,beta1prev,Z2,A2,beta2prev,d)
      U = Lymass(Z1,A1,beta1prev+Csng(sbeta1)) + _
          Lymass(Z2,A2,beta2prev+Csng(sbeta2)) + _
          ECoul(Z1,A1,beta1prev+sbeta1,Z2,A2,beta2prev+Csng(sbeta2),d)

'   L1:
       For N = 1 To 1000

'   L2:
         For N1 = 1 To N
           N2 = N-N1
           beta1 = beta1prev + sbeta1*N1
           beta2 = beta2prev + sbeta2*N2
           U = LyMass(Z1,A1,beta1) + _
               LyMass(Z2,A2,beta2) + _
               ECoul(Z1,A1,beta1,Z2,A2,beta2,d)
           If U < Ubest Then
             Ubest = U
             beta1opt = beta1
             beta2opt = beta2
             Nopt = N
           EndIf
         Next
         If N-Nopt > 2 Then Exit For
       Next

/'>'/
       If N > 998 Then Print #f,"Beta_Equi not converged: ",Z1,N
/'<'/

       Else  ' Analytical approximation
        ' Must be adapted if the relevant parameters of GEF are modified!
         xcoul = (Z1 + Z2)^2 / (A1 + A2)^(1.0/3.0)
         x = (Z1 / (Z1 + Z2))^(xcoul/xcoul236U)
         y = 1.2512E-4 + 0.00122851*x - 0.00267707*x^2 _
                       + 0.00372901*x^3 - 0.00219903*x^4       
         beta1opt = y * xcoul 

         x = (Z2 / (Z1 + Z2))^(xcoul/xcoul236U)
         y = 1.2512E-4 + 0.00122851*x - 0.00267707*x^2 _
                       + 0.00372901*x^3 - 0.00219903*x^4       
         beta2opt = y * xcoul 
       
       End If

   End Sub

/'>'/

   Sub Eva(Ilh As Integer,Z_CN As Single,A_CN As Single,E_INIT As Single, _
       T As Single, J_Frag As Single, _
       Byref Z_RES As Single,ByRef A_RES As Single, ByRef E_FINAL As Single,  _
       Array_En() As Single, Array_Tn() As Single, Array_Eg0() As Single)
       
            /' Z_CN,A_CN,E_init       Parameters of initial nucleus '/
            /' Z_RES,A_RES,E_FINAL    after evaporation '/
            /' T temperature coefficient of level density '/
            /' Array_En       kinetic energies of neutrons '/
            /' Array_Tn       neutron emission time after scission '/
            /' Array_Eg0      energies of statistical gammas '/
            Static As Single E_MIN = 0   /' Final energy for evaporation chain '/
            Dim As Single SN,SNeff,SNmean,SNexp        /' Neutron separation energy '/
            Dim As Single RShell
            Dim As Integer ITry,Ifold
            Dim As Single Ai,Af,Zi,Zf,Ni,Nf,Ei,Ef
            Dim As Single bshift,bshiftm
            Dim As Single Tm, Td, Tf, Tmean, TCT
            Dim As Single Gamma_n,Gamma_g,Pgamma
            Dim As Single rho,rhom
            Dim As Single alev
            Dim As Single g_koeff
            Dim As Single E_kin
            Dim As Single Tn,Tn_acc
            Dim As Single J_crit  ' critical angular momentum
            Dim As Single Fred   ' reduction of pairing gap by ang. momentum
            Dim As Integer In_gamma  ' counts gammas for Array_Eg0()
            Dim As Ubyte B_Expmass = 0  ' use mass model or empirical masses

          E_min = E_Final
            
          In_gamma = 0       
            
          Tn_acc = 0  
' Tn_acc = 60
          Ifold = 1

          Ai = A_CN
          Zi = Z_CN
          Ei = E_INIT
          
          J_crit = 8.5 * sqr(Ai/100.0) ' L. G. Moretto, Nucl. Phys. A 185 (1972) 145
          
          If J_frag >= J_crit Then
            Fred = 0
          Else
            Fred = sqr(1.0 - J_frag/J_crit) ' L. G. Moretto, Nucl. Phys. A 185 (1972) 145
          End If

          /' Shell effects included '/
          If B_Expmass = 0 Then
            SN = U_Mass(Zi,Ai-1.E0) + Fred * Lypair(Zi,Ai-1.E0) _
              - (U_Mass(Zi,Ai) + Fred * Lypair(Zi,Ai)) 
            SNeff = U_Mass(Zi,Ai-1.E0) - U_Mass(Zi,Ai)    ' with shells, without pairing
        '   SNmean = 0.5E0 * (U_Mass(Zi,Ai-2.E0) - U_Mass(Zi,Ai))
          Else
            SNexp = ( AME2012(Zi,Ai-1) - AME2012(Zi,Ai) )  ' empirical
            SNeff = U_Mass(Zi,Ai-1.E0) - U_Mass(Zi,Ai)    ' with shells, without pairing
            SN = (1.0 - Fred) * SNexp + Fred * SNeff      
          End If 
     
          /' Shell effects excluded '/
      '   SN = LDMass(Zi,Ai-1.E0,0.0) + Lypair(Zi,Ai-1.E0) _
      '     - (LDMass(Zi,Ai,0.0) + Lypair(Zi,Ai)) 
      '   SNeff = LDMass(Zi,Ai-1.E0,0.0) - LDMass(Zi,Ai,0.0)  
          

          Zf = Zi
          Af = Ai
          Ef = Ei
          
          If Ei < SN Then Goto Raus

          Do
          
        /' Treat gamma competition '/
            Tm = U_temp(Zi,Ai,Ei,1,1,Tscale,Econd)       ' emitting nucleus
            Td = U_temp(Zi,Ai-1,Ei-SNeff,1,1,Tscale,Econd)

            If Ilh > 0 Then  ' Emission from fragments
              Gamma_g = 0.624 * Ai^1.6 * Tm^5    ' in meV (Ignatyuk, Bologna)
              Gamma_g = Gamma_g * 1.E-9        ' in MeV
            Else             ' Emission between saddle and scission
              Gamma_g = 0
            End If  
                        
            Tmean = (Tm + Td)/2
            
 '          Gamma_n = (Ai-1)^0.66667 * 0.0302 * Td^2 / exp(SNeff/Tmean)   ' in MeV (Moretto)
            Gamma_n = (Ai-1)^0.66667 * 0.13 * Td^2 / exp(SNeff/Td)  ' in MeV (Mor. PRC 54,3062)
            

            Tn = Pexp(0.658 / Gamma_n)   ' in units of 10^-21 s (hbar=0.658zs*MeV)
  Tn = Tn * 2      ' Due to pre-exponential factor of Maxwellien, Tn is about 2 times to large   
            Tn_acc = Tn_acc + Tn
            
            ' Influence on lev.dens. of pairing at low E*
            ' (Constant temperature assumed)
            If Ei-Sn < Abs(Fred * Lypair(Zi,Ai-1)) Then  ' rest energy below mean g.s. of odd-odd nuclei
              If Zi Mod 2 < 0.5 or (Ai-Zi-1) Mod 2 < 0.5 Then  ' even Z or even N
                Gamma_n = exp(-12.E0/sqr(Ai)/Td) * Gamma_n
              End If
              If Zi Mod 2 < 0.5 and (Ai-Zi-1) Mod 2 < 0.5 Then  'n Z and even N
              ' For low level density below pairing gap in even-even nuclei
                Gamma_n = exp(-12.E0/sqr(Ai)/Td) * Gamma_n
              End If    
            End If            
              /' Reduces the even-odd effect in neutron number 
                 due to low level density below the pairing gap '/
            
            
            
            Pgamma = Gamma_g / (Gamma_g + Gamma_n)   
  
            If RND < Pgamma Then  ' gamma will be emitted
              In_gamma = In_gamma + 1
              Scope
                Dim As Integer N
                Dim As Single Eg

                Eg = P_Egamma_high(Zi,Ai,Ei)      
   
                Array_Eg0(In_gamma) = Eg
   
                ' Accumulate E1 gammas
                N = CInt(Eg*1000)
                If N > 0 Then
                  Ngtot = Ngtot + 1
                  If Ilh = 1 Then Nglight = Nglight + 1
                  If Ilh = 2 Then Ngheavy = Ngheavy + 1
                  Egtot1000 = Egtot1000 + EG*1000
                  If N <= Ubound(Egamma) Then    
                    Egamma(N) = Egamma(N) + 1 
                    StoreEgammaA(N,A_CN)
                  End If  
                End If  
                Ei = Ei - Eg        
              End Scope
            End If
              
            IF Ei-Sn <= E_MIN THEN
              Zf = Zi
              Af = Ai
              Ef = Ei
              Goto Raus
            End If

            ITry = 0
          Too_Low:
            ITry = ITry + 1
            If ITry < 99 Then
               Td = U_Temp(Zi,Ai-1,Ei-SNeff,1,1,Tscale,Econd)
                  ' maximum residual energy of daughter nucleus (for En_kin = 0)
                
               E_kin = PMaxwellMod(Td,Ai-1)   /' Maxwell, with partial 1/v behaviour '/
               
               Tf = U_temp(Zi,Ai-1,Ei-E_kin-SNeff,1,1,Tscale,Econd)
                  ' final energy of daughter nucleus with En_kin considered 
               
      '         If Ei-E_kin-2*Td > 10 Then   ' In Fermi-gas regime
              If Ei-E_kin-Td > 5 Then   ' to avoid Tf at negative energies
        '        If RND > sqr( Exp(E_kin/Td)/ Exp(E_kin/Tf) ) Then goto Too_low
 '    If RND > ( Exp(E_kin/Td)/ Exp(E_kin/Tf) )^0.33333 Then goto Too_low
                If RND > ( Exp(E_kin/Td)/ Exp(E_kin/Tf) )^0.25 Then goto Too_low
                 ' Modified Maxwell that adapts to the Fermi-gas regime

      '         If RND > ( Exp(E_kin/Td)^2/ Exp(E_kin/Tf) )^0.3333333 Then goto Too_low
                 ' adjusted to data, justification not clear
              End If   
 
            Else
              /' E_kin too high after several attemps '/
              /' no neutron emitted '/
              Af = Ai
              Zf = Zi
              Ef = Ei
                
              GOTO Raus
            EndIf

            If E_kin > Ei-SN Then
              /' E_kin from PMaxwell is not available '/
              /' Try again '/
              Goto Too_Low
            End If

            Af = Ai - 1
            Zf = Zi
            Ef = Ei - E_kin - SN

           /' ANAL(EN,E_kin);
              ANAL(ENM(I_MODE),E_kin); '/

            /' Shell effects included '/  
            If B_Expmass = 0 Then
              SN = (U_Mass(Zf,Af-1.E0) + Fred * Lypair(Zf,Af-1.E0)) _
                     - (U_Mass(Zf,Af) + Fred * Lypair(Zf,Af)) 
              SNeff = U_Mass(Zf,Af-1.E0) - U_Mass(Zf,Af)  
           '  SNmean = 0.5E0 * (U_Mass(Zf,Af-2.E0) - U_Mass(Zf,Af))
            Else
              SNexp = ( AME2012(Zf,Af-1) - AME2012(Zf,Af) )  ' empirical
              SNeff = U_Mass(Zf,Af-1.E0) - U_Mass(Zf,Af)    ' with shells, without pairing
              SN = (1.0 - Fred) * SNexp + Fred * SNeff      
            End If
        
            /' Shell effects excluded '/  
         '   SN = LDMass(Zf,Af-1.E0,0.0) + Lypair(Zf,Af-1.E0) _
         '     - (LDMass(Zf,Af,0.0) + Lypair(Zf,Af)) 
         '   SNeff = LDMass(Zf,Af-1.E0,0.0) - LDMass(Zf,Af,0.0)          

            Ai = Af
            Zi = Zf
            Ei = Ef
              
            Array_En(Ifold) = E_kin * (Af-1.E0) / Af
            Array_Tn(Ifold) = Tn_acc
            Ifold = Ifold + 1
              

          Loop While Ei-SN > E_min
            
        Raus:  
          A_RES = Af
          Z_RES = Zf
          E_FINAL = MAX(Ef,0.0) 

   End Sub
   
   
  
   Function u_accel(A1 As Single,Z1 As Single,A2 As Single,Z2 As Single, _
      TKE As Single,E0 As Single,Tn As Single) As Single 
      ' returns the velocity of the fragment 1 after time Tn

      'Acceleration of fission fragments by their Coulomb field    

     ' natural constants
     Static As Single e2 = 1.44   ' MeV
     Static As Single u = 931.5  ' MeV / c^2 
     Static As Single hbarc = 197  ' MeV fm

     ' variables
     Dim As Single Ared,d0,v0
     Dim As Single d, t, dt, a, v, v1, E
     Dim As Single vinf         ' relative velocity at infinity

     Ared = A1 * A2 / (A1 + A2)
     vinf = sqr(TKE/Ared)         ' sqr(E/A), Ekin asymmptotic in MeV
     If t > 100 or TKE < E0 Then 
       v = vinf
       goto match
     End If

     d0 = e2 * Z1 * Z2 / (TKE-E0) ' fm   
     v0 = sqr(E0)   ' sqr(E/A), Ekin at scission in MeV

     d = d0
     v = 0
     dt = 0.01
     For t = 0 To 1 Step dt   ' in  10^-21 s
       if t >= Tn Then Goto match
       E = E0 + e2 * Z1 * Z2 * (1/d0 - 1/d)   ' MeV
       v = sqr(E/Ared)         ' sqr(E/A), E in MeV
       d = d + v * 14 * dt             ' in fm
     Next
     dt = 0.1
     For t = 1.1 To 10 Step dt   ' in  10^-21 s
       if t >= Tn Then Goto match
       E = E0 + e2 * Z1 * Z2 * (1/d0 - 1/d)   ' MeV
       v = sqr(E/Ared)         ' sqr(E/A), E in MeV
       d = d + v * 14 * dt             ' in fm
     Next
     dt = 1
     For t = 11 To 100 Step dt   ' in  10^-21 s
       if t >= Tn Then Goto match
       E = E0 + e2 * Z1 * Z2 * (1/d0 - 1/d)   ' MeV
       v = sqr(E/Ared)         ' sqr(E/A), E in MeV
       d = d + v * 14 * dt             ' in fm
     Next
     match:
     v1 = v * A2/(A1+A2)
     u_accel = v1  
   End Function
   
   
   Function P_Egamma_low(Zi as Single, Ai As Single, Ei As Single) As Single
   ' Random function, returns gamma energy in MeV
   ' For energies below Sn: no competition with neutrons
     Dim As Single Rres
     Dim As Integer N
     Dim As Single sigMax, Eg, Erest, rhorest, fg
     Dim As Single xran, yran
     Dim As Single Tm
     Dim As Single GammaExp
     
     Dim As Single betadef = 0
     Dim As Single gammadef = 0

     Dim As Single G0(3),E0(3)
     Dim As Integer I
     
     Dim As Single alev
     
     N = Int(Ei*10 + 0.5) 
     If N <= 0 Then N = 1
     ReDim As Single sigma(N)  ' sigma is not normalized!
                               ' (Normalization is done by Monte-Carlo procedure.)
     
     Tm = U_temp(Zi,Ai,Ei,1,1,Tscale,Econd)
     
     betadef = DEFOtab(Ai-Zi,Zi)       ' ground-state deformation
     gammadef = -120 * betadef + 47.4  ' A. Junghans
     
   ' Print Tm
   ' Tm = Tm / 2 ' + 0.1 * (1.0 / U_I_Shell(Zi,Ai) - 1)
   ' Print Tm
   ' sleep  
   ' For eventual specific behaviour of gamma strength in magic nuclei
'betadef = 0
'gammadef = 0   
     If betadef = 0 And gammadef = 0 Then 
       E0(2) = E0_GDR(Zi,Ai)
       G0(2) = Width_GDR(E0(2))
     
     ' Establish distribution
       sigMax = 0
       For Eg = 0.1 To Ei Step 0.1
          N = CInt(Eg*10)
/'       sigma(N) = exp(-Eg/Tm) * Eg^2 '/ ' for testing shape of gamma-strength function
         sigma(N) = exp(-Eg/Tm) * _
            Eg^3 * (G0(2) * Eg) / ( (Eg^2 - E0(2)^2)^2 + G0(2)^2 * E0(2)^2 )  
           ' + 0.7 * G0(2) * 4 * pi^2 * Tm^2 / E0(2)^5 )  
           ' last line: correction for low gamma energy (PRC 41 (190) 1941)
         if sigma(N) > sigMax then sigMax = sigma(N)
       Next
     Else
       sigMax = 0
       For I = 1 To 3
         E0(I) = E0_GDR(Zi,Ai) * Efac_def_GDR(betadef,gammadef,I)
         G0(I) = Width_GDR(E0(I))
      ' Establish distribution
         For Eg = 0.1 To Ei Step 0.1
            N = CInt(Eg*10)
'            sigma(N) = sigma(N) + exp(-Eg/Tm) * Eg^3 *_
'                (G0(I) * Eg) / ( (Eg^2 - E0(I)^2)^2 + G0(I)^2 * E0(I)^2 )
                
            sigma(N) = sigma(N) + exp(-Eg/Tm) * Eg^3 *_
               ( (G0(I) * Eg) / ( (Eg^2 - E0(I)^2)^2 + G0(I)^2 * E0(I)^2 )  _
              /' +  0.001 * exp(-0.5) '/  _  
              /' + 0.7 * G0(I) * 4 * pi^2 * Tm^2 / E0(I)^5 '/ ) 
             ' exponential: M1 strength at low energy, PRL 111, 232504 (2013)    
             ' last line: correction for low gamma energy (PRC 41 (1990) 1941)
           if sigma(N) > sigMax then sigMax = sigma(N)
         Next         
       Next
     End If     
     
    ' Dice gamma energy from distribution
     diceagain_gamma_low:
     xran = rnd * Ei * 10   ' in units of 100 keV
     yran = rnd * sigMax
     if yran > sigma(Cint(xran)) then goto diceagain_gamma_low
     
     P_Egamma_low = xran/10     ' convert to MeV 
   End Function       
  
     Function P_Egamma_high(Zi as Single, Ai As Single, Ei As Single) As Single
   ' Random function, returns gamma energy in MeV 
   ' From PRL 49 (1982) 434
   ' For energies above Sn: competition with neutrons included
     Dim As Integer N
     Dim As Single sigMax, Eg
     Dim As Single xran, yran
     Dim As Single Tm
     Dim As Single G0,E0
     
     N = Int(Ei*10 + 0.5) 
     If N <= 0 Then N = 1
     ReDim As Single sigma(N)  ' sigma is not normalized
                               ' (Normalization is done by Monte-Carlo procedure.)
     
     E0 = E0_GDR(Zi,Ai)
     G0 = Width_GDR(E0)
     Tm = U_temp(Zi,Ai,Ei,1,1,Tscale,Econd)
     
     ' Establish distribution
     sigMax = 0
     For Eg = 0.1 To Ei Step 0.1
        N = CInt(Eg*10.0)
        sigma(N) = Eg^3 / Tm^2 * exp(-Eg/Tm) * _
           (G0 * Eg) / ( (Eg^2 - E0^2)^2 + G0^2 * E0^2 )
       if sigma(N) > sigMax then sigMax = sigma(N)
     Next   
     
    ' Dice gamma energy from distribution
     diceagain_gamma_high:
     xran = rnd * Ei * 10   ' in units of 100 keV
     yran = rnd * sigMax
     if yran > sigma(Cint(xran)) then goto diceagain_gamma_high  
     
     P_Egamma_high = xran/10     ' convert to MeV 
   End Function   

/'<'/    
  Function U_Ired(Z As Single,A As Single) As Single
    ' Effective moment of inertia by pairing with correction for excitation energy
      Dim As Single I_rigid_spher,IfragEff
      
      /'<FO REAL*4 U_SHELL FO>'/
      
      I_rigid_spher = 1.16E0^2 * A^1.6667E0 / 103.8415E0 
  '   IfragEff = I_rigid_spher + 0.003 * A^(4.0/3.0) * U_shell(Cint(Z),Cint(A))
  '   IfragEff = I_rigid_spher + 0.005 * A^(4.0/3.0) * U_shell(Cint(Z),Cint(A))
                      ' reduction due to shell (Deleplanque et al. PRC 69 (2004) 044309)
      IfragEff = 0.45 * I_rigid_spher ' Effect of superfluidity 
  '   IfragEff = 0.65 * IfragEff   ' Average effect of superfluidity and deformation 

     U_Ired = IfragEff     
   End Function
   
  Function U_IredFF(Z As Single,A As Single) As Single
    ' Effective moment of inertia by pairing with correction for excitation energy
    ' of final fission fragments
    
      /'<FO REAL*4 U_Ired FO>'/    
      /'<FO REAL*4 U_I_Shell FO>'/    

     U_IredFF = U_Ired(Z,A) * U_I_Shell(Z,A)    
   End Function
   
   Function U_I_Shell(Z As Single,A As Single) As Single
      Dim As Integer N_shells(6)
      ' Shell effect on the effective moment of inertia
      Dim As Integer I 
      Dim As Single dNmin, dZmin, dNsubmin
      Dim As Single Inv_add = 0
      Dim As Single I_inv_add_Z = 0
      Dim As Single I_inv_add_N = 0
      Dim As Single I_inv_add_Nsub = 0
      N_shells(1) = 20
      N_shells(2) = 28
      N_shells(3) = 50
      N_shells(4) = 82
      N_shells(5) = 126
      N_shells(6) = 56
      dNmin = 100
      dZmin = 100
      dNsubmin = 100
      For I = 1 To 5
        dZmin = Min(dZmin,Abs(N_shells(I) - Z))
      Next I
      
      For I = 1 To 5
        dNmin = Min(dNmin,Abs(N_shells(I) - (A-Z))) 
      Next I  
      
      dNsubmin = Abs(N_shells(6) - (A-Z))
  
     ' Effect of shells:
      If dZmin < 10.0 Then
'        I_inv_add_Z = 0.33 * (6.0 * sqr(A/140.) - dZmin) * sqr(140./A)
        I_inv_add_Z = 0.33 * (6.0 * sqr(A/140.0) - dZmin) * (140.0/A)^1.5
        ' A^(-1/3) dependence: "A simple phenomenology for 2gamma+ states",
        ' N. V. Zamfir, D. Bucurescu, R. F. Casten, M. Ivascu,
        ' Phys. Lett. B 241 (1990) 463
        I_inv_add_Z = Max(I_inv_add_Z,0.0)
      End If
      If dNmin < 10.0 Then
'        I_inv_add_N = 0.42 * (8.0 * sqr(A/140.) - dNmin) * sqr(140./A)
        I_inv_add_N = 0.42 * (8.0 * sqr(A/140.0) - dNmin) * (140.0/A)^1.5
        I_inv_add_N = Max(I_inv_add_N,0.0)
      End If    
      If DNsubmin < 6.0 Then
   '    I_inv_add_Nsub = 1.7 * (4.0 - dNsubmin) * (1.0 - 0.32 * Abs(40.0-Z))
        I_inv_add_Nsub = 1.7 * (4.0 - dNsubmin) * (1.0 - 0.18 * Abs(40.0-Z))
            ' N = 56 subshell only around Z = 40
        I_inv_add_Nsub = Max(I_inv_add_Nsub,0.0)
      End If      
      U_I_shell = 1.0 / (1.0 + Max(I_inv_add_N,I_inv_add_Nsub) + I_inv_add_Z)
'Print "*",I_inv_add_Z, I_inv_add_N, I_inv_add_Nsub,1.0 / (1.0 + Max(I_inv_add_N,I_inv_add_Nsub) + I_inv_add_Z)  
   End Function
   

   Function U_alev_ld(Z As Single, A As Single) As Single
    '  U_alev_ld = 0.073 * A + 0.095 * A^0.666667  'Ignatyuk (1970's)
       U_alev_ld = 0.078 * A + 0.115 * A^0.6666667  ' Ignatyuk (Bologna 2000) 
    '  U_alev_ld = 0.089 * A    ' only volume term
   End Function
    
   Function U_Temp(Z As Single, A As Single, E As Single, Ishell As Integer, _
           Ipair As Integer, Tscale As Single,Econd As Single) As Single
       ' Temperature (modified Gilbert-Cameron composite level density)    
       ' KHS (10. 2. 2012)       
       Dim As Single alev  
       Dim As Single Eeff0,Eeff1,Rho0,Rho1,TCT,TFG 
       Static As Single fgamma = 0.055      
       Dim As Single RShell,RPair,Res
       /'<FO REAL*4 U_ALEV_LD FO>'/
       /'<FO REAL*4 U_SHELL FO>'/
       /'<FO REAL*4 LYPAIR FO>'/
       /'<FO REAL*4 TEGIDY FO>'/ 
       ' Used global parameters: Tscale
    '   alev = U_alev_ld(Z,A) * 1.1   ' Factor adjusted to high-energy prompt neutrons in U235(nth,f)
    '  alev = U_alev_ld(Z,A) * 0.8  ' " with the correction for non-constant T (FG range)
       alev = U_alev_ld(Z,A)
       
       If Ishell = 1 Then
         RShell = U_Shell(Cint(Z),Cint(A))
       Else
         RShell = 0.0
       End If    
       TCT = TEgidy(A,RShell,Tscale)  
       
       If Ipair = 1 Then
         RPair = Lypair(CInt(Z),CInt(A))
       Else
         Rpair = 0.0
       End If    
       Eeff0 = E - Econd + RPair + Rshell*(1.0 - exp(-fgamma * E))
       
       If Eeff0 > 0.5 Then
         Eeff1 = Eeff0 + 0.1
         Rho0 = 1.E0/Eeff0^1.25 * exp(2.E0 * sqr(alev * Eeff0))
         Rho1 = 1.E0/Eeff1^1.25 * exp(2.E0 * sqr(alev * Eeff1))
'         Rho0 = 1.E0/Eeff0 * exp(2.E0 * sqr(alev * Eeff0))
'         Rho1 = 1.E0/Eeff1 * exp(2.E0 * sqr(alev * Eeff1))
         TFG = 0.1E0 / (log(Rho1) - log(Rho0))
       Else 
         TFG = 0.0
       End If
       Res = TCT
       If TFG > Res Then Res = TFG

' If Res > 1.4 Then Res = 1.4

       U_Temp = Res
   End Function
/'>'/   

   Function U_Temp2(Z As Single, A As Single, E As Single, Rshell As Single, _
          Rpair As Single, Tscale As Single,Econd As Single) As Single
       ' Temperature (modified Gilbert-Cameron composite level density)    
       ' KHS (10. 2. 2012)       
       Dim As Single alev  
       Dim As Single Eeff0,Eeff1,Rho0,Rho1,TCT,TFG 
       Static As Single fgamma = 0.055      
       Dim As Single Res
       ' Used global parameters: Tscale
    '  alev = U_alev_ld(Z,A) * 1.1   ' Factor adjusted to high-energy prompt neutrons in U235(nth,f)
    '  alev = U_alev_ld(Z,A) * 0.86  ' " with the correction for non-constant T (FG range)
       alev = U_alev_ld(Z,A)
       
       TCT = TEgidy(A,RShell,Tscale)  
       
       Eeff0 = E - Econd + RPair + Rshell*(1.0 - exp(-fgamma * E))
   '    Eeff0 = E - Econd + Lypair(CInt(Z),CInt(A)) + Rshell*(1.0 - exp(-fgamma * E))
       
       If Eeff0 > 0.5 Then
         Eeff1 = Eeff0 + 0.1
         Rho0 = 1.E0/Eeff0^1.25 * exp(2.E0 * sqr(alev * Eeff0))
         Rho1 = 1.E0/Eeff1^1.25 * exp(2.E0 * sqr(alev * Eeff1))
'         Rho0 = 1.E0/Eeff0 * exp(2.E0 * sqr(alev * Eeff0))
'         Rho1 = 1.E0/Eeff1 * exp(2.E0 * sqr(alev * Eeff1))
         TFG = 0.1E0 / (log(Rho1) - log(Rho0))
       Else 
         TFG = 0.0
       End If
       Res = TCT
       If TFG > Res Then Res = TFG
       U_Temp2 = Res
   End Function
   
   Function E0_GDR(Z As Single, A As Single) As Single
     ' Calculates the centroid energy of the GDR for spherical nucleus
     ' according to the FRDM (ADNDT 59 (1995) 185 and PLB 670 (2008) 200)
     Static As Single epsilon = 0.0768
     Static As Single J = 32.7
     Static As Single Q = 29.2
     Static As Single R0 = 1.16
     Static As Single mstar = 874
     Static As Single hbar = 197.3
     Dim As Single Aonethird,u,N,E0
   
     ' according to [9] in Phys. Lett. B 690 (2010) 473:
      E0_GDR = 18.0/A^0.333333 + 25.0 / A^0.1666667
   
     ' according to the FRDM (ADNDT 59 (1995) 185 and PLB 670 (2008) 200):
  '    Aonethird = A^0.333333
  '    N = A - Z
  '    u = (1-epsilon)/Aonethird * 3*J/Q
  '    E0_GDR = hbar /(R0*Aonethird)*sqr(8*J*A^2/ (mstar*4*N*Z) ) * _
  '      (1 + u - epsilon * (1+epsilon+3*u)/(1+epsilon+u))^(-1/2) 
   End Function
   
   Function Width_GDR(E0 As Single) As Single
     ' Spreading width of the GDR (Nucl. Phys. A 531 (1991) 27)
      Width_GDR = 1.99 * (E0/10)^1.6
   End Function
   
   Function Efac_def_GDR(Beta As Single,Gamma As Single,K As Single) As Single
     ' Modification factors of the resonance energy due to triaxial deformation
     ' Hill-Wheeler parameterisation (PRC 89 (1953) 1102)
     ' Possible values for K:  K-2 = -1, 0, 1
     If beta = 0 and gamma = 0 then
       Efac_def_GDR = 1
     Else  
       Efac_def_GDR = 1/(exp(sqr(5/(4*pi))*Beta * cos(gamma - 0.666667*(K-2)*pi)))
     End If   
   End Function  
   
   Function GgGtot(Z As Single, A As Single, E As Single, Egamma As Single) As Single
     ' From PRL 49 (1982) 434
     ' Probability to emit a gamma of energy Egamma in competition to neutron emission
      Dim As Single EG, GG, T, SN
      EG = E0_GDR(Z,A)
      GG = Width_GDR(EG)
      T = U_Temp(Z,A,E,1,1,Tscale,Econd)
      SN = U_Mass(Z,A-1.E0) + Lypair(Z,A-1.E0) _
        - (U_Mass(Z,A) + Lypair(Z,A)) 
      GgGtot = Egamma^3 / T^2 * exp((Sn-Egamma)/T) _
          * GG * EG / ((Egamma^2 - EG^2)^2 + GG^2 * EG^2)
   End Function

   Function E_next(T1 As Single,T2 As Single,E1 As Single,E2 As Single,A1 As Single,A2 As Single) _
                     As Single
       /' Samples the energy transfer in one step between two nuclei '/
       /' in thermal contact '/
       /' The energy transfer is only determined by the available phase space. '/
       /' Only one kind of nucleons considered! '/

        /' T1,T2 Temperatures of the two nuclei '/
        /' E1,E2 Initial energies of the two nuclei '/
        /' A1, A2 Mass numbers of the two nuclei '/

        Dim As Single E12
        Dim As Single Delta1,Delta2 /' Pairing gaps '/
        Dim As Single Delta_E1,Delta_E2
        Dim As Single E1final       /' Energy 1 after transfer '/
        Dim As Single E1mod,E2mod
        /'<FO REAL*4 PEXPLIM FO>'/

        /' Assumed level densities: '/
        /' Even number of nucleons:
           1 ground state at energy E = - 2 Delta not considered,
           continuous level density above E = 0 : rho1,2 = a1,2 * exp(E1,2/T1,2) '/

      E12 = E1 + E2 /' Total energy '/

      E1mod = E1
      E2mod = E2
      If (E1mod > E2mod) Then
        Delta_E1 = Pexplim(-1.E0/T1,0.0,E1mod)
        E1mod = E1mod - Delta_E1
        E2mod = E2mod + Delta_E1
        Delta_E2 = Pexplim(-1.E0/T2,0.0,E2mod)
        E2mod = E2mod - Delta_E2
        E1mod = E1mod + Delta_E2
      Else
        Delta_E2 = Pexplim(-1.E0/T2,0.0,E2mod)
        E2mod = E2mod - Delta_E2
        E1mod = E1mod + Delta_E2
        Delta_E1 = Pexplim(-1.E0/T1,0.0,E1mod)
        E1mod = E1mod - Delta_E1
        E2mod = E2mod + Delta_E1
      EndIf
      E1final = E1mod

   /' Select;
        When (E1 > E2) Do;
        L3:
          Delta_E1 = Pexplim(-1.E0/T1,0.0,E1);
          E1final = E1 - Delta_E1;
          Delta_E1 = Pexplim(-1.E0/T2,0.0,E12-E1final);
          E1final = E1final + Delta_E1;
        End;
        When (E1 <= E2) Do;
        L4:
          Delta_E1 = Pexplim(-1.E0/T2,0.0,E12-E1);
          E1final = E1 + Delta_E1;
          Delta_E1 = Pexplim(-1.E0/T1,0.0,E1final);
          E1final = E1final - Delta_E1;
        End;
        Otherwise Do;
          List('This should not happen.');
        End;
      End; '/

      E_next = E1final

   End Function


   Function Pexplim(R_lambda As Single,xmin As Single,xmax As Single) As Single
      /' random number from an exponential between xmin and xmax '/
         /' decay constant: f(x) = exp(lambda * x) !!! '/
         /' xmin, xmax: limits for sampling '/
        Dim As Single umin, umax  /' xmin, xmax transformed '/
        Dim As Single u  /' help variable '/
        Dim As Single R_res  /' sampled value '/

      If ABS(R_lambda) < 1.E-30 Then
        R_res = xmin + Rnd * (xmax-xmin)
      Else
        umin = exp(xmin*R_lambda)
        umax = exp(xmax*R_lambda)
        u = umin + Rnd * (umax-umin)
        R_res = 1.E0/R_lambda * log(u)
        Pexplim = R_res
      EndIf
   End Function

/'<'/

 Function U_Even_Odd(I_Channel As Integer,PEO As Single) As Single
   ' Creates even-odd fluctuations 
   Dim As Single R
   If I_Channel Mod 2 = 0 Then
     R = 1.0 + PEO
   Else
     R = 1.0 - PEO
   End If
   U_Even_Odd = R   
 End Function
 
/'>'/ 

   Function EVEN_ODD(R_ORIGIN As Single,R_EVEN_ODD As Single) As Integer

     /' Procedure to calculate I_OUT from R_IN in a way that         '/
     /' on the average a flat distribution in R_IN results in a      '/
     /' fluctuating distribution in I_OUT with an even-odd effect as '/
     /' given by R_EVEN_ODD                                          '/

     /' ------------------------------------------------------------ '/
     /' EXAMPLES :                                                   '/
     /' ------------------------------------------------------------ '/
     /'    If R_EVEN_ODD = 0 :                                       '/
     /'           CEIL(R_IN)  ----                                   '/
     /'                                                              '/
     /'              R_IN ->                                         '/
     /'            (somewhere in between CEIL(R_IN) and FLOOR(R_IN)) '/
     /'                                                              '/
     /'           FLOOR(R_IN) ----       --> I_OUT                   '/
     /' ------------------------------------------------------------ '/
     /'    If R_EVEN_ODD > 0 :                                       '/
     /'      The interval for the above treatment is                 '/
     /'         larger for FLOOR(R_IN) = even and                    '/
     /'         smaller for FLOOR(R_IN) = odd                        '/
     /'    For R_EVEN_ODD < 0 : just opposite treatment              '/
     /' ------------------------------------------------------------ '/

     /' ------------------------------------------------------------ '/
     /' On input:   R_ORIGIN    nuclear charge (real number)         '/
     /'             R_EVEN_ODD  requested even-odd effect            '/
     /' Intermediate quantity: R_IN = R_ORIGIN + 0.5                 '/
     /' On output:  I_OUT       nuclear charge (integer)             '/
     /' ------------------------------------------------------------ '/

     Dim As Single R_IN,R_REST,R_HELP
     Dim As Single R_FLOOR
     Dim As Single R_MIDDLE
     Dim As Integer I_OUT

     R_EVEN_ODD = MIN(R_EVEN_ODD,1)
     R_IN = R_ORIGIN + 0.5E0
     R_FLOOR = FLOOR(R_IN)
     If Abs(R_EVEN_ODD) < 1.E-3 Then
       I_OUT = R_FLOOR	
     Else
       R_REST = R_IN - R_FLOOR
       R_MIDDLE = R_FLOOR + 0.5E0
       IF R_FLOOR Mod 2 = 0 THEN  /' even before modif. '/
         R_HELP = R_MIDDLE + (R_REST - 0.5E0) _
           * 1.E0 / Max(0.01,(1.E0 + R_EVEN_ODD))
         R_HELP = Min(R_HELP,R_MIDDLE+1)
         R_HELP = Max(R_HELP,R_MIDDLE-1)
       ELSE  /' odd before modification '/
         R_HELP = R_MIDDLE + (R_REST - 0.5E0) _
           * 1.E0 / Max(0.01,(1.E0 - R_EVEN_ODD))
         R_HELP = Min(R_HELP,R_MIDDLE+1)
         R_HELP = Max(R_HELP,R_MIDDLE-1)
       EndIf
       I_OUT = FLOOR(R_HELP)
     EndIf
     Even_odd = I_OUT
   End Function

/'<'/

   Function BFTF(RZ As Single,RA As Single,I_Switch As Integer) As Single
    /' Fission barriers from Myers and Swiatecki, Thomas-Fermi model '/
    /'  I_Switch: 0: liquid-drop; 1: with shells and pairing, 
        2: averaged over pairing, 3: with shell and pairing + pairing gap at barrier '/
      ' 4: liquid-drop + g.s. shell, no Z correction
      Dim As Single RN,RI,Rkappa,RS,RF,RX
      Dim As Single RX0 = 48.5428
      Dim As Single RX1 = 34.15
      Dim As Single RB 
      Dim As Integer IZ,IA
     /'<FO REAL*4 U_SHELL FO>'/
     /'<FO REAL*4 U_SHELL_EXP FO>'/
     /'<FO REAL*4 U_SHELL_EO_EXP FO>'/
     /'<FO REAL*4 LYPAIR FO>'/
     
     IZ = Cint(RZ)
     IA = Cint(RA)
     RN = RA - RZ
     RI = (RN-RZ) / RA
     Rkappa = 1.9E0 + (RZ - 80.E0) / 75.E0
     RS = RA^0.666667E0 * (1.E0 - Rkappa * RI^2)
     RX = RZ^2 / (RA * (1.E0 - Rkappa * RI^2))
     If RX < 30 Then   /' out of range '/
       RF = 1.E10
     End If
     If RX > RX0 Then  /' out of range '/
       RF = 0.0
     End If
     If RX < RX1 And RX > 30 Then 
       RF = 0.595553E0 - 0.124136E0 * (RX - RX1)
     End If
     If RX >= RX1 And RX <= RX0 Then 
       RF = 0.000199749 * (RX0 - RX)^3
     End If
     RB = RF * RS

     Select Case I_Switch
       Case 0
         BFTF = RB
       Case 1 ' including even-odd staggering due to increased pairing strength at barrier
         ' Tentative modification from comparison with experimental fission barriers
         ' (shell correction at the barrier?)
         If RZ > 86.5 Then RB = RB - 0.15 * (RZ - 86.5)
     '    If RZ > 90 Then RB = RB + 0.3 * (RZ - 90.0)
     '    If RZ > 98 Then RB = RB - 0.15 * (RZ - 98.0) 
         If RZ > 90 Then RB = RB + 0.35 * (RZ - 90.0)
         If RZ > 93 Then RB = RB + 0.15 * (RZ - 93.0)
         If RZ > 95 Then RB = RB - 0.25 * (RZ - 95.0) 
     '    BFTF = RB - U_Shell(IZ,IA)
     '    BFTF = RB - U_Shell_exp(IZ,IA)
         BFTF = RB - U_Shell_EO_exp(IZ,IA) + Lypair(IZ,IA) * 14.0/12.0
       Case 2 ' averaged over even-odd staggering
         If RZ > 86.5 Then RB = RB - 0.15 * (RZ - 86.5)
         If RZ > 90 Then RB = RB + 0.35 * (RZ - 90.0)
         If RZ > 93 Then RB = RB + 0.15 * (RZ - 93.0)
         If RZ > 95 Then RB = RB - 0.25 * (RZ - 95.0) 
         BFTF = RB - U_Shell_exp(IZ,IA)
       Case 3 ' like Case 1 + pairing gap at barrier
         If RZ > 86.5 Then RB = RB - 0.15 * (RZ - 86.5)
         If RZ > 90 Then RB = RB + 0.35 * (RZ - 90.0)
         If RZ > 93 Then RB = RB + 0.15 * (RZ - 93.0)
         If RZ > 95 Then RB = RB - 0.25 * (RZ - 95.0) 
         BFTF = RB - U_Shell_EO_exp(IZ,IA) 
       Case 4 ' like case 3 but without Z correction
       ' This is the direct description from the topographic theorem.
         BFTF = RB - U_Shell_exp(IZ,IA)
       Case Else
         Print "Undefined option in BFTF"
'         Sleep
     End Select  
 /'  If I_Switch = 0 Then 
       BFTF = RB
     Else 
      ' Tentative modification from comparison with experimental fission barriers
      ' (shell correction at the barrier?)
       If RZ > 86.5 Then RB = RB - 0.15 * (RZ - 86.5)
   '    If RZ > 90 Then RB = RB + 0.3 * (RZ - 90.0)
   '    If RZ > 98 Then RB = RB - 0.15 * (RZ - 98.0) 
       If RZ > 90 Then RB = RB + 0.35 * (RZ - 90.0)
       If RZ > 93 Then RB = RB + 0.15 * (RZ - 93.0)
       If RZ > 95 Then RB = RB - 0.25 * (RZ - 95.0) 
          
   '    BFTF = RB - U_Shell(IZ,IA)
   '    BFTF = RB - U_Shell_exp(IZ,IA)
       BFTF = RB - U_Shell_EO_exp(IZ,IA) + Lypair(IZ,IA) * 14.0/12.0
     End If '/
   End Function

   Function BFTFA(RZ As Single,RA As Single,I_Switch As Integer) As Single
    /' inner barrier height '/
     Dim As Single EA,BF0,Z4A,Z3A,DB 
     Dim As Single coeff = 0.5
     /'<FO REAL*4 BFTF FO>'/
     BF0 = BFTF(RZ,RA,I_Switch)
   ' Z4A = RZ^4 / RA
     '  EB - EA from fit to Smirenkin barriers:
     '  V. M. Kupriyanov, K. K. Istekov, B. I. Fursov, G. N. Smirenkin
     '  Sov. J. Nucl. Phys. 32 (1980) 184
   '  DB = -10.3517 + 1.6027E-5 * Z4A + 5.4945E-11 * Z4A^2  ' EA - EB
   
     '  EB - EA from fit to data from Dahlinger et al. (KHS, 21. Dec. 2012)
     Z3A = RZ^3 / RA
     DB = -(5.40101 - 0.00666175*Z3A + 1.52531E-6*Z3A^2)
     If DB > 0.0 Then
       EA = BF0 - DB
     Else
       EA = BF0 
     End If 
     BFTFA = EA
   End Function

   Function BFTFB(RZ As Single,RA As Single,I_Switch As Integer) As Single
    /' outer barrier height '/
     Dim As Single EB,BF0,Z4A,Z3A,DB 
     Dim As Single coeff = 0.5
     /'<FO REAL*4 BFTF FO>'/
     BF0 = BFTF(RZ,RA,I_Switch)
   ' Z4A = RZ^4 / RA
     '  EB - EA from fit to Smirenkin barriers:
     '  V. M. Kupriyanov, K. K. Istekov, B. I. Fursov, G. N. Smirenkin
     '  Sov. J. Nucl. Phys. 32 (1980) 184
  '   DB = -10.3517 + 1.6027E-5 * Z4A + 5.4945E-11 * Z4A^2  ' EA - EB
  
     '  EB - EA from fit to data from Dahlinger et al. (KHS, 21. Dec. 2012)
     Z3A = RZ^3 / RA
     DB = -(5.40101 - 0.00666175*Z3A + 1.52531E-6*Z3A^2)  
     If DB < 0.0 Then
       EB = BF0 + DB
     Else
       EB = BF0 
     End If 
     BFTFB = EB
   End Function
   


   /' Utility functions '/


   Function Gaussintegral(R_x As Single,R_sigma As Single) As Single
     /' Smoothed step function. Grows from 0 to 1 around R_x
        with a Gauss-integral function with given sigma'/
     Dim As Single R_ret
     ' Note: The variable R_sigma = standard deviation / sqr(2) !
     /'<FO REAL*4 ERF FO>'/
       R_ret = 0.5E0 + 0.5E0 * Erf(R_x / R_sigma)
       Gaussintegral = R_ret
   End Function

   Function U_Box(x As Single,sigma As Single, _
         length As Single) As Single
     Dim As Single y
     ' Note: The variable sigma = standard deviation / sqr(2) !
     /'<FO REAL*4 GAUSSINTEGRAL FO>'/      
     y = Gaussintegral(x+0.5*length,sigma) - Gaussintegral(x-0.5*length,sigma)
     U_Box = y/length
   End Function
   
   Function U_Box2(x As Single,sigma1 As Single, sigma2 As Single, _
         length As Single) As Single
     Dim As Single y
     ' Note: The variable sigma = standard deviation / sqr(2) !
     /'<FO REAL*4 GAUSSINTEGRAL FO>'/      
     y = Gaussintegral(x+0.5*length,sigma2) - Gaussintegral(x-0.5*length,sigma1)
     U_Box2 = y/length
   End Function
   
   Function U_Gauss(x As Single,sigma As Single) As Single
     Dim As Single y
     /'<FO Const As Single pi = 3.14159 FO>'/      
     
     y = 1.0 / (sqr(2.0 * pi) * sigma) * exp(-x^2/ ( 2.0 * sigma^2 ) )
     U_Gauss = y
   End Function  

   Function U_Gauss_mod(x As Single,sigma As Single) As Single
    ' Gaussian with Sheppard correction
     Dim As Single y
     Dim As Single sigma_mod
     /'<FO Const As Single pi = 3.14159 FO>'/      
     sigma_mod = sqr(sigma^2 + 1./12.)
     
     y = 1.0 / (sqr(2.0 * pi) * sigma_mod) * exp(-x^2/ ( 2.0 * sigma_mod^2 ) )
     U_Gauss_mod = y
   End Function  

/'>'/   
   
Public Function PBox(Mean As Single,Sigma As Single, _
              Bottom As Single) As Single
   ' Rectangular distribution folded with a Gaussian distribution   
   Dim As Single R
   R = PGauss(Mean,Sigma)
   R = R + (Rnd-0.5)*Bottom
   PBox = R
End Function

Public Function PBox2(Mean As Single,Sigma1 As Single,Sigma2 As Single, _
              Bottom As Single) As Single
   ' Rectangular distribution folded with a Gaussian distribution. 
   ' One wing is steeper.  
   ' Sigma1 = lower side, Sigma2 = upper side
   Dim As Single Sigma,R
   Sigma = Max(Sigma1,Sigma2)
  Pbox_Repeat: 
   R = PGauss(Mean,Sigma)
   R = R + (Rnd-0.5)*Bottom
   If Sigma1 < Sigma2 Then
     If R < Mean - 0.5*Bottom Then
       If RND > Exp( -(R - Mean + 0.5*Bottom)^2 / (2.0 * Sigma1^2) ) _
               / Exp( -(R - Mean + 0.5*Bottom)^2 / (2.0 * Sigma2^2) ) Then
         R = Mean - 0.5*Bottom + (Mean - 0.5*Bottom - R)
       End If
     End If
   End If
   If Sigma2 <= Sigma1 Then
     If R > Mean + 0.5*Bottom Then
       If RND > Exp( -(R - Mean - 0.5*Bottom)^2 / (2.0 * Sigma2^2) ) _ 
               / Exp( -(R - Mean - 0.5*Bottom)^2 / (2.0 * Sigma1^2) ) Then
         R = Mean + 0.5*Bottom - (R - Mean - 0.5*Bottom)
       End If
     End If
   End If
   PBox2 = R
End Function

Public Function PPower(Order As Integer, Rmin As Single, Rmax As Single) As Single
 ' Random generator of a power function: (y = x^Order -> x_random = RND^(1/(Order+1))
 ' PPower = 0 at Rmin to PPower = Ymax at Rmax 
   Dim As Single R
   R = Rmin + (Rmax-Rmin) * Rnd^(1.0/(Order+1))  
   PPower = R 
End Function

Public Function PPower_Griffin_v(Order As Integer, Rmin As Single, Rmax As Single) As Single
 ' Random generator of a power function: (y = x^Order -> x_random = RND^(1/(Order+1))
 ' PPower = 0 at Rmin to PPower = Ymax at Rmax 
   Dim As Single R,v_particle,RRND
   Repeat_Griffin:
   R = Rmin + (Rmax-Rmin) * Rnd^(1.0/(Order))  
   v_particle = sqr(Abs((R-Rmax)/(Rmin-Rmax)))
   RRND = RND
   If RRND > v_particle Then 
     Goto Repeat_Griffin
   End If  
   PPower_Griffin_v = R
End Function

Public Function PPower_Griffin_E(Order As Integer, Rmin As Single, Rmax As Single) As Single
 ' Random generator of a power function: (y = x^Order -> x_random = RND^(1/(Order+1))
 ' PPower = 0 at Rmin to PPower = Ymax at Rmax 
   Dim As Single R,E_particle
   Repeat_Griffin:
   R = Rmin + (Rmax-Rmin) * Rnd^(1.0/(Order))  
   E_particle = (R-Rmax)/(Rmin-Rmax)
   If Rnd > E_particle Then Goto Repeat_Griffin
   PPower_Griffin_E = R
End Function

Public Function PGauss(Mean As Single,Sigma As Single) As Single
  ' Box-Mueller method
  Static As Integer ISet = 0
  Dim As Single V1,V2,R,Fac,GasDev,Result
  Static As Single GSet
  If ISet = 0 then
    Repeat:
    V1 = 2.E0 * Rnd - 1.E0
    V2 = 2.E0 * Rnd - 1.E0
    R = V1^2 + V2^2
    If R >= 1.E0 or R = 0.0 Then Goto Repeat
    Fac = Sqr(-2.E0 * Log(R)/R)
    GSet = V1 * Fac
    GasDev = V2 * Fac
    ISet = 1
  Else
    GasDev = GSet
    ISet = 0
  End If
  Result = Sigma * GasDev
  PGauss = Mean + Result
End Function

Public Function PLinGauss(R_Sigma As Single) As Single
  /' Random-number generator for linear * Gaussian function '/
  /' Distribution of nuclear angular momenta '/
  Dim As Single R_Res,B_rms
  B_rms = R_Sigma / sqr(2.0) ' Because 
    ' the sum of two PGauss functions increases the width.
  R_Res = Abs(PGauss(0,B_rms)) + Abs(PGauss(0,B_rms))
  R_Res = R_Res + B_rms/4.E0 * (1.E0 - exp(-R_Res/R_Sigma))
    ' correction of shape (approximative)
  PLinGauss = R_Res
End Function

/'<'/
Public Function U_LinGauss(x As Single, R_Sigma As Single) As Single
  /' Gaussian times a linear function '/
  /' Not normalized! '/
  Dim As Single R_Res
  If R_Sigma > 0.0 Then
    R_Res = x * exp(-x^2/(2.0 * R_Sigma^2))
  Else
    R_Res = 0.0
  End If    
  U_LinGauss = R_Res
End Function
/'>'/

Public Function PExp(R_Tau As Single) As Single
  /' Random-number generator for an exponential distribution '/
  Dim As Single X1,R_Res
  Again:
  X1 = RND
  If X1 > 1.E-10 And X1 < 0.99999E0 Then  ' for avoiding numerical problems
     R_Res = - R_Tau * Log(X1)
  Else
     Goto Again
  End If
  PExp = R_Res
End Function

Public Function PMaxwell(R_T As Single) As Single
   /' Random-number generator for a surface Maxwell distribution '/
   /' y = x * exp(-x/T) '/
   Dim As Double R_Res,R_T_int
   R_T_int = R_T
   R_Res = -R_T_int * (Log(Rnd) + Log(Rnd))
   PMaxwell = R_Res
End Function

Public Function PMaxwellv(R_T As Single) As Single
  /' Random generator according to a distribution similar to a '/
  /' Maxwell distribution with quantum-mech. x-section for neutrons '/ 
  /' (approximation by KHS) '/
  /' Y = SQRT(X) * EXP(-X/T) '/
      Dim As Single EN
      EN = 2.E0 * R_T * Sqr(Log(Rnd) * Log(Rnd))
      PMaxwellv = EN
End Function      

Public Function PMaxwellMod(R_T As Single,R_A As Single) As Single
  /' Random generator according to a distribution similar to a '/
  /' Maxwell distribution with quantum-mech. x-section for neutrons '/ 
  /' (approximation by KHS) '/
  /' Y = SQRT(X) * EXP(-X/T) '/
      Dim As Single EN
      If RND < 3.3 / sqr(R_A) Then  ' according to PR-116-683 (Dostrowsky et al.)
        EN = 2.E0 * R_T * Sqr(Log(Rnd) * Log(Rnd))
      Else
        EN = -R_T * (Log(Rnd) + Log(Rnd))
      End If  
      PMaxwellMod = EN
End Function

Public Function Floor(R As Single) As Single
	Floor = Int(R)
End Function

Public Function Ceil(R As Single) As Single
    If R > Floor(R) Then 
      Ceil = Floor(R) + 1
    Else 
      Ceil = R
    End If  
End Function

Public Function CC_Count(CIn As String, CDiv As String) As Integer
  /' Returns the number of pieces in a string '/
  Dim As Integer I = 0, N
  Dim As String CRest
  CRest = Trim(CIn)  ' remove leading and trailing blanks
  N = 0
  If CRest = "" Then
    Return N
  End If
  I = Instr(CRest,CDiv)
  If I = 0 then
     N = 1
     Return N
  Else
     While I > 0
        N = N + 1
        CRest = Mid(CRest,I+Len(CDiv))
        CRest = Trim(CRest)
        I = Instr(CRest,CDiv)
     Wend 
     N = N + 1
     Return N
  End If  
End Function

Public Sub CC_Cut(CIn As String, CDiv As String, COut() As String, _
               ByRef N As Integer) 
  /' Cuts a string into pieces with CDiv as divider '/ 
  /' Special treatment for blank as devider: 
       multiple blanks count as one divider '/              
  Dim As Integer I = 0
  Dim As String CRest
  CRest = Trim(CIn)  ' remove leading and trailing blanks
  N = 0
  If CRest = "" Then
    Return
  End If
  I = Instr(CRest,CDiv)
  If I = 0 then
     N = 1
     COut(1) = CIn
     Return
  Else
     While I > 0
        N = N + 1
        If N > Ubound(COut) Then 
          N = N - 1
          Print "<E> Dimension of COut too small in CC_cut(";CIn;")" 
        End if
        COut(N) = Mid(CRest,1,I-1)
        CRest = Mid(CRest,I+Len(CDiv))
        CRest = Trim(CRest)
        I = Instr(CRest,CDiv)
     Wend 
     N = N + 1
     If N > Ubound(COut) Then
          N = N - 1
          Print "<E> Dimension of COut too small in CC_cut(";CIn;")" 
     End if
     COut(N) = CRest
     Return
  End if
End Sub

Public Function Round(R As Single, N As Integer) As Single
 ' R   Input value
 ' N   Number of significant digits
  Dim As Single RN10, Rred, Rextended, Rrounded, Rout, Rabs
  Dim As Integer N10,Isign
  If R = 0 Then
    Round = 0
  Else
    Isign = Sgn(R)
    Rabs = Abs(R)
    N10 = Int(Log10(Rabs))
    RN10 = 10^N10
    Rred = Rabs / RN10
    Rextended = Rred * 10^(N-1)
    Rrounded = Fix(Rextended + 0.5)
    Rout = Rrounded / 10^(N-1) * RN10
    Round = Isign * Rout
  End If  
End Function

Function Modulo(I As ULongint, J As ULongint) As Longint
  Dim As ULongint Iratio,Iresult
  Iratio = I \ J
  Iresult = I - J * Iratio
  Modulo = Iresult
End Function

Function PLoss(IL As Ulongint) As Integer
  ' Extracts and returns number of prompt protons 
  Dim As String Ctest
  Dim As Integer I,NP
  Ctest = Oct(IL)
  NP = 0
  For I = 1 To Len(Ctest)
    If Mid(Ctest,I,1) = "2" Or Mid(Ctest,I,1) = "4" Then NP = NP + 1
  Next
  PLoss = NP
End Function

#Ifdef B_EgammaA
Sub StoreEgammaA(Egamma1keV As Integer,Apre As Integer)
  ' Accumulate 2-dimensional E-gamma vs. A-pre (different bin sizes)
  Dim As Integer Ichannel
  Ichannel = Int(Egamma1keV * 0.5)
  If Apre >= Lbound(EgammaA2,2) And Apre <= Ubound(EgammaA2,2) Then
    If Ichannel >= Lbound(EgammaA2,1) And Ichannel <= Ubound(EgammaA2,1) Then 
      EgammaA2(Ichannel,Apre) = EgammaA2(Ichannel,Apre) + 1
    End If  
    Ichannel = Int(Egamma1keV * 0.1)
    If Ichannel >= Lbound(EgammaA10,1) And Ichannel <= Ubound(EgammaA10,1) Then 
      EgammaA10(Ichannel,Apre) = EgammaA10(Ichannel,Apre) + 1
    End If  
    Ichannel = Int(Egamma1keV * 0.01)
    If Ichannel >= Lbound(EgammaA100,1) And Ichannel <= Ubound(EgammaA100,1) Then 
      EgammaA100(Ichannel,Apre) = EgammaA100(Ichannel,Apre) + 1
    End If  
    Ichannel = Int(Egamma1keV * 0.001)
    If Ichannel >= Lbound(EgammaA1000,1) And Ichannel <= Ubound(EgammaA1000,1) Then 
      EgammaA1000(Ichannel,Apre) = EgammaA1000(Ichannel,Apre) + 1
    End If
  End If    
End Sub
#Endif

Sub Printcomments
   Dim As Integer IZ,IA

   Print #f,"  <Comments>"
   Print #f,"Further comments:"
   Print #f," "
   Print #f,"Yields are given in percent, normalized to 200%."
   Print #f,"The lowest yields from this Monte-Carlo calculation"
   Print #f,"are unavoidably subject to large statistical fluctuations."
   Print #f," "
   If B_Error_On = 1 Then 
   Print #f,"Uncertainties and covariances or correlation coefficients,"
   Print #f,"if required, are determined by calculations with perturbed"
   Print #f,"parameters. They only consider the uncertainties of the"
   Print #f,"description of the fission process. Uncertainties of more" 
   Print #f,"general ingredients, for example the description of the "
   Print #f,"level density, the gamma strength function and others that"
   Print #f,"are common to all systems are not included. Therefore, the" 
   Print #f,"uncertainties given above are suited for comparing different"
   Print #f,"systems, but they may be underestimated on an absolute scale."
   Print #f," "
   End If
   Print #f,"Small shell effects in the symmetric fission channel influence"
   Print #f,"the relative weight of this channel in the lighter actinides."
   Print #f,"Values different from zero were deduced for several nuclei from measured"
   Print #f,"mass distributions (see table below). They are included in the code. For"
   Print #f,"other systems, a larger uncertainty on the relative weight of the symmetric"
   Print #f,"channel is exptected."
   Print #f," "
   Print #f," Z            A_CN     Shell effect at symmetry"
   For IZ = 80 To 100
     For IA = 160 To 300
       If IZ <> 94 Then
         If Abs(U_Delta_S0(IZ,IA) - 0.3) > 0.001 Then
           Print #f, IZ, IA, U_Delta_S0(IZ,IA)
         End If
       End If  
       If IZ = 94 Then
         If Abs(U_Delta_S0(IZ,IA) - 0.25) > 0.001 Then
           Print #f, IZ, IA, U_Delta_S0(IZ,IA)
         End If
       End If       
     Next IA
   Next IZ
   Print #f, " "
   Print #f, "Values not given in the table are +0.25 MeV for Pu and +0.3 MeV for all other nuclei. "
   Print #f, " "
   Print #f, "Note that A_CN denotes the mass of the compound nucleus!"
   Print #f, "In the case of neutron-induced fission, this differs from the mass of the target nucleus."
   Print #f,"  </Comments>"


	
End Sub
