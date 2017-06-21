
 '    Dim Shared As Single _POLARadd = 0.32 /' Offset for enhanced polarization '/  
    Dim Shared As Single  ZC_Mode_4L = 41.90  ' enhances S1  
    Dim Shared As Single P_Z_Curvmod_S1 = 1.75    /' Scales energy-dependent shift '/ 
    Dim Shared As Single S2leftmod = 0.55      /' Asymmetry in diffuseness of S2 mass peak '/ 
      Dim Shared As Single S2leftmod_global = 0.6
    Dim Shared As Single P_Z_Curvmod_S2 = 10   /' Scales energy-dependent shift '/
  '  Dim Shared As Single _P_Z_Curv_S3 = 0.076
  '  Dim Shared As Single _P_Z_Curv_S4 = 0.025  /' Curvature in Z of Mode 4 '/
    Dim Shared As Single P_Z_Curvmod_S3 = 10   /' Scales energy-dependent shift '/
    Dim Shared As Single P_Z_Curv_SL4 = 0.28 
'Dim Shared As Single P_Z_Curv_SL4 = 0.2    
    Dim Shared As Single P_Z_Curvmod_S4 = 10   /' Scales energy-dependent shift '/
    Dim Shared As Single P_Shell_SL4 = -1.3   /' Shell enhancing S1 '/
    Dim Shared As Single PZ_S3_olap_pos = 39.7    /' Pos. of S3 shell in light fragment (in Z) '/
   ' Dim Shared As Single PZ_S3_olap_curv = 0.008 /' for width of S3 shell in light fragment '/
    Dim Shared As Single PZ_S3_olap_curv = 0.0065
    Dim Shared As Single Level_S11 = -1.3         /' Level for mode S11 '/
    Dim Shared As Single Shell_fading = 50   /' fading of shell effect with E* '/
    Dim Shared As Single T_low_S11 = 0.36     /' Slope parameter for tunneling '/
    Dim Shared As Single _P_att_pol = 4.5     /' Attenuation of 132Sn shell '/
    Dim Shared As Single dE_Defo_S1 = -2.8    /' Deformation energy expense for Mode 1 '/
    Dim Shared As Single dE_Defo_S2 = 0       /' Deformation energy expense for Mode 2 '/
    Dim Shared As Single dE_Defo_S3 = 0       /' Deformation energy expense for Mode 3 '/
    Dim Shared As Single dE_Defo_S4 = 0      
     /' Deformation energy expense for Mode 4 '/
'    Dim Shared As Single betaL0 = 24.5
'    Dim Shared As Single betaL1 = 0.65 
    Dim Shared As Single betaL0 = 25.9
    Dim Shared As Single betaL1 = 0.72
'    Dim Shared As Single betaL0 = 25.4  ' last value
'    Dim Shared As Single betaL1 = 0.69  ' last value
    Dim Shared As Single betaH0 = 48.0    /' Offset for deformation of heavy fragment '/
    Dim Shared As Single betaH1 = 0.55
    Dim Shared As Single kappa = 0     /' N/Z dedendence of A-asym. potential '/
    Dim Shared As Single TCOLLFRAC = 0.04     /' Tcoll per energy gain from saddle to scission '/
    Dim Shared As Single ECOLLFRAC = 0.055  'last value
'  Dim Shared As Single ECOLLFRAC = 0.1    
    Dim Shared As Single TFCOLL = 0.034  
    Dim Shared As Single TCOLLMIN = 0.12
'    Dim Shared As Single ESHIFTSASCI_intr = -58   /' Shift of saddle-scission energy '/ ' last value
    Dim Shared As Single ESHIFTSASCI_intr = -67  /' Shift of saddle-scission energy '/ 
    Dim Shared As Single ESHIFTSASCI_coll = -90   /' Shift of saddle-scission energy '/
    Dim Shared As Single ESHIFTSASCI_coll_global = -90 
    Dim Shared As Single EDISSFRAC = 0.35
    Dim Shared As Single SIGDEFO = 0.165  
    Dim Shared As Single SIGDEFO_0 = 0.165
    Dim Shared As Single SIGDEFO_slope = 0
    Dim Shared As Single EexcSIGrel = 0.7
    Dim Shared As Single DNECK = 1            /' Tip distance at scission / fm '/
    Dim Shared As Single FTRUNC50 = 1         /' Truncation near Z = 50 '/
    Dim Shared As Single ZTRUNC50 = 50        /' Z value for truncation '/
    Dim Shared As Single FTRUNC28 = 0.56      /' Truncation near Z = 28 '/
    Dim Shared As Single ZTRUNC28 = 30.5      /' Z value for truncation '/
    Dim Shared As Single ZMAX_S2 = 60         /' Maximum Z of S2 channel in light fragment '/
    Dim Shared As Single NTRANSFEREO = 6      /' Steps for E sorting for even-odd effect '/
    Dim Shared As Single NTRANSFERE = 12      /' Steps for E sorting for energy division '/
    Dim Shared As Single Csort = 0.1          /' Smoothing of energy sorting '/
    Dim Shared As Single PZ_EO_symm = 2.25    /' Even-odd effect in Z at symmetry '/
    Dim Shared As Single PN_EO_Symm = 0.5     /' Even-odd effect in N at symmetry '/
    Dim Shared As Single R_EO_THRESH = 0.04   /' Threshold for asymmetry-driven even-odd effect'/
    Dim Shared As Single R_EO_SIGMA = 0.35
    Dim Shared As Single R_EO_MAX = 0.40      /' Maximum even-odd effect '/
    Dim Shared As Single POLARfac = 1  /' Enhancement of polarization of ligu. drop '/
    Dim Shared As Single T_POL_RED = 0.01  /' Reduction of temperature for sigma(Z) '/
    Dim Shared As Single ZPOL1 = 0           /' Extra charge polarization of S1 '/
    Dim Shared As Single P_n_x = 0     /' Enhanced inverse neutron x section '/
    Dim Shared As Single Tscale = 1
    Dim Shared As Single EOscale = 1.0  /' Scaling factor for even-odd structure in yields '/
    Dim Shared As Single Econd = 2   
    Dim Shared As Integer Emode = 1      /' 0: E over BF_B; 1: E over gs; 2: E_neutron; 12: E_proton '/
    Dim Shared As Single T_orbital = 0  /' From orbital ang. momentum '/
    Dim Shared As Single _Jscaling = 1.0   /' General scaling of fragment angular momenta '/
    Dim Shared As Single Spin_odd = 0.4  /' RMS Spin enhancement for odd Z '/ 
/'>'/                                    /' Value of 0.4 adjusted to data. In conflict with Naik! '/


StartAgain:  

    Redim Shared Anl_Par(1000) As Analyzer_Attributes
    I_Anl = 0
   

/'<'/

  /' I. Properties of nuclide distributions '/

    ReDim Shared Beta(0 To 6,1 To 2,150) As Single
    /'<FO Beta = 0.0 FO>'/
/'>'/
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Beta"
    Anl_Par(I_Anl).C_Title = "Mean fragment deformation at scission"
    Anl_Par(I_Anl).C_xaxis = "Atomic number"
    Anl_Par(I_Anl).C_yaxis = "beta"
    Anl_Par(I_Anl).C_Linesymbol = "LT0"  
    Anl_Par(I_Anl).R_ALim(2,1) = 1   ' first bin (light/heavy fragment)
/'<'/

    ReDim Shared Edefo(0 To 4,1 To 2,150) As Single
/'>'/
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Edefo"
    Anl_Par(I_Anl).C_Title = "Fragment deformation energy at scission"
    Anl_Par(I_Anl).C_xaxis = "Atomic number"
    Anl_Par(I_Anl).C_yaxis = "E / MeV"
    Anl_Par(I_Anl).C_Linesymbol = "LT0"  
    Anl_Par(I_Anl).R_ALim(2,1) = 1   ' first bin (light/heavy fragment)
/'<'/

    ReDim Shared Zmean(0 To 4,1 To 2,350) As Single
/'>'/
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Zmean"
    Anl_Par(I_Anl).C_Title = "Mean Z at scission"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "Zm$e$a$n$"
    Anl_Par(I_Anl).C_Linesymbol = "LT0"  
    Anl_Par(I_Anl).R_ALim(2,1) = 1   ' first bin (light/heavy fragment)
/'<'/

    ReDim Shared Zshift(0 To 4,1 To 2,350) As Single
/'>'/
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Zshift"
    Anl_Par(I_Anl).C_Title = "Z polarisation at scission"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "Zm$e$a$n$ - ZU$C$D$"
    Anl_Par(I_Anl).C_Linesymbol = "LT0"  
    Anl_Par(I_Anl).R_ALim(2,1) = 1   ' first bin (light/heavy fragment)
/'<'/

    ReDim Shared Temp(0 To 4,1 To 2,350) As Single
/'>'/
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Temp"
    Anl_Par(I_Anl).C_Title = "Nuclear temperature (level-density parameter)"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "T / MeV"
    Anl_Par(I_Anl).C_Linesymbol = "LT0"  
    Anl_Par(I_Anl).R_ALim(2,1) = 1   ' first bin (light/heavy fragment)
/'<'/

    ReDim Shared TempFF(0 To 4,1 To 2,350) As Single
/'>'/
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "TempFF"
    Anl_Par(I_Anl).C_Title = "Nuclear temperature (level-density parameter) of FF"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "T / MeV"
    Anl_Par(I_Anl).C_Linesymbol = "LT0"  
    Anl_Par(I_Anl).R_ALim(2,1) = 1   ' first bin (light/heavy fragment)
/'<'/

    ReDim Shared Eshell(0 To 4,1 To 2,350) As Single
/'>'/
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Eshell"
    Anl_Par(I_Anl).C_Title = "Local shell effect over pre-neutron mass"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "de^U / MeV"
    Anl_Par(I_Anl).C_Linesymbol = "LT0"  
    Anl_Par(I_Anl).R_ALim(2,1) = 1   ' first bin (light/heavy fragment)
/'<'/

    ReDim Shared PEOZ(0 To 6,1 To 2,350) As Single
/'>'/
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "PEOZ"
    Anl_Par(I_Anl).C_Title = "Local even-odd effect in Z"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "de^P"
    Anl_Par(I_Anl).C_Linesymbol = "LT0"  
    Anl_Par(I_Anl).R_ALim(2,1) = 1   ' first bin (light/heavy fragment)
/'<'/

    ReDim Shared PEON(0 To 6,1 To 2,350) As Single   ' pre-neutron evaporation
/'>'/
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "PEON"
    Anl_Par(I_Anl).C_Title = "Local even-odd effect in N"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "de^N"
    Anl_Par(I_Anl).C_Linesymbol = "LT0"  
    Anl_Par(I_Anl).R_ALim(2,1) = 1   ' first bin (light/heavy fragment)
/'<'/

    ReDim Shared EPART(0 To 6,1 To 2,350) As Single
    /'<FO EPART = 0.0 FO>'/
/'>'/
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "EPART"
    Anl_Par(I_Anl).C_Title = "Energy partition"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "Mean fragment intrinsic excitation energy at scission / MeV"
    Anl_Par(I_Anl).C_Linesymbol = "LT0"  
    Anl_Par(I_Anl).R_ALim(2,1) = 1   ' first bin (light/heavy fragment)
/'<'/
                               
    Redim Shared SpinRMSNZ(0 To 6,1 To 2,1 To 200,1 To 150) As Single
/'>'/
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "SpinRMSNZ"
    Anl_Par(I_Anl).C_Title = "RMS spin of fragments"
    Anl_Par(I_Anl).C_xaxis = "Neutron number"
    Anl_Par(I_Anl).C_yaxis = "Atomic number"
    Anl_Par(I_Anl).I_Dim = 2
    Anl_Par(I_Anl).R_ALim(2,1) = 1   ' first bin (light/heavy fragment)
    Anl_Par(I_Anl).R_ALim(3,1) = 1   ' first bin (neutron number)
    Anl_Par(I_Anl).R_ALim(4,1) = 1   ' first bin (atomic number)
/'<'/
                               

  /' Masses etc. '/
                               
    ReDim Shared BEldmTF(0 To 203,0 To 136) As Single
/'>'/
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "BEldmTF"
    Anl_Par(I_Anl).C_Title = "Liquid-drop mass (-BE)"
    Anl_Par(I_Anl).C_xaxis = "Neutron number"
    Anl_Par(I_Anl).C_yaxis = "Atomic number"
    Anl_Par(I_Anl).I_Dim = 2
/'<'/

    ReDim Shared BEexp(0 To 203,0 To 136) As Single
/'>'/
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "BEexp"
    Anl_Par(I_Anl).C_Title = "Experimental mass (-BE)"
    Anl_Par(I_Anl).C_xaxis = "Neutron number"
    Anl_Par(I_Anl).C_yaxis = "Atomic number"
    Anl_Par(I_Anl).I_Dim = 2
/'<'/

    Redim Shared DEFOtab(0 To 203,0 To 136) As Single
/'>'/
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "be^g$s$ (Moeller)"
    Anl_Par(I_Anl).C_Title = "Nuclear deformations (Moeller)"
    Anl_Par(I_Anl).C_xaxis = "Neutron number"
    Anl_Par(I_Anl).C_yaxis = "Atomic number"
    Anl_Par(I_Anl).I_Dim = 2
/'<'/    
                               
    ReDim Shared ShellMO(0 To 203,0 To 136) As Single
/'>'/
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ShellMO"
    Anl_Par(I_Anl).C_Title = "Shell effect"
    Anl_Par(I_Anl).C_xaxis = "Neutron number"
    Anl_Par(I_Anl).C_yaxis = "Atomic number"
    Anl_Par(I_Anl).I_Dim = 2
/'<'/

    ReDim Shared EVOD(0 To 203,0 To 136) As Single
/'>'/
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "EVOD"
    Anl_Par(I_Anl).C_Title = "Even-odd fluctuating binding energy"
    Anl_Par(I_Anl).C_xaxis = "Neutron number"
    Anl_Par(I_Anl).C_yaxis = "Atomic number"
    Anl_Par(I_Anl).I_Dim = 2
/'<'/

/'>'/
    #include "Spectra.bas" 
    #include once "DCLplotting.bas"   ' For plotting and Chi-square  
/'<'/

    ReDim Shared NZPRE(0 to 200,0 to 150) As Single 
/'>'/
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "NZPRE"
    Anl_Par(I_Anl).C_Title = "Nuclide distribution, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Neutron number"
    Anl_Par(I_Anl).C_yaxis = "Atomic number"
    Anl_Par(I_Anl).I_Dim = 2
/'<'/
                               
    ReDim Shared NZMPRE(0 To 6,0 to 200,0 to 150) As Single 
/'>'/
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "NZMPRE"
    Anl_Par(I_Anl).C_Title = "Nuclide distribution of modes, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Neutron number"
    Anl_Par(I_Anl).C_yaxis = "Atomic number"
    Anl_Par(I_Anl).I_Dim = 2
    
    N_Anl = I_Anl   ' Total number of analyzers

    ' Input/Return parameters of subroutine Eva:
    ReDim Shared As Single Array_En_light(50)  ' neutron energy array
    ReDim Shared As Single Array_Vn_light_long(50)  ' neutron velocity array longitudinal
    ReDim Shared As Single Array_Vn_light_perp(50)  ' neutron velocity array perpendicular
    ReDim Shared As Single Array_En_heavy(50)  ' neutron energy array
    ReDim Shared As Single Array_Vn_heavy_long(50)  ' neutron velocity array longitudinal
    ReDim Shared As Single Array_Vn_heavy_perp(50)  ' neutron velocity array perpendicular
    ReDim Shared As Single Array_Tn(50)  ' neutron decay times after scission
    ReDim Shared As Single Array_Eg0_light(100)  ' statistical gamma energy array
    ReDim Shared As Single Array_Eg0_heavy(100)  ' statistical gamma energy array
    
    ' For list-mode output:
    ReDim Shared As Single Array_En_post(100)  ' neutron energy array in lab system
    Dim As Integer In_post 
    ReDim Shared As Single Array_Eg1_light(100) ' statistical gamma energy array
    ReDim Shared As Single Array_Eg1_heavy(100) ' statistical gamma energy array
    Dim As Integer Ig1_light, Ig1_heavy         ' after prompt-neutron emission
    ReDim Shared As Single Array_Eg2_light(100) ' collective gamma energy array
    ReDim Shared As Single Array_Eg2_heavy(100) ' collective gamma energy array
    Dim As Integer Ig2_light, Ig2_heavy         
    
