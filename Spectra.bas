  /' Calculated spectra: '/

  /' II. Nuclide distributions '/

    ReDim ZPROV(150) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ZPROV"
    Anl_Par(I_Anl).C_Title = "Provisional element distribution"
    Anl_Par(I_Anl).C_xaxis = "Atomic number"
    Anl_Par(I_Anl).C_yaxis = "Yield"
    Anl_Par(I_Anl).C_Linesymbol = "LTR11"  

    ReDim ZMPROV(0 To 6,150) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ZMPROV"
    Anl_Par(I_Anl).C_Title = "Provisional element distribution"
    Anl_Par(I_Anl).C_xaxis = "Atomic number"
    Anl_Par(I_Anl).C_yaxis = "Yield"
    Anl_Par(I_Anl).C_Linesymbol = "RBT1"  

    ReDim Shared NZPOST(200,150) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "NZPOST"
    Anl_Par(I_Anl).C_Title = "Nuclide distribution, post-neutron"
    Anl_Par(I_Anl).C_xaxis = "Neutron number"
    Anl_Par(I_Anl).C_yaxis = "Atomic number"

    ReDim ZPOST(150) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ZPOST"
    Anl_Par(I_Anl).C_Title = "Element distribution, post-neutron"
    Anl_Par(I_Anl).C_xaxis = "Atomic number"
    Anl_Par(I_Anl).C_yaxis = "Yield"
    Anl_Par(I_Anl).C_Linesymbol = "RT11"  

    ReDim ZMPOST(0 To 6,150) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ZMPOST"
    Anl_Par(I_Anl).C_Title = "Element distribution, post-neutron"
    Anl_Par(I_Anl).C_xaxis = "Atomic number"
    Anl_Par(I_Anl).C_yaxis = "Yield"
    Anl_Par(I_Anl).C_Linesymbol = "RBT1"  

    ReDim NPRE(200) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "NPRE"
    Anl_Par(I_Anl).C_Title = "Neutron-number distribution, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Neutron number"
    Anl_Par(I_Anl).C_yaxis = "Yield"
    Anl_Par(I_Anl).C_Linesymbol = "RT11"  

    ReDim NMPRE(0 To 6,200) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "NMPRE"
    Anl_Par(I_Anl).C_Title = "Neutron-number distribution, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Neutron number"
    Anl_Par(I_Anl).C_yaxis = "Yield"
    Anl_Par(I_Anl).C_Linesymbol = "RBT1"  

    ReDim NPOST(200) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "NPOST"
    Anl_Par(I_Anl).C_Title = "Neutron-number distribution, post-neutron"
    Anl_Par(I_Anl).C_xaxis = "Neutron number"
    Anl_Par(I_Anl).C_yaxis = "Yield"
    Anl_Par(I_Anl).C_Linesymbol = "RT11"  
    
    ReDim NMPOST(0 To 6,200) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "NMPOST"
    Anl_Par(I_Anl).C_Title = "Neutron-number distribution, post-neutron"
    Anl_Par(I_Anl).C_xaxis = "Neutron number"
    Anl_Par(I_Anl).C_yaxis = "Yield"
    Anl_Par(I_Anl).C_Linesymbol = "RBT1"  

    ReDim Shared APOST(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "APOST"
    Anl_Par(I_Anl).C_Title = "Mass distribution, post-neutron"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "Yield"
    Anl_Par(I_Anl).C_Linesymbol = "BT11"  

    ReDim AMPOST(0 To 6,350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "AMPOST"
    Anl_Par(I_Anl).C_Title = "Mass distribution, post-neutron"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "Yield"
    Anl_Par(I_Anl).C_Linesymbol = "GT1"  

    ReDim APROV(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "APROV"
    Anl_Par(I_Anl).C_Title = "Provisional mass distribution, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "Yield"
    Anl_Par(I_Anl).C_Linesymbol = "GT11"  

    ReDim AMPROV(0 To 6,350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "AMPROV"
    Anl_Par(I_Anl).C_Title = "Provisional mass distribution, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "Yield"
    Anl_Par(I_Anl).C_Linesymbol = "GT1"  

    ReDim APRE(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "APRE"
    Anl_Par(I_Anl).C_Title = "Mass distribution, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "Yield"
    Anl_Par(I_Anl).C_Linesymbol = "GT11"  

    ReDim AMPRE(0 To 6,350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "AMPRE"
    Anl_Par(I_Anl).C_Title = "Mass distribution, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "Yield"
    Anl_Par(I_Anl).C_Linesymbol = "GT1"  

    ReDim ZISOPRE(350,150) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ZISOPRE"
    Anl_Par(I_Anl).C_Title = "Isobaric Z distribution for A = &, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Atomic number"
    Anl_Par(I_Anl).C_yaxis = "Yield"
    Anl_Par(I_Anl).C_Linesymbol = "LRT11"  

    ReDim ZISOPOST(350,150) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ZISOPOST"
    Anl_Par(I_Anl).C_Title = "Isobaric Z distribution for A = &, post-neutron"
    Anl_Par(I_Anl).C_xaxis = "Atomic number"
    Anl_Par(I_Anl).C_yaxis = "Yield"
    Anl_Par(I_Anl).C_Linesymbol = "LRT11"  

    ReDim SigmaZpre(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "SigmaZpre"
    Anl_Par(I_Anl).C_Title = "Sigma of isobaric Z distribution"
    Anl_Par(I_Anl).C_xaxis = "Mass number, pre-neutron"
    Anl_Par(I_Anl).C_yaxis = "si^Z$"
    Anl_Par(I_Anl).C_Linesymbol = "LRT11"  

    ReDim SigmaZpost(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "SigmaZpost"
    Anl_Par(I_Anl).C_Title = "Sigma of isobaric Z distribution"
    Anl_Par(I_Anl).C_xaxis = "Mass number, post-neutron"
    Anl_Par(I_Anl).C_yaxis = "si^Z$"
    Anl_Par(I_Anl).C_Linesymbol = "LRT11"  

    ReDim Zpolarpre(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Zpolarpre"
    Anl_Par(I_Anl).C_Title = "Charge polarisation"
    Anl_Par(I_Anl).C_xaxis = "Mass number, pre-neutron"
    Anl_Par(I_Anl).C_yaxis = "Zb$a$r$ - ZU$C$D$"
    Anl_Par(I_Anl).C_Linesymbol = "LRT11"  
                               
    ReDim Zpolarmac(350) As Double                            
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Zpolarmac"
    Anl_Par(I_Anl).C_Title = "Charge polarisation, macroscopic"
    Anl_Par(I_Anl).C_xaxis = "Mass number, pre-neutron"
    Anl_Par(I_Anl).C_yaxis = "Zb$a$r$ - ZU$C$D$"
    Anl_Par(I_Anl).C_Linesymbol = "LGT11"  

    ReDim Zpolarpost(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Zpolarpost"
    Anl_Par(I_Anl).C_Title = "Charge polarisation"
    Anl_Par(I_Anl).C_xaxis = "Mass number, post-neutron"
    Anl_Par(I_Anl).C_yaxis = "Zb$a$r$ - ZU$C$D$"
    Anl_Par(I_Anl).C_Linesymbol = "LRT11"  

    ReDim Edefo2d(350,800) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Edefo2d"
    Anl_Par(I_Anl).C_Title = "Deformation energy over mass number, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "Ed$e$f$o$ / MeV"
    Anl_Par(I_Anl).R_Alim(2,3) = 0.1
    Anl_Par(I_Anl).I_Dim = 2
    
    Redim EdefoA(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "EdefoA"
    Anl_Par(I_Anl).C_Title = "Mean deformation energy versus mass number, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "<Ed$e$f$o$> / MeV"
    Anl_Par(I_Anl).R_Alim(1,3) = 1
    Anl_Par(I_Anl).C_Linesymbol = "LGT11"

    Redim JFRAGpre(200,150,0 To 300) As Double  ' Array of N and Z
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "JFRAGpre"
    Anl_Par(I_Anl).C_Title = "Fragment spin distribution, unit 1 hb^, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "J / hb^"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "LTR11"  

    Redim JFRAGpost(200,150,0 To 300) As Double  ' Array of N and Z
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "JFRAGpost"
    Anl_Par(I_Anl).C_Title = "Fragment spin distribution, unit 1 hb^, post-neutron"
    Anl_Par(I_Anl).C_xaxis = "J / hb^"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "LTR11"  

    ReDim Eintr2d(350,1000) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Eintr2d"
    Anl_Par(I_Anl).C_Title = "Intrinsic energy over mass number, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "Ei$n$t$r$ / MeV"
    Anl_Par(I_Anl).R_Alim(2,3) = 0.1
    Anl_Par(I_Anl).I_Dim = 2

    Redim EintrA(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "EintrA"
    Anl_Par(I_Anl).C_Title = "Mean intrinsic energy versus mass number, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "<Ei$n$t$r$> / MeV"
    Anl_Par(I_Anl).C_Linesymbol = "LGT11"

    ReDim Ecoll2d(350,500) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Ecoll2d"
    Anl_Par(I_Anl).C_Title = "Collective energy over mass number, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "Ec$o$l$l$ / MeV"
    Anl_Par(I_Anl).R_Alim(2,3) = 0.1
    Anl_Par(I_Anl).I_Dim = 2

    Redim EcollA(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "EcollA"
    Anl_Par(I_Anl).C_Title = "Mean collective energy versus mass number, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "<Ec$o$l$l$> / MeV"
    Anl_Par(I_Anl).R_Alim(1,3) = 1
    Anl_Par(I_Anl).C_Linesymbol = "LGT11"

    ReDim Eexc2d(350,1000) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Eexc2d"
    Anl_Par(I_Anl).C_Title = "Excitation energy over mass number, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "Ee$x$c$ / MeV"
    Anl_Par(I_Anl).C_Linesymbol = "LTR11"  
    Anl_Par(I_Anl).R_Alim(2,3) = 0.1
    Anl_Par(I_Anl).I_Dim = 2

    ReDim N2dpre(350,20) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "N2dpre"
    Anl_Par(I_Anl).C_Title = "Prompt-neutron multiplicity over pre-neutron mass"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "Neutron multiplicity"
    Anl_Par(I_Anl).I_Dim = 2

    ReDim N2dpost(350,20) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "N2dpost"
    Anl_Par(I_Anl).C_Title = "Prompt-neutron multiplicity over post-neutron mass"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "Neutron multiplicity"
    Anl_Par(I_Anl).I_Dim = 2
                               
    ReDim NApre(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "NApre"
    Anl_Par(I_Anl).C_Title = "Mean prompt neutron multiplicity as a function of pre-neutron mass"
    Anl_Par(I_Anl).C_xaxis = "Mass number, pre-neutron"
    Anl_Par(I_Anl).C_yaxis = "Mean neutron multiplicity"
    Anl_Par(I_Anl).C_Linesymbol = "LTR11"  
    
    ReDim NApost(350) As Double                           
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "NApost"
    Anl_Par(I_Anl).C_Title = "Mean prompt neutron multiplicity as a function of post-neutron mass"
    Anl_Par(I_Anl).C_xaxis = "Mass number, post-neutron"
    Anl_Par(I_Anl).C_yaxis = "Mean neutron multiplicity"
    Anl_Par(I_Anl).C_Linesymbol = "LTR11"  

    ReDim EPCN(100000) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "EPCN"
    Anl_Par(I_Anl).C_Title = "Prompt proton-energy spectrum from CN"
    Anl_Par(I_Anl).C_xaxis = "Energy / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTR0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.001
                               
 /' ReDim EN(1000) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "EN"
    Anl_Par(I_Anl).C_Title = "Overall prompt neutron-energy spectrum"
    Anl_Par(I_Anl).C_xaxis = "Energy / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTR0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.1 '/
                               
    ReDim ENCN(100000) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ENCN"
    Anl_Par(I_Anl).C_Title = "Prompt neutron-energy spectrum from CN"
    Anl_Par(I_Anl).C_xaxis = "Energy / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTRB0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.001
    
    ReDim ENsci(100000) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ENsci"
    Anl_Par(I_Anl).C_Title = "Prompt neutron-energy spectrum - between saddle and scission"
    Anl_Par(I_Anl).C_xaxis = "Energy / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTRB0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.001
    
    ReDim ENCNtest(1000) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ENCNtest"
    Anl_Par(I_Anl).C_Title = "Prompt neutron-energy spectrum from CN"
    Anl_Par(I_Anl).C_xaxis = "Energy / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTRB0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.1
    
    ReDim ENfr(100000) As Double                           
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ENfr"
    Anl_Par(I_Anl).C_Title = "Prompt neutron-energy spectrum from fragments"
    Anl_Par(I_Anl).C_xaxis = "Energy / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTRG0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.001

    ReDim ENfrvar(303) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ENfrvar"
    Anl_Par(I_Anl).C_Title = "Prompt neutron-energy spectrum from fragments, variable binsize"
    Anl_Par(I_Anl).C_xaxis = "Energy / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTRRB0"
     ' Limits are given in eV  
    Dim As Double ENfrvar_lim(303) = _
    {0,0.00001,0.00002,0.00004,0.00006,0.00008,0.0001,0.0002,0.0004, _
     0.0006,0.0008,0.001,0.002,0.004,0.006,0.008,0.01,0.02,0.04,0.06, _
     0.08,0.1,0.2,0.4,0.6,0.8,1.0000E+00,2.0000E+00,4.0000E+00,6.0000E+00, _
     8.0000E+00,1.0000E+01,2.0000E+01,4.0000E+01,6.0000E+01,8.0000E+01, _
     1.0000E+02,2.0000E+02,4.0000E+02,6.0000E+02,8.0000E+02,1.0000E+03, _
     2.0000E+03,4.0000E+03,6.0000E+03,8.0000E+03,1.0000E+04,2.0000E+04, _
     3.0000E+04,4.0000E+04,5.0000E+04,6.0000E+04,7.0000E+04,8.0000E+04, _
     9.0000E+04,1.0000E+05,1.5000E+05,2.0000E+05,2.5000E+05,3.0000E+05, _
     3.5000E+05,4.0000E+05,4.5000E+05,5.0000E+05,5.5000E+05,6.0000E+05, _
     6.5000E+05,7.0000E+05,7.5000E+05,8.0000E+05,8.5000E+05,9.0000E+05, _
     9.5000E+05,1.0000E+06,1.0500E+06,1.1000E+06,1.1500E+06,1.2000E+06, _
     1.2500E+06,1.3000E+06,1.3500E+06,1.4000E+06,1.4500E+06,1.5000E+06, _
     1.5500E+06,1.6000E+06,1.6500E+06,1.7000E+06,1.7500E+06,1.8000E+06, _ 
     1.8500E+06,1.9000E+06,1.9500E+06,2.0000E+06,2.0500E+06,2.1000E+06, _
     2.1500E+06,2.2000E+06,2.2500E+06,2.3000E+06,2.3500E+06,2.4000E+06, _
     2.4500E+06,2.5000E+06,2.5500E+06,2.6000E+06,2.6500E+06,2.7000E+06, _
     2.7500E+06,2.8000E+06,2.8500E+06,2.9000E+06,2.9500E+06,3.0000E+06, _
     3.0500E+06,3.1000E+06,3.1500E+06,3.2000E+06,3.2500E+06,3.3000E+06, _
     3.3500E+06,3.4000E+06,3.4500E+06,3.5000E+06,3.5500E+06,3.6000E+06, _
     3.6500E+06,3.7000E+06,3.7500E+06,3.8000E+06,3.8500E+06,3.9000E+06, _
     3.9500E+06,4.0000E+06,4.0500E+06,4.1000E+06,4.1500E+06,4.2000E+06, _
     4.2500E+06,4.3000E+06,4.3500E+06,4.4000E+06,4.4500E+06,4.5000E+06, _
     4.5500E+06,4.6000E+06,4.6500E+06,4.7000E+06,4.7500E+06,4.8000E+06, _
     4.8500E+06,4.9000E+06,4.9500E+06,5.0000E+06,5.1000E+06,5.2000E+06, _
     5.3000E+06,5.4000E+06,5.5000E+06,5.6000E+06,5.7000E+06,5.8000E+06, _
     5.9000E+06,6.0000E+06,6.1000E+06,6.2000E+06,6.3000E+06,6.4000E+06, _
     6.5000E+06,6.6000E+06,6.7000E+06,6.8000E+06,6.9000E+06,7.0000E+06, _
     7.1000E+06,7.2000E+06,7.3000E+06,7.4000E+06,7.5000E+06,7.6000E+06, _
     7.7000E+06,7.8000E+06,7.9000E+06,8.0000E+06,8.1000E+06,8.2000E+06, _
     8.3000E+06,8.4000E+06,8.5000E+06,8.6000E+06,8.7000E+06,8.8000E+06, _
     8.9000E+06,9.0000E+06,9.1000E+06,9.2000E+06,9.3000E+06,9.4000E+06, _
     9.5000E+06,9.6000E+06,9.7000E+06,9.8000E+06,9.9000E+06,1.0000E+07, _
     1.0200E+07,1.0400E+07,1.0600E+07,1.0800E+07,1.1000E+07,1.1200E+07, _
     1.1400E+07,1.1600E+07,1.1800E+07,1.2000E+07,1.2200E+07,1.2400E+07, _
     1.2600E+07,1.2800E+07,1.3000E+07,1.3200E+07,1.3400E+07,1.3600E+07, _
     1.3800E+07,1.4000E+07,1.4200E+07,1.4400E+07,1.4600E+07,1.4800E+07, _
     1.5000E+07,1.5200E+07,1.5400E+07,1.5600E+07,1.5800E+07,1.6000E+07, _
     1.6200E+07,1.6400E+07,1.6600E+07,1.6800E+07,1.7000E+07,1.7200E+07, _
     1.7400E+07,1.7600E+07,1.7800E+07,1.8000E+07,1.8200E+07,1.8400E+07, _
     1.8600E+07,1.8800E+07,1.9000E+07,1.9200E+07,1.9400E+07,1.9600E+07, _
     1.9800E+07,2.0000E+07,2.0200E+07,2.0400E+07,2.0600E+07,2.0800E+07, _
     2.1000E+07,2.1200E+07,2.1400E+07,2.1600E+07,2.1800E+07,2.2000E+07, _
     2.2200E+07,2.2400E+07,2.2600E+07,2.2800E+07,2.3000E+07,2.3200E+07, _
     2.3400E+07,2.3600E+07,2.3800E+07,2.4000E+07,2.4200E+07,2.4400E+07, _
     2.4600E+07,2.4800E+07,2.5000E+07,2.5200E+07,2.5400E+07,2.5600E+07, _
     2.5800E+07,2.6000E+07,2.6200E+07,2.6400E+07,2.6600E+07,2.6800E+07, _
     2.7000E+07,2.7200E+07,2.7400E+07,2.7600E+07,2.7800E+07,2.8000E+07, _
     2.8200E+07,2.8400E+07,2.8600E+07,2.8800E+07,2.9000E+07,2.9200E+07, _
     2.9400E+07,2.9600E+07,2.9800E+07,3.0000E+07}

    ReDim ENfrfs(100000) As Double                           
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ENfrfs"
    Anl_Par(I_Anl).C_Title = "Prompt neutron-energy spectrum from fragments in fragment frame"
    Anl_Par(I_Anl).C_xaxis = "Energy / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTGB0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.001

    ReDim ENfrC(5,5,100000) As Double 
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ENfrC"
    Anl_Par(I_Anl).C_Title = "Prompt neutron-energy spectrum from fragments for Chances (nlost, plost)"
    Anl_Par(I_Anl).C_xaxis = "Energy / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTGB0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.001
                               
    Redim ENlight(100000) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ENlight"
    Anl_Par(I_Anl).C_Title = "Prompt neutron-energy spectrum from light fragments"
    Anl_Par(I_Anl).C_xaxis = "Energy / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTRB0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.001
    
    Redim ENheavy(100000) As Double   
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ENheavy"
    Anl_Par(I_Anl).C_Title = "Prompt neutron-energy spectrum from heavy fragments"
    Anl_Par(I_Anl).C_xaxis = "Energy / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTBG0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.001
    
    Redim ENM(0 to 6,100000) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ENM"
    Anl_Par(I_Anl).C_Title = "Neutron-energy spectrum for mode &"
    Anl_Par(I_Anl).C_xaxis = "Energy / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTR0"  
    Anl_Par(I_Anl).R_Alim(2,3) = 0.001
    
    Redim ENApre2d(350,1000) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ENApre2d"
    Anl_Par(I_Anl).C_Title = "Neutron-energy spectrum over pre-neutron mass (from fragments)"
    Anl_Par(I_Anl).C_xaxis = "Apre"
    Anl_Par(I_Anl).C_yaxis = "Energy / MeV"
    Anl_Par(I_Anl).R_Alim(2,3) = 0.1
    
    Redim ENApost2d(350,1000) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ENApost2d"
    Anl_Par(I_Anl).C_Title = "Neutron-energy spectrum over post-neutron mass (from fragments)"
    Anl_Par(I_Anl).C_xaxis = "Apost"
    Anl_Par(I_Anl).C_yaxis = "Energy / MeV"
    Anl_Par(I_Anl).R_Alim(2,3) = 0.1
    
    Redim ENApre(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ENApre"
    Anl_Par(I_Anl).C_Title = "Mean neutron energy over pre-neutron mass (from fragments)"
    Anl_Par(I_Anl).C_xaxis = "Apre"
    Anl_Par(I_Anl).C_yaxis = "Energy / MeV"
    Anl_Par(I_Anl).C_Linesymbol = "HTR0"  
    Anl_Par(I_Anl).R_Alim(2,3) = 1
    
    Redim ENApost(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ENApost"
    Anl_Par(I_Anl).C_Title = "Mean neutron energy over post-neutron mass (from fragments)"
    Anl_Par(I_Anl).C_xaxis = "Apost"
    Anl_Par(I_Anl).C_yaxis = "Energy / MeV"
    Anl_Par(I_Anl).C_Linesymbol = "HTB0"  
    Anl_Par(I_Anl).R_Alim(2,3) = 1
    
    Redim ENApre2dfs(350,1000) As Double  ' fs = fragment system
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ENApre2dfs"
    Anl_Par(I_Anl).C_Title = "Neutron-energy spectrum over pre-neutron mass (from fragments in fragment frame)"
    Anl_Par(I_Anl).C_xaxis = "Apre"
    Anl_Par(I_Anl).C_yaxis = "Energy / MeV"
    Anl_Par(I_Anl).R_Alim(2,3) = 0.1
    
    Redim ENApost2dfs(350,1000) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ENApost2dfs"
    Anl_Par(I_Anl).C_Title = "Neutron-energy spectrum over post-neutron mass (from fragments in fragment frame)"
    Anl_Par(I_Anl).C_xaxis = "Apost"
    Anl_Par(I_Anl).C_yaxis = "Energy / MeV"
    Anl_Par(I_Anl).R_Alim(2,3) = 0.1
    
    Redim ENAprefs(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ENAprefs"
    Anl_Par(I_Anl).C_Title = "Mean neutron energy over pre-neutron mass (from fragments in fragment frame)"
    Anl_Par(I_Anl).C_xaxis = "Apre"
    Anl_Par(I_Anl).C_yaxis = "Energy / MeV"
    Anl_Par(I_Anl).C_Linesymbol = "HTR0"  
    Anl_Par(I_Anl).R_Alim(2,3) = 1

    Redim ENApostfs(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ENApostfs"
    Anl_Par(I_Anl).C_Title = "Mean neutron energy over post-neutron mass (from fragments in fragment frame)"
    Anl_Par(I_Anl).C_xaxis = "Apost"
    Anl_Par(I_Anl).C_yaxis = "Energy / MeV"
    Anl_Par(I_Anl).C_Linesymbol = "HTB0"  
    Anl_Par(I_Anl).R_Alim(2,3) = 1
    
    ReDim NP(50) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "NP"
    Anl_Par(I_Anl).C_Title = "Proton multiplicity"
    Anl_Par(I_Anl).C_xaxis = "Number of protons"
    Anl_Par(I_Anl).C_yaxis = "Probability"
    Anl_Par(I_Anl).C_Linesymbol = "LTR11"  
                               
    ReDim NN(50) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "NN"
    Anl_Par(I_Anl).C_Title = "Neutron multiplicity"
    Anl_Par(I_Anl).C_xaxis = "Number of neutrons"
    Anl_Par(I_Anl).C_yaxis = "Probability"
    Anl_Par(I_Anl).C_Linesymbol = "LTR11"  
                               
    ReDim NNCN(50) As Double 
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "NNCN"
    Anl_Par(I_Anl).C_Title = "Neutron multiplicity (from CN)"
    Anl_Par(I_Anl).C_xaxis = "Number of neutrons"
    Anl_Par(I_Anl).C_yaxis = "Probability"
    Anl_Par(I_Anl).C_Linesymbol = "LTR11"  
    
    ReDim NNsci(50) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "NNsci"
    Anl_Par(I_Anl).C_Title = "Neutron multiplicity (between saddle and scission)"
    Anl_Par(I_Anl).C_xaxis = "Number of neutrons"
    Anl_Par(I_Anl).C_yaxis = "Probability"
    Anl_Par(I_Anl).C_Linesymbol = "LTRB11"
    
    ReDim NNfr(50) As Double                            
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "NNfr"
    Anl_Par(I_Anl).C_Title = "Neutron multiplicity (from fragments)"
    Anl_Par(I_Anl).C_xaxis = "Number of neutrons"
    Anl_Par(I_Anl).C_yaxis = "Probability"
    Anl_Par(I_Anl).C_Linesymbol = "LTR11"  
                               
    ReDim NNlight(50) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "NNlight"
    Anl_Par(I_Anl).C_Title = "Neutron multiplicity (from light fragments)"
    Anl_Par(I_Anl).C_xaxis = "Number of neutrons"
    Anl_Par(I_Anl).C_yaxis = "Probability"
    Anl_Par(I_Anl).C_Linesymbol = "LTR11"  
                               
    ReDim NNheavy(50) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "NNheavy"
    Anl_Par(I_Anl).C_Title = "Neutron multiplicity (from heavy fragments)"
    Anl_Par(I_Anl).C_xaxis = "Number of neutrons"
    Anl_Par(I_Anl).C_yaxis = "Probability"
    Anl_Par(I_Anl).C_Linesymbol = "LTR11"  

    ReDim Ndirlight(-100 To 100) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Ndirlight"
    Anl_Par(I_Anl).C_Title = "Neutron angle vs. light fragment (cos)"
    Anl_Par(I_Anl).C_xaxis = "cos(alpha)"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "LTR0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.01
                               
    ReDim nuTKEpre(0 To 250,0 To 20) As Double  
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "nuTKEpre"
    Anl_Par(I_Anl).C_Title = "Neutron multiplicity vs. TKE (pre-neutron)"
    Anl_Par(I_Anl).C_xaxis = "TKE / MeV"
    Anl_Par(I_Anl).C_yaxis = "nu^"
    Anl_Par(I_Anl).I_Dim = 2

    ReDim nuTKEpost(0 To 250,0 To 20) As Double  
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "nuTKEpost"
    Anl_Par(I_Anl).C_Title = "Neutron multiplicity vs. TKE (post-neutron)"
    Anl_Par(I_Anl).C_xaxis = "TKE / MeV"
    Anl_Par(I_Anl).C_yaxis = "nu^"
    Anl_Par(I_Anl).I_Dim = 2

    ReDim DPLOCAL(150) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "DPLOCAL"
    Anl_Par(I_Anl).C_Title = "Local even-odd effect in Z"
    Anl_Par(I_Anl).C_xaxis = "Z (add 0.5!)"
    Anl_Par(I_Anl).C_yaxis = "de^p$"
    Anl_Par(I_Anl).C_Linesymbol = "LTR11"  

    ReDim DNLOCAL(200) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "DNLOCAL"
    Anl_Par(I_Anl).C_Title = "Local even-odd effect in N"
    Anl_Par(I_Anl).C_xaxis = "N (add 0.5!)"
    Anl_Par(I_Anl).C_yaxis = "de^n$"
    Anl_Par(I_Anl).C_Linesymbol = "LTB11"  

    ReDim AEkinpre(200,350) As Double    ' pre-neutron
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "AEkinpre"
    Anl_Par(I_Anl).C_Title = "Ekin over Apre"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "Ek$i$n$ / MeV"
    Anl_Par(I_Anl).I_Dim = 2
    
    ReDim EkinApre(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "EkinApre"
    Anl_Par(I_Anl).C_Title = "Mean Ekin versus A, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "<Ek$i$n$> / MeV"
    
    ReDim Ekinpre(300) As Double    ' pre-neutron
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Ekinpre"
    Anl_Par(I_Anl).C_Title = "Fragment kinetic energy, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Ek$i$n$ / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTR0"  
    
    Redim EkinpreM(0 to 6,300) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "EkinpreM"
    Anl_Par(I_Anl).C_Title = "Fragment kinetic energy, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Ek$i$n$ / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTRB0"  
    
    ReDim AEkinpost(350,300) As Double    ' post-neutron
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "AEkinpost"
    Anl_Par(I_Anl).C_Title = "Ekin over Apost"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "Ek$i$n$ / MeV"
    Anl_Par(I_Anl).I_Dim = 2
                               
    ReDim EkinApost(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "EkinApost"
    Anl_Par(I_Anl).C_Title = "Mean Ekin versus A, post-neutron"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "<Ek$i$n$> / MeV"

    Redim Ekinpost(300) As Double   ' post-neutron
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Ekinpost"
    Anl_Par(I_Anl).C_Title = "Fragment kinetic energy, post-neutron"
    Anl_Par(I_Anl).C_xaxis = "Ek$i$n$ / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTB0"  
    
    Redim EkinpostM(0 to 6,300) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "EkinpostM"
    Anl_Par(I_Anl).C_Title = "Fragment kinetic energy, post-neutron"
    Anl_Par(I_Anl).C_xaxis = "Ek$i$n$ / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTRB0"  

    ReDim ATKEpre(350,300) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ATKEpre"
    Anl_Par(I_Anl).C_Title = "TKE over Apre"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "TKE / MeV"
    Anl_Par(I_Anl).I_Dim = 2
    
    ReDim ATKEpost(350,300) As Double                           
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "ATKEpost"
    Anl_Par(I_Anl).C_Title = "TKE over Apost"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "TKE / MeV"
    Anl_Par(I_Anl).I_Dim = 2
                               
    ReDim TKEpre(300) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "TKEpre"
    Anl_Par(I_Anl).C_Title = "TKE pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "TKE / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTR0"  
    
    ReDim TKEApre(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "TKEApre"
    Anl_Par(I_Anl).C_Title = "Mean TKE versus A, pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "<TKE> / MeV"
    Anl_Par(I_Anl).C_Linesymbol = "HTB0"

  '  Redim TKEApreM(0 to 6,350) As Double
  '  I_Anl = I_Anl + 1
  '  Anl_Par(I_Anl).C_Name = "TKEApreM"
  '  Anl_Par(I_Anl).C_Title = "Mean TKE versus A, pre-neutron, per mode"
  '  Anl_Par(I_Anl).C_xaxis = "Mass number"
  '  Anl_Par(I_Anl).C_yaxis = "<TKE> / MeV"
  '  Anl_Par(I_Anl).C_Linesymbol = "HTGB0"
                               
    ReDim TKEpreM(0 to 6,300) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "TKEpreM"
    Anl_Par(I_Anl).C_Title = "TKE pre-neutron"
    Anl_Par(I_Anl).C_xaxis = "TKE / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTBR0"  

    ReDim TKEpost(300) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "TKEpost"
    Anl_Par(I_Anl).C_Title = "TKE post-neutron"
    Anl_Par(I_Anl).C_xaxis = "TKE / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTR0"  

    ReDim TKEpostM(0 to 6,300) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "TKEpostM"
    Anl_Par(I_Anl).C_Title = "TKE post-neutron"
    Anl_Par(I_Anl).C_xaxis = "TKE / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTBR0"  

    ReDim TKEApost(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "TKEApost"
    Anl_Par(I_Anl).C_Title = "Mean TKE versus A, post-neutron"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "<TKE> / MeV"
    Anl_Par(I_Anl).C_Linesymbol = "HTB0"

  '  Redim TKEApostM(0 to 6,350) As Double
  '  I_Anl = I_Anl + 1
  '  Anl_Par(I_Anl).C_Name = "TKEApostM"
  '  Anl_Par(I_Anl).C_Title = "Mean TKE versus A, post-neutron, per mode"
  '  Anl_Par(I_Anl).C_xaxis = "Mass number"
  '  Anl_Par(I_Anl).C_yaxis = "<TKE> / MeV"
  '  Anl_Par(I_Anl).C_Linesymbol = "HTGB0"
                               
    ReDim Shared TotXE(300) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "TotXE"
    Anl_Par(I_Anl).C_Title = "Total excitation energy"
    Anl_Par(I_Anl).C_xaxis = "TXE / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTR0"  

    ReDim Shared Qvalues(300) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Qvalues"
    Anl_Par(I_Anl).C_Title = "Spectrum of Q values"
    Anl_Par(I_Anl).C_xaxis = "Q value / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTR0"  
                               
    ReDim Shared AQpre(350,300) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "AQpre"
    Anl_Par(I_Anl).C_Title = "Q value over Ap$r$e$"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "Q value / MeV"
    Anl_Par(I_Anl).I_Dim = 2
    
    Redim Shared QA(350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "QA"
    Anl_Par(I_Anl).C_Title = "Mean Q value versus Ap$r$e$"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "<Q value> / MeV"
    Anl_Par(I_Anl).C_Linesymbol = "HTB0"  

    ReDim Shared EGammaCN(100000) As Double    ' individual gammas
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "EgammaCN"
    Anl_Par(I_Anl).C_Title = "Prompt-gamma spectrum, pre-fission"
    Anl_Par(I_Anl).C_xaxis = "Ega^$ / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTBR0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.001

    ReDim Shared EGamma(100000) As Double    ' individual gammas
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Egamma"
    Anl_Par(I_Anl).C_Title = "Prompt-gamma spectrum"
    Anl_Par(I_Anl).C_xaxis = "Ega^$ / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTBR0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.001
    
  #Ifdef B_EgammaA   
    Redim Shared EgammaA2(999,350) As Double ' 0 to 2 MeV, Bin = 2 keV
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "EgammaA2"
    Anl_Par(I_Anl).C_Title = "Prompt-gamma spectrum from fragments vs. pre-neutron mass, bin = 1 keV"
    Anl_Par(I_Anl).C_xaxis = "Ega^$/MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTBR0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.002
    
    Redim Shared EgammaA10(200 To 499,350) As Double ' 2 to 5 MeV, Bin = 10 keV
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "EgammaA10"
    Anl_Par(I_Anl).C_Title = "Prompt-gamma spectrum from fragments vs. pre-neutron mass, bin = 10 keV"
    Anl_Par(I_Anl).C_xaxis = "Ega^$/MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTBR0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.01
    
    Redim Shared EgammaA100(50 To 99,350) As Double ' 5 to 10 MeV, Bin = 100 keV
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "EgammaA100"   
    Anl_Par(I_Anl).C_xaxis = "Ega^$/MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTBR0"  
    Anl_Par(I_Anl).C_Title = "Prompt-gamma spectrum from fragemnts vs. pre-neutron mass, bin = 100 keV"
    Anl_Par(I_Anl).R_Alim(1,3) = 0.10

    Redim Shared EgammaA1000(10 To 29,350) As Double ' 10 To 30 MeV, Bin = 1000 keV
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "EgammaA1000"   
    Anl_Par(I_Anl).C_xaxis = "Ega^$/MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTBR0"  
    Anl_Par(I_Anl).C_Title = "Prompt-gamma spectrum from fragments vs. pre-neutron mass, bin = 1 MeV"
    Anl_Par(I_Anl).R_Alim(1,3) = 1
  #Endif  

    ReDim Shared EGammaL(100000) As Double    ' individual gammas
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "EgammaL"
    Anl_Par(I_Anl).C_Title = "Prompt-gamma spectrum, light fragments"
    Anl_Par(I_Anl).C_xaxis = "Ega^$ / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTR0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.001

    ReDim Shared EGammaH(100000) As Double    ' individual gammas
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "EgammaH"
    Anl_Par(I_Anl).C_Title = "Prompt-gamma spectrum, heavy fragments"
    Anl_Par(I_Anl).C_xaxis = "Ega^$ / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTB0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.001

    ReDim Shared EGammaE2(100000) As Double    ' individual gammas
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "EgammaE2"
    Anl_Par(I_Anl).C_Title = "Prompt-E2-gamma spectrum (from fragments)"
    Anl_Par(I_Anl).C_xaxis = "Ega^$ / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTG0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.001
        
    Redim Shared NGammatot(100) As Double    ' number of gammas in one fission event (from fragments)
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "NGammatot"
    Anl_Par(I_Anl).C_Title = "Gamma multiplicity per fission (from fragments)"
    Anl_Par(I_Anl).C_xaxis = "Gamma multiplicity"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "LTB11"  
    
    Redim Shared NgammaA(350,100) As Double  ' number of gammas per fission in one A
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "NgammaA"
    Anl_Par(I_Anl).C_Title = "Gamma multiplicity per fission"
    Anl_Par(I_Anl).C_xaxis = "Post-neutron fragment mass number"
    Anl_Par(I_Anl).C_yaxis = "Gamma multiplicity"
    Anl_Par(I_Anl).I_Dim = 2
      
    Redim Shared EGammatot(500000) As Double   ' total gammas in 1 fission
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Egammatot"
    Anl_Par(I_Anl).C_Title = "Total gamma energy per fission"
    Anl_Par(I_Anl).C_xaxis = "Total gamma energy / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTBR0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.001

    ReDim Shared Eentrance(500000) As Double   ' Eentrance - Erot      
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "Eentrance"
    Anl_Par(I_Anl).C_Title = "Entrance energy above yrast line"
    Anl_Par(I_Anl).C_xaxis = "Energy after last neutron / MeV"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "HTR0"  
    Anl_Par(I_Anl).R_Alim(1,3) = 0.001


   /' Spectra for error analysis '/
  
    ReDim Shared d_NZPOST(2,200,150) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "d_NZPOST"
    Anl_Par(I_Anl).C_Title = "Post-neutron fragment distribution (uncertainties)"
    Anl_Par(I_Anl).C_xaxis = "Post-neutron neutron number"
    Anl_Par(I_Anl).C_yaxis = "Atomic number"
    Anl_Par(I_Anl).I_Dim = 2  
                               
    ReDim d_ZISOPOST(2,350,150) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "d_ZISOPOST"
    Anl_Par(I_Anl).C_Title = "Isobaric Z distribution, post-neutron (uncertainties)"
    Anl_Par(I_Anl).C_xaxis = "Z distribution for A = &"
    Anl_Par(I_Anl).C_yaxis = "Counts"
    Anl_Par(I_Anl).C_Linesymbol = "LTR11"  
     ' dim. 0,1,2: sigma, N*m1, N*m2, all logarithmic
                           
    ReDim d_APOST(2,350) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "d_APOST"
    Anl_Par(I_Anl).C_Title = "Mass distribution, post neutron (uncertainties)"
    Anl_Par(I_Anl).C_xaxis = "Mass number"
    Anl_Par(I_Anl).C_yaxis = "Yield"
    Anl_Par(I_Anl).C_Linesymbol = "BT11"  
    Anl_Par(I_Anl).I_Dim = 1  ' first dimension!
                               
    ReDim d_ZPOST(2,150) As Double
    I_Anl = I_Anl + 1
    Anl_Par(I_Anl).C_Name = "d_ZPOST"
    Anl_Par(I_Anl).C_Title = "Element distribution, post-neutron (uncertainties)"
    Anl_Par(I_Anl).C_xaxis = "Atomic number"
    Anl_Par(I_Anl).C_yaxis = "Yield"
    Anl_Par(I_Anl).C_Linesymbol = "RT11"  
    Anl_Par(I_Anl).I_Dim = 1  ' first dimension!

    ' Arrays for calculation of uncertainties
    Dim As Double d_NCN(2)     
    Dim As Double d_Nsci(2)
    Dim As Double d_Nfr(2)
    Dim As Double d_Nlight(2)     
    Dim As Double d_Nheavy(2)
    Dim As Double d_Ntot(2)
    Dim As Double d_ENfr(2)
    Dim As Double d_Ng(2)
    Dim As Double d_Eg(2)
    Dim As Double d_Egtot(2)
    Dim As Double d_TKEpre(2)
    Dim As Double d_TKEpost(2)
    Dim As Double d_TotXE(2)
    
