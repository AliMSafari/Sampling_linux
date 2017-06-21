  Scope
    Dim As Integer I,J,K
    /' Spectra for error analysis '/

    For I = Lbound(d_NZPOST,1) To Ubound(d_NZPOST,1)
      For J = Lbound(d_NZPOST,2) To Ubound(d_NZPOST,2)
        For K = Lbound(d_NZPOST,3) To Ubound(d_NZPOST,3)
           d_NZPOST(I,J,K) = 0
        Next  
      Next 
    Next   
    For I = Lbound(d_ZISOPOST,1) To Ubound(d_ZISOPOST,1)
      For J = Lbound(d_ZISOPOST,2) To Ubound(d_ZISOPOST,2)
        For K = Lbound(d_ZISOPOST,3) To Ubound(d_ZISOPOST,3)
          d_ZISOPOST(I,J,K) = 0
        Next
      Next
    Next
     ' dim. 0,1,2: sigma, N*m1, N*m2, all logarithmic

    For I = Lbound(d_APOST,1) To Ubound(d_APOST,1)
      For J = Lbound(d_APOST,2) To Ubound(d_APOST,2)
        d_APOST(I,J) = 0
      Next
    Next                             
    For I = Lbound(d_ZPOST,1) To Ubound(d_ZPOST,1)
      For J = Lbound(d_ZPOST,2) To Ubound(d_ZPOST,2)
        d_ZPOST(I,J) = 0
      Next
    Next        
    
    For I = 0 To 2
      d_NCN(I) = 0
      d_Nsci(I) = 0
      d_Nfr(I) = 0
      d_Nlight(I) = 0
      d_Nheavy(I) = 0
      d_Ntot(I) = 0
      d_Enfr(I) = 0
      d_Ng(I) = 0
      d_Eg(I) = 0
      d_Egtot(I) = 0
      d_TKEpre(I) = 0
      d_TKEpost(I) = 0
      d_TotXE(I) = 0
    Next I 
  End Scope           
