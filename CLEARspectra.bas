  /' Clear all calculated spectra: '/

  /' II. Nuclide distributions '/

Scope
    Dim As Integer I,J,K
    For I = Lbound(ZPROV,1) To Ubound(ZPROV,1)
      ZPROV(I) = 0
    Next
    For I = Lbound(ZMPROV,1) To Ubound(ZMPROV,1)
      For J = Lbound(ZMPROV,2) To Ubound(ZMPROV,2)
        ZMPROV(I,J) = 0
      Next
    Next
    For I = Lbound(NZPOST,1) To Ubound(NZPOST,1)
      For J = Lbound(NZPOST,2) To Ubound(NZPOST,2)
        NZPOST(I,J) = 0
      Next
    Next  
    For I = Lbound(ZPOST,1) To Ubound(ZPOST,1)
      ZPOST(I) = 0
    Next
    For I = Lbound(ZMPOST,1) To Ubound(ZMPOST,1)
      For J = Lbound(ZMPOST,2) To Ubound(ZMPOST,2)
        ZMPOST(I,J) = 0
      Next
    Next
    For I = Lbound(NPRE,1) To Ubound(NPRE,1)
      NPRE(I) = 0
    Next
    For I = Lbound(NMPRE,1) To Ubound(NMPRE,1)
      For J = Lbound(NMPRE,2) To Ubound(NMPRE,2)
        NMPRE(I,J) = 0
      Next
    Next  
    For I = Lbound(NPOST,1) To Ubound(NPOST,1)
      NPOST(I) = 0
    Next
    For I = Lbound(NMPOST,1) To Ubound(NMPOST,1)
      For J = Lbound(NMPOST,2) To Ubound(NMPOST,2)
        NMPOST(I,J) = 0
      Next
    Next  
    For I = Lbound(APOST,1) To Ubound(APOST,1)
      APOST(I) = 0
    Next
    For I = Lbound(AMPOST,1) To Ubound(AMPOST,1)
      For J = Lbound(AMPOST,2) To Ubound(AMPOST,2)
        AMPOST(I,J) = 0
      Next
    Next
    For I = Lbound(APROV,1) To Ubound(APROV,1)
      APROV(I) = 0
    Next
    For I = Lbound(AMPROV,1) To Ubound(AMPROV,1)
      For J = Lbound(AMPROV,2) To Ubound(AMPROV,2)
        AMPROV(I,J) = 0
      Next
    Next  
    For I = Lbound(APRE,1) To Ubound(APRE,1)
      APRE(I) = 0
    Next
    For I = Lbound(AMPRE,1) To Ubound(AMPRE,1)
      For J = Lbound(AMPRE,2) To Ubound(AMPRE,2)
        AMPRE(I,J) = 0
      Next
    Next
    For I = Lbound(ZISOPRE,1) To Ubound(ZISOPRE,1)
      For J = Lbound(ZISOPRE,2) To Ubound(ZISOPRE,2)
        ZISOPRE(I,J) = 0
      Next
    Next  
    For I = Lbound(ZISOPOST,1) To Ubound(ZISOPOST,1)
      For J = Lbound(ZISOPOST,2) To Ubound(ZISOPOST,2)
        ZISOPOST(I,J) = 0
      Next
    Next
    For I = Lbound(SigmaZpre,1) To Ubound(SigmaZpre,1)
      SigmaZpre(I) = 0
    Next  
    For I = Lbound(SigmaZpost,1) To Ubound(SigmaZpost,1)
      SigmaZpost(I) = 0
    Next
    For I = Lbound(Zpolarpre,1) To Ubound(Zpolarpre,1)
      Zpolarpre(I) = 0
    Next
    For I = Lbound(Zpolarmac,1) To Ubound(Zpolarmac,1)
      Zpolarmac(I) = 0
    Next                      
    For I = Lbound(Zpolarpost,1) To Ubound(Zpolarpost,1)
      Zpolarpost(I) = 0
    Next
    For I = Lbound(Edefo2d,1) To Ubound(Edefo2d,1)
      For J = Lbound(Edefo2d,2) To Ubound(Edefo2d,2)
        Edefo2d(I,J) = 0
      Next
    Next    
    For I = Lbound(EdefoA,1) To Ubound(EdefoA,1)
      EdefoA(I) = 0
    Next I                           
    For I = Lbound(JFRAGpre,1) To Ubound(JFRAGpre,1)
      For J = Lbound(JFRAGpre,2) To Ubound(JFRAGpre,2)
        For K = Lbound(JFRAGpre,3) To Ubound(JFRAGpre,3)
          JFRAGpre(I,J,K) = 0
        Next
      Next
    Next  
    For I = Lbound(JFRAGpost,1) To Ubound(JFRAGpost,1)
      For J = Lbound(JFRAGpost,2) To Ubound(JFRAGpost,2)
        For K = Lbound(JFRAGpost,3) To Ubound(JFRAGpost,3)
          JFRAGpost(I,J,K) = 0
        Next
      Next
    Next  
    For I = Lbound(Eintr2d,1) to Ubound(Eintr2d,1)
      For J = Lbound(Eintr2d,2) to Ubound(Eintr2d,2)
         Eintr2d(I,J) = 0
      Next
    Next  
    For I = Lbound(EintrA,1) To Ubound(EintrA,1)
      EintrA(I) = 0
    Next I
    For I = Lbound(Ecoll2d,1) To Ubound(Ecoll2d,1)
      For J = Lbound(Ecoll2d,2) To Ubound(Ecoll2d,2)
        Ecoll2d(I,J) = 0
      Next
    Next
    For I = Lbound(EcollA,1) To Ubound(EcollA,1)
      EcollA(I) = 0
    Next I
    For I = Lbound(Eexc2d,1) To Ubound(Eexc2d,1)
      For J = Lbound(Eexc2d,2) To Ubound(Eexc2d,2)
        Eexc2d(I,J) = 0
      Next
    Next
    For I = Lbound(N2dpre,1) To Ubound(N2dpre,1)
      For J = Lbound(N2dpre,2) To Ubound(N2dpre,2)
         N2dpre(I,J) = 0
      Next
    Next
    For I = Lbound(N2dpost,1) To Ubound(N2dpost,1)
      For J = Lbound(N2dpost,2) To Ubound(N2dpost,2)
        N2dpost(I,J) = 0
      Next  
    Next
    For I = Lbound(NApre,1) To Ubound(NApre,1)
      NApre(I) = 0
    Next  
    For I = Lbound(NApost,1) To Ubound(NApost,1)
      NApost(I) = 0
    Next                               
  /'For I = Lbound(ENCN,1) To Ubound(ENCN,1)
      ENCN(I) = 0
    Next'/     
  /'For I = Lbound(EpCN,1) To Ubound(EpCN,1)
      EpCN(I) = 0
    Next I'/  
    For I = Lbound(ENsci,1) To Ubound(ENsci,1)
      ENsci(I) = 0
    Next I
    For I = Lbound(ENfr,1) To Ubound(ENfr,1)
      ENfr(I) = 0
    Next       
    For I = Lbound(ENfrvar,1) To Ubound(ENfrvar,1)
      ENfrvar(I) = 0
    Next       
    For I = Lbound(ENfrfs,1) To Ubound(ENfrfs,1)
      ENfrfs(I) = 0
    Next       
    For I = Lbound(EnfrC,1) To Ubound(EnfrC,1)
      For J = Lbound(EnfrC,2) To Ubound(EnfrC,2)
        For K = Lbound(EnfrC,3) To Ubound(EnfrC,3)
          EnfrC(I,J,K) = 0
        Next
      Next
    Next    
  /'For I = Lbound(EN,1) To Ubound(EN,1)
      EN(I) = 0 
    Next '/   
    For I = Lbound(ENlight,1) To Ubound(ENlight,1)
      ENlight(I) = 0
    Next
    For I = Lbound(ENheavy,1) To Ubound(ENheavy,1)
      ENheavy(I) = 0
    Next   
    For I = Lbound(ENM,1) To Ubound(ENM,1)
      For J = Lbound(ENM,2) To Ubound(ENM,2)
        ENM(I,J) = 0
      Next
    Next      
    For I = Lbound(ENApre2d,1) To Ubound(ENApre2d,1)
      For J = Lbound(ENApre2d,2) To Ubound(ENApre2d,2)
        ENApre2d(I,J) = 0
      Next
    Next      
    For I = Lbound(ENApost2d,1) To Ubound(ENApost2d,1)
      For J = Lbound(ENApost2d,2) To Ubound(ENApost2d,2)
        ENApost2d(I,J) = 0
      Next
    Next      
    For I = Lbound(ENApre,1) To Ubound(ENApre,1)
      ENApre(I) = 0
    Next
    For I = Lbound(ENApost,1) To Ubound(ENApost,1)
      ENApost(I) = 0
    Next
    For I = Lbound(ENApre2dfs,1) To Ubound(ENApre2dfs,1)
      For J = Lbound(ENApre2dfs,2) To Ubound(ENApre2dfs,2)
        ENApre2dfs(I,J) = 0
      Next
    Next      
    For I = Lbound(ENApost2dfs,1) To Ubound(ENApost2dfs,1)
      For J = Lbound(ENApost2dfs,2) To Ubound(ENApost2dfs,2)
        ENApost2dfs(I,J) = 0
      Next
    Next      
    For I = Lbound(ENAprefs,1) To Ubound(ENAprefs,1)
      ENAprefs(I) = 0
    Next
    For I = Lbound(ENApostfs,1) To Ubound(ENApostfs,1)
      ENApostfs(I) = 0
    Next
    For I = Lbound(NNCN,1) To Ubound(NNCN,1)
      NNCN(I) = 0
    Next
    For I = Lbound(NNsci,1) To Ubound(NNsci,1)
      NNsci(I) = 0
    Next
    For I = Lbound(NNfr,1) To Ubound(NNfr,1)
      NNfr(I) = 0
    Next
    For I = Lbound(NN,1) To Ubound(NN,1)
      NN(I) = 0
    Next
    For I = Lbound(NNlight,1) To Ubound(NNlight,1)
      NNlight(I) = 0
    Next
    For I  = Lbound(NNheavy,1) To Ubound(NNheavy,1)
      NNheavy(I) = 0
    Next I
    For I = Lbound(ENM,1) To Ubound(ENM,1)
      For J = Lbound(ENM,2) To Ubound(ENM,2)
        ENM(I,J) = 0
      Next      
    Next
    For I = Lbound(Ndirlight,1) To Ubound(Ndirlight,1)
      Ndirlight(I) = 0
    Next
    For I = Lbound(nuTKEpre,1) To Ubound(nuTKEpre,1)
      For J = Lbound(nuTKEpre,2) To Ubound(nuTKEpre,2)
        nuTKEpre(I,J) = 0
      Next                                 
    Next                              
    For I = Lbound(nuTKEpost,1) To Ubound(nuTKEpost,1)
      For J = Lbound(nuTKEpost,2) To Ubound(nuTKEpost,2)
        nuTKEpost(I,J) = 0
      Next
    Next
    For I = Lbound(DPLOCAL,1) To Ubound(DPLOCAL,1)
      DPLOCAL(I) = 0
    Next      
    For I = Lbound(DNLOCAL,1) To Ubound(DNLOCAL,1)
      DNLOCAL(I) = 0
    Next  
    For I = Lbound(AEkinpre,1) To Ubound(AEkinpre,1)
      For J = Lbound(AEkinpre,2) To Ubound(AEkinpre,2)
        AEkinpre(I,J) = 0
      Next
    Next  
    For I = Lbound(EkinApre,1) To Ubound(EkinApre,1)
      EkinApre(I) = 0
    Next  
    For I = Lbound(Ekinpre,1) To Ubound(Ekinpre,1)
      Ekinpre(I) = 0
    Next  
    For I = Lbound(AEkinpost,1) To Ubound(AEkinpost,1)
      For J = Lbound(AEkinpost,2) To Ubound(AEkinpost,2)
        AEkinpost(I,J) = 0
      Next
    Next
    For I = Lbound(EkinApost,1) To Ubound(EkinApost,1)
      EkinApost(I) = 0
    Next  
    For I = Lbound(Ekinpost,1) To Ubound(Ekinpost,1)
      Ekinpost(I) = 0
    Next  
    For I = Lbound(TKEpre,1) To Ubound(TKEpre,1)
      TKEpre(I) = 0
    Next
    For I = Lbound(TKEApre,1) To Ubound(TKEApre,1)
      TKEApre(I) = 0
    Next I
 '   For I = Lbound(TKEApreM,1) To Ubound(TKEApreM,1)
 '     For J = Lbound(TKEApreM,2) To Ubound(TKEApreM,2)
 '       TKEApreM(I,J) = 0
 '     Next J
 '   Next I
    For I = Lbound(TKEpost,1) To Ubound(TKEpost,1)
      TKEpost(I) = 0
    Next                               
    For I = Lbound(TKEApost,1) To Ubound(TKEApost,1)
      TKEApost(I) = 0
    Next I
 '   For I = Lbound(TKEApostM,1) To Ubound(TKEApostM,1)
 '     For J = Lbound(TKEApostM,2) To Ubound(TKEApostM,2)
 '       TKEApostM(I,J) = 0
 '     Next J
 '   Next I
    For I = Lbound(TotXE,1) To Ubound(TotXE,1)
      TotXE(I) = 0
    Next                               
    For I = Lbound(Qvalues,1) To Ubound(Qvalues,1)
      Qvalues(I) = 0
    Next
    For I = Lbound(AQpre,1) To Ubound(AQpre,1)
      For J = Lbound(AQpre,2) To Ubound(AQpre,2)
        AQpre(I,J) = 0
      Next
    Next  
    For I = Lbound(QA,1) To Ubound(QA,1)
      QA(I) = 0
    Next I
   /' For I = Lbound(EgammaCN,1) To Ubound(EgammaCN,1)
      EgammaCN(I) = 0
    Next '/                                 
    For I = Lbound(Egamma,1) To Ubound(Egamma,1)
      Egamma(I) = 0
    Next
   #Ifdef B_EgammaA   
    For I = Lbound(EgammaA2,1) To Ubound(EgammaA2,1)
      For J = Lbound(EgammaA2,2) To Ubound(EgammaA2,2)
        EgammaA2(I,J) = 0
      Next J
    Next I
    For I = Lbound(EgammaA10,1) To Ubound(EgammaA10,1)
      For J = Lbound(EgammaA10,2) To Ubound(EgammaA10,2)
        EgammaA10(I,J) = 0
      Next J
    Next I
    For I = Lbound(EgammaA100,1) To Ubound(EgammaA100,1)
      For J = Lbound(EgammaA100,2) To Ubound(EgammaA100,2)
        EgammaA100(I,J) = 0
      Next J
    Next I
    For I = Lbound(EgammaA1000,1) To Ubound(EgammaA1000,1)
      For J = Lbound(EgammaA1000,2) To Ubound(EgammaA1000,2)
        EgammaA1000(I,J) = 0
      Next J
    Next I 
   #Endif 
    For I = Lbound(EgammaL,1) To Ubound(EgammaL,1)
      EgammaL(I) = 0
    Next                                 
    For I = Lbound(EgammaH,1) To Ubound(EgammaH,1)
      EgammaH(I) = 0
    Next                                 
    For I = Lbound(EgammaE2,1) To Ubound(EgammaE2,1)
      EgammaE2(I) = 0
    Next
    For I = Lbound(Ngammatot,1) To Ubound(Ngammatot,1)
      Ngammatot(I) = 0
    Next                           
    For I = Lbound(NgammaA,1) To Ubound(NgammaA,1)
      For J = Lbound(NgammaA,2) To Ubound(NgammaA,2)
        NgammaA(I,J) = 0
      Next  
    Next                               
    For I = Lbound(Egammatot,1) To Ubound(Egammatot,1)
      Egammatot(I) = 0
    Next    
    For I = Lbound(Eentrance,1) To Ubound(Eentrance,1)
      Eentrance(I) = 0
    Next                                   
    For I = Lbound(NZPRE,1) To Ubound(NZPRE,1) 
      For J = Lbound(NZPRE,2) To Ubound(NZPRE,2)
        NZPRE(I,J) = 0
      Next
    Next
    For I = Lbound(NZMPRE,1) To Ubound(NZMPRE,1)
      For J = Lbound(NZMPRE,2) To Ubound(NZMPRE,2)
        For K = Lbound(NZMPRE,3) To Ubound(NZMPRE,3)
          NZMPRE(I,J,K) = 0
        Next
      Next
    Next  
  End Scope
