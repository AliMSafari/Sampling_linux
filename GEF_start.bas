Static Shared As String C_GEF_Version 
C_GEF_Version = "2016/1.2"
' example:      "2012/3.1"  (must have the same length) 

   Print " "
   Print "   **************************************************************************"
   Print "   *       Model code for a general description of fission observables      *"
   Print "   *                                 G E F                                  *"
   Print "   *                           Version "+C_GEF_Version+"                             *"
   Print "   * Copyright 2009,2010,2011,2012,2013,2014,2015,2016                      *"
   Print "   *          K.-H. Schmidt and B. Jurado                                  *"
   Print "   **************************************************************************"
   Print " "
    
   
  
'   Dim As String cyes
'   Print " I accept using this code under the GNU General Public License conditions."
'   Print " (See Readme.txt and License.txt, which are provided with this code or"
'   Print "  <http://www.gnu.org/licenses/> .)" 
'   Input " Enter YES if you agree: ",cyes
'   If UCASE(cyes) <> "YES" Then End
'   Print " "
/'<'/

'
'    Copyright 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016: 
'       Dr. Karl-Heinz Schmidt, Rheinstrasse 4, 64390 Erzhausen, Germany
'       and 
'       Dr. Beatriz Jurado, Centre d'Etudes Nucleaires de Bordeaux-Gradignan,
'       Chemin du Solarium, Le Haut Vigneau, BP 120, 33175 Gradignan, Cedex,
'       France 
'
'    This program is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    This program is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with this program.  If not, see <http://www.gnu.org/licenses/>.


  /' Documentation: '/
  /' (1) K.-H. Schmidt and B. Jurado, Contribution to '/
  /'     ESNT Workshop "The scission process", Saclay (France), April 12-16, 2010 '/
  /' (2) B. Jurado and K.-H. Schmidt, Contribution to '/
  /'     Seminar an fission, Gent (Belgium), May 17-20, 2010 '/
  /' (3) B. Jurado and K.-H. Schmidt, Contribution to '/
  /'     EFNUDAT Workshop, Paris (France), May 25-27, 2010 '/
  /' (4) K.-H. Schmidt and B. Jurado, Contribution to '/
  /'     EFNUDAT Workshop, Paris (France), May 25-27, 2010 '/
  /' (5) K.-H. Schmidt and B. Jurado, '/
  /'     Final Report to EFNUDAT, October, 2010 '/
  /' (6) K.-H. Schmidt and B. Jurado, Phys. Rev. Lett. 104 (2010) 21250 '/
  /' (7) K.-H. Schmidt and B. Jurado, Phys. Rev. C 82 (2011) 014607 '/
  /' (8) K.-H. Schmidt and B. Jurado, Phys. Rev. C 83 (2011) 061601 '/
  /' (9) K.-H. Schmidt and B. Jurado, arXiv:1007.0741v1[nucl-th] (2010) '/
  /' (10) K.-H. Schmidt and B. Jurado, JEF/DOC 1423, NEA of OECD, 2012 '/  
  /' (11) K.-H. Schmidt and B. Jurado, Phys. Rev. C 86 (2012) 044322 '/
  /' (12) K.-H. Schmidt, B. Jurado, Ch. Amouroux, JEFF-Report 24, NEA of OECD, 2014 '/
  /' (13) B. Jurado, K.-H. Schmidt, J. Phys. G: Nucl. Part. Phys. 42 (2015) 055101 '/
  /' (14) K.-H. Schmidt, B. Jurado, Eur. Phys. J. A 51 (2015) 176 '/
  /' (15) K.-H. Schmidt, B. Jurado, C. Amouroux, C. Schmitt, Nucl. Data Sheets 131 (2016) 107 '/


  /' Further documentation and the newest version of the GEF code are '/
  /' available from                                                   '/
  /' http://www.cenbg.in2p3.fr/GEF and http://www.khs-erzhausen.de/ . '/

  
'    The development of the GEF code has been supported by the European Union,
'    EURATOM 6 in the Framework Program "European Facilities for Nuclear Data
'    Measurements" (EFNUDAT), contract number FP6-036434, the Framework
'    Program "European Research Infrastructure for Nuclear Data Applications
'    (ERINDA), contract number FP7-269499, and by the OECD Nuclear Energy Agency.


' Technical remark: The code contains commented sections with 
' control-character sequences (/'<'/ and /'>'/) that serve to automatically 
' produce a deterministic version of the GEF code as a subroutine from this 
' source with a dedicated pre-processor, named ExtractSub.bas. 
' Sections starting with the sequence /'<'/ and ending with the sequence /'>'/
' are used in the deterministic GEF version.
' ExtractSub.bas produces the deterministic code GEFSUB.bas in FreeBASIC.
' The translator FBtoFO.bas can be used to translate this code into FORTRAN99.
' In order to enable this translation, GEF.bas contains already some additional
' statements ( e.g.  /'<FO REAL*4 GAUSSINTEGRAL FO>'/ )   
' that are only used by the translator to provide some specific 
' FORTRAN statements that cannot be produced automatically. 
   

  /' K.-H. Schmidt / B. Jurado, 07/Feb./2009 '/
  /' SEFI9 is taken as a basis and extended by new features in SEFI14 (May 2010), KHS '/
  /' Several improvements (even-odd effect, charge polarization etc. (June 2010), KHS '/
  /' SEFI15 converted from PL/I to FreeBASIC on 04/July/2010, KHS '/
  /' Error in LyPair corrected (26/July/2010) KHS '/
  /' Indices corrected in U_Shell inside Eva (1/Aug/2010) KHS '/
  /' Major developments, sigma_E*(scission), sigma_Z(A) etc. (14/Aug/2010) KHS '/
  /' Macroscopic masses from Thomas-Fermi model (Myers & Swiatecki) (17/Aug/2010) KHS '/
  /' 3 reference options for energy input (4/Sept/2010) KHS '/
  /' Graphic output of mass distribution added (if there are problems with the X11 
      installation on LINUX, the graphics may be suppressed by simply commenting 
      the line  -> #Include Once "DCLPlotting.bas" <- )  (5/Sept/2010) KHS '/
  /' Comparison with ENDF compilation in graphic output (12/Sept/2010) KHS '/
  /' Super-long fission channel included (14/Sept/2010) KHS '/
  /' Overlap of S1 and S2 fission channels in both fragments included (18/Oct/2010) KHS '/
  /' Output of neutron multiplicity distribution added (20/Oct/2010) KHS '/
  /' Decreasing curvature of shells with increasing E* (24/Oct/2010) KHS '/
  /' Angular momenta of fission fragments added (18/Dec/2010) KHS '/
  /' CN angular momentum considered (14/Jan/2011) KHS '/
  /' Numerical stability improved (28/Jan/2011) KHS '/
  /' Output in ENDF format (optional) (4/Feb/2011) KHS '/
  /' Input list from file supported (31/Jan/2011) KHS '/
  /' Multiprocessing supported (5/Feb/2011) KHS '/
  /' Polarization for symmetric fission channel improved (12/Feb/2011) KHS '/
  /' GUI for input (23/Feb/2011) KHS '/
  /' Calculation of fission-fragment angular momentum refined (5/May/2011) KHS '/
  /' Neutron inverse cross section modified (5/August/2011) KHS '/
  /' Even-odd staggering in neutron emission improved (5/August/2011) KHS '/
  /' Slight modifications in angular-momentum distributions (19/October/2011) KHS '/
  /' Gamma emission added (23/November/2011) KHS '/
  /' TKE added (24/November/2011) KHS '/
  /' Neutron spectrum added (25/November/2011) KHS '/
  /' Neutron-gamma competition added (4/December/2011) KHS '/
  /' Composite level density (Egidy + Ignatyuk) refined (31/January/2012) KHS '/
  /' Treatment of GDR refined (14/February/2012) KHS '/
  /' Deformation of S3 channel changed (27/February/2012) KHS '/
  /' Z=44 deformed shell added (supports S1 around Pu) (27/February/2012) KHS '/
  /' Uncertainties from perturbed fission yields (3/March/2012) KHS '/
  /' Validity range extended to Z=120 (with a warning message) (8/March/2012) KHS '/
  /' Neutron emission during fragment acceleration (18/March/2012) KHS '/
  /' Several optimizations (15/April/2012) KHS '/
  /' TF masses of Myers & Swiatecki corrected (pairing shift) (29/May/2012) KHS '/
  /' Correction on intrinsic excitation energy (05/June/2012) KHS '/
  /' Correction on gamma emission (25/September/2012) KHS '/
  /' Free choice of listmode values (13/October/2012) KHS '/
  /' Excitation-energy distribution from file (14/October/2012) KHS '/
  /' Transfer of input from GUI corrected (02/November/2012) KHS '/
  /' Parameters of perturbed calculations modified (02/November/2012) KHS '/
  /' Model parameters je-adjusted (08/November/2012) KHS '/
  /' Input options for isomeric target nuclei (09/November/2012) KHS '/
  /' Random initialisation of the random generator (26/November/2012) KHS '/
  /' Covariance matrix for Z, Apre, Apost, ZApre, ZApost (06/December/2012) KHS '/
  /' Output file in XML format (06/December/2012) KHS '/
  /' Multi-chance fission supported (13/December/2012) KHS '/
  /' Pre-compound emission for (n,f) included (13/December/2012) KHS '/
  /' Some technical corrections and modifications (17/December/2012) KHS '/
  /' Transition from asymmetric to symmetric fission around Fm improved (19/December/2012) KHS '/
  /' Influence of S2 channel on S1 channel in the other fragment included (20/December/2012) KHS'/
  /' List-mode output of pre-fission neutron energies (21/December/2012) KHS '/
  /' Parameterisation for EB-EA from fit to data in Dahlinger et al. (21/December/2012) KHS '/
  /' Fission channel at Z=42 added (seen around Pu in light fragment and around Hg) (23/Dec./2012) KHS '/
  /' Gamma-n / Gamma-f according to Moretto (IAEA Rochester) (23/December/2012) KHS '/
  /' Pre-compound neutron energies modified (24/December/2012) KHS '/
  /' Some technical revisions to avoid crashes in covariances (26/December/2012) KHS '/
  /' Influence of shells on yrast line from Deleplanque et al. (26/December/2012) KHS '/
  /' Fission threshold in multi-chance fission modified (30/December/2012) KHS '/
  /' Output of energies at fission for multi-chance fission (30/December/2012) KHS '/
  /' Several revisions (15/January/2013) KHS '/
  /' New optical model fit (3/February/2013) KHS '/
  /' Gamma-f / Gamma-n modified (3/February/2013) KHS '/
  /' Handling for reading input from file corrected (5/February/2013) KHS '/ 
  /' Data transfer from GUI corrected (6/February/2013) KHS '/
  /' Input dialog re-organized (6/February/2013) KHS '/  
  /' Mass-dependent deformation and charge polarization revised (9/February/2013) KHS '/
  /' Calculation of combined fission channels S12, S22 revised (10/February/2013) KHS '/
  /' Extension of validity range to heavier nuclei (10/February/2013) KHS '/
  /' Improved description of prompt-neutron spectra (21/February/2013) KHS '/
  /' Several technical corrections and developments (April-May/2013) KHS '/
  /' Pre-fission emission of protons considered (14/Mai,2013) KHS '/
  /' Structure of energy list in input file modified (26/May/2013) KHS '/
  /' Option "local fit" added (26/May/2013) KHS '/
  /' Neutron evaporation corrected (more realistic even-odd effect in isotonic distr.) (21/June(2013) KHS '/
  /' Even-odd effect in TKE added (23/June/2013) KHS '/
  /' Calculation of Z-A-covariance matrix corrected (16/July/2013) KHS '/ 
  /' Output of multi-variant distributions corrected for input from file (24/July/2013) KHS '/
  /' New global fit (most important: Energy gain from saddle to scission reduced) (15/September/2013) KHS '/
  /' Even-odd effect in neutron number of fragments modified (17/September/2013) KHS '/
  /' Curvatures of fission valleys adjusted to experimental shells around 132Sn (18/September/2013) KHS '/
  /' Width of S0 corrected: Fit of Rusanov (18/September/2013) KHS '/
  /' Random generator Box with asymmetric diffuseness for S2 (18/September/2013) KHS '/
  /' Gaussian random generator revised (20/September/2013) KHS '/
  /' Mass shift of fission channels with E* modified (22/September/2013) KHS '/
  /' Energy dependence of S1 position corrected, slightly modified parameters (25/September/2013) KHS '/
  /' Initial angular momentum introduced as an input parameter (02/October/2013) KHS '/
  /' Calculation of prompt-neutron emission improved, some model parameters modified (12/October/2013) KHS '/
  /' Technical error, causing incomplete covariance matrices on output corrected  (17/October/2013) KHS '/
  /' Washing of shell effects considerd in shape fluctuations (26/October/2013) KHS '/
  /' Post-scission neutrons added to list-mode output (8/November/2013) KHS '/
  /' Fission-gamma competition refined (10/November/2013) KHS '/
  /' New global fit of model parameters (18/November/2013) KHS '/
  /' Multi-chance fission modified (18/November/2013) KHS '/
  /' A numerical instability removed (20/November/2013) KHS '/
  /' Calculation of multi-chance fission: corrected and modified (28/November/2013) KHS '/
  /' Spurious even-odd effect in fission probabilities (from Moeller's shells) removed (4/December/2013) KHS '/
  /' More precise calculation of E* in n-induced fission (6/December/2013) KHS '/
  /' Pre-fission gamma strength increased (adjusted to Pf(E*) of 238U (22/December/2013) KHS '/
  /' Kinetic energy of prompt neutrons in cm system (fragment-neutron) (10/Janurary/2014) KHS '/
  /' Binnig of prompt-neutron spectra corrected (shift by 50 keV) (10/January/2014) KHS '/
  /' Description of fission barriers modified (15/January/2014) KHS '/
  /' Description of energy-dependent fission probability modified (17/January/2014) KHS '/
  /' E* at scission modified for fission below Bf (4/February/2014) KHS '/
  /' Influence of Z=44 shell on deformation of light fragment (4/February/2014) KHS '/
  /' Tunneling for S1 enhanced (16/February/2014) KHS '/
  /' New global fit of fission channels (strength and position) (19/March/2014) KHS '/
  /' Normalization of distributions for deterministic version corrected (23/May/2014) KHS '/
  /' Calculation of "corrected sample variance" revised (05/September/2014) KHS '/
  /' Uncertainties calculated with "corrected sample variance" (05/September/2014) KHS '/
  /' Shape of "S22" fission channel modified (08/December/2014) KHS '/
  /' Binsize for prompt neutrons set to 1 keV (09/December/2014) KHS '/
  /' Binsize for prompt gammas set to 1 keV (09/December/2014) KHS '/
  /' Fragment excitation energies in listmode (10/December/2014) KHS '/
  /' Shell in symmetric fission channel included in GEFSUB (12/December/2014) KHS '/
  /' Ground-state spin of fragments is taken into account (22/January/2015) KHS '/
  /' Pre-fission gamma emission included (20/February/2015) KHS '/
  /' VMI model for E2 gammas developed and implemented (15/March/2015) KHS '/
  /' Binning of spectrum EN corrected (28/March/2015) KHS '/ 
  /' Energetics of symmetric fission channel revised (5/April/2015) KHS '/
  /' Output of isomeric yields extended (# of events added) (10/April/2015) KHS '/
  /' Control-output for progress of calculation (17/April/2015) KHS '/
  /' Angular-momentum dependence of pairing gap in eva (7/May/2015) KHS '/
  /' Angular-momentum dependent Gf: influence of fissility and temperature (13/May/2015) KHS '/
  /' Influence of angular momentum on mass width by increasing fissiliy (14/May/2015) KHS '/
  /' Mass distribution truncated at zero and A_CN (14/May/2015) KHS '/
  /' Yields of fission modes also for multi-chance fission (7/August/2015) KHS '/
  /' Tentative description of structure in FF distributions around A_CN = 180-200 (8/August/2015) KHS '/
  /' Improved description of asymmetric fission in light nuclei (A_CN < 220) (9/August/2015) KHS '/
  /' Extension of the validity range to lighter nuclei (9/August/2015) KHS '/
  /' Adaptions of ERF function to new FreeBASIC compiler (12/Sept(2015) KHS '/
  /' Allowance for calculations with very large number of events (15/Sept(2015) KHS '/
  /' Slight adjustement of parameters to "repair" deterioation for low E* (21/Sept/2015) KHS '/
  /' Correction of some formatting issues (03/Oct/2015) KHS '/
  /' Calculation of covariances for independent yields corrected (6/Nov/2015) KHS '/
  /' Calculation of covariances between two systems corrected (6/Nov/2015) KHS '/
  /' Output of correlations matrices added (6/Nov/2015) KHS '/
  /' Espectrum.in can now also provide the CN spin (13/Nov/2015) KHS '/
  /' Output for parallel computing better protected (1/March/2016) KHS '/
  /' Output of random files provided (4/March/2016) KHS '/
  /' Prompt-neutron spectrum with variable bin size added (6/April/2016) KHS '/
  /' Overflow problem in tanh and coth corrected (11/April/2016) KHS '/
  /' Experimental masses in evaporation routine (not yet finished) (23/April/2016) KHS '/
  /' Subscriptrange problem in output of neutron energies corrected (29/April/2016) KHS '/  
  /' Gamma spectra with conditions on pre-neutron mass (29/Mai/2016) KHS '/
  /' Relation between energies: experimental masses used (07/Sept/2016) KHS '/
  /' Partial support of p-induced fission (up to Ep = 30 MeV)
       (pre-equilibrium emission not yet implemented, preliminary l-distribution) (07/Sept/2016) KHS '/
  /' Model parameters of fission channels optimized (20/Sept/2016) KHS '/
  /' Neutrons emitted between outer saddle and scission added (21/Sept/2016) KHS '/  
  /' Output of delayed-neutron multiplicities added (25/Sept/2016) KHS '/
  /' New adjustment of model parameters (13/Oct/2016) KHS '/  
  /' Output of delayed-neutron emitters added (13/Oct/2016) KHS '/ 
  /' Uncertainty range of angular momentum added (24/Oct/2016) KHS '/
  /' Uncertainties of prompt-gamma and prompt-neutron characteristics added (24/Oct/2016) KHS '/
  /' Uncertainty of TKE (pre and post) added (06/Nov/2016) KHS '/
  /' Normalization of Maxwell distribution corrected (07/Nov/2016) KHS '/
  /' Declaration of some variables changed to prevent overflow (24/Nov/2016) KHS '/
  /' Energy dependence of shape fluctuations modified (26/Nov/2016/KHS '/
  /' Formatting of output slightly modified (15/Dec/2016/KHS '/
  
  /' FreeBASIC is available from http://www.freebasic.net/ '/
  /' FreeBASIC runs on Windows, Linux, and DOS. '/
  /' FreeBASIC compiles a binary code that uses the C run-time library. '/
  
  
/'>'/
  #Include Once "file.bi"
  #Include "vbcompat.bi"  
/'<'/  
  #Include "utilities.bi"
/'>'/
  #ifdef __FB_WIN32__
    #include "Mutex.bas"
    ' Declaration of Waitsynch_GUI
  #endif
  
  ' #include once "DCLendf.bas"       ' Only for ENDF
  #ifdef B_ENDF     
    #include once "DCLbranching.bas"
  #endif    
  
  #define B_EgammaA  ' Comment this line, if the mass-dependent gammas are not needed.

   ' Check and create subfolders for output of GEF
   If CHDIR("ctl") Then
     MKDIR("ctl")
     Print "Subfolder /ctl created for output of GEF."
   Else
     CHDIR("..")
   End If  
   
   If CHDIR("out") Then
     MKDIR("out")
     Print "Subfolder /out created for output of GEF."
   Else
     CHDIR("..")
   End If  
   
   If CHDIR("tmp") Then
     MKDIR("tmp") 
     Print "Subfolder /tmp created for output of GEF."
   Else
     CHDIR("..")
   End If    

   If CHDIR("dmp") Then
     MKDIR("dmp") 
     Print "Subfolder /dmp created for output of GEF."
   Else
     CHDIR("..")
   End If    

/'<'/


  /' Functions and subroutines '/
  
   Declare Function Getyield(E_rel As Single,E As Single,T_low As Single,T_high As Single) As Single
     	
   Declare Function Masscurv(Z As Single, A As Single, RL As Single, kappa As Single) As Single
   Declare Function Masscurv1(Z As Single, A As Single, RL As Single, kappa As Single) As Single

   Declare Function De_Saddle_Scission(Z_square_over_Athird As Single, _
           ESHIFTSASCI As Single) As Single

   Declare Function TEgidy(A As Single,DU As Single,Fred As Single) As Single
   
   Declare Function TRusanov(E As Single, A As Single) As Single

   Declare Function LyMass(Z As Single,A As Single,beta As Single) As Single

   Declare Function LyPair(Z As Integer,A As Integer) As Single

   Declare Function TFPair(Z As Integer,A As Integer) As Single

   Declare Function Pmass(Z As Single,A As Single,beta As Single) As Single

   Declare Function FEDEFOLys(Z As Single,A As Single,beta As Single) As Single
   
   Declare Function FEDEFOP(Z As Single,A As Single,beta As Single) As Single

   Declare Function LDMass(Z As Single,A As Single,beta As Single) As Single
   
   Declare Function AME2012(Z As Integer,A As Integer) As Single

   Declare Function U_SHELL(Z As Integer,A As Integer) As Single
   
   Declare Function U_SHELL_exp(Z As Integer, A As Integer) As Single   
   
   Declare Function U_SHELL_EO_exp(Z As Integer, A As Integer) As Single

   Declare Function U_MASS(Z As Single,A As Single) As Single

   Declare Function ECOUL( _
	        Z1 As Single,A1 As Single,beta1 As Single,Z2 As Single,A2 As Single, _
           beta2 As Single,d As Single) As Single

   Declare Function beta_light(Z As Integer,betaL0 As Single,betaL1 As Single) As Single

   Declare Function beta_heavy(Z As Integer,betaH0 As Single,betaH1 As Single) As Single

   Declare Function _
           Z_equi(ZCN As Integer,A1 As Integer,A2 As Integer, _
           beta1 As Single,beta2 As Single,d As Single, Imode As Integer, _
           POLARadd As Single,POLARfac As Single) _
           As Single

   Declare Sub Beta_opt_light(A1 As Single,A2 As Single,Z1 As Single,Z2 As Single, _
             d As Single,beta2_imposed As Single,ByRef beta1_opt As Single)

   Declare Sub Beta_Equi( _
          A1 As Single,A2 As Single,Z1 As Single,Z2 As Single,d As Single, _
          beta1prev As Single,beta2prev As Single, _
          ByRef beta1opt As Single,ByRef beta2opt As Single)
/'>'/
   Declare Sub Eva(Ilh As Integer, _
   	Z_CN As Single,A_CN As Single,E_INIT As Single,T As Single, J_frag As Single, _
   ByRef Z_RES As Single,ByRef A_RES As Single, ByRef E_FINAL As Single, _
   Array_En() As Single, Array_Tn() As Single, Array_Eg0() As Single)
   
   Declare Function u_accel(A1 As Single,Z1 As Single,A2 As Single,Z2 As Single, _
         TKE As Single,E0 As Single,Tn As Single) As Single 
   
   Declare Function P_Egamma_high(Zi as Single, Ai As Single, Ei As Single) As Single

   Declare Function P_Egamma_low(Zi as Single, Ai As Single, Ei As Single) As Single
/'<'/      

   Declare Function U_Ired(Z As Single,A As Single) As Single

   Declare Function U_IredFF(Z As Single,A As Single) As Single
   
   Declare Function U_I_Shell(Z As Single, A As Single) As Single
   
   Declare Function U_alev_ld(Z As Single, A As Single) As Single
   
   Declare Function U_Temp(Z As Single, A As Single, E As Single, Ishell As Integer, _
             Ipair As Integer, Tscale As Single,Econd As Single) As Single
/'>'/   
   Declare Function U_Temp2(Z As Single, A As Single, E As Single, Rshell As Single, _
             Rpair As Single, Tscale As Single,Econd As Single) As Single

   Declare Function E0_GDR(Z As Single, A As Single) As Single
   
   Declare Function Width_GDR(E0 As Single) As Single
   
   Declare Function Sigma_GDR(Z As Single,A As Single,E As Single,E0 As Single, _
              WidthK As Single) As Single
              
   Declare Function Efac_def_GDR(Beta As Single,Gamma As Single,K As Single) As Single
   
   Declare Function TK_GDR(Z As Single,A As Single,E As Single) As Single   
   
   Declare Function GgGtot(Z As Single, A As Single, E As Single, Egamma As Single) As Single
   
   Declare Function E_next(T1 As Single,T2 As Single,E1 As Single,E2 As Single,A1 As Single,A2 As Single) _
                     As Single
   Declare Function EVEN_ODD(R_ORIGIN As Single,R_EVEN_ODD As Single) As Integer
/'<'/

   Declare Function U_Even_Odd(I_Channel As Integer,PEO As Single) As Single
   
   Declare Function BFTF(RZ As Single,RA As Single,I_Switch As Integer) As Single
   Declare Function BFTFA(RZ As Single,RA As Single,I_Switch As Integer) As Single
   Declare Function BFTFB(RZ As Single,RA As Single,I_Switch As Integer) As Single

   Declare Function Gaussintegral(R_x As Single,R_sigma As Single) As Single
/'>'/

   Declare Sub Printcomments

/'<'/

   /' Utility functions '/


   Declare Function U_Box(x As Single,sigma As Single, _ 
                  width As Single) As Single
   Declare Function U_Box2(x As Single,sigma1 As Single, sigma2 As Single, _ 
                  width As Single) As Single
   Declare Function U_Gauss(x As Single,sigma As Single) As Single
   Declare Function U_Gauss_mod(x As Single,sigma As Single) As Single
   Declare Function U_LinGauss(x As Single,R_Sigma As Single) As Single
/'>'/
   Declare Function CC_Count(CIn As String, CDiv As String) As Integer
   Declare Sub CC_Cut(CIn As String,CDiv As String,COut() As String, _
               Byref N as Integer)
   Declare Function Round(R As Single, N As Integer) As Single        
   Declare Function Pexplim(R_lambda As Single,xmin As Single,xmax As Single) As Single
   Declare Function PBox(Mean As Single,Sigma As Single,Width As Single) As Single 
   Declare Function PBox2(Mean As Single,Sigma1 As Single,Sigma2 As Single, _
              Bottom As Single) As Single
   Declare Function PPower(Order As Integer, Rmin As Single, Rmax As Single) As Single    
   Declare Function PPower_Griffin_v(Order As Integer, Rmin As Single, Rmax As Single) As Single    
   Declare Function PPower_Griffin_E(Order As Integer, Rmin As Single, Rmax As Single) As Single    
   Declare Function PGauss(Mean As Single,Sigma As Single) As Single
   Declare Function PLinGauss(Sigma As Single) As Single
   Declare Function PExp(Tau As Single) As Single
   Declare Function PMaxwell(R_T As Single) As Single
   Declare Function PMaxwellMod(R_T As Single,R_A As Single) As Single
   Declare Function PMaxwellv(R_T As Single) As Single
   Declare Function Floor(R As Single) As Single
   Declare Function Ceil(R As Single) As Single
   Declare Function Modulo(I As ULongint, J As ULongint) As Longint  
   Declare Function PLoss(IL As Ulongint) As Integer
  #Ifdef B_EgammaA 
   Declare Sub StoreEgammaA(Egamma1keV As Integer, Apre As Integer)
  #EndIf 
/'<'/

   Declare Function U_Valid(I_Z As Integer,I_A As Integer) As Ubyte
 
   
   Declare Function U_Delta_S0(I_Z As Integer,I_A As Integer) As Single 

/'>'/   

   
   Declare Sub await (Ithread As Integer,Ithreadmax As Integer) 
   Sub await(Ithread As Integer,Ithreadmax As Integer)
     Dim As Integer isec
     Do
       sleep 100
       isec = second(now) mod (Ithreadmax + 1)
     Loop Until isec = Ithread  
   End Sub
   
   
   Dim As Integer I_Anl = 0
   Dim Shared As Integer N_Anl
   
       ' UDT for analyzer attributes
   Type Analyzer_Attributes
     As String*128 C_Name
     As String*128 C_Title
     As String*128 C_xaxis
     As String*128 C_yaxis
     As String*128 C_Linesymbol
     As Single R_ALim(1 to 4,1 to 3) 
     As Integer I_Dim
     Declare Constructor
     Declare Destructor
   End Type
   
   Constructor Analyzer_Attributes
     R_ALim(1,1) = 0  ' first bin (or element of array)
     R_ALim(1,3) = 1  ' binsize
     R_ALim(2,1) = 0  ' first bin (or element of array)
     R_ALim(2,3) = 1  ' binsize
     R_Alim(3,1) = 0  ' first bin (or element of array)
     R_ALim(3,3) = 1  ' binsize
     R_Alim(4,1) = 0  ' first bin (or element of array)
     R_ALim(4,3) = 1  ' binsize
     I_Dim = 1        ' dimension of analyzer
     C_xaxis = "Channel"
     C_yaxis = "Counts"
     C_Linesymbol = "HT0"
   End Constructor
   Destructor Analyzer_Attributes
   End Destructor
   
   Declare Function Find_IAnl(CAnl As String) As Integer
   Declare Sub U_DMP_1D(Array() As Double,CName As String,Ccmt As String)  
   Declare Sub U_DMP_1D_S(Array() As Single,CName As String,Ccmt As String)  

   Dim Shared As String CdmpFolder
   Dim Shared As Integer DMPFile

      
   Dim Shared As Ubyte Bsub = 0  ' For extracting the subroutine version of GEF.

/'<'/


/' Internal variables '/
    Const As Single pi = 3.14159
    Dim Shared As Integer I_N_CN /' Neutron number of fissioning nucleus '/
    Dim Shared As Longint I,J,K
    Dim Shared As Single T_coll_Mode_1,T_coll_Mode_2,T_coll_Mode_3,T_coll_Mode_4
    Dim Shared As Single T_asym_Mode_1,T_asym_Mode_2,T_asym_Mode_3,T_asym_Mode_4,T_asym_Mode_0
    Dim Shared As Single Sigpol_Mode_1,Sigpol_Mode_2,Sigpol_Mode_3,Sigpol_Mode_4
    Dim Shared As Single R_Z_Curv_S0,R_Z_Curv1_S0,R_A_Curv1_S0
    Dim Shared As Single ZC_Mode_0,ZC_Mode_1,ZC_Mode_2,ZC_Mode_3,ZC_Mode_4
    Dim Shared As Single SigZ_Mode_0,SigZ_Mode_1,SigZ_Mode_2,SigZ_Mode_3,SigZ_Mode_4
    Dim Shared As Single SN,Sprot
    Dim Shared As Single E_exc_S0_prov,E_exc_S1_prov,E_exc_S2_prov,E_exc_S3_prov,E_exc_S4_prov
    Dim Shared As Single E_exc_S11_prov,E_exc_S22_prov
    Dim Shared As Single E_exc_Barr
    Dim Shared As Single E_LD_S1,E_LD_S2,E_LD_S3,E_LD_S4
    Dim Shared As Single R_Shell_S1_eff,R_Shell_S2_eff,R_Shell_S3_eff,R_Shell_S4_eff
    Dim Shared As Single Yield_Norm
    Dim Shared As Single R_E_exc_eff
    Dim Shared As Single R_Z_Heavy,R_Z_Light
    Dim Shared As Integer I_Mode
    Dim Shared As Single T_Pol_Mode_0,T_Pol_Mode_1,T_Pol_Mode_2,T_Pol_Mode_3,T_Pol_Mode_4
    Dim Shared As Single E_Min_Barr
    Dim Shared As Single RI
    Dim Shared As Single rbeta, beta1, beta2
    Dim Shared As Single rbeta_ld, rbeta_shell
    Dim Shared As Single ZUCD
    Dim Shared As Single Z
    Dim Shared As Single E_tunn
    Dim Shared As Single beta1_opt,beta2_opt,beta1_prev,beta2_prev
    Dim Shared As Single Z1,Z2
    Dim Shared As Integer IZ1,IN1,IZ2,IN2
    Dim Shared As Single A1,A2
    Dim Shared As Integer IA1,IA2
    Dim Shared As Single E_defo
    Dim Shared As Single R_Pol_Curv_S0, R_Pol_Curv_S1, R_Pol_Curv_S2,R_Pol_Curv_S3,R_Pol_Curv_S4
    Dim Shared As Single RA,RZ
    Dim Shared As Single SigA_Mode_0, SigA_Mode_1, SigA_Mode_2,SigA_Mode_3,SigA_Mode_4
    Dim Shared As Single AC_Mode_0, AC_Mode_1, AC_Mode_2, AC_Mode_3, AC_Mode_4
    Dim Shared As Single R_A_heavy, R_A_light
    Dim Shared As Single RZpol
/'>'/    
    Dim As Single Eexc_light,Eexc_heavy
/'<'/    
    Dim Shared As Single T_intr_Mode_0,T_intr_Mode_1_heavy,T_intr_Mode_1_light
    Dim Shared As Single T_intr_Mode_2_heavy,T_intr_Mode_2_light
    Dim Shared As Single T_intr_Mode_3_heavy,T_intr_Mode_3_light
    Dim Shared As Single T_intr_Mode_4_heavy,T_intr_Mode_4_light
    Dim Shared As Single T
    Dim Shared As Single DU0,DU1,DU2,DU3,DU4
/'>'/    
    Dim Shared As Single E_intr,E_intr_light,E_intr_heavy
    Dim Shared As Single E_intr_light_mean,E_intr_heavy_mean
    Dim Shared As Single Eexc_heavy_mean, Eexc_light_mean
    Dim Shared As Single Ecoll,Ecoll_mean,Ecoll_heavy,Ecoll_light
    Static As Single E_EXC_TRUE,E_EXC_ISO  ' Excitation energy of isomeric state
    Dim Shared As Single Qvalue,TKEmin
    Dim Shared As Single Rtest
    Dim Shared As Single Theavy,Tlight
/'<'/    
    Dim Shared As Single T_low_S1_used
    Dim Shared As Single SigA_Mode_11,SigA_Mode_22
    Dim Shared As Integer Ngtot = 0
    Dim Shared As Integer Nglight = 0
    Dim Shared As Integer Ngheavy = 0
    Dim Shared As Single Egtot1000 = 0
    Dim Shared As Single S1_enhance, S2_enhance
    Dim Shared As Single DZ_S2_lowE = 0    
    Dim Shared As Integer I_A_CN,I_Z_CN
/'>'/  
    Dim Shared As Single TKE, TKE_post, Ekinlight, Ekinheavy, Ekinlight_post, Ekinheavy_post
    Dim Shared As Single vkinlight,vkinheavy

  /' Input parameters: '/
    Dim Shared As UByte B_Error_On = 1        /' Error analysis required '/
    Dim As UByte B_Error_Analysis
    Dim As Integer N_Error_Max = 10    /' Number of different random parameter sets '/    
    Dim As Integer I_Error = 0         /' Counts the parameter sets '/
    
    Dim As UByte B_Random_on = 0       /' Write ENDF random files '/
    
    Dim As Integer I_DelGam = 0

    Dim Shared As Integer P_Z_CN = 92           /'`Z of fissioning nucleus '/
    Dim Shared As Integer P_A_CN = 236          /' A of fissioning nucleus '/
    Dim As Single P_E_exc = 2                   /' Energy above lowest outer barrier EB '/
    
    Dim As UByte B_Double_Covar = 0    /' Option: covariances for yields of 2 fragments '/
    Dim As Integer I_Double_Covar      /' Sequence of calculations for 2 fragments '/
    Dim As Integer N_Double_Covar
    Dim As Integer P_Z_CN_Double 
    Dim As Integer P_A_CN_Double
    Dim As Single P_E_exc_Double
    
/'<'/
    Dim Shared As Single P_I_rms_CN = 0                  /' rms initial angular momentum '/

    ' Model parameters of GEF

    Dim Shared As Single  Emax_valid = 100      /' Maximum allowed excitation energy '/
