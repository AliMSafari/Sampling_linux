1. LICENCE AND COPYRIGHT

'    The development of the GEF code has been supported by the European Union,
'    EURATOM 6, Framework Program "European Facilities for Nuclear Data
'    Measurements" (EFNUDAT), contract number FP6-036434, the Framework
'    Program "European Research Infrastructure for Nuclear Data Applications"
'    (ERINDA),  contract number FP7-269499, and by the Nuclear Energy Agency of 
'    the OECD.

'
'    The GEF code is free software: you can redistribute it and/or modify
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


'    Copyright 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016: 
'       Dr. Karl-Heinz Schmidt, Rheinstraße 4, 64390 Erzhausen, Germany
'       and 
'       Dr. Beatriz Jurado, Centre d'Etudes Nucleaires de Bordeaux-Gradignan,
'       Chemin du Solarium, Le Haut Vigneau, BP 120, 33175 Gradignan, Cedex,
'       France 
 

2. NAME OF THE PROGRAM:  GEF 2016/1.2


3. DESCRIPTION OF PROGRAM OR FUNCTION

GEF is a computer code for the simulation of the nuclear fission process. 
The GEF code calculates pre-neutron and post-neutron fission-fragment nuclide 
yields, angular-momentum distributions, isomeric yields, prompt-neutron yields 
and prompt-neutron spectra, prompt-gamma spectra, and several other quantities 
for a wide range of fissioning nuclei from polonium to seaborgium in 
spontaneous fission and neutron-induced fission. Multi-chance fission (fission
after emission of neutrons) is included. For neutron-induced fission, the
pre-compound emission of neutrons is considered. Output is provided as tables 
and as parameters of fission observables on an event-by-event basis.

Specific features of the GEF code:
- The mass division and the charge polarisation are calculated assuming a 
statistical population of states in the fission valleys at freeze-out. 
The freeze-out time considers the influence of fission dynamics and is not 
the same for the different collective variables.
- The separability principle [1] governs the interplay of macroscopic and 
microscopic effects.
- Five fission channels are considered. The strengths of the shells in the 
fission valleys are identical for all fissioning systems. The mean positions 
of the heavy fragments in the asymmetric fission channels are essentially 
constant in atomic number, as suggested by experimental data [2].
- The stiffness of the macroscopic potential with respect to mass asymmetry 
is deduced from the widths of measured mass distributions [3].
- The excitation-energy-sorting mechanism [4,5,6,7] determines the prompt 
neutron yields and the odd-even effect in fission-fragment yields of even-Z 
and odd-Z systems.
- Prompt neutron emission from the fragments is calculated with a Monte-Carlo 
statistical code using level densities from empirical systematics [8] and 
binding energies with theoretical shell effects with gamma competition included.
- Spectra and multiplicities of prompt gamma emission are provided. 
Non-statistical gamma emission is calculated with a dedicated VMI model. 
- Model uncertainties and covariances are determined by a series of calculations 
with perturbed parameters. Covariances of fission yields from two different
systems are available.
- Multi-chance fission is supported.
- Pre-compound emission of neutrons is considered for neutron-induced fission.

The official GEF websites are http://www.khs-erzhausen.de and 
http://www.cenbg.in2p3.fr/GEF.


4. METHOD OF SOLUTION

The Monte-Carlo method is used. 
Uncertainties are deduced from perturbed calculations. 


5. TYPICAL RUNNING TIME

A typical calculation with 100 000 events takes about 5 seconds on one processor 
of an Intel i7 CPU (2.80GHz). Calculations with perturbed parameters and 
calculations at higher excitation energies, where multi-chance fission occurs, 
require somwhat more time.


6. RELATED AND AUXILIARY PROGRAMS

The main routines are written in FreeBASIC (http://www.freebasic.net/). FeeBASIC 
produces compiled binary code using the C run-time library. Graphics output is 
based on the X11 library. A graphical user interface is provided for Windows [a], 
written in JustBasic (http://www.justbasic.com/), which has a specific run-time 
library. The Windows version of GEF runs also under WINE on LINUX.

                                       
7. REFERENCES

 [1] Experimental evidence for the separability of compound-nucleus and fragment properties in fission, 
     K -H Schmidt, A Kelic, M V Ricciardi, Europh. Lett. 83 (2008) 32001
 [2] Nuclear-fission studies with relativistic secondary beams: analysis of fission channels, 
     C. Boeckstiegel et al., Nucl. Phys. A 802 (2008) 12
 [3] Shell effects in the symmetric-modal fission of pre-actinide nuclei, 
     S. I. Mulgin, K.-H. Schmidt, A. Grewe, S. V. Zhdanov, Nucl. Phys. A 640 (1998) 375
 [4] Entropy-driven excitation-energy sorting in superfluid fission dynamics,
     K.-H. Schmidt, B. Jurado, Phys. Rev. Lett. 104 (2010) 212501
 [5] New insight into superfluid nuclear dynamics from the even-odd effect in fission, 
     K.-H. Schmidt, B. Jurado, arXiv:1007.0741v1 [nucl-th]
 [6] Thermodynamics of nuclei in thermal contact, 
     K.-H. Schmidt, B. Jurado, Phys. Rev. C 82 (2011) 014607
 [7] Final excitation energy of fission fragments, 
     K.-H. Schmidt, B. Jurado, Phys. Rev. C 83 (2011) 061601(R)
 [8] Inconsistencies in the description of pairing effects in nuclear level densities, 
     K.-H. Schmidt, B. Jurado, Phys. Rev. C 86 (2012) 044322
 [9] General description of fission observables,
     K.-H. Schmidt, B. Jurado, Ch. Amouroux, JEFF-Report 24, NEA of OECD, 2014  
[10] Revealing hidden regularities with a general approach to fission
     K.-H. Schmidt, B. Jurado, Eur. Phys. J. A 51 (2015) 176
[11] General description of fission observables: GEF model code
     K.-H. Schmidt, B. Jurado, C. Amouroux, C. Schmitt, Nucl. Data Sheets 131 (2016) 107   


8. HARDWARE REQUIREMENTS

GEF can be compiled and installed under Windows [a] and Linux, using exactly 
the same sources files. Specific executables are provided for the two systems.
GEF was tested on Windows [a] and Linux.

Memory < 250 MByte; Disc < 100 MByte, eventually more for event-wise output.


9. PROGRAMMING LANGUAGE(S) USED

Computer language
on Linux: FreeBASIC; on Windows [a]: FreeBASIC and JustBasic


10. OPERATING SYSTEM UNDER WHICH PROGRAM IS EXECUTED

a) Windows [a] XP or newer
b) Any Linux distribution, 32-bit or 64-bit. Eventually, some additional 
libraries need to be installed, see www.freebasic.net -> documentation 
-> using the FreeBASIC compiler -> Installing FreeBASIC.
(It is recommended to use the 32-bit version of FreeBASIC on a 64-bit system 
for better numerical stability. Please install the appropriate libraries!)


11. OTHER PROGRAMMING OR OPERATING INFORMATION OR RESTRICTIONS 

Multi-chance fission is supported, except when a distribution of excitation 
energies at fission is provided on input. 
The results on neutron emission prior to fission and prompt-neutron emission 
between saddle and sission, and from the fragments are given separately. 
The sequence of the events in the list-mode output is sorted by energy at 
fission in the case of multi-chance fission in order to save computing time.
An optional enhancement factor may be specified. A value >1 increases the 
statistics of the Monte-Carlo calculation and hence reduces the statistical 
uncertainties of the results. Default value is 1.E5 events. With this value, 
the statistical uncertainties are already smaller than the model uncertainties 
in most cases. Higher statistics may be useful to compare different systems, to 
study systematic trends and to determine reliable covariances.
GEF provides all results event by event in a list-mode file on demand.


12. NAME AND ESTABLISHMENT OF AUTHORS

K.-H. Schmidt, Rheinstr. 4, 64390 Erzhausen, Germany
B. Jurado, CENBG, CNRS/IN2 P3, Chemin du Solarium B.P. 120, F-33175 Gradignan, France


13. MATERIAL AVAILABLE

FreeBASIC [c] source files. JustBasic [d] executable and run-time-library.
Executables for Windows [a] and Linux.
ReadMe file with technical instructions.


14. CATEGORIES

Nuclear fission

Keywords: Monte-Carlo method, event generator, macroscopic-microscopic model, 
separability principle, energy sorting, statistical model, 
pre-equilibrium emission, multi-chance fission, neutron evaporation,
prompt-gamma emission, uncertainties and covariance matrix of fission-fragment yields.


15. PRACTICAL HINTS

Please keep the sub-folder structure of GEF.zip. Subfolders that are 
needed by the code for output are created automatically, if they do 
not exist. GEF does not overwrite or delete the output files. Files 
in the folders out, tmp, and dmp that are not needed any more should 
be deleted explicitely.
"out" contains the main output as ASCII tables.
"tmp" contains more specific or internal information as ASCII tables.
"dmp" contains spectra in SATAN analyzer format. 
"ctl" contains control files for multithreaded calculations.

On Windows [a]:
The file GEF.zip provides an executable of the main programm (GEF.exe) 
and - in the subfolder GUI - a graphical user interface.
GEF is started by running "GEF.bat" (!) in a command window.
All user input must be entered by the GUI window!

If you want to apply any changes, use an IDE (e.g. FBIDE [b]) for editing
any of the source files (*.bas). Compile the main routine GEF.bas under 
FreeBASIC [c]. The other files are automatically included in the compilation 
process. (Compile by pressing the "compile" botton of FBIDE when GEF.bas is open.) 
The GUI is written in JustBasic [d].

On Linux:
The file GEF.zip provides an executable (GEF) that runs directly in a 
terminal by entering "./GEF". (Do not forget to set the file properties to
"execute as a programm".)

The GUI that is provided in the Windows version may also be used under Linux
by running the Windows version of GEF under Wine [e] without any loss of
performance.

If you want to make any changes to GEF, prepare an executable, using 
an IDE (e.g. GEANY [f]) with the FreeBASIC [c] compiler. GEF.bas is the main 
routine. The other files are automatically included in the compilation process.
Compile by pressing the "compile" botton of GEANY when GEF.bas is open or
by the command "fbc GEF.bas".
Remark: Installation of additional packages may be required. (See
http://www.freebasic.net/ -> Documentation -> User Manual -> 
Using the FreeBASIC Compiler -> Installing FreeBASIC.)
E.g. the graphics output requires the installation of the X11 library. 
If the graphics does not work, you may suppress it by commenting the line
( #Include Once "DCLPlotting.bas" ) in GEF.BAS.


Required input of GEF:

Z and A of fissioning nucleus or target
Excitation mode and excitation energy


Quantities available on output of GEF:

Contributions of fission chances
Relative yields of fission channels
Element-yield distribution*) 
Isotonic-yield distribution (pre- and post-neutron)
Isobaric-yield distribution*)
Mass-chain yields (pre- and post-neutron)*)
Fragment angular-momentum distributions (for every nuclide)
Relative independent isomeric yields
Prompt-gamma spectrum 
Prompt-neutron spectrum 
Neutron-multiplicity distribution
Energies and directions of prompt neutrons (pre- and post-scission)
(Many more quantities are internally calculated and may be listed.)

*) Including uncertainties and covariances.


Advanced options:

- UNCERTAINTIES
Uncertainty analysis from calculations with perturbed parameters is available.
These calculations are also used to determine covariances between different 
observables as required by the model. Covariances between the fission yields
of two different systems can also be provided. 

- ENERGY DISTRIBUTION
Instead of a single energy, also a distribution of excitation energies
above the ground-state at fission may be provided in a file. 
The file name is fixed: Espectrum.in. 
     - Example: (energy, weight)
           3.9    0.1
           4.0    0.2
           4.1    0.4
           4.2    0.7
           ...
Each line gives an energy (in MeV) and a weight. 
Energy steps of about 100 keV are recommended. The spectrum may be
un-normalized. 
It is also possible to insert the rms compound-nucleus angular momentum as
the second row to the input data in Espectrum.in. (Zero angular momentum is 
replaced by the ground-state spin of the compound nucleus.)
     - Example: (energy, spin, weight)
           3.9     9.8    0.1
           4.0    10.1    0.2
           4.1    10.4    0.4
           4.2    10.9    0.7
           ...
The corresponding option is chosen by the GUI under Windows or by the 
option "ES" under Linux. Note that GEF calculates only first-chance fission 
for this option!   

- INPUT LIST
GEF supports reading an input list from file. This option is chosen if 
the file "file.in" is found.
Instructions:

1. Create a file with the following information: 
     First line: Statistical enhancement factor (default = 1 corresponds
                 to 10^5 events per system). A larger factor increases 
                 the number of calculated events accordingly.
     Second line: Energy value or list of energy values
         For neutron-induced fission: List of energy values in ascending order.
         For spontaneous fission: Energy value. (Only one value is allowed.)
     Following lines: Specification of the fissioning system.
                 (Z_CN, A_CN, kind of fission)
         A range of consecutive isotopes can be specified in a shorter way:            
                 (Z_CN, A_CN_first - A_CN_last, kind of fission)
     - Example for spontaneous fission:
             10
             0
             94, 238-242, "GS"
             98, 250, "GS"
             ' 98, 250, "GS"
             98, 252, "GS"
             END
             99, 250, "GS"
             99, 251, "GS"
       In this example, the system 98, 250 is skipped due to the comment sign,
       and the last two lines after the END line are disregarded.      
             ...      
     - Example for neutron-induced fission:
             2
             0.0253E-6, 0.4, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14  
             92, 234-239, "EN"
             ...
       In the case of neutron-induced fission, a sequence of calculations is
       performed with the energies given in the second line of the input file.

     - Example for fission from a shape isomer:
       (The isomers must be listed in the file NucProp.bas.)
             100
             0 
             94, 241, "IS1"
             94, 242, "IS1"
             ...

2. Create the file "file.in", which contains the names of the input
   files (one per line). Again, single lines marked by a comment sign are
   disregarded, and reading is stopped by an "END" line.
     - Example
       "U238NF.in"
       ' "CF252SF.in"     
       "PU240SF.in"
   In this example, only the files U238NF.in and PU240SF.in are treated.
   CF252SF.in is skipped due to the comment sign.

- PARALLEL COMPUTING 
GEF also supports running several processes in parallel, which calculate
the systems given in the input file (specified in "input.in") in parallel 
in a coordinated way. This enables making efficient use of multi-processor 
machines. Before starting a new sequence of calculations, the files 
"/ctl/done.ctl", "/ctl/sync.ctl and "/ctl/thread.ctl" must be deleted. 
You may also delete the complete "/ctl" folder.
After this, open a new command window (terminal) for each GEF process to 
be started (Linux: "./GEF", Windows: "GEF.bat"), until you reach the limit 
that is stored in the variable I_thread_max in GEF.bas.
Make sure that the value of I_thread_max has the right value for your system!
If necessary, change the value and recompile GEF.bas! 
Multi-processing calculations are performed without graphics output.


16. DETERMINISTIC VERSION OF GEF AS A SUBROUTINE IN FREEBASIC AND FORTRAN.

A deterministic version of the GEF code provides pre-neutron fission-fragment 
nuclide distributions and kinetic energies. It is written as a subroutine that
is called with a specific compound nucleus, its excitation energy and its
angular momentum on input. See the file "GEFSUB.pdf" for details.


[a] Windows is either a registered trademark or a trademark of Microsoft 
Corporation in the United States and/or other countries.
[b] FBIDE is available from http://fbide.freebasic.net/ with no cost.
[c] FreeBASIC is available from http://www.freebasic.net/ with no cost.
[d] JustBasic is available from http://www.justbasic.com/ with no cost.
[e] Wine is a windows compatibility layer for Linux (http://www.winehq.org/)
[f] Geany is available from http://www.geany.org/ with no cost.


In case of problems, please contact  schmidt-erzhausen at t-online.de .
