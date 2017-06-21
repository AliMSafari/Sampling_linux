import os
from numpy import random, sqrt, log, sin, cos, pi
from pylab import show, hist, subplot, figure
from subprocess import call




'''
Setting up the directory and base conditions

'''


gef_directory = os.getcwd() + "/"
print "Working Directory:", gef_directory

gef_bas1 = gef_directory + "GEF_start.bas"
gef_bas2 = gef_directory + "GEF_end1.bas"
gef_bas3 = gef_directory + "GEF_end.bas"
gef_bas = gef_directory +"GEF.bas"
gef_exe = gef_directory+ "GEF"

element = "U"
charge = 92
mass_TN = 235
mass_CN = 236
energyMeV = 2.53E-08
enhancementStat_list = [10]

sample_per_parameter = 50
Size_of_Gaussian_Sampling = 1000


'''
defining various functions for Normal distibution sampling and compiling/running GEF
'''

'''
def normal_dit(u1,u2, sample_per_parameter):  # this functions takes u1, the mean, and u2, the variance, of a normal distibution and returns n samples
    return numpy.random,normal(u1, u2, sample_per_parameter)
'''

def gaussian(u1, u2):
     z1 = sqrt(-2*log(u1))*cos(2*pi*u2)
     z2 = sqrt(-2*log(u1))*sin(2*pi*u2)
     return z1,z2

def compile_run_gef(param_folder, sample, variance_condition, enhancementStat): 

     # Remove old file
     if os.path.isdir("tmp"):
          os.system("rm -r dmp ctl out ENDF")  #if the directory tmp excist then the following folders are being deleted

     removeGEF = "rm -f temp.txt "+gef_bas 
     makeNewGEFSourceCode = "cat " +gef_bas1+ " temp_mean.txt "+gef_bas2+" temp_var.txt " + gef_bas3+ " > "+gef_bas
     compileGEF = "fbc "+ gef_bas
     exeGEF = gef_exe+" < file.in > gef.out"

     # removing the old temp parameter and GEF files (bas)
     os.system(removeGEF)

     #write the sample parameter to the new GEF source .bas
     temp_parameter_file = open('temp_mean.txt', 'w')
     for i in range(0, len(param_name)):
         temp_parameter_file.write("    Dim As Single    _"+param_name[i]+" = "+str(sample_parameter_value[i])+"\n")
     temp_parameter_file.close();

     #write the new sample parameter of variance in the GEF source .bas
     temp_variance_file = open('temp_var.txt', 'w')
     if variance_condition == "Normal_Variance":
          for i in range(0, len(param_name)):
               temp_variance_file.write("    Dim As Single Var_"+param_name[i]+" =  "+ str(param_def_var[i])+ "\n")
     else:
          for i in range(0, len(param_name)):
               temp_variance_file.write("    Dim As Single Var_"+param_name[i]+" =  0\n")
     temp_variance_file.close()

     #Writing the new GEF.bas file including the variation in the parameter
     os.system(makeNewGEFSourceCode)

     #compiling the new GEF source code
     print "    The new GEF source code with the new parameter is being compiled"
     os.system(compileGEF)
     print "    ...Successful"

     print "    GEF is running with the new parameters"
     os.system(exeGEF)
     print "    ...Done"


     # Store the file output in the parameter variation and iteration folder
     StorageDirectory = "param_var/"+str(variance_condition)+"/"+param_folder+"/"+"enhancementStat_"+str(enhancementStat)+"/"+"IterationNom_"+str(sample)
     makeDirOutPut = "mkdir -p "+StorageDirectory
     CopyENDF = "cp -f ENDF/* "+StorageDirectory
     copyout = "cp -f out/* "+StorageDirectory

     #Check to not overwrite the existing outputs

     if os.path.isdir(StorageDirectory):
          if os.listdir(StorageDirectory) != "":
               print("--------> WARNING: Overwriting existing output of"+" sample: "+str(sample)+ "<-----")

     os.system(makeDirOutPut)
     os.system(CopyENDF)
     os.system(copyout)


     sampleFile = StorageDirectory+"/Parameters.txt"
     # Write the sampled parameters to storage

     temp_sample_file = open(sampleFile, 'w')
     for i in range(0, len(param_name)):
          temp_sample_file.write("        "+param_name[i]+" = "+str(sample_parameter_value[i])+"\n")
     temp_sample_file.close();


'''
Defining the inital parameter values and distribution moments
'''

param_name = [
     "P_DZ_Mean_S1", "P_DZ_Mean_S2", "P_A_Width_S2", "P_DZ_Mean_S3",
     "P_DZ_Mean_S4", "Delta_S0    ", "P_Shell_S1  ", "P_Shell_S2  ",
     "P_Shell_S3  ", "P_Shell_S4  ", "P_Z_Curv_S1 ", "P_Z_Curv_S2 ",
     "P_Z_Curv_S3 ", "P_Z_Curv_S4 ", "T_low_S1    ", "T_low_S2    ",
     "T_low_S3    ", "T_low_S4    ", "T_low_SL    ", "HOMPOL      ",
     "POLARadd    " ]

param_folder_name = [
     "P_DZ_Mean_S1", "P_DZ_Mean_S2", "P_A_Width_S2", "P_DZ_Mean_S3",
     "P_DZ_Mean_S4", "Delta_S0"    , "P_Shell_S1"  , "P_Shell_S2"  ,
     "P_Shell_S3"  , "P_Shell_S4"  , "P_Z_Curv_S1" , "P_Z_Curv_S2" ,
     "P_Z_Curv_S3" , "P_Z_Curv_S4" , "T_low_S1"    , "T_low_S2"    ,
     "T_low_S3"    , "T_low_S4"    , "T_low_SL"    , "HOMPOL"      ,
     "POLARadd" ]

'''
# The mean values taken from DR BMC paper in ANE
param_mean = [
     0.050 , -1.0  , 14.48 , 0.0   ,
     0.7   , 0.0   , -1.85 , -4.0  ,
     -6.0  , -1.30 , 0.30  , 0.095 ,
     0.076 , 0.28  , 0.34  , 0.31  ,
     0.31  , 0.31  , 0.31  , 2.00  ,
     0.32 ]
'''
#The mean values are taken from the GEF source code file on 13 Jun 2017
param_mean = [
     0.050 , -1.0  , 14.5 , 0.0   ,
     0.0   , 0.0   , -1.85 , -4.0  ,
     -6.0  , -1.0 , 0.30  , 0.095 ,
     0.068 , 0.05  , 0.342  , 0.31  ,
     0.31  , 0.31  , 0.31  , 2.00  ,
     0.35 ]


#The default variance values taken from GEF-5.2 basic code versino
param_def_var = [
     0.1    , 0.1    , 0.724  , 0.1    ,
     0.1    , 0.1    , 0.1    , 0.1    ,
     0.2    , 0.05   , 0.015  , 0.00475,
     0.0038 , 0.014  , 0.001  , 0.001  ,
     0.001  , 0.001  , 0.001  , 0.4    ,
     0.032 ]   

'''
Running the sampling process
'''

# Initialise the sampled parameter list
sample_parameter_value = [0] * len(param_name)


# Initialise the history of sampled parameters
sample_history = []
a_list = [0] * sample_per_parameter



def running_sampling_process():

     os.system("date")

     print (" ")
     print("-------------------------------------------------------------")
     print("---------")
     print("-- Calculate for target nucleus: "+element+str(mass_TN))
     print("--                       Energy = "+str(energyMeV))
     print("--")
     print("-------------------------------------------------------------")


     for i in range(0, len(param_name)):
          sample_history.append(list(a_list))

     # Write the GEF input file pointer

     gef_in_file = open('file.in', 'w')
     gef_in_file.write(str("gef_input"))
     gef_in_file.close();

     # Write the GEF input file
     gef_input_file = open('gef_input', 'w')
     gef_input_file.write(str(enhancementStat)+"\n")
     gef_input_file.write(str(energyMeV)+"\n")
     gef_input_file.write(str(charge)+", "+str(mass_CN)+", \"EN\"")
     gef_input_file.close();


     # Generating a set of randomly distributed variables for sampling
     u1 = random.rand(sample_per_parameter*len(param_name))
     u2 = random.rand(sample_per_parameter*len(param_name))

     z1,z2 = gaussian(u1, u2)


     # Printing the mean parameters for sampling
     print(" ")
     print("   Here is a list of mean parameters values used in sampling distributions:")
     for i in range(0, len(param_name)):
          print("      "+param_name[i]+" = "+str(param_mean[i]))
     print (" ")


     # Looping over parameters and a set of samples for each parameter to determine yield variation for that parameter
     for param_counter in range(0,3):

          for sample_counter in range(0, sample_per_parameter):

               print "   "
               print "--------------------------------------------------------------------------------------"
               print "------"
               print "-- Running GEF for initial variation of "+param_name[param_counter]+" for sample iteration: "+str(sample_counter)
               print "------"
               print "--------------------------------------------------------------------------------------"

               for write_counter in range(0, len(param_name)):
                    if write_counter == param_counter: 
                         sample_parameter_value[write_counter] = random.normal(param_mean[write_counter], param_def_var[write_counter], Size_of_Gaussian_Sampling)[random.randint(Size_of_Gaussian_Sampling, size = 1)][0]
                         print ("    "+param_name[write_counter]+" has set to "+str(sample_parameter_value[write_counter]))
                         #sample_history[parameter_counter][sample_counter] = sample_parameter_value[write_counter]

                    else:
                         sample_parameter_value[write_counter] = param_mean[write_counter]

               print "All other parameters are mean values"
               print "   "

               compile_run_gef(param_folder_name[param_counter] , sample_counter, variance_condition, enhancementStat)
               print "   "
               os.system("date")


for i in range(len(enhancementStat_list)):
     enhancementStat = enhancementStat_list[i]
     print "enhancementStat is", enhancementStat
     variance_condition = "Zero_Variance"
     running_sampling_process()




##############################
#
# Pritning out the distribution of the parameters selected to verify
#
##############################

# print sample history

'''

def hist_draw():
	figure()
	subplot(331)
	hist(sample_history[0])
	subplot(332)
	hist(sample_history[1])
	subplot(333)
	hist(sample_history[2])
	subplot(334)
	hist(sample_history[3])
	subplot(335)
	hist(sample_history[4])
	subplot(336)
	hist(sample_history[5])
	subplot(337)
	hist(sample_history[6])
	subplot(338)
	hist(sample_history[7])
	subplot(339)
	hist(sample_history[8])
	show()

     '''
