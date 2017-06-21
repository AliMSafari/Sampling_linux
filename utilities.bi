   Declare Function Min(R1 As Single,R2 As Single) As Single
   Declare Function Max(R1 As Single,R2 As Single) As Single
   Declare Function Erf(x As Single) As Single
   Declare Function Erfc(x As Single) As Single
   Declare Function Tanh(x As Single) As Single
   Declare Function Coth(x As Single) As Single
   Declare Function Log10(R As Single) As Single
   
   
   Public Function Min(R1 As Single,R2 As Single) As Single
   	Dim Rmin As Single
   	If R1 < R2 Then
   	  Rmin = R1
   	Else
   	  Rmin = R2
   	EndIf
   	Min = Rmin
   End Function

   Public Function Max(R1 As Single,R2 As Single) As Single
   	Dim Rmax As Single
   	If R1 > R2 Then
   	  Rmax = R1
   	Else
   	  Rmax = R2
   	EndIf
   	Max = Rmax
   End Function
   
   Public Function Erf(x As Single) As Single
   /' Sergei Winitzki, 2008: relative accuracy < 1.4E-4 '/
   '  Dim As Single a = 0.147
   '  Const As Single b = 1.27324  ' 4/pi,
   '  Const As Single pi = 3.14159
   '  Dim As Single Result
   '  Result = Sqr(1.E0 - exp(-x^2 * (b + a * x^2) / (1.E0 - a * x^2) ) )
   '  If x < 0 Then Result = - Result
   '  WiErf = Result
     Erf = 1.E0 - Erfc(x)
   End Function

   Public Function Erfc(x As Single) As Single
   /' Complementary error function from numerical recipes '/
     Dim As Double t,z,r
     z = Abs(x)
     t = 1.E0/(1.E0 + 0.5E0 * z)
     r = t*exp(-z*z-1.26551223+t*(1.00002368+t*(0.37409196 _
      +t*(0.09678418+t*(-0.18628806+t*(0.27886807+t*(-1.13520398 _
      +t*(1.48851587+t*(-0.82215223+t*0.17087277)))))))))
     If (x < 0) Then
       Erfc = 2.E0 - r
     Else
       Erfc = r
     Endif
   End Function   
   
   Public Function Tanh(x As Single) As Single
     Dim As Single Result
     If x >= 0 Then
       Result = (1 - exp(-2*x))/(1 + exp(-2*x))
     Else
       Result = (exp(2*x) - 1)/(exp(2*x) + 1)
     EndIf
     Tanh = Result
   End Function

   Public Function Coth(x As Single) As Single
     Dim As Single Result
     If x >= 0 Then
       Result = (1 + exp(-2*x))/(1 - exp(-2*x))
     Else
       Result = (exp(2*x) + 1)/(exp(2*x) - 1)
     EndIf
     Coth = Result
   End Function

   Public Function Log10(R As Single) As Single
     log10 = Log(R) / Log(10)
   End Function
