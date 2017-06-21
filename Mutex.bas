#Include Once "vbcompat.bi"

Declare Sub WaitSynch

Sub WaitSynch_GUI
' Wait for synchronisation with GUI
' GEF starts at (n + 0) seconds
  Dim As Integer I
  Dim As Double Tlast
  Tlast = Timevalue(Time)
  For I = 1 To 100
    Sleep 100,1
    If Timevalue(Time) > Tlast Then Exit For
    Tlast = Timevalue(Time)
  Next
End Sub

