Function DeleteNaraeonSSDToolsService
  ExecWait '"$SYSDIR\sc.exe" stop NaraeonSSDToolsDiag'
  ExecWait '"$SYSDIR\sc.exe" delete NaraeonSSDToolsDiag'
FunctionEnd

Function un.DeleteNaraeonSSDToolsService
  ExecWait '"$SYSDIR\sc.exe" stop NaraeonSSDToolsDiag'
  ExecWait '"$SYSDIR\sc.exe" delete NaraeonSSDToolsDiag'
FunctionEnd