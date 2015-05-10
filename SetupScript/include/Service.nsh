!include x64.nsh

Function DeleteNaraeonSSDToolsService
  ExecWait '"$SYSDIR\sc.exe" stop NaraeonSSDToolsDiag'
  ExecWait '"$SYSDIR\sc.exe" delete NaraeonSSDToolsDiag'
FunctionEnd

Function un.DeleteNaraeonSSDToolsService
  ExecWait '"$SYSDIR\sc.exe" stop NaraeonSSDToolsDiag'
  ExecWait '"$SYSDIR\sc.exe" delete NaraeonSSDToolsDiag'
FunctionEnd

Function CreateNaraeonSSDToolsService
  ExecWait '"$SYSDIR\sc.exe" create NaraeonSSDToolsDiag binPath= "$INSTDIR\SSDTools\NSTDiagSvc_New.exe" DisplayName= "Naraeon SSD Tools - SSD life alerter" start= auto'
  ${if} ${RunningX64}
    WriteRegDWORD HKLM "SYSTEM\CurrentControlSet\Services\NaraeonSSDToolsDiag" "WOW64" 1
  ${endif}
FunctionEnd