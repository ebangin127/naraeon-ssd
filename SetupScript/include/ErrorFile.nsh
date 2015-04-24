Function SetErrFile
  WriteRegStr HKEY_CLASSES_ROOT ".err" "" "Naraeon.ErrorFile"
  WriteRegStr HKEY_CLASSES_ROOT "Naraeon.ErrorFile" "" ""
  WriteRegStr HKEY_CLASSES_ROOT "Naraeon.ErrorFile\DefaultIcon" "" "$INSTDIR\SSDTools\Image\warning.ico"
  WriteRegStr HKEY_CLASSES_ROOT "Naraeon.ErrorFile\shell" "" ""
  WriteRegStr HKEY_CLASSES_ROOT "Naraeon.ErrorFile\shell\open" "" ""
  WriteRegStr HKEY_CLASSES_ROOT "Naraeon.ErrorFile\shell\open\command" "" 'notepad "%1"'
  WriteRegStr HKEY_LOCAL_MACHINE "SYSTEM\CurrentControlSet\Services\NaraeonSSDToolsDiag" "Description" "Naraeon SSD Tools - SSD life alerter service."
FunctionEnd

Function un.DeleteErrFile
  DeleteRegKey HKEY_CLASSES_ROOT ".err"
  DeleteRegKey HKEY_CLASSES_ROOT "Naraeon.ErrorFile"
FunctionEnd