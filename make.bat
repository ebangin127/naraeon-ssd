@SET BDS=C:\Program Files (x86)\Embarcadero\RAD Studio\10.0
@SET BDSINCLUDE=C:\Program Files (x86)\Embarcadero\RAD Studio\10.0\include
@SET BDSCOMMONDIR=C:\Users\Public\Documents\RAD Studio\10.0
@SET FrameworkDir=C:\Windows\Microsoft.NET\Framework\v3.5
@SET FrameworkVersion=v3.5
@SET FrameworkSDKDir=
@SET PATH=%FrameworkDir%;%FrameworkSDKDir%;C:\Program Files (x86)\Embarcadero\RAD Studio\10.0\bin;C:\Program Files (x86)\Embarcadero\RAD Studio\10.0\bin64;%PATH%
@SET LANGDIR=EN
%~d0
cd "%~dp0"
cd trunk
MSBuild SSDTools.dproj
cd Service
MSBuild NSTDiagSvc_New.dproj
cd ..
cd ..
cd SetupScript
makensis Patch.nsi
makensis Setup.nsi