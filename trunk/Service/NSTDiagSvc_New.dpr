program NSTDiagSvc_New;

uses
  Vcl.SvcMgr,
  uMain in 'uMain.pas' {NaraeonSSDToolsDiag: TService},
  Windows,
  uRegFunctions in '..\Modules\Windows\uRegFunctions.pas',
  uLanguageSettings in '..\Modules\Language\uLanguageSettings.pas',
  uExeFunctions in '..\Modules\Windows\uExeFunctions.pas',
  uDiskFunctions in '..\Modules\Disk\uDiskFunctions.pas',
  uPartitionFunctions in '..\Modules\Disk\uPartitionFunctions.pas',
  uSMARTFunctions in '..\Modules\Disk\uSMARTFunctions.pas',
  uTrimCommand in '..\Modules\Disk\uTrimCommand.pas',
  uIntFunctions in '..\Modules\Etc\uIntFunctions.pas',
  uStrFunctions in '..\Modules\Etc\uStrFunctions.pas',
  uLogSystem in '..\Classes\LogSystem\uLogSystem.pas',
  uSSDInfo in '..\Classes\SSDInfo\uSSDInfo.pas',
  uSSDSupport in '..\Classes\SSDInfo\uSSDSupport.pas',
  uATALowOps in '..\Classes\ATALowOps\uATALowOps.pas';

{$R *.RES}

begin
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //

  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TNaraeonSSDToolsDiag, NaraeonSSDToolsDiag);
  Application.Run;
end.
