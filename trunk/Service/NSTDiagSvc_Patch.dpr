program NSTDiagSvc_Patch;

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
  uIntFunctions in '..\Modules\Etc\uIntFunctions.pas',
  uStrFunctions in '..\Modules\Etc\uStrFunctions.pas',
  uLogSystem in '..\Classes\LogSystem\uLogSystem.pas',
  uNSTSupport in '..\NSTSupport\Abstraction\uNSTSupport.pas',
  uSSDSupport in '..\Classes\SSDInfo\uSSDSupport.pas',
  uDirectCommandSet in '..\Classes\ATALowOps\uDirectCommandSet.pas',
  uGetFirm in '..\Classes\GetFirm\uGetFirm.pas',
  uPhysicalDriveList in '..\Classes\SSDList\uPhysicalDriveList.pas',
  uDatasizeUnit in '..\Classes\MeasureUnit\uDatasizeUnit.pas',
  uTimeUnit in '..\Classes\MeasureUnit\uTimeUnit.pas',
  uDiskGeometryGetter in '..\WindowsFileAPI\uDiskGeometryGetter.pas',
  uDriveAvailabilityGetter in '..\WindowsFileAPI\uDriveAvailabilityGetter.pas',
  uFixedDriveListGetter in '..\WindowsFileAPI\uFixedDriveListGetter.pas',
  uMotherDriveGetter in '..\WindowsFileAPI\uMotherDriveGetter.pas',
  uPartitionListGetter in '..\WindowsFileAPI\uPartitionListGetter.pas',
  uIoControlFile in '..\WindowsFileAPI\Abstraction\uIoControlFile.pas',
  uOSFile in '..\WindowsFileAPI\Abstraction\uOSFile.pas',
  uOSFileWithHandle in '..\WindowsFileAPI\Abstraction\uOSFileWithHandle.pas',
  uDrive in '..\Objects\uDrive.pas',
  uPhysicalDrive in '..\Objects\uPhysicalDrive.pas';

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
