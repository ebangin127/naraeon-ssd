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
  uIntFunctions in '..\Modules\Etc\uIntFunctions.pas',
  uStrFunctions in '..\Modules\Etc\uStrFunctions.pas',
  uLogSystem in '..\Classes\LogSystem\uLogSystem.pas',
  uDatasizeUnit in '..\Classes\MeasureUnit\uDatasizeUnit.pas',
  uTimeUnit in '..\Classes\MeasureUnit\uTimeUnit.pas',
  uPhysicalDrive in '..\Objects\uPhysicalDrive.pas',
  uBufferInterpreter in '..\WindowsFileAPI\Abstraction\uBufferInterpreter.pas',
  uIoControlFile in '..\WindowsFileAPI\Abstraction\uIoControlFile.pas',
  uOSFile in '..\WindowsFileAPI\Abstraction\uOSFile.pas',
  uOSFileWithHandle in '..\WindowsFileAPI\Abstraction\uOSFileWithHandle.pas',
  uCommandSet in '..\WindowsFileAPI\CommandSet\Abstraction\uCommandSet.pas',
  uAutoCommandSet in '..\WindowsFileAPI\CommandSet\Auto\uAutoCommandSet.pas',
  uATACommandSet in '..\WindowsFileAPI\CommandSet\Specific\uATACommandSet.pas',
  uSATCommandSet in '..\WindowsFileAPI\CommandSet\Specific\uSATCommandSet.pas',
  uSMARTValueList in '..\Objects\uSMARTValueList.pas',
  uDiskGeometryGetter in '..\WindowsFileAPI\PhysicalDrive\Getter\uDiskGeometryGetter.pas',
  uDriveAvailabilityGetter in '..\WindowsFileAPI\PhysicalDrive\Getter\uDriveAvailabilityGetter.pas',
  uNCQAvailabilityGetter in '..\WindowsFileAPI\PhysicalDrive\Getter\uNCQAvailabilityGetter.pas',
  uPartitionListGetter in '..\WindowsFileAPI\PhysicalDrive\Getter\uPartitionListGetter.pas',
  uFixedDriveListGetter in '..\WindowsFileAPI\Partition\Getter\uFixedDriveListGetter.pas',
  uPartitionExtentGetter in '..\WindowsFileAPI\Partition\Getter\uPartitionExtentGetter.pas',
  uATABufferInterpreter in '..\WindowsFileAPI\Interpreter\uATABufferInterpreter.pas',
  uNSTSupport in '..\NSTSupport\Abstraction\uNSTSupport.pas',
  uAutoNSTSupport in '..\NSTSupport\Auto\uAutoNSTSupport.pas',
  uPhysicalDriveListGetter in '..\WindowsFileAPI\PhysicalDrive\ListGetter\Abstraction\uPhysicalDriveListGetter.pas',
  uAutoPhysicalDriveListGetter in '..\WindowsFileAPI\PhysicalDrive\ListGetter\Auto\uAutoPhysicalDriveListGetter.pas',
  uBruteForcePhysicalDriveListGetter in '..\WindowsFileAPI\PhysicalDrive\ListGetter\Specific\uBruteForcePhysicalDriveListGetter.pas',
  uWMIPhysicalDriveListGetter in '..\WindowsFileAPI\PhysicalDrive\ListGetter\Specific\uWMIPhysicalDriveListGetter.pas',
  uCrucialNSTSupport in '..\NSTSupport\Support\uCrucialNSTSupport.pas',
  uLiteonNSTSupport in '..\NSTSupport\Support\uLiteonNSTSupport.pas',
  uMachXtremeNSTSupport in '..\NSTSupport\Support\uMachXtremeNSTSupport.pas',
  uPlextorNSTSupport in '..\NSTSupport\Support\uPlextorNSTSupport.pas',
  uSamsungNSTSupport in '..\NSTSupport\Support\uSamsungNSTSupport.pas',
  uSandiskNSTSupport in '..\NSTSupport\Support\uSandiskNSTSupport.pas',
  uSeagateNSTSupport in '..\NSTSupport\Support\uSeagateNSTSupport.pas',
  uToshibaNSTSupport in '..\NSTSupport\Support\uToshibaNSTSupport.pas',
  uHynixSandforceNSTSupport in '..\NSTSupport\SandforceSupport\uHynixSandforceNSTSupport.pas',
  uMachXtremeSandforceNSTSupport in '..\NSTSupport\SandforceSupport\uMachXtremeSandforceNSTSupport.pas',
  uOCZSandforceNSTSupport in '..\NSTSupport\SandforceSupport\uOCZSandforceNSTSupport.pas',
  uPatriotSandforceNSTSupport in '..\NSTSupport\SandforceSupport\uPatriotSandforceNSTSupport.pas',
  uToshibaSandforceNSTSupport in '..\NSTSupport\SandforceSupport\uToshibaSandforceNSTSupport.pas',
  uSandforceNSTSupport in '..\NSTSupport\Abstraction\uSandforceNSTSupport.pas',
  uDiagnosisService in 'Objects\uDiagnosisService.pas',
  uPathManager in '..\Classes\PathManager\uPathManager.pas',
  uPhysicalDriveList in '..\WindowsFileAPI\PhysicalDrive\List\uPhysicalDriveList.pas',
  uListChangeGetter in '..\WindowsFileAPI\PhysicalDrive\Getter\uListChangeGetter.pas';

{$R *.RES}
{$SETPEOPTFLAGS $140}

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
