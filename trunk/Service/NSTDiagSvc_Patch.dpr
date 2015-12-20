program NSTDiagSvc_Patch;

uses
  Vcl.SvcMgr,
  Windows,
  uLanguageSettings in '..\Resource\uLanguageSettings.pas',
  uProcessOpener in '..\Objects\uProcessOpener.pas',
  uBusPhysicalDrive in '..\WindowsFileAPI\PhysicalDrive\Part\uBusPhysicalDrive.pas',
  uBufferInterpreter in '..\WindowsFileAPI\Abstraction\uBufferInterpreter.pas',
  uIoControlFile in '..\WindowsFileAPI\Abstraction\uIoControlFile.pas',
  uOSFile in '..\WindowsFileAPI\Abstraction\uOSFile.pas',
  uOSFileWithHandle in '..\WindowsFileAPI\Abstraction\uOSFileWithHandle.pas',
  uCommandSet in '..\WindowsFileAPI\CommandSet\Abstraction\uCommandSet.pas',
  uATACommandSet in '..\WindowsFileAPI\CommandSet\Specific\uATACommandSet.pas',
  uSATCommandSet in '..\WindowsFileAPI\CommandSet\Specific\uSATCommandSet.pas',
  uSMARTValueList in '..\Objects\uSMARTValueList.pas',
  uDiskGeometryGetter in '..\WindowsFileAPI\PhysicalDrive\Getter\uDiskGeometryGetter.pas',
  uDriveAvailabilityGetter in '..\WindowsFileAPI\PhysicalDrive\Getter\uDriveAvailabilityGetter.pas',
  uNCQAvailabilityGetter in '..\WindowsFileAPI\PhysicalDrive\Getter\uNCQAvailabilityGetter.pas',
  uPartitionListGetter in '..\WindowsFileAPI\PhysicalDrive\Getter\uPartitionListGetter.pas',
  uPartitionExtentGetter in '..\WindowsFileAPI\Partition\Getter\uPartitionExtentGetter.pas',
  uATABufferInterpreter in '..\WindowsFileAPI\Interpreter\uATABufferInterpreter.pas',
  uNSTSupport in '..\NSTSupport\Abstraction\uNSTSupport.pas',
  uPhysicalDriveListGetter in '..\WindowsFileAPI\PhysicalDrive\ListGetter\Abstraction\uPhysicalDriveListGetter.pas',
  uAutoPhysicalDriveListGetter in '..\WindowsFileAPI\PhysicalDrive\ListGetter\Auto\uAutoPhysicalDriveListGetter.pas',
  uBruteForcePhysicalDriveListGetter in '..\WindowsFileAPI\PhysicalDrive\ListGetter\Specific\uBruteForcePhysicalDriveListGetter.pas',
  uWMIPhysicalDriveListGetter in '..\WindowsFileAPI\PhysicalDrive\ListGetter\Specific\uWMIPhysicalDriveListGetter.pas',
  uLiteonNSTSupport in '..\NSTSupport\Support\uLiteonNSTSupport.pas',
  uMachXtremeNSTSupport in '..\NSTSupport\Support\uMachXtremeNSTSupport.pas',
  uPlextorNSTSupport in '..\NSTSupport\Support\uPlextorNSTSupport.pas',
  uSamsungNSTSupport in '..\NSTSupport\Support\uSamsungNSTSupport.pas',
  uSandiskNSTSupport in '..\NSTSupport\Support\uSandiskNSTSupport.pas',
  uSeagateNSTSupport in '..\NSTSupport\Support\uSeagateNSTSupport.pas',
  uSandforceNSTSupport in '..\NSTSupport\Abstraction\uSandforceNSTSupport.pas',
  uDiagnosisService in 'Objects\uDiagnosisService.pas',
  uPathManager in '..\Objects\uPathManager.pas',
  uPhysicalDriveList in '..\WindowsFileAPI\PhysicalDrive\List\uPhysicalDriveList.pas',
  uListChangeGetter in '..\WindowsFileAPI\PhysicalDrive\Getter\uListChangeGetter.pas',
  uLegacyATACommandSet in '..\WindowsFileAPI\CommandSet\Specific\uLegacyATACommandSet.pas',
  uMain in 'uMain.pas' {NaraeonSSDToolsDiag: TService},
  uDriveListGetter in '..\WindowsFileAPI\Abstraction\uDriveListGetter.pas',
  uFixedDriveListGetter in '..\WindowsFileAPI\Partition\Getter\uFixedDriveListGetter.pas',
  uRemovableDriveListGetter in '..\WindowsFileAPI\Partition\Getter\uRemovableDriveListGetter.pas',
  uADATANSTSupport in '..\NSTSupport\Support\uADATANSTSupport.pas',
  uCrucialNSTSupport in '..\NSTSupport\Support\uCrucialNSTSupport.pas',
  uPhisonNSTSupport in '..\NSTSupport\Support\uPhisonNSTSupport.pas',
  uToshibaNSTSupport in '..\NSTSupport\Support\uToshibaNSTSupport.pas',
  uADATASandforceNSTSupport in '..\NSTSupport\SandforceSupport\uADATASandforceNSTSupport.pas',
  uHynixSandforceNSTSupport in '..\NSTSupport\SandforceSupport\uHynixSandforceNSTSupport.pas',
  uMachXtremeSandforceNSTSupport in '..\NSTSupport\SandforceSupport\uMachXtremeSandforceNSTSupport.pas',
  uOCZSandforceNSTSupport in '..\NSTSupport\SandforceSupport\uOCZSandforceNSTSupport.pas',
  uPatriotSandforceNSTSupport in '..\NSTSupport\SandforceSupport\uPatriotSandforceNSTSupport.pas',
  uToshibaSandforceNSTSupport in '..\NSTSupport\SandforceSupport\uToshibaSandforceNSTSupport.pas',
  uNSTSupportFactory in '..\NSTSupport\Factory\uNSTSupportFactory.pas',
  uCommandSetFactory in '..\WindowsFileAPI\CommandSet\Factory\uCommandSetFactory.pas',
  uWriteBufferSettingVerifier in '..\Objects\uWriteBufferSettingVerifier.pas',
  uRegistryHelper in '..\Objects\uRegistryHelper.pas',
  uSecurityDescriptor in '..\Objects\uSecurityDescriptor.pas',
  uAverageLogger in '..\Objects\AverageLogger\Abstraction\uAverageLogger.pas',
  uAverageCountLogger in '..\Objects\AverageLogger\Specific\uAverageCountLogger.pas',
  uAverageWriteLogger in '..\Objects\AverageLogger\Specific\uAverageWriteLogger.pas',
  uDatasizeUnit in '..\Modules\uDatasizeUnit.pas',
  uStringHelper in '..\Modules\uStringHelper.pas',
  uTimeUnit in '..\Modules\uTimeUnit.pas',
  uUINT64 in '..\Modules\uUINT64.pas',
  uWindowsVersion in '..\Modules\uWindowsVersion.pas',
  uNSTRegistry in '..\Objects\uNSTRegistry.pas';

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
