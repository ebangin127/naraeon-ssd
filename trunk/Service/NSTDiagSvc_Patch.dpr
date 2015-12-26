program NSTDiagSvc_Patch;

uses
  Vcl.SvcMgr,
  Windows,
  uLanguageSettings in '..\Resource\uLanguageSettings.pas',
  uProcessOpener in '..\Objects\uProcessOpener.pas',
  Device.SMART.List in '..\Objects\Device.SMART.List.pas',
  uDiskGeometryGetter in '..\WindowsFileAPI\PhysicalDrive\Getter\uDiskGeometryGetter.pas',
  uDriveAvailabilityGetter in '..\WindowsFileAPI\PhysicalDrive\Getter\uDriveAvailabilityGetter.pas',
  uNCQAvailabilityGetter in '..\WindowsFileAPI\PhysicalDrive\Getter\uNCQAvailabilityGetter.pas',
  uPartitionListGetter in '..\WindowsFileAPI\PhysicalDrive\Getter\uPartitionListGetter.pas',
  uPartitionExtentGetter in '..\WindowsFileAPI\Partition\Getter\uPartitionExtentGetter.pas',
  uNSTSupport in '..\NSTSupport\Abstraction\uNSTSupport.pas',
  uLiteonNSTSupport in '..\NSTSupport\Support\uLiteonNSTSupport.pas',
  uMachXtremeNSTSupport in '..\NSTSupport\Support\uMachXtremeNSTSupport.pas',
  uPlextorNSTSupport in '..\NSTSupport\Support\uPlextorNSTSupport.pas',
  uSamsungNSTSupport in '..\NSTSupport\Support\uSamsungNSTSupport.pas',
  uSandiskNSTSupport in '..\NSTSupport\Support\uSandiskNSTSupport.pas',
  uSeagateNSTSupport in '..\NSTSupport\Support\uSeagateNSTSupport.pas',
  uSandforceNSTSupport in '..\NSTSupport\Abstraction\uSandforceNSTSupport.pas',
  uDiagnosisService in 'Objects\uDiagnosisService.pas',
  uPathManager in '..\Objects\uPathManager.pas',
  uListChangeGetter in '..\WindowsFileAPI\PhysicalDrive\Getter\uListChangeGetter.pas',
  uMain in 'uMain.pas' {NaraeonSSDToolsDiag: TService},
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
  uWriteBufferSettingVerifier in '..\Objects\uWriteBufferSettingVerifier.pas',
  uRegistryHelper in '..\Objects\uRegistryHelper.pas',
  uSecurityDescriptor in '..\Objects\uSecurityDescriptor.pas',
  uStringHelper in '..\Modules\uStringHelper.pas',
  uTimeUnit in '..\Modules\uTimeUnit.pas',
  uWindowsVersion in '..\Modules\uWindowsVersion.pas',
  uNSTRegistry in '..\Objects\uNSTRegistry.pas',
  uBusPhysicalDrive in '..\WindowsFileAPI\PhysicalDrive\Part\uBusPhysicalDrive.pas',
  uOSPhysicalDrive in '..\WindowsFileAPI\PhysicalDrive\Part\uOSPhysicalDrive.pas',
  Device.PhysicalDrive in '..\Objects\Device.PhysicalDrive.pas',
  uVersionHelper in '..\Modules\uVersionHelper.pas',
  OSFile.Handle in '..\WindowsFileAPI\OSFile.Handle.pas',
  OSFile.Interfaced in '..\WindowsFileAPI\OSFile.Interfaced.pas',
  OSFile.IoControl in '..\WindowsFileAPI\OSFile.IoControl.pas',
  OSFile in '..\WindowsFileAPI\OSFile.pas',
  Getter.DriveList in '..\WindowsFileAPI\Getter.DriveList.pas',
  Getter.DriveList.Fixed in '..\WindowsFileAPI\Getter.DriveList.Fixed.pas',
  CommandSet.ATA.Legacy in '..\WindowsFileAPI\CommandSet.ATA.Legacy.pas',
  CommandSet.ATA in '..\WindowsFileAPI\CommandSet.ATA.pas',
  CommandSet.Factory in '..\WindowsFileAPI\CommandSet.Factory.pas',
  CommandSet.NVMe.Intel in '..\WindowsFileAPI\CommandSet.NVMe.Intel.pas',
  CommandSet.NVMe.Intel.PortPart in '..\WindowsFileAPI\CommandSet.NVMe.Intel.PortPart.pas',
  CommandSet.NVMe in '..\WindowsFileAPI\CommandSet.NVMe.pas',
  CommandSet.NVMe.Samsung in '..\WindowsFileAPI\CommandSet.NVMe.Samsung.pas',
  CommandSet in '..\WindowsFileAPI\CommandSet.pas',
  CommandSet.SAT in '..\WindowsFileAPI\CommandSet.SAT.pas',
  Support.Intel.NVMe in '..\NSTSupport\Support\Support.Intel.NVMe.pas',
  Support.Samsung.NVMe in '..\NSTSupport\Support\Support.Samsung.NVMe.pas',
  AverageLogger.Count in '..\Objects\AverageLogger.Count.pas',
  AverageLogger in '..\Objects\AverageLogger.pas',
  AverageLogger.Write in '..\Objects\AverageLogger.Write.pas',
  BufferInterpreter.ATA in '..\WindowsFileAPI\BufferInterpreter.ATA.pas',
  BufferInterpreter.NVMe.Intel in '..\WindowsFileAPI\BufferInterpreter.NVMe.Intel.pas',
  BufferInterpreter.NVMe in '..\WindowsFileAPI\BufferInterpreter.NVMe.pas',
  BufferInterpreter in '..\WindowsFileAPI\BufferInterpreter.pas',
  Getter.SCSIAddress in '..\Objects\Getter.SCSIAddress.pas',
  Getter.SlotSpeed in '..\Objects\Getter.SlotSpeed.pas',
  Getter.SlotSpeedByDeviceID in '..\Objects\Getter.SlotSpeedByDeviceID.pas',
  MeasureUnit.DataSize in '..\Modules\MeasureUnit.DataSize.pas',
  Device.SlotSpeed in '..\Modules\Device.SlotSpeed.pas',
  OS.SetupAPI in '..\Modules\OS.SetupAPI.pas',
  Device.PhysicalDrive.List in '..\WindowsFileAPI\Device.PhysicalDrive.List.pas',
  Getter.PhysicalDriveList.Auto in '..\WindowsFileAPI\Getter.PhysicalDriveList.Auto.pas',
  Getter.PhysicalDriveList.BruteForce in '..\WindowsFileAPI\Getter.PhysicalDriveList.BruteForce.pas',
  Getter.PhysicalDriveList in '..\WindowsFileAPI\Getter.PhysicalDriveList.pas',
  Getter.PhysicalDriveList.WMI in '..\WindowsFileAPI\Getter.PhysicalDriveList.WMI.pas';

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
