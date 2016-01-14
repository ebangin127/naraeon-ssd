program SSDTools;

uses
  Vcl.Forms,
  Windows,
  SysUtils,
  Classes,
  ShellAPI,
  uLanguageSettings in 'Resource\uLanguageSettings.pas',
  AsciiCheck in 'Modules\AsciiCheck.pas',
  uNSTSupport in 'NSTSupport\Abstraction\uNSTSupport.pas',
  Version in 'Modules\Version.pas',
  uDownloadPath in 'Objects\uDownloadPath.pas',
  Extern.SevenZip in 'ExternProgram\Extern.SevenZip.pas',
  uTrimList in 'Objects\uTrimList.pas',
  Extern.Rufus in 'ExternProgram\Extern.Rufus.pas',
  uPathManager in 'Objects\uPathManager.pas',
  uSemiAutoTrimmer in 'Objects\uSemiAutoTrimmer.pas',
  PrerequisiteChecker in 'Objects\PrerequisiteChecker.pas',
  uBusPhysicalDrive in 'WindowsFileAPI\PhysicalDrive\Part\uBusPhysicalDrive.pas',
  Device.SMART.List in 'Objects\Device.SMART.List.pas',
  uVersionPublisher in 'Resource\uVersionPublisher.pas',
  uPartition in 'Objects\uPartition.pas',
  uRegistryHelper in 'Objects\uRegistryHelper.pas',
  uDiskGeometryGetter in 'WindowsFileAPI\PhysicalDrive\Getter\uDiskGeometryGetter.pas',
  uDriveAvailabilityGetter in 'WindowsFileAPI\PhysicalDrive\Getter\uDriveAvailabilityGetter.pas',
  uPartitionListGetter in 'WindowsFileAPI\PhysicalDrive\Getter\uPartitionListGetter.pas',
  uFirmwareGetter in 'Objects\uFirmwareGetter.pas',
  uNCQAvailabilityGetter in 'WindowsFileAPI\PhysicalDrive\Getter\uNCQAvailabilityGetter.pas',
  uUpdater in 'ThreadHelper\uUpdater.pas',
  uParameter in 'Objects\uParameter.pas',
  Form.Alert in 'Form\Form.Alert.pas' {fAlert},
  Form.Browser in 'Form\Form.Browser.pas' {fBrowser},
  Form.Main in 'Form\Form.Main.pas' {fMain},
  Form.Message in 'Form\Form.Message.pas' {fMessage},
  uListTrimmer in 'ThreadHelper\uListTrimmer.pas',
  uTrimBasicsGetter in 'WindowsFileAPI\Partition\TrimBasics\Abstraction\uTrimBasicsGetter.pas',
  uNTFSTrimBasicsGetter in 'WindowsFileAPI\Partition\TrimBasics\Getter\uNTFSTrimBasicsGetter.pas',
  uFATTrimBasicsGetter in 'WindowsFileAPI\Partition\TrimBasics\Getter\uFATTrimBasicsGetter.pas',
  uTrimBasicsGetterFactory in 'WindowsFileAPI\Partition\TrimBasics\Factory\uTrimBasicsGetterFactory.pas',
  uSandforceNSTSupport in 'NSTSupport\Abstraction\uSandforceNSTSupport.pas',
  uToshibaSandforceNSTSupport in 'NSTSupport\SandforceSupport\uToshibaSandforceNSTSupport.pas',
  uHynixSandforceNSTSupport in 'NSTSupport\SandforceSupport\uHynixSandforceNSTSupport.pas',
  uOCZSandforceNSTSupport in 'NSTSupport\SandforceSupport\uOCZSandforceNSTSupport.pas',
  uPatriotSandforceNSTSupport in 'NSTSupport\SandforceSupport\uPatriotSandforceNSTSupport.pas',
  uMachXtremeSandforceNSTSupport in 'NSTSupport\SandforceSupport\uMachXtremeSandforceNSTSupport.pas',
  uNSToolsMutex in 'Objects\uNSToolsMutex.pas',
  uServiceController in 'Objects\uServiceController.pas',
  uIdentifyDiagnosis in 'Objects\uIdentifyDiagnosis.pas',
  uCrucialNSTSupport in 'NSTSupport\Support\uCrucialNSTSupport.pas',
  uLiteonNSTSupport in 'NSTSupport\Support\uLiteonNSTSupport.pas',
  uMachXtremeNSTSupport in 'NSTSupport\Support\uMachXtremeNSTSupport.pas',
  uPlextorNSTSupport in 'NSTSupport\Support\uPlextorNSTSupport.pas',
  uSamsungNSTSupport in 'NSTSupport\Support\uSamsungNSTSupport.pas',
  uSandiskNSTSupport in 'NSTSupport\Support\uSandiskNSTSupport.pas',
  uSeagateNSTSupport in 'NSTSupport\Support\uSeagateNSTSupport.pas',
  uToshibaNSTSupport in 'NSTSupport\Support\uToshibaNSTSupport.pas',
  uPhisonNSTSupport in 'NSTSupport\Support\uPhisonNSTSupport.pas',
  uListChangeGetter in 'WindowsFileAPI\PhysicalDrive\Getter\uListChangeGetter.pas',
  uDownloader in 'ThreadHelper\uDownloader.pas',
  uFirmwareDownloader in 'Objects\uFirmwareDownloader.pas',
  OS.Codesign in 'Modules\OS.Codesign.pas',
  OS.DeleteDirectory in 'Modules\OS.DeleteDirectory.pas',
  uProcessOpener in 'Objects\uProcessOpener.pas',
  OS.PlugAndPlay in 'Modules\OS.PlugAndPlay.pas',
  uNSTRegistry in 'Objects\uNSTRegistry.pas',
  uPartitionExtentGetter in 'WindowsFileAPI\Partition\Getter\uPartitionExtentGetter.pas',
  uVolumeBitmapGetter in 'WindowsFileAPI\Partition\Getter\uVolumeBitmapGetter.pas',
  uVolumeLabelGetter in 'WindowsFileAPI\Partition\Getter\uVolumeLabelGetter.pas',
  uADATASandforceNSTSupport in 'NSTSupport\SandforceSupport\uADATASandforceNSTSupport.pas',
  uADATANSTSupport in 'NSTSupport\Support\uADATANSTSupport.pas',
  uPartitionTrimmer in 'ThreadHelper\uPartitionTrimmer.pas',
  uDeviceTrimmer in 'ThreadHelper\uDeviceTrimmer.pas',
  uNSTSupportFactory in 'NSTSupport\Factory\uNSTSupportFactory.pas',
  OS.WindowsVersion in 'Modules\OS.WindowsVersion.pas',
  uSecurityDescriptor in 'Objects\uSecurityDescriptor.pas',
  MeasureUnit.DataSize in 'Modules\MeasureUnit.DataSize.pas',
  MeasureUnit.Time in 'Modules\MeasureUnit.Time.pas',
  OS.VersionHelper in 'Modules\OS.VersionHelper.pas',
  Device.PhysicalDrive in 'Objects\Device.PhysicalDrive.pas',
  uOSPhysicalDrive in 'WindowsFileAPI\PhysicalDrive\Part\uOSPhysicalDrive.pas',
  Web.HTTP in 'Objects\Web.HTTP.pas',
  Web.HTTPS in 'Objects\Web.HTTPS.pas',
  Web in 'Objects\Web.pas',
  Optimizer.Defrag in 'Objects\Optimizer.Defrag.pas',
  Optimizer.Hibernation in 'Objects\Optimizer.Hibernation.pas',
  Optimizer.Indexer in 'Objects\Optimizer.Indexer.pas',
  Optimizer.LastAccess in 'Objects\Optimizer.LastAccess.pas',
  Optimizer in 'Objects\Optimizer.pas',
  Optimizer.Prefetch in 'Objects\Optimizer.Prefetch.pas',
  Optimizer.SystemRestore in 'Objects\Optimizer.SystemRestore.pas',
  Optimizer.Template in 'Objects\Optimizer.Template.pas',
  Component.ButtonGroup in 'View\Component.ButtonGroup.pas',
  Component.ProgressSection in 'View\Component.ProgressSection.pas',
  Component.SSDLabel.List in 'View\Component.SSDLabel.List.pas',
  Component.SSDLabel in 'View\Component.SSDLabel.pas',
  Component.UAWebbrowser in 'View\Component.UAWebbrowser.pas',
  Initializer.Locale in 'View\Initializer.Locale.pas',
  Initializer.Mainpart in 'View\Initializer.Mainpart.pas',
  Initializer.PartitionAlign in 'View\Initializer.PartitionAlign.pas',
  Initializer in 'View\Initializer.pas',
  Initializer.PhysicalDrive in 'View\Initializer.PhysicalDrive.pas',
  Initializer.ReplacedSector in 'View\Initializer.ReplacedSector.pas',
  Initializer.SMART in 'View\Initializer.SMART.pas',
  Initializer.SSDLabelListRefresh in 'View\Initializer.SSDLabelListRefresh.pas',
  Initializer.TotalWrite in 'View\Initializer.TotalWrite.pas',
  ThreadToView.Trim in 'View\ThreadToView.Trim.pas',
  AverageLogger.Count in 'Objects\AverageLogger.Count.pas',
  AverageLogger in 'Objects\AverageLogger.pas',
  AverageLogger.Write in 'Objects\AverageLogger.Write.pas',
  Thread.Download in 'Objects\Thread.Download.pas',
  Thread.Trim in 'Objects\Thread.Trim.pas',
  Thread.Update in 'Objects\Thread.Update.pas',
  BufferInterpreter.ATA in 'WindowsFileAPI\BufferInterpreter.ATA.pas',
  BufferInterpreter.NVMe.Intel in 'WindowsFileAPI\BufferInterpreter.NVMe.Intel.pas',
  BufferInterpreter.NVMe in 'WindowsFileAPI\BufferInterpreter.NVMe.pas',
  BufferInterpreter in 'WindowsFileAPI\BufferInterpreter.pas',
  CommandSet.NVMe.Intel in 'WindowsFileAPI\CommandSet.NVMe.Intel.pas',
  CommandSet.NVMe.Intel.PortPart in 'WindowsFileAPI\CommandSet.NVMe.Intel.PortPart.pas',
  CommandSet.NVMe in 'WindowsFileAPI\CommandSet.NVMe.pas',
  CommandSet.NVMe.Samsung in 'WindowsFileAPI\CommandSet.NVMe.Samsung.pas',
  Getter.SCSIAddress in 'Objects\Getter.SCSIAddress.pas',
  Support.Samsung.NVMe in 'NSTSupport\Support\Support.Samsung.NVMe.pas',
  Support.Intel.NVMe in 'NSTSupport\Support\Support.Intel.NVMe.pas',
  OS.SetupAPI in 'Modules\OS.SetupAPI.pas',
  Getter.SlotSpeed in 'Objects\Getter.SlotSpeed.pas',
  Getter.SlotSpeedByDeviceID in 'Objects\Getter.SlotSpeedByDeviceID.pas',
  Getter.PhysicalDriveList.Auto in 'WindowsFileAPI\Getter.PhysicalDriveList.Auto.pas',
  Getter.PhysicalDriveList.BruteForce in 'WindowsFileAPI\Getter.PhysicalDriveList.BruteForce.pas',
  Getter.PhysicalDriveList in 'WindowsFileAPI\Getter.PhysicalDriveList.pas',
  Getter.PhysicalDriveList.WMI in 'WindowsFileAPI\Getter.PhysicalDriveList.WMI.pas',
  Device.PhysicalDrive.List in 'WindowsFileAPI\Device.PhysicalDrive.List.pas',
  CommandSet.Factory in 'WindowsFileAPI\CommandSet.Factory.pas',
  CommandSet in 'WindowsFileAPI\CommandSet.pas',
  CommandSet.ATA.Legacy in 'WindowsFileAPI\CommandSet.ATA.Legacy.pas',
  CommandSet.ATA in 'WindowsFileAPI\CommandSet.ATA.pas',
  CommandSet.SAT in 'WindowsFileAPI\CommandSet.SAT.pas',
  Getter.DriveList in 'WindowsFileAPI\Getter.DriveList.pas',
  OSFile.Handle in 'WindowsFileAPI\OSFile.Handle.pas',
  OSFile.Interfaced in 'WindowsFileAPI\OSFile.Interfaced.pas',
  OSFile.IoControl in 'WindowsFileAPI\OSFile.IoControl.pas',
  OSFile in 'WindowsFileAPI\OSFile.pas',
  Getter.DriveList.Fixed in 'WindowsFileAPI\Getter.DriveList.Fixed.pas',
  Getter.DriveList.Removable in 'WindowsFileAPI\Getter.DriveList.Removable.pas',
  Device.SlotSpeed in 'Modules\Device.SlotSpeed.pas',
  Getter.CodesignVerifier.Publisher in 'Objects\Getter.CodesignVerifier.Publisher.pas',
  Getter.CodesignVerifier in 'Objects\Getter.CodesignVerifier.pas';

{$R *.res}
{$SETPEOPTFLAGS $140}

var
  MainformCaption: String;
  NSToolsMutex: TNSToolsMutex;
  Parameter: TParameter;

const
  NormallyOpenedMutex = 'NSToolsOpened';

procedure SetForegroundOfExistingInstance;
var
  ExistingInstanceWindow: THandle;
begin
  ExistingInstanceWindow := FindWindow(PChar('TfMain'), PChar(MainformCaption));
  if ExistingInstanceWindow <> 0 then
  begin
    ShowWindow(ExistingInstanceWindow, SW_RESTORE);
    SetForegroundWindow(ExistingInstanceWindow);
    CloseHandle(ExistingInstanceWindow);
  end;
end;

procedure StartNSToolsInstance;
begin
  NSToolsMutex.CreateMutex;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfMain, fMain);
  fMain.Caption := MainformCaption;
  Application.Run;
end;

procedure InitializePath;
begin
  PathManager.SetPath(Application);
end;

procedure SetLanguageSettings;
begin
  DetermineLanguage;
  MainformCaption :=
    'Naraeon SSD Tools ' +
    CurrentVersion +
    CapToSeeSerial[CurrLang];
end;

procedure IfAnotherInstanceExistsExitElseStartThisInstance;
begin
  if NSToolsMutex.OpenMutex then
    SetForegroundOfExistingInstance
  else
    StartNSToolsInstance;
end;

procedure ReadParameterAndDoAsInstructed;
begin
  Parameter := TParameter.Create;
  NSToolsMutex := TNSToolsMutex.Create(NormallyOpenedMutex);

  if Parameter.ProcessParameterAndIfNormalReturnTrue(ParamStr(1)) then
    IfAnotherInstanceExistsExitElseStartThisInstance;

  FreeAndNil(NSToolsMutex);
  FreeAndNil(Parameter);
end;

begin
  Application.Initialize;
  InitializePath;
  SetLanguageSettings;
  ReadParameterAndDoAsInstructed;
end.
