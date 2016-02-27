program SSDTools;

uses
  Vcl.Forms,
  Windows,
  SysUtils,
  Classes,
  ShellAPI,
  Global.LanguageString in 'Resource\Global.LanguageString.pas',
  Global.Constant in 'Resource\Global.Constant.pas',
  AsciiCheck in 'Modules\AsciiCheck.pas',
  Support in 'Support\Support.pas',
  Version in 'Modules\Version.pas',
  Getter.WebPath in 'Objects\Getter.WebPath.pas',
  Extern.SevenZip in 'ExternProgram\Extern.SevenZip.pas',
  TrimList in 'Objects\TrimList.pas',
  Extern.Rufus in 'ExternProgram\Extern.Rufus.pas',
  OS.EnvironmentVariable in 'Objects\OS.EnvironmentVariable.pas',
  SemiAutoTrimmer in 'Objects\SemiAutoTrimmer.pas',
  PrerequisiteChecker in 'Objects\PrerequisiteChecker.pas',
  Device.PhysicalDrive.Bus in 'WindowsFileAPI\Device.PhysicalDrive.Bus.pas',
  Device.SMART.List in 'WindowsFileAPI\Device.SMART.List.pas',
  Partition in 'Objects\Partition.pas',
  Getter.PhysicalDrive.DiskGeometry in 'WindowsFileAPI\Getter.PhysicalDrive.DiskGeometry.pas',
  Getter.PhysicalDrive.DriveAvailability in 'WindowsFileAPI\Getter.PhysicalDrive.DriveAvailability.pas',
  Getter.PhysicalDrive.PartitionList in 'WindowsFileAPI\Getter.PhysicalDrive.PartitionList.pas',
  Getter.LatestFirmware in 'Objects\Getter.LatestFirmware.pas',
  Getter.PhysicalDrive.NCQAvailability in 'WindowsFileAPI\Getter.PhysicalDrive.NCQAvailability.pas',
  Thread.Update.Helper in 'Objects\Thread.Update.Helper.pas',
  Parameter in 'Objects\Parameter.pas',
  Form.Alert in 'Form\Form.Alert.pas' {fAlert},
  Form.Browser in 'Form\Form.Browser.pas' {fBrowser},
  Form.Main in 'Form\Form.Main.pas' {fMain},
  Form.Message in 'Form\Form.Message.pas' {fMessage},
  Getter.TrimBasics in 'WindowsFileAPI\Getter.TrimBasics.pas',
  Getter.TrimBasics.NTFS in 'WindowsFileAPI\Getter.TrimBasics.NTFS.pas',
  Getter.TrimBasics.FAT in 'WindowsFileAPI\Getter.TrimBasics.FAT.pas',
  Getter.TrimBasics.Factory in 'WindowsFileAPI\Getter.TrimBasics.Factory.pas',
  Support.Sandforce in 'Support\Support.Sandforce.pas',
  Support.Sandforce.Toshiba in 'Support\Support.Sandforce.Toshiba.pas',
  Support.Sandforce.Hynix in 'Support\Support.Sandforce.Hynix.pas',
  Support.Sandforce.OCZ in 'Support\Support.Sandforce.OCZ.pas',
  Support.Sandforce.Patriot in 'Support\Support.Sandforce.Patriot.pas',
  Support.Sandforce.MachXtreme in 'Support\Support.Sandforce.MachXtreme.pas',
  OS.MutexManager in 'Objects\OS.MutexManager.pas',
  OS.ServiceController in 'Objects\OS.ServiceController.pas',
  IdentifyDiagnosis in 'Objects\IdentifyDiagnosis.pas',
  Support.Crucial in 'Support\Support.Crucial.pas',
  Support.Liteon in 'Support\Support.Liteon.pas',
  Support.MachXtreme in 'Support\Support.MachXtreme.pas',
  Support.Plextor in 'Support\Support.Plextor.pas',
  Support.Samsung in 'Support\Support.Samsung.pas',
  Support.Sandisk in 'Support\Support.Sandisk.pas',
  Support.Seagate in 'Support\Support.Seagate.pas',
  Support.Toshiba in 'Support\Support.Toshiba.pas',
  Support.Phison in 'Support\Support.Phison.pas',
  Getter.PhysicalDrive.ListChange in 'WindowsFileAPI\Getter.PhysicalDrive.ListChange.pas',
  Thread.Download.Helper in 'Objects\Thread.Download.Helper.pas',
  Downloader.Firmware in 'Objects\Downloader.Firmware.pas',
  OS.Codesign in 'Modules\OS.Codesign.pas',
  OS.DeleteDirectory in 'Modules\OS.DeleteDirectory.pas',
  OS.ProcessOpener in 'Objects\OS.ProcessOpener.pas',
  OS.PlugAndPlay in 'Modules\OS.PlugAndPlay.pas',
  Getter.VolumeLabel in 'WindowsFileAPI\Getter.VolumeLabel.pas',
  Support.Sandforce.ADATA in 'Support\Support.Sandforce.ADATA.pas',
  Support.ADATA in 'Support\Support.ADATA.pas',
  Support.Factory in 'Support\Support.Factory.pas',
  OS.Version.Helper in 'Modules\OS.Version.Helper.pas',
  OS.SecurityDescriptor in 'WindowsFileAPI\OS.SecurityDescriptor.pas',
  MeasureUnit.DataSize in 'Modules\MeasureUnit.DataSize.pas',
  MeasureUnit.Time in 'Modules\MeasureUnit.Time.pas',
  Getter.OS.Version in 'WindowsFileAPI\Getter.OS.Version.pas',
  Device.PhysicalDrive in 'WindowsFileAPI\Device.PhysicalDrive.pas',
  Device.PhysicalDrive.OS in 'WindowsFileAPI\Device.PhysicalDrive.OS.pas',
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
  Getter.SCSIAddress in 'WindowsFileAPI\Getter.SCSIAddress.pas',
  Support.NVMe.Samsung in 'Support\Support.NVMe.Samsung.pas',
  Support.NVMe.Intel in 'Support\Support.NVMe.Intel.pas',
  OS.SetupAPI in 'Modules\OS.SetupAPI.pas',
  Getter.SlotSpeed in 'WindowsFileAPI\Getter.SlotSpeed.pas',
  Getter.SlotSpeedByDeviceID in 'WindowsFileAPI\Getter.SlotSpeedByDeviceID.pas',
  Getter.PhysicalDriveList.Auto in 'WindowsFileAPI\Getter.PhysicalDriveList.Auto.pas',
  Getter.PhysicalDriveList.OS in 'WindowsFileAPI\Getter.PhysicalDriveList.OS.pas',
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
  Getter.CodesignVerifier in 'Objects\Getter.CodesignVerifier.pas',
  Thread.Trim.Helper.Device in 'Objects\Thread.Trim.Helper.Device.pas',
  Thread.Trim.Helper.List in 'Objects\Thread.Trim.Helper.List.pas',
  Getter.PartitionExtent in 'WindowsFileAPI\Getter.PartitionExtent.pas',
  Getter.VolumeBitmap in 'WindowsFileAPI\Getter.VolumeBitmap.pas',
  Getter.DeviceDriver in 'WindowsFileAPI\Getter.DeviceDriver.pas',
  Initializer.CriticalWarning in 'View\Initializer.CriticalWarning.pas',
  CommandSet.NVMe.WithoutDriver in 'WindowsFileAPI\CommandSet.NVMe.WithoutDriver.pas',
  BufferInterpreter.SCSI in 'WindowsFileAPI\BufferInterpreter.SCSI.pas',
  Thread.Trim.Helper.Partition.OS in 'Objects\Thread.Trim.Helper.Partition.OS.pas',
  Thread.Trim.Helper.Partition in 'Objects\Thread.Trim.Helper.Partition.pas',
  Thread.Trim.Helper.Partition.Direct in 'Objects\Thread.Trim.Helper.Partition.Direct.pas',
  Global.HelpPage in 'Resource\Global.HelpPage.pas',
  WMI in 'WindowsFileAPI\WMI.pas',
  OSFile.ForInternal in 'WindowsFileAPI\OSFile.ForInternal.pas',
  Optimizer.P2P in 'Objects\Optimizer.P2P.pas',
  Registry.Helper.Internal in 'WindowsFileAPI\Registry.Helper.Internal.pas',
  Registry.Helper in 'WindowsFileAPI\Registry.Helper.pas',
  Getter.PhysicalDriveList.OS.Path in 'WindowsFileAPI\Getter.PhysicalDriveList.OS.Path.pas',
  Getter.External in 'Objects\Getter.External.pas',
  OS.Partition.Lock in 'Objects\OS.Partition.Lock.pas',
  Getter.Filesystem.Name in 'Objects\Getter.Filesystem.Name.pas',
  Support.Sandisk.USB in 'Support\Support.Sandisk.USB.pas';

{$R *.res}
{$SETPEOPTFLAGS $140}

var
  MainformCaption: String;
  NSToolsMutex: TMutexManager;
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
  EnvironmentVariable.SetPath(Application);
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
  NSToolsMutex := TMutexManager.Create(NormallyOpenedMutex);

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
