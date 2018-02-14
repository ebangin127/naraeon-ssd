program NSTDiagSvc_Patch;

uses
  Vcl.SvcMgr,
  Windows,
  Global.LanguageString in '..\Resource\Global.LanguageString.pas',
  OS.ProcessOpener in '..\Objects\OS.ProcessOpener.pas',
  Device.SMART.List in '..\WindowsFileAPI\Device.SMART.List.pas',
  Getter.PhysicalDrive.DiskGeometry in '..\WindowsFileAPI\Getter.PhysicalDrive.DiskGeometry.pas',
  Getter.PhysicalDrive.DriveAvailability in '..\WindowsFileAPI\Getter.PhysicalDrive.DriveAvailability.pas',
  Getter.PhysicalDrive.NCQAvailability in '..\WindowsFileAPI\Getter.PhysicalDrive.NCQAvailability.pas',
  Getter.PhysicalDrive.PartitionList in '..\WindowsFileAPI\Getter.PhysicalDrive.PartitionList.pas',
  Support in '..\Support\Support.pas',
  Support.Liteon in '..\Support\Support.Liteon.pas',
  Support.MachXtreme in '..\Support\Support.MachXtreme.pas',
  Support.Plextor in '..\Support\Support.Plextor.pas',
  Support.Samsung in '..\Support\Support.Samsung.pas',
  Support.Sandisk in '..\Support\Support.Sandisk.pas',
  Support.Seagate in '..\Support\Support.Seagate.pas',
  Support.Sandforce in '..\Support\Support.Sandforce.pas',
  uDiagnosisService in 'Objects\uDiagnosisService.pas',
  OS.EnvironmentVariable in '..\Objects\OS.EnvironmentVariable.pas',
  Getter.PhysicalDrive.ListChange in '..\WindowsFileAPI\Getter.PhysicalDrive.ListChange.pas',
  uMain in 'uMain.pas' {NaraeonSSDToolsDiag: TService},
  Support.ADATA in '..\Support\Support.ADATA.pas',
  Support.Crucial in '..\Support\Support.Crucial.pas',
  Support.Phison in '..\Support\Support.Phison.pas',
  Support.Toshiba in '..\Support\Support.Toshiba.pas',
  Support.Sandforce.ADATA in '..\Support\Support.Sandforce.ADATA.pas',
  Support.Sandforce.Hynix in '..\Support\Support.Sandforce.Hynix.pas',
  Support.Sandforce.MachXtreme in '..\Support\Support.Sandforce.MachXtreme.pas',
  Support.Sandforce.OCZ in '..\Support\Support.Sandforce.OCZ.pas',
  Support.Sandforce.Patriot in '..\Support\Support.Sandforce.Patriot.pas',
  Support.Sandforce.Toshiba in '..\Support\Support.Sandforce.Toshiba.pas',
  Support.Factory in '..\Support\Support.Factory.pas',
  AsciiCheck in '..\Modules\AsciiCheck.pas',
  MeasureUnit.Time in '..\Modules\MeasureUnit.Time.pas',
  OS.Version.Helper in '..\Modules\OS.Version.Helper.pas',
  Device.PhysicalDrive.Bus in '..\WindowsFileAPI\Device.PhysicalDrive.Bus.pas',
  Device.PhysicalDrive.OS in '..\WindowsFileAPI\Device.PhysicalDrive.OS.pas',
  Device.PhysicalDrive in '..\WindowsFileAPI\Device.PhysicalDrive.pas',
  Getter.OS.Version in '..\WindowsFileAPI\Getter.OS.Version.pas',
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
  Support.NVMe.Intel in '..\Support\Support.NVMe.Intel.pas',
  Support.NVMe.Samsung in '..\Support\Support.NVMe.Samsung.pas',
  AverageLogger.Count in '..\Objects\AverageLogger.Count.pas',
  AverageLogger in '..\Objects\AverageLogger.pas',
  AverageLogger.Write in '..\Objects\AverageLogger.Write.pas',
  BufferInterpreter.ATA in '..\WindowsFileAPI\BufferInterpreter.ATA.pas',
  BufferInterpreter.NVMe.Intel in '..\WindowsFileAPI\BufferInterpreter.NVMe.Intel.pas',
  BufferInterpreter.NVMe in '..\WindowsFileAPI\BufferInterpreter.NVMe.pas',
  BufferInterpreter in '..\WindowsFileAPI\BufferInterpreter.pas',
  Getter.SCSIAddress in '..\WindowsFileAPI\Getter.SCSIAddress.pas',
  Getter.SlotSpeed in '..\WindowsFileAPI\Getter.SlotSpeed.pas',
  Getter.SlotSpeedByDeviceID in '..\WindowsFileAPI\Getter.SlotSpeedByDeviceID.pas',
  MeasureUnit.DataSize in '..\Modules\MeasureUnit.DataSize.pas',
  Device.SlotSpeed in '..\Modules\Device.SlotSpeed.pas',
  OS.SetupAPI in '..\Modules\OS.SetupAPI.pas',
  Device.PhysicalDrive.List in '..\WindowsFileAPI\Device.PhysicalDrive.List.pas',
  Getter.PhysicalDriveList.Auto in '..\WindowsFileAPI\Getter.PhysicalDriveList.Auto.pas',
  Getter.PhysicalDriveList.OS in '..\WindowsFileAPI\Getter.PhysicalDriveList.OS.pas',
  Getter.PhysicalDriveList in '..\WindowsFileAPI\Getter.PhysicalDriveList.pas',
  Getter.PhysicalDriveList.WMI in '..\WindowsFileAPI\Getter.PhysicalDriveList.WMI.pas',
  Getter.PartitionExtent in '..\WindowsFileAPI\Getter.PartitionExtent.pas',
  CommandSet.NVMe.WithoutDriver in '..\WindowsFileAPI\CommandSet.NVMe.WithoutDriver.pas',
  BufferInterpreter.SCSI in '..\WindowsFileAPI\BufferInterpreter.SCSI.pas',
  WMI in '..\WindowsFileAPI\WMI.pas',
  OS.SecurityDescriptor in '..\WindowsFileAPI\OS.SecurityDescriptor.pas',
  OSFile.ForInternal in '..\WindowsFileAPI\OSFile.ForInternal.pas',
  Version in '..\Modules\Version.pas',
  Registry.Helper.Internal in '..\WindowsFileAPI\Registry.Helper.Internal.pas',
  Registry.Helper in '..\WindowsFileAPI\Registry.Helper.pas',
  Getter.PhysicalDriveList.OS.Path in '..\WindowsFileAPI\Getter.PhysicalDriveList.OS.Path.pas',
  Support.Sandisk.USB in '..\Support\Support.Sandisk.USB.pas',
  CommandSet.NVMe.OS in '..\WindowsFileAPI\CommandSet.NVMe.OS.pas',
  OS.Handle in '..\WindowsFileAPI\OS.Handle.pas',
  Support.Transcend in '..\Support\Support.Transcend.pas',
  Support.Fallback.CDI in '..\Support\Support.Fallback.CDI.pas',
  SMARTSupport.Corsair in '..\SMARTSupport\SMARTSupport.Corsair.pas',
  SMARTSupport.Factory in '..\SMARTSupport\SMARTSupport.Factory.pas',
  SMARTSupport.Fallback in '..\SMARTSupport\SMARTSupport.Fallback.pas',
  SMARTSupport.Fallback.SSD in '..\SMARTSupport\SMARTSupport.Fallback.SSD.pas',
  SMARTSupport.Indilinx in '..\SMARTSupport\SMARTSupport.Indilinx.pas',
  SMARTSupport.Intel in '..\SMARTSupport\SMARTSupport.Intel.pas',
  SMARTSupport.JMicron60x in '..\SMARTSupport\SMARTSupport.JMicron60x.pas',
  SMARTSupport.JMicron61x in '..\SMARTSupport\SMARTSupport.JMicron61x.pas',
  SMARTSupport.Kingston in '..\SMARTSupport\SMARTSupport.Kingston.pas',
  SMARTSupport.Micron.New in '..\SMARTSupport\SMARTSupport.Micron.New.pas',
  SMARTSupport.Micron.Old in '..\SMARTSupport\SMARTSupport.Micron.Old.pas',
  SMARTSupport.Mtron in '..\SMARTSupport\SMARTSupport.Mtron.pas',
  SMARTSupport.NVMe.Intel in '..\SMARTSupport\SMARTSupport.NVMe.Intel.pas',
  SMARTSupport.NVMe in '..\SMARTSupport\SMARTSupport.NVMe.pas',
  SMARTSupport.OCZ in '..\SMARTSupport\SMARTSupport.OCZ.pas',
  SMARTSupport.OCZ.Vector in '..\SMARTSupport\SMARTSupport.OCZ.Vector.pas',
  SMARTSupport in '..\SMARTSupport\SMARTSupport.pas',
  SMARTSupport.Plextor in '..\SMARTSupport\SMARTSupport.Plextor.pas',
  SMARTSupport.Samsung in '..\SMARTSupport\SMARTSupport.Samsung.pas',
  SMARTSupport.Sandforce in '..\SMARTSupport\SMARTSupport.Sandforce.pas',
  SMARTSupport.Sandisk.GB in '..\SMARTSupport\SMARTSupport.Sandisk.GB.pas',
  SMARTSupport.Sandisk in '..\SMARTSupport\SMARTSupport.Sandisk.pas',
  SMARTSupport.Seagate.NotSSD in '..\SMARTSupport\SMARTSupport.Seagate.NotSSD.pas',
  SMARTSupport.Seagate.SSD in '..\SMARTSupport\SMARTSupport.Seagate.SSD.pas',
  SMARTSupport.Toshiba in '..\SMARTSupport\SMARTSupport.Toshiba.pas',
  SMARTSupport.WD in '..\SMARTSupport\SMARTSupport.WD.pas';

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
