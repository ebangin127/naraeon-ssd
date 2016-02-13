program SSDToolsTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$APPTYPE CONSOLE}

uses
  Support.Sandforce in '..\Support\Support.Sandforce.pas',
  Support in '..\Support\Support.pas',
  Device.SMART.List in '..\Objects\Device.SMART.List.pas',
  DUnitTestRunner,
  ShellApi,
  Windows,
  Support.Crucial in '..\Support\Support.Crucial.pas',
  Support.Liteon in '..\Support\Support.Liteon.pas',
  Support.MachXtreme in '..\Support\Support.MachXtreme.pas',
  Support.Phison in '..\Support\Support.Phison.pas',
  Support.Plextor in '..\Support\Support.Plextor.pas',
  Support.Samsung in '..\Support\Support.Samsung.pas',
  Support.Sandisk in '..\Support\Support.Sandisk.pas',
  Support.Seagate in '..\Support\Support.Seagate.pas',
  Support.Toshiba in '..\Support\Support.Toshiba.pas',
  Support.Sandforce.Hynix in '..\Support\Support.Sandforce.Hynix.pas',
  Support.Sandforce.MachXtreme in '..\Support\Support.Sandforce.MachXtreme.pas',
  Support.Sandforce.OCZ in '..\Support\Support.Sandforce.OCZ.pas',
  Support.Sandforce.Patriot in '..\Support\Support.Sandforce.Patriot.pas',
  Support.Sandforce.Toshiba in '..\Support\Support.Sandforce.Toshiba.pas',
  Support.Sandforce.ADATA in '..\Support\Support.Sandforce.ADATA.pas',
  Support.ADATA in '..\Support\Support.ADATA.pas',
  OSFile in '..\WindowsFileAPI\OSFile.pas',
  Support.Factory in '..\Support\Support.Factory.pas',
  OS.WriteBufferSettingVerifier in '..\Objects\OS.WriteBufferSettingVerifier.pas',
  Registry.Helper in '..\Objects\Registry.Helper.pas',
  Registry.Helper.Internal in '..\Objects\Registry.Helper.Internal.pas',
  OS.ProcessOpener in '..\Objects\OS.ProcessOpener.pas',
  OS.Version.Helper in '..\Modules\OS.Version.Helper.pas',
  AsciiCheck in '..\Modules\AsciiCheck.pas',
  Getter.OS.Version in '..\Objects\Getter.OS.Version.pas',
  Support.NVMe.Intel in '..\Support\Support.NVMe.Intel.pas',
  Support.NVMe.Samsung in '..\Support\Support.NVMe.Samsung.pas',
  AverageLogger.Count in '..\Objects\AverageLogger.Count.pas',
  AverageLogger in '..\Objects\AverageLogger.pas',
  AverageLogger.Write in '..\Objects\AverageLogger.Write.pas',
  Thread.Trim.Helper.Partition.Direct in '..\Objects\Thread.Trim.Helper.Partition.Direct.pas',
  BufferInterpreter.SCSI in '..\WindowsFileAPI\BufferInterpreter.SCSI.pas',
  BufferInterpreter.NVMe in '..\WindowsFileAPI\BufferInterpreter.NVMe.pas',
  BufferInterpreter.NVMe.Intel in '..\WindowsFileAPI\BufferInterpreter.NVMe.Intel.pas',
  BufferInterpreter.ATA in '..\WindowsFileAPI\BufferInterpreter.ATA.pas',
  CommandSet.Factory in '..\WindowsFileAPI\CommandSet.Factory.pas',
  CommandSet in '..\WindowsFileAPI\CommandSet.pas',
  Mock.CommandSets in 'Mock\Mock.CommandSets.pas',
  Mock.OSFile.IoControl in 'Mock\Mock.OSFile.IoControl.pas',
  TestAverageCountLogger in 'TestAverageCountLogger.pas',
  TestAverageWriteLogger in 'TestAverageWriteLogger.pas',
  TestBufferInterpreter.ATA in 'TestBufferInterpreter.ATA.pas',
  TestBufferInterpreter.NVMe.Intel in 'TestBufferInterpreter.NVMe.Intel.pas',
  TestBufferInterpreter.SCSI in 'TestBufferInterpreter.SCSI.pas',
  TestCommandSet.Factory in 'TestCommandSet.Factory.pas',
  TestNSTSupport in 'TestNSTSupport.pas',
  TestWriteBufferSettingVerifier in 'TestWriteBufferSettingVerifier.pas',
  TestSecurityDescriptorManipulator in 'TestSecurityDescriptorManipulator.pas',
  Mock.DeviceTrimmer in 'Mock\Mock.DeviceTrimmer.pas',
  Mock.Getter.TrimBasics.Factory in 'Mock\Mock.Getter.TrimBasics.Factory.pas',
  Mock.Getter.VolumeBitmap in 'Mock\Mock.Getter.VolumeBitmap.pas',
  BufferInterpreter in '..\WindowsFileAPI\BufferInterpreter.pas',
  Device.SlotSpeed in '..\Modules\Device.SlotSpeed.pas',
  MeasureUnit.DataSize in '..\Modules\MeasureUnit.DataSize.pas',
  Thread.Trim.Helper.Partition.OS in '..\Objects\Thread.Trim.Helper.Partition.OS.pas',
  Thread.Trim.Helper.Partition in '..\Objects\Thread.Trim.Helper.Partition.pas',
  TestThread.Trim.Helper.Partition in 'TestThread.Trim.Helper.Partition.pas',
  TestDirectPartitionTrimmer in 'TestDirectPartitionTrimmer.pas',
  TestOSFile in 'TestOSFile.pas',
  OS.SecurityDescriptor in '..\WindowsFileAPI\OS.SecurityDescriptor.pas',
  OSFile.Handle in '..\WindowsFileAPI\OSFile.Handle.pas',
  TestOSFile.Handle in 'TestOSFile.Handle.pas',
  TestOSFile.IoControl in 'TestOSFile.IoControl.pas',
  OSFile.IoControl in '..\WindowsFileAPI\OSFile.IoControl.pas',
  TestOS.Version.Helper in 'TestOS.Version.Helper.pas',
  Version in '..\Modules\Version.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
  ReadLn;
end.

