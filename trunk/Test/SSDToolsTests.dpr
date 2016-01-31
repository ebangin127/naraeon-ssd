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
  TestAverageCountLogger in 'Objects\AverageLogger\TestAverageCountLogger.pas',
  TestAverageWriteLogger in 'Objects\AverageLogger\TestAverageWriteLogger.pas',
  OSFile in '..\WindowsFileAPI\OSFile.pas',
  Support.Factory in '..\Support\Support.Factory.pas',
  TestNSTSupport in 'NSTSupport\TestNSTSupport.pas',
  OS.WriteBufferSettingVerifier in '..\Objects\OS.WriteBufferSettingVerifier.pas',
  Registry.Helper in '..\Objects\Registry.Helper.pas',
  Registry.Helper.Internal in '..\Objects\Registry.Helper.Internal.pas',
  OS.ProcessOpener in '..\Objects\OS.ProcessOpener.pas',
  OS.WindowsVersion in '..\Modules\OS.WindowsVersion.pas',
  OS.SecurityDescriptor in '..\Objects\OS.SecurityDescriptor.pas',
  AsciiCheck in '..\Modules\AsciiCheck.pas',
  OS.VersionHelper in '..\Modules\OS.VersionHelper.pas',
  Support.NVMe.Intel in '..\Support\Support.NVMe.Intel.pas',
  Support.NVMe.Samsung in '..\Support\Support.NVMe.Samsung.pas',
  AverageLogger.Count in '..\Objects\AverageLogger.Count.pas',
  AverageLogger in '..\Objects\AverageLogger.pas',
  AverageLogger.Write in '..\Objects\AverageLogger.Write.pas',
  TestPartitionTrimmer in 'ThreadHelper\TestPartitionTrimmer.pas',
  uMockDeviceTrimmer in 'ThreadHelper\uMockDeviceTrimmer.pas',
  uMockTrimBasicsGetterFactory in 'ThreadHelper\uMockTrimBasicsGetterFactory.pas',
  uMockVolumeBitmapGetter in 'ThreadHelper\uMockVolumeBitmapGetter.pas',
  Thread.Trim.Helper.Partition in '..\Objects\Thread.Trim.Helper.Partition.pas',
  BufferInterpreter.SCSI in '..\WindowsFileAPI\BufferInterpreter.SCSI.pas',
  TestBufferInterpreter.SCSI in 'TestBufferInterpreter.SCSI.pas',
  BufferInterpreter.NVMe in '..\WindowsFileAPI\BufferInterpreter.NVMe.pas',
  TestBufferInterpreter.NVMe.Intel in 'TestBufferInterpreter.NVMe.Intel.pas',
  BufferInterpreter.NVMe.Intel in '..\WindowsFileAPI\BufferInterpreter.NVMe.Intel.pas',
  TestBufferInterpreter.ATA in 'TestBufferInterpreter.ATA.pas',
  BufferInterpreter.ATA in '..\WindowsFileAPI\BufferInterpreter.ATA.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
  ReadLn;
end.

