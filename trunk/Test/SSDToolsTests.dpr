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
  uUINT64 in '..\Modules\uUINT64.pas',
  TestAverageWriteLogger in 'Objects\AverageLogger\TestAverageWriteLogger.pas',
  TestPartitionTrimmer in 'Objects\TestPartitionTrimmer.pas',
  uMockTrimBasicsGetterFactory in 'Objects\uMockTrimBasicsGetterFactory.pas',
  uMockDeviceTrimmer in 'Objects\uMockDeviceTrimmer.pas',
  uMockVolumeBitmapGetter in 'Objects\uMockVolumeBitmapGetter.pas',
  PartitionTrimmer in '..\Objects\PartitionTrimmer.pas',
  OSFile in '..\WindowsFileAPI\OSFile.pas',
  Support.Factory in '..\Support\Support.Factory.pas',
  TestNSTSupport in 'NSTSupport\TestNSTSupport.pas',
  TestWriteBufferSettingVerifier in 'Objects\TestWriteBufferSettingVerifier.pas',
  OS.WriteBufferSettingVerifier in '..\Objects\OS.WriteBufferSettingVerifier.pas',
  Registry.Helper in '..\Objects\Registry.Helper.pas',
  Registry.Helper.Internal in '..\Objects\Registry.Helper.Internal.pas',
  OS.ProcessOpener in '..\Objects\OS.ProcessOpener.pas',
  uAverageLogger in '..\Objects\AverageLogger\Abstraction\uAverageLogger.pas',
  uAverageCountLogger in '..\Objects\AverageLogger\Specific\uAverageCountLogger.pas',
  uAverageWriteLogger in '..\Objects\AverageLogger\Specific\uAverageWriteLogger.pas',
  OS.WindowsVersion in '..\Modules\OS.WindowsVersion.pas',
  OS.SecurityDescriptor in '..\Objects\OS.SecurityDescriptor.pas',
  AsciiCheck in '..\Modules\AsciiCheck.pas',
  OS.VersionHelper in '..\Modules\OS.VersionHelper.pas',
  Support.NVMe.Intel in '..\Support\Support.NVMe.Intel.pas',
  Support.NVMe.Samsung in '..\Support\Support.NVMe.Samsung.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
  ReadLn;
end.

