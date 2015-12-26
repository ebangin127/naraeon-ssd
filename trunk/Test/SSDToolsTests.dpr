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
  uSandforceNSTSupport in '..\NSTSupport\Abstraction\uSandforceNSTSupport.pas',
  uNSTSupport in '..\NSTSupport\Abstraction\uNSTSupport.pas',
  Device.SMART.List in '..\Objects\Device.SMART.List.pas',
  DUnitTestRunner,
  ShellApi,
  Windows,
  uCrucialNSTSupport in '..\NSTSupport\Support\uCrucialNSTSupport.pas',
  uLiteonNSTSupport in '..\NSTSupport\Support\uLiteonNSTSupport.pas',
  uMachXtremeNSTSupport in '..\NSTSupport\Support\uMachXtremeNSTSupport.pas',
  uPhisonNSTSupport in '..\NSTSupport\Support\uPhisonNSTSupport.pas',
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
  uADATASandforceNSTSupport in '..\NSTSupport\SandforceSupport\uADATASandforceNSTSupport.pas',
  uADATANSTSupport in '..\NSTSupport\Support\uADATANSTSupport.pas',
  TestAverageCountLogger in 'Objects\AverageLogger\TestAverageCountLogger.pas',
  uUINT64 in '..\Modules\uUINT64.pas',
  TestAverageWriteLogger in 'Objects\AverageLogger\TestAverageWriteLogger.pas',
  TestPartitionTrimmer in 'ThreadHelper\TestPartitionTrimmer.pas',
  uMockTrimBasicsGetterFactory in 'ThreadHelper\uMockTrimBasicsGetterFactory.pas',
  uMockDeviceTrimmer in 'ThreadHelper\uMockDeviceTrimmer.pas',
  uMockVolumeBitmapGetter in 'ThreadHelper\uMockVolumeBitmapGetter.pas',
  uPartitionTrimmer in '..\ThreadHelper\uPartitionTrimmer.pas',
  OSFile in '..\WindowsFileAPI\OSFile.pas',
  uNSTSupportFactory in '..\NSTSupport\Factory\uNSTSupportFactory.pas',
  TestNSTSupport in 'NSTSupport\TestNSTSupport.pas',
  TestWriteBufferSettingVerifier in 'Objects\TestWriteBufferSettingVerifier.pas',
  uWriteBufferSettingVerifier in '..\Objects\uWriteBufferSettingVerifier.pas',
  uNSTRegistry in '..\Objects\uNSTRegistry.pas',
  uRegistryHelper in '..\Objects\uRegistryHelper.pas',
  uProcessOpener in '..\Objects\uProcessOpener.pas',
  uAverageLogger in '..\Objects\AverageLogger\Abstraction\uAverageLogger.pas',
  uAverageCountLogger in '..\Objects\AverageLogger\Specific\uAverageCountLogger.pas',
  uAverageWriteLogger in '..\Objects\AverageLogger\Specific\uAverageWriteLogger.pas',
  uWindowsVersion in '..\Modules\uWindowsVersion.pas',
  uSecurityDescriptor in '..\Objects\uSecurityDescriptor.pas',
  uStringHelper in '..\Modules\uStringHelper.pas',
  uVersionHelper in '..\Modules\uVersionHelper.pas',
  Support.Intel.NVMe in '..\NSTSupport\Support\Support.Intel.NVMe.pas',
  Support.Samsung.NVMe in '..\NSTSupport\Support\Support.Samsung.NVMe.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
  ReadLn;
end.

