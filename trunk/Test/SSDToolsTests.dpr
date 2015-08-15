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
  uSMARTValueList in '..\Objects\uSMARTValueList.pas',
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
  TestAverageCountLogger in 'Classes\AverageLogger\TestAverageCountLogger.pas',
  uAverageLogger in '..\Classes\AverageLogger\Abstraction\uAverageLogger.pas',
  uAverageCountLogger in '..\Classes\AverageLogger\Specific\uAverageCountLogger.pas',
  uAverageWriteLogger in '..\Classes\AverageLogger\Specific\uAverageWriteLogger.pas',
  uUINT64 in '..\Modules\uUINT64.pas',
  TestAverageWriteLogger in 'Classes\AverageLogger\TestAverageWriteLogger.pas',
  TestPartitionTrimmer in 'ThreadHelper\TestPartitionTrimmer.pas',
  uMockAutoTrimBasicsGetter in 'ThreadHelper\uMockAutoTrimBasicsGetter.pas',
  uMockDeviceTrimmer in 'ThreadHelper\uMockDeviceTrimmer.pas',
  uMockVolumeBitmapGetter in 'ThreadHelper\uMockVolumeBitmapGetter.pas',
  uPartitionTrimmer in '..\ThreadHelper\uPartitionTrimmer.pas',
  uOSFile in '..\WindowsFileAPI\Abstraction\uOSFile.pas',
  uNSTSupportFactory in '..\NSTSupport\Factory\uNSTSupportFactory.pas',
  TestNSTSupport in 'NSTSupport\TestNSTSupport.pas',
  TestWriteBufferSettingVerifier in 'Objects\TestWriteBufferSettingVerifier.pas',
  uWriteBufferSettingVerifier in '..\Objects\uWriteBufferSettingVerifier.pas',
  uStaticRegistry in '..\Objects\uStaticRegistry.pas',
  uRegistryHelper in '..\Objects\uRegistryHelper.pas',
  uProcessOpener in '..\Objects\uProcessOpener.pas';

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
  ReadLn;
end.

