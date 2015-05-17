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
  uAutoNSTSupport in '..\NSTSupport\Auto\uAutoNSTSupport.pas',
  uHynixSandforceNSTSupport in '..\NSTSupport\SandforceSupport\uHynixSandforceNSTSupport.pas',
  uMachXtremeSandforceNSTSupport in '..\NSTSupport\SandforceSupport\uMachXtremeSandforceNSTSupport.pas',
  uOCZSandforceNSTSupport in '..\NSTSupport\SandforceSupport\uOCZSandforceNSTSupport.pas',
  uPatriotSandforceNSTSupport in '..\NSTSupport\SandforceSupport\uPatriotSandforceNSTSupport.pas',
  uToshibaSandforceNSTSupport in '..\NSTSupport\SandforceSupport\uToshibaSandforceNSTSupport.pas',
  uCrucialNSTSupport in '..\NSTSupport\Support\uCrucialNSTSupport.pas',
  uLiteonNSTSupport in '..\NSTSupport\Support\uLiteonNSTSupport.pas',
  uMachXtremeNSTSupport in '..\NSTSupport\Support\uMachXtremeNSTSupport.pas',
  uPlextorNSTSupport in '..\NSTSupport\Support\uPlextorNSTSupport.pas',
  uSamsungNSTSupport in '..\NSTSupport\Support\uSamsungNSTSupport.pas',
  uSandiskNSTSupport in '..\NSTSupport\Support\uSandiskNSTSupport.pas',
  uSeagateNSTSupport in '..\NSTSupport\Support\uSeagateNSTSupport.pas',
  uToshibaNSTSupport in '..\NSTSupport\Support\uToshibaNSTSupport.pas',
  uNSTSupport in '..\NSTSupport\Abstraction\uNSTSupport.pas',
  uSMARTValueList in '..\Objects\uSMARTValueList.pas',
  DUnitTestRunner,
  TestAutoNSTSupport in 'NSTSupport\TestAutoNSTSupport.pas',
  ShellApi, Windows;

{$R *.RES}

begin
  DUnitTestRunner.RunRegisteredTests;
  ReadLn;
end.

