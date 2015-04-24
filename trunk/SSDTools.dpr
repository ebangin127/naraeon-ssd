program SSDTools;

uses
  Vcl.Forms,
  Windows,
  SysUtils,
  Classes,
  ShellAPI,
  uDiskFunctions in 'Modules\Disk\uDiskFunctions.pas',
  uIntFunctions in 'Modules\Etc\uIntFunctions.pas',
  uLanguageSettings in 'Modules\Language\uLanguageSettings.pas',
  uRegFunctions in 'Modules\Windows\uRegFunctions.pas',
  uFileFunctions in 'Modules\Windows\uFileFunctions.pas',
  uStrFunctions in 'Modules\Etc\uStrFunctions.pas',
  uExeFunctions in 'Modules\Windows\uExeFunctions.pas',
  uLogSystem in 'Classes\LogSystem\uLogSystem.pas',
  uOptimizer in 'Classes\Optimizer\uOptimizer.pas',
  uNSTSupport in 'NSTSupport\Abstraction\uNSTSupport.pas',
  uTrimThread in 'Classes\Threads\uTrimThread.pas',
  uUpdateThread in 'Classes\Threads\uUpdateThread.pas',
  uVersion in 'Version\uVersion.pas',
  uDownloadPath in 'Classes\DownloadPath\uDownloadPath.pas',
  uPlugAndPlay in 'Modules\Windows\uPlugAndPlay.pas',
  uFirmware in 'ModulesForUI\Firmware\uFirmware.pas',
  uUSBDrive in 'ModulesForUI\USBDrive\uUSBDrive.pas',
  uRefresh in 'ModulesForUI\Refresh\uRefresh.pas',
  uUAWebbrowser in 'Classes\UAWebbrowser\uUAWebbrowser.pas',
  uButtonGroup in 'ModulesForUI\ButtonGroup\uButtonGroup.pas',
  uInit in 'ModulesForUI\Init\uInit.pas',
  uSevenZip in 'Classes\SevenZip\uSevenZip.pas',
  uTrimList in 'Classes\Threads\uTrimList.pas',
  uRufus in 'Classes\Rufus\uRufus.pas',
  uPathManager in 'Classes\PathManager\uPathManager.pas',
  uSemiAutoTrimmer in 'Objects\uSemiAutoTrimmer.pas',
  uErase in 'Classes\Erase\uErase.pas',
  uDatasizeUnit in 'Classes\MeasureUnit\uDatasizeUnit.pas',
  uTimeUnit in 'Classes\MeasureUnit\uTimeUnit.pas',
  uInitializer in 'Classes\Initializer\uInitializer.pas',
  uLocaleApplier in 'ModulesForUI\Init\uLocaleApplier.pas',
  uIoControlFile in 'WindowsFileAPI\Abstraction\uIoControlFile.pas',
  uOSFileWithHandle in 'WindowsFileAPI\Abstraction\uOSFileWithHandle.pas',
  uOSFile in 'WindowsFileAPI\Abstraction\uOSFile.pas',
  uPhysicalDrive in 'Objects\uPhysicalDrive.pas',
  uSMARTValueList in 'Objects\uSMARTValueList.pas',
  uBufferInterpreter in 'WindowsFileAPI\Abstraction\uBufferInterpreter.pas',
  uATABufferInterpreter in 'WindowsFileAPI\Interpreter\uATABufferInterpreter.pas',
  uGlobalSettings in 'GlobalSettings\uGlobalSettings.pas',
  uPartition in 'Objects\uPartition.pas',
  uRegistryHelper in 'Objects\uRegistryHelper.pas',
  uPartitionExtentGetter in 'WindowsFileAPI\Partition\Getter\uPartitionExtentGetter.pas',
  uFixedDriveListGetter in 'WindowsFileAPI\Partition\Getter\uFixedDriveListGetter.pas',
  uAutoPhysicalDriveListGetter in 'WindowsFileAPI\PhysicalDrive\ListGetter\Auto\uAutoPhysicalDriveListGetter.pas',
  uBruteForcePhysicalDriveListGetter in 'WindowsFileAPI\PhysicalDrive\ListGetter\Specific\uBruteForcePhysicalDriveListGetter.pas',
  uWMIPhysicalDriveListGetter in 'WindowsFileAPI\PhysicalDrive\ListGetter\Specific\uWMIPhysicalDriveListGetter.pas',
  uPhysicalDriveList in 'WindowsFileAPI\PhysicalDrive\List\uPhysicalDriveList.pas',
  uDiskGeometryGetter in 'WindowsFileAPI\PhysicalDrive\Getter\uDiskGeometryGetter.pas',
  uDriveAvailabilityGetter in 'WindowsFileAPI\PhysicalDrive\Getter\uDriveAvailabilityGetter.pas',
  uPartitionListGetter in 'WindowsFileAPI\PhysicalDrive\Getter\uPartitionListGetter.pas',
  uAutoCommandSet in 'WindowsFileAPI\CommandSet\Auto\uAutoCommandSet.pas',
  uATACommandSet in 'WindowsFileAPI\CommandSet\Specific\uATACommandSet.pas',
  uSATCommandSet in 'WindowsFileAPI\CommandSet\Specific\uSATCommandSet.pas',
  uAutoNSTSupport in 'NSTSupport\Auto\uAutoNSTSupport.pas',
  uPhysicalDriveListGetter in 'WindowsFileAPI\PhysicalDrive\ListGetter\Abstraction\uPhysicalDriveListGetter.pas',
  uCommandSet in 'WindowsFileAPI\CommandSet\Abstraction\uCommandSet.pas',
  uWeb in 'Web\Abstraction\uWeb.pas',
  uHTTPSWeb in 'Web\Getter\uHTTPSWeb.pas',
  uFirmwareGetter in 'Objects\uFirmwareGetter.pas',
  uNCQAvailabilityGetter in 'WindowsFileAPI\PhysicalDrive\Getter\uNCQAvailabilityGetter.pas',
  uUpdater in 'ThreadHelper\uUpdater.pas',
  uParameter in 'Objects\uParameter.pas',
  uHTTPWeb in 'Web\Getter\uHTTPWeb.pas',
  uAlert in 'Form\uAlert.pas' {fAlert},
  uBrowser in 'Form\uBrowser.pas' {fBrowser},
  uMain in 'Form\uMain.pas' {fMain},
  uMessage in 'Form\uMessage.pas' {fMessage},
  uListTrimmer in 'ThreadHelper\uListTrimmer.pas',
  uTrimBasicsGetter in 'WindowsFileAPI\Partition\TrimBasics\Abstraction\uTrimBasicsGetter.pas',
  uNTFSTrimBasicsGetter in 'WindowsFileAPI\Partition\TrimBasics\Getter\uNTFSTrimBasicsGetter.pas',
  uFATTrimBasicsGetter in 'WindowsFileAPI\Partition\TrimBasics\Getter\uFATTrimBasicsGetter.pas',
  uVolumeBitmapGetter in 'WindowsFileAPI\Partition\Getter\uVolumeBitmapGetter.pas',
  uAutoTrimBasicsGetter in 'WindowsFileAPI\Partition\TrimBasics\Auto\uAutoTrimBasicsGetter.pas',
  uPartitionTrimmer in 'ThreadHelper\uPartitionTrimmer.pas',
  uTrimThreadToModel in 'ThreadToModel\uTrimThreadToModel.pas',
  uSandforceNSTSupport in 'NSTSupport\Abstraction\uSandforceNSTSupport.pas',
  uToshibaSandforceNSTSupport in 'NSTSupport\SandforceSupport\uToshibaSandforceNSTSupport.pas',
  uHynixSandforceNSTSupport in 'NSTSupport\SandforceSupport\uHynixSandforceNSTSupport.pas',
  uOCZSandforceNSTSupport in 'NSTSupport\SandforceSupport\uOCZSandforceNSTSupport.pas',
  uPatriotSandforceNSTSupport in 'NSTSupport\SandforceSupport\uPatriotSandforceNSTSupport.pas',
  uMachXtremeSandforceNSTSupport in 'NSTSupport\SandforceSupport\uMachXtremeSandforceNSTSupport.pas',
  uCodesignVerifier in 'Objects\uCodesignVerifier.pas',
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
  uProgressSection in 'Model\uProgressSection.pas';

{$R *.res}
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
  if (ExistingInstanceWindow <> 0) and
     (Copy(ParamStr(1), Length(ParamStr(1)) - 3, 4) <> '.err') then
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
  TPathManager.SetPath(Application);
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
