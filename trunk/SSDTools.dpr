program SSDTools;

uses
  Vcl.Forms,
  Windows,
  SysUtils,
  Classes,
  ShellAPI,
  uLanguageSettings in 'Resource\uLanguageSettings.pas',
  uStringHelper in 'Modules\uStringHelper.pas',
  uNSTSupport in 'NSTSupport\Abstraction\uNSTSupport.pas',
  uTrimThread in 'Threads\uTrimThread.pas',
  uUpdateThread in 'Threads\uUpdateThread.pas',
  uVersion in 'Modules\uVersion.pas',
  uDownloadPath in 'Objects\uDownloadPath.pas',
  uUAWebbrowser in 'Model\uUAWebbrowser.pas',
  uButtonGroup in 'Model\uButtonGroup.pas',
  uInit in 'Model\uInit.pas',
  uSevenZip in 'ExternProgram\uSevenZip.pas',
  uTrimList in 'Objects\uTrimList.pas',
  uRufus in 'ExternProgram\uRufus.pas',
  uPathManager in 'Objects\uPathManager.pas',
  uSemiAutoTrimmer in 'Objects\uSemiAutoTrimmer.pas',
  uInitializer in 'Objects\uInitializer.pas',
  uLocaleApplier in 'Model\uLocaleApplier.pas',
  uPhysicalDrive in 'Objects\uPhysicalDrive.pas',
  uSMARTValueList in 'Objects\uSMARTValueList.pas',
  uATABufferInterpreter in 'WindowsFileAPI\Interpreter\uATABufferInterpreter.pas',
  uVersionPublisher in 'Resource\uVersionPublisher.pas',
  uPartition in 'Objects\uPartition.pas',
  uRegistryHelper in 'Objects\uRegistryHelper.pas',
  uAutoPhysicalDriveListGetter in 'WindowsFileAPI\PhysicalDrive\ListGetter\Auto\uAutoPhysicalDriveListGetter.pas',
  uBruteForcePhysicalDriveListGetter in 'WindowsFileAPI\PhysicalDrive\ListGetter\Specific\uBruteForcePhysicalDriveListGetter.pas',
  uWMIPhysicalDriveListGetter in 'WindowsFileAPI\PhysicalDrive\ListGetter\Specific\uWMIPhysicalDriveListGetter.pas',
  uPhysicalDriveList in 'WindowsFileAPI\PhysicalDrive\List\uPhysicalDriveList.pas',
  uDiskGeometryGetter in 'WindowsFileAPI\PhysicalDrive\Getter\uDiskGeometryGetter.pas',
  uDriveAvailabilityGetter in 'WindowsFileAPI\PhysicalDrive\Getter\uDriveAvailabilityGetter.pas',
  uPartitionListGetter in 'WindowsFileAPI\PhysicalDrive\Getter\uPartitionListGetter.pas',
  uATACommandSet in 'WindowsFileAPI\CommandSet\Specific\uATACommandSet.pas',
  uSATCommandSet in 'WindowsFileAPI\CommandSet\Specific\uSATCommandSet.pas',
  uPhysicalDriveListGetter in 'WindowsFileAPI\PhysicalDrive\ListGetter\Abstraction\uPhysicalDriveListGetter.pas',
  uCommandSet in 'WindowsFileAPI\CommandSet\Abstraction\uCommandSet.pas',
  uWeb in 'Objects\Web\Abstraction\uWeb.pas',
  uHTTPSWeb in 'Objects\Web\Implementation\uHTTPSWeb.pas',
  uFirmwareGetter in 'Objects\uFirmwareGetter.pas',
  uNCQAvailabilityGetter in 'WindowsFileAPI\PhysicalDrive\Getter\uNCQAvailabilityGetter.pas',
  uUpdater in 'ThreadHelper\uUpdater.pas',
  uParameter in 'Objects\uParameter.pas',
  uHTTPWeb in 'Objects\Web\Implementation\uHTTPWeb.pas',
  uAlert in 'Form\uAlert.pas' {fAlert},
  uBrowser in 'Form\uBrowser.pas' {fBrowser},
  uMain in 'Form\uMain.pas' {fMain},
  uMessage in 'Form\uMessage.pas' {fMessage},
  uListTrimmer in 'ThreadHelper\uListTrimmer.pas',
  uTrimBasicsGetter in 'WindowsFileAPI\Partition\TrimBasics\Abstraction\uTrimBasicsGetter.pas',
  uNTFSTrimBasicsGetter in 'WindowsFileAPI\Partition\TrimBasics\Getter\uNTFSTrimBasicsGetter.pas',
  uFATTrimBasicsGetter in 'WindowsFileAPI\Partition\TrimBasics\Getter\uFATTrimBasicsGetter.pas',
  uTrimBasicsGetterFactory in 'WindowsFileAPI\Partition\TrimBasics\Factory\uTrimBasicsGetterFactory.pas',
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
  uPhisonNSTSupport in 'NSTSupport\Support\uPhisonNSTSupport.pas',
  uProgressSection in 'Model\uProgressSection.pas',
  uMainformMainpartApplier in 'Model\uMainformMainpartApplier.pas',
  uMainformPartitionAlignApplier in 'Model\uMainformPartitionAlignApplier.pas',
  uMainformReplacedSectorApplier in 'Model\uMainformReplacedSectorApplier.pas',
  uMainformSMARTApplier in 'Model\uMainformSMARTApplier.pas',
  uMainformTotalWriteApplier in 'Model\uMainformTotalWriteApplier.pas',
  uMainformPhysicalDriveApplier in 'Model\uMainformPhysicalDriveApplier.pas',
  uSSDLabel in 'Model\uSSDLabel.pas',
  uSSDLabelListRefresher in 'Model\uSSDLabelListRefresher.pas',
  uSSDLabelList in 'Model\uSSDLabelList.pas',
  uListChangeGetter in 'WindowsFileAPI\PhysicalDrive\Getter\uListChangeGetter.pas',
  uCodesignPublisherVerifier in 'Objects\uCodesignPublisherVerifier.pas',
  uDownloadThread in 'Threads\uDownloadThread.pas',
  uDownloader in 'ThreadHelper\uDownloader.pas',
  uFirmwareDownloader in 'Objects\uFirmwareDownloader.pas',
  uLegacyATACommandSet in 'WindowsFileAPI\CommandSet\Specific\uLegacyATACommandSet.pas',
  uCodesignExtern in 'Modules\uCodesignExtern.pas',
  uDeleteDirectory in 'Modules\uDeleteDirectory.pas',
  uProcessOpener in 'Objects\uProcessOpener.pas',
  uPlugAndPlay in 'Modules\uPlugAndPlay.pas',
  uNSTRegistry in 'Objects\uNSTRegistry.pas',
  uFixedDriveListGetter in 'WindowsFileAPI\Partition\Getter\uFixedDriveListGetter.pas',
  uPartitionExtentGetter in 'WindowsFileAPI\Partition\Getter\uPartitionExtentGetter.pas',
  uRemovableDriveListGetter in 'WindowsFileAPI\Partition\Getter\uRemovableDriveListGetter.pas',
  uVolumeBitmapGetter in 'WindowsFileAPI\Partition\Getter\uVolumeBitmapGetter.pas',
  uVolumeLabelGetter in 'WindowsFileAPI\Partition\Getter\uVolumeLabelGetter.pas',
  uBufferInterpreter in 'WindowsFileAPI\Abstraction\uBufferInterpreter.pas',
  uDriveListGetter in 'WindowsFileAPI\Abstraction\uDriveListGetter.pas',
  uIoControlFile in 'WindowsFileAPI\Abstraction\uIoControlFile.pas',
  uOSFile in 'WindowsFileAPI\Abstraction\uOSFile.pas',
  uOSFileWithHandle in 'WindowsFileAPI\Abstraction\uOSFileWithHandle.pas',
  uADATASandforceNSTSupport in 'NSTSupport\SandforceSupport\uADATASandforceNSTSupport.pas',
  uADATANSTSupport in 'NSTSupport\Support\uADATANSTSupport.pas',
  uPartitionTrimmer in 'ThreadHelper\uPartitionTrimmer.pas',
  uDeviceTrimmer in 'ThreadHelper\uDeviceTrimmer.pas',
  uCommandSetFactory in 'WindowsFileAPI\CommandSet\Factory\uCommandSetFactory.pas',
  uNSTSupportFactory in 'NSTSupport\Factory\uNSTSupportFactory.pas',
  uIs64Bit in 'Modules\uIs64Bit.pas',
  uSecurityDescriptor in 'Objects\uSecurityDescriptor.pas',
  uDatasizeUnit in 'Modules\uDatasizeUnit.pas',
  uTimeUnit in 'Modules\uTimeUnit.pas',
  uAverageLogger in 'Objects\AverageLogger\Abstraction\uAverageLogger.pas',
  uAverageCountLogger in 'Objects\AverageLogger\Specific\uAverageCountLogger.pas',
  uAverageWriteLogger in 'Objects\AverageLogger\Specific\uAverageWriteLogger.pas',
  uOptimizationUnit in 'Objects\Optimizer\Abstraction\uOptimizationUnit.pas',
  uLegacyOptimizer in 'Objects\uLegacyOptimizer.pas',
  uOptimizer in 'Objects\Optimizer\uOptimizer.pas',
  uVersionHelper in 'Modules\uVersionHelper.pas';

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
