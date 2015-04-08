program SSDTools;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {fMain},
  uAlert in 'uAlert.pas' {fAlert},
  Windows,
  SysUtils,
  Classes,
  ShellAPI,
  uMessage in 'uMessage.pas' {fMessage},
  uBrowser in 'uBrowser.pas' {fBrowser},
  uDiskFunctions in 'Modules\Disk\uDiskFunctions.pas',
  uPartitionFunctions in 'Modules\Disk\uPartitionFunctions.pas',
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
  uVersion in 'Classes\Version\uVersion.pas',
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
  uDiag in 'Classes\Diag\uDiag.pas',
  uPathManager in 'Classes\PathManager\uPathManager.pas',
  uSemiAuto in 'Classes\SemiAuto\uSemiAuto.pas',
  uErase in 'Classes\Erase\uErase.pas',
  uDatasizeUnit in 'Classes\MeasureUnit\uDatasizeUnit.pas',
  uTimeUnit in 'Classes\MeasureUnit\uTimeUnit.pas',
  uInitializer in 'Classes\Initializer\uInitializer.pas',
  uLocaleApplier in 'ModulesForUI\Init\uLocaleApplier.pas',
  uNTFSStruct in 'Classes\Partition\uNTFSStruct.pas',
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
  uCrucialNSTSupport in 'NSTSupport\Support\uCrucialNSTSupport.pas',
  uLiteonNSTSupport in 'NSTSupport\Support\uLiteonNSTSupport.pas',
  uPlextorNSTSupport in 'NSTSupport\Support\uPlextorNSTSupport.pas',
  uSandiskNSTSupport in 'NSTSupport\Support\uSandiskNSTSupport.pas',
  uSeagateNSTSupport in 'NSTSupport\Support\uSeagateNSTSupport.pas',
  uToshibaNSTSupport in 'NSTSupport\Support\uToshibaNSTSupport.pas',
  uAutoNSTSupport in 'NSTSupport\Auto\uAutoNSTSupport.pas',
  uPhysicalDriveListGetter in 'WindowsFileAPI\PhysicalDrive\ListGetter\Abstraction\uPhysicalDriveListGetter.pas',
  uCommandSet in 'WindowsFileAPI\CommandSet\Abstraction\uCommandSet.pas',
  uWeb in 'Web\Abstraction\uWeb.pas',
  uGetFirm in 'Classes\GetFirm\uGetFirm.pas',
  uHTTPWeb in 'Web\Getter\uHTTPWeb.pas',
  uHTTPSWeb in 'Web\Getter\uHTTPSWeb.pas',
  uFirmwareGetter in 'Objects\uFirmwareGetter.pas';

type
  TRunMode = (RM_NORMAL, RM_DIAG, RM_UNINSTALL, RM_SEMIAUTO);

{$R *.res}
var
  //캡션 생성 및 뮤텍스 찾기
  Cap: String;

  //명령행 해석
  ParamInUpper: String;
  RunMode: TRunMode;

  //현재 프로세스 뮤텍스 관리
  MutexAppear: LongInt;

begin
  Application.Initialize;
  Cap := 'Naraeon SSD Tools ' + CurrentVersion + CapToSeeSerial[CurrLang];

  TPathManager.GetPath(Application);
  DetermineLanguage;

  ParamInUpper := UpperCase(ParamStr(1));

  SimulationMode := ParamInUpper = '/SIMULMODE';
  RunMode := RM_NORMAL;

  if ParamInUpper = '/DIAG' then
    RunMode := RM_DIAG
  else if ParamInUpper = '/UNINSTALL' then
    RunMode := RM_UNINSTALL
  else if (not SimulationMode) and (ParamInUpper <> '') then
    RunMode := RM_SEMIAUTO;

  MutexAppear := 0;

  TGetFirm.CreateCache;
  case RunMode of
  RM_NORMAL:
  begin
    SimulationMode := ParamInUpper = '/SIMULMODE';

    MutexAppear := OpenMutex(MUTEX_ALL_ACCESS, False, 'NSToolsOpened');

    if MutexAppear <> 0 then
    begin
      MutexAppear := FindWindow(PChar('TfMain'), PChar(Cap));
      if (MutexAppear <> 0)
      and (Copy(ParamStr(1), Length(ParamStr(1)) - 3, 4) <> '.err') then
      begin
        ShowWindow(MutexAppear, SW_RESTORE);
        SetForegroundWindow(MutexAppear);
        CloseHandle(MutexAppear);
      end;
    end
    else
    begin
      MutexAppear := CreateMutex(Nil, True, 'NSToolsOpened');
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TfMain, fMain);
  fMain.Caption := Cap;
      Application.Run;
    end;
  end;

  RM_UNINSTALL:
  begin
    DeletePrevSvc;
  end;

  RM_DIAG:
  begin
    TDiag.Diagnosis;
  end;

  RM_SEMIAUTO:
  begin
    TSemiAuto.SemiAutoTrim(ParamStr(1), ParamStr(2));
    TGetFirm.DestroyCache;
  end;

  end;
  TGetFirm.DestroyCache;

  if MutexAppear <> 0 then
  begin
    ReleaseMutex(MutexAppear);
    CloseHandle(MutexAppear);
  end;
end.
