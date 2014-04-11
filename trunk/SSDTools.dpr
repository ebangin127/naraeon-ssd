program SSDTools;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {fMain},
  uAlert in 'uAlert.pas' {fAlert},
  Windows,
  SysUtils,
  Classes,
  Dialogs,
  uMessage in 'uMessage.pas' {fMessage},
  uBrowser in 'uBrowser.pas' {fBrowser},
  uDiskFunctions in 'Modules\Disk\uDiskFunctions.pas',
  uPartitionFunctions in 'Modules\Disk\uPartitionFunctions.pas',
  uSMARTFunctions in 'Modules\Disk\uSMARTFunctions.pas',
  uTrimCommand in 'Modules\Disk\uTrimCommand.pas',
  uIntFunctions in 'Modules\Etc\uIntFunctions.pas',
  uLanguageSettings in 'Modules\Language\uLanguageSettings.pas',
  uRegFunctions in 'Modules\Windows\uRegFunctions.pas',
  uFileFunctions in 'Modules\Windows\uFileFunctions.pas',
  uStrFunctions in 'Modules\Etc\uStrFunctions.pas',
  uExeFunctions in 'Modules\Windows\uExeFunctions.pas',
  uImager in 'Modules\Windows\uImager.pas',
  uLogSystem in 'Classes\LogSystem\uLogSystem.pas',
  uOptimizer in 'Classes\Optimizer\uOptimizer.pas',
  uSSDInfo in 'Classes\SSDInfo\uSSDInfo.pas',
  uSSDVersion in 'Classes\SSDInfo\uSSDVersion.pas',
  uTrimThread in 'Classes\Threads\uTrimThread.pas',
  uUpdateThread in 'Classes\Threads\uUpdateThread.pas',
  uVersion in 'Classes\Version\uVersion.pas',
  uDownloadPath in 'Classes\DownloadPath\uDownloadPath.pas',
  uPlugAndPlay in 'Modules\Windows\uPlugAndPlay.pas';

{$R *.res}
var
  //반자동 트림용 쓰레드
  TrimThread: TTrimThread;

  //캡션 생성 및 뮤텍스 찾기
  Cap: String;

  //트림 진행용(파티션 찾기까지)
  Drives: TDriveLetters;
  TempSSDInfo: TSSDInfo_NST;
  RobustMode, ATAorSCSI, Completed: Boolean;

  //트림 진행용(트림 자체)
  CurrDrvNum, CurrPartition: Integer;
  AllDrv: TStringList;
  CurrDrv: Integer;
  hdrive: THandle;

begin
  Application.Initialize;
  Cap := 'Naraeon SSD Tools ' + CurrentVersion + CapToSeeSerial[CurrLang];
  if (ParamStr(1) <> '') and (Copy(ParamStr(1), Length(ParamStr(1)) - 3, 4) <> '.err') then
  begin
    MainLoaded := false;
    MutexAppear := OpenMutex(MUTEX_ALL_ACCESS, False, 'NSToolsOpened2');
    if MutexAppear <> 0 then Application.Terminate
    else MutexAppear := CreateMutex(Nil, True, 'NSToolsOpened2');

    if GetSystemDefaultLangID = 1042 then
      CurrLang := LANG_HANGUL
    else
      CurrLang := LANG_ENGLISH;

    PartCount := 0;
    CompletedPartition := 0;
    CurrDrvNum := 0;
    SetLength(NeedTrimPartition, 0);

    TempSSDInfo := TSSDInfo_NST.Create;
    AllDrv := GetSSDList.ResultList;
    if (AllDrv.Count = 1) and (AllDrv[0] = '/') then
    begin
      AllDrv.Clear;
      RobustMode := true;
      for CurrDrv := 0 to 99 do
        AllDrv.Add(IntToStr(CurrDrv));
    end
    else
      RobustMode := false;

    ATAorSCSI := false;
    Completed := false;
    for CurrDrv := 0 to AllDrv.Count - 1 do
    begin
      if (AllDrv[CurrDrv] <> '/') and (AllDrv[CurrDrv] <> '') then
      begin
        hdrive := CreateFile(PChar('\\.\PhysicalDrive' + AllDrv[CurrDrv]), GENERIC_READ or GENERIC_WRITE,
                                    FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
        if GetLastError = 0 then
        begin
          try
            if ATAorSCSI = ATAMode then TempSSDInfo.ATAorSCSI := ATAModel
            else if ATAorSCSI = SCSIMode then TempSSDInfo.ATAorSCSI := SCSIModel;
            if RobustMode then
            begin
              TempSSDInfo.ATAorSCSI := DetermineModel;
            end;
            TempSSDInfo.SetDeviceName('PhysicalDrive' + AllDrv[CurrDrv]);
          finally
            if TempSSDInfo.SupportedDevice <> SUPPORT_NONE then
            begin
              if (TempSSDInfo.Serial = ParamStr(1)) and (TempSSDInfo.ATAorSCSI = ATAModel) then
              begin
                Drives := GetPartitionList(ExtractDrvNum(TempSSDInfo.DeviceName));
                SetLength(NeedTrimPartition, Length(NeedTrimPartition) + Length(Drives.Letters));
                for CurrPartition := 1 to Length(Drives.Letters) do
                begin
                  NeedTrimPartition[CurrDrvNum] := Drives.Letters[CurrPartition] + ':';
                  CurrDrvNum := CurrDrvNum + 1;
                end;
                Completed := true;
              end;
            end;
          end;
        end;
        CloseHandle(hdrive);
      end
      else
      begin
        ATAorSCSI := SCSIMode;
      end;
      if Completed then Break;
    end;
    TrimStat := 0;
    FreeAndNil(TempSSDInfo);

    if TrimThread <> Nil then FreeAndNil(TrimThread);
    TrimThread := TTrimThread.Create(true);
    TrimThread.Priority := tpLower;
    TrimThread.Start;
    while TrimStat < 2 do Sleep(10);
    Sleep(10);
    FreeAndNil(TrimThread);
    ReleaseMutex(MutexAppear);
    CloseHandle(MutexAppear);
  end
  else
  begin
    MutexAppear := OpenMutex(MUTEX_ALL_ACCESS, False, 'NSToolsOpened1');

    if GetSystemDefaultLangID = 1042 then
      CurrLang := LANG_HANGUL
    else
      CurrLang := LANG_ENGLISH;
    Cap := 'Naraeon SSD Tools ' + CurrentVersion + CapToSeeSerial[CurrLang];

    if MutexAppear <> 0 then
    begin
      MutexAppear := FindWindow(PChar('TfMain'), PChar(Cap));
      if (MutexAppear <> 0) and (Copy(ParamStr(1), Length(ParamStr(1)) - 3, 4) <> '.err') then
      begin
        ShowWindow(MutexAppear, SW_RESTORE);
        SetForegroundWindow(MutexAppear);
        CloseHandle(MutexAppear);
      end;
    end
    else
    begin
      MutexAppear := CreateMutex(Nil, True, 'NSToolsOpened1');
      Application.MainFormOnTaskbar := True;
      Application.CreateForm(TfMain, fMain);
  fMain.Caption := Cap;
      Application.Run;
    end;
  end;
end.
