unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.CheckLst, WinInet, UITypes,
  Vcl.OleCtrls, Vcl.ExtCtrls, IdHttp, IdComponent, ShellApi, Math,
  Vcl.Imaging.pngimage, ShlObj, Vcl.Mask, Vcl.ComCtrls,
  uAlert, uMessage, uBrowser, uLanguageSettings,
  uLogSystem, uSevenZip, uOptimizer, uUSBDrive,
  uDiskFunctions, uExeFunctions,
  uFileFunctions, uStrFunctions, uDownloadPath, uPlugAndPlay,
  uFirmware, uButtonGroup, uInit, uRufus, uPathManager,
  uUpdateThread, uTrimThread, uTrimList, uLocaleApplier,
  uPhysicalDrive, uPartitionListGetter, uPhysicalDriveList,
  uFirmwareGetter, uGlobalSettings, uCodesignVerifier,
  uSSDLabelList, uSSDLabel, uMainformPhysicalDriveApplier,
  uSSDLabelListRefresher;

const
  WM_AFTER_SHOW = WM_USER + 300;

type
  TfMain = class(TForm)
    gInfo: TGroupBox;
    lSerial: TLabel;
    lSectors: TLabel;
    lFirmware: TLabel;
    lNotsafe: TLabel;
    iFirmUp: TImage;
    lFirmUp: TLabel;
    iErase: TImage;
    lErase: TLabel;
    iOptimize: TImage;
    lOptimize: TLabel;
    tRefresh: TTimer;
    lPartitionAlign: TLabel;
    iAnalytics: TImage;
    lAnalytics: TLabel;
    lPError: TLabel;
    gAnalytics: TGroupBox;
    lAnaly: TLabel;
    l1Month: TLabel;
    lTodayUsage: TLabel;
    lHost: TLabel;
    lOntime: TLabel;
    iTrim: TImage;
    lTrim: TLabel;
    gFirmware: TGroupBox;
    lUpdate: TLabel;
    lUSB: TLabel;
    lNewFirm: TLabel;
    bFirmStart: TButton;
    cAgree: TCheckBox;
    cUSB: TComboBox;
    gErase: TGroupBox;
    lEraseUSB: TLabel;
    lUSBErase: TLabel;
    bEraseUSBStart: TButton;
    cEraseAgree: TCheckBox;
    cUSBErase: TComboBox;
    lConnState: TLabel;
    gOpt: TGroupBox;
    lNameOpt: TLabel;
    lList: TCheckListBox;
    bStart: TButton;
    gTrim: TGroupBox;
    lTrimName: TLabel;
    bTrimStart: TButton;
    gDownload: TGroupBox;
    lDownload: TLabel;
    lProgress: TLabel;
    pDownload: TProgressBar;
    bCancel: TButton;
    lSpeed: TLabel;
    cTrimList: TCheckListBox;
    gSchedule: TGroupBox;
    lSchName: TLabel;
    lDrives: TLabel;
    bReturn: TButton;
    cTrimRunning: TCheckBox;
    lSchExp: TLabel;
    bRtn: TButton;
    lName: TLabel;
    iLogo: TImage;
    SSDSelLbl: TLabel;
    bSchedule: TButton;
    gSSDSel: TGroupBox;
    tListLeave: TTimer;
    iBRange: TImage;
    iBG: TImage;
    iHelp: TImage;
    lHelp: TLabel;

    //생성자와 파괴자
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    //드라이브 리스트 관련 함수
    procedure CloseDriveList;

    //폼 관련 함수
    procedure FormClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tListLeaveTimer(Sender: TObject);

    //클릭 이벤트
    procedure bStartClick(Sender: TObject);
    procedure SSDLabelClick(Sender: TObject);
    procedure iOptimizeClick(Sender: TObject);
    procedure iFirmUpClick(Sender: TObject);
    procedure bFirmStartClick(Sender: TObject);
    procedure bEraseUSBStartClick(Sender: TObject);
    procedure iEraseClick(Sender: TObject);
    procedure iSCheckClick(Sender: TObject);
    procedure iAnalyticsClick(Sender: TObject);
    procedure iTrimClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bTrimStartClick(Sender: TObject);
    procedure bReturnClick(Sender: TObject);
    procedure lSerialClick(Sender: TObject);
    procedure bRtnClick(Sender: TObject);
    procedure bScheduleClick(Sender: TObject);
    procedure gInfoClick(Sender: TObject);
    procedure cTrimRunningClick(Sender: TObject);

    //엔터/나가기
    procedure SSDSelLblMouseEnter(Sender: TObject);
    procedure SSDSelLblMouseLeave(Sender: TObject);

    //다운로드 진행
    function DownloadFile(Src: TDownloadFile; Dest: TDownloadFile;
      DownloadCaption, CancelCaption: String): Boolean;
    procedure DownloaderWork(Sender: TObject; AWorkMode: TWorkMode;
      AWorkCount: Int64);
    procedure ProgressDownload;

    //펌웨어/Rufus 다운로드
    function DownloadRufus: Boolean;

    procedure InitUIToRefresh;
    procedure tRefreshTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure RefreshByPhysicalDrive;
    procedure RefreshDrives;
    function TryToCreatePhysicalDriveWithEntry(DeviceNumber: Integer): Boolean;
    procedure CreateNewPhysicalDrive;
    procedure IfThisDriveNotValidSelectOther;
    procedure FindAndSelectValidDrive;
    procedure SetSelectedDriveLabelBold;
  protected
    //쓰레드 관련
    TrimThread: TTrimThread;
    UpdateThread: TUpdateThread;

    //다운로드 관련
    LastDwldCount: Int64;
    CurrDwldCount: Int64;
    Max: Int64;
    Aborted: Boolean;

    //표시 정보 관련
    ShowSerial: Boolean;
    ListEnter: Integer;

    //최적화 관련
    Optimizer: TNSTOptimizer;


    procedure WmAfterShow(var Msg: TMessage); message WM_AFTER_SHOW;
    procedure WMDeviceChange(var Msg: TMessage); message WM_DEVICECHANGE;
  public
    CurrDrive: String;
    SSDLabel: TSSDLabelList;
    PhysicalDriveList: TPhysicalDriveList;
    PhysicalDrive: TPhysicalDrive;
    FirmwareGetter: TFirmwareGetter;
    WICImage: TWICImage;
    ButtonGroup: TButtonGroup;

    //현재 드라이브 관련
    FirstiOptLeft: Integer;

    procedure ShowProgress;
    procedure HideProgress;
  end;

var
  fMain: TfMain;

const
  MinimumSize = 290;
  MaximumSize = 535;

implementation

{$R *.dfm}

procedure TfMain.bCancelClick(Sender: TObject);
begin
  Aborted := true;
end;

procedure TfMain.bEraseUSBStartClick(Sender: TObject);
var
  FileName: String;
  TempFolder: String;
begin
  if cEraseAgree.Checked = false then
  begin
    AlertCreate(Self, AlrtNoCheck[CurrLang]);
    exit;
  end;

  tRefresh.Enabled := false;

  FileName := TPathManager.AppPath + 'Erase\pmagic.7z';

  if (FileExists(FileName)) and (TRufus.CheckRufus) then
  begin
    AlertCreate(Self, AlrtStartFormat[CurrLang]);

    TempFolder := TPathManager.TempFolder;
    CreateDir(TempFolder);

    TSevenZip.Extract(
      TPathManager.AppPath + '7z\7z.exe',
      FileName,
      TempFolder,
      CapTrimName[LANG_ENGLISH] +
      CapStartManTrim[LANG_ENGLISH] +
      BtSemiAutoTrim[LANG_ENGLISH] +
      CapLocalDisk[LANG_ENGLISH] +
      CapRemvDisk[LANG_ENGLISH] +
      CapProg1[LANG_ENGLISH] +
      CapProg2[LANG_ENGLISH] +
      CapProg2[LANG_ENGLISH]
    );

    FileName := TempFolder + 'pmagic.iso';

    TRufus.RunRufus(Copy(cUSBErase.Items[cUSBErase.ItemIndex], 1, 3), FileName);
    AlertCreate(Self, AlrtEraEnd[CurrLang]);
    DeleteDirectory(TempFolder);
  end
  else
  begin
    AlertCreate(Self, AlrtBootFail[CurrLang]);
    BrowserCreate(Self);
  end;

  tRefresh.Enabled := true;
end;

procedure TfMain.bFirmStartClick(Sender: TObject);
var
  DownloadedFirmware: TDownloadedFirmware;
  ifConnected: DWORD;
begin
  InternetGetConnectedState(@ifConnected, 0);
  if (ifConnected = INTERNET_CONNECTION_OFFLINE) or
      (ifConnected = 0) then
  begin
    AlertCreate(Self, AlrtNoInternet[CurrLang]);
    exit;
  end
  else if (cAgree.Checked = false) and (Sender <> Self) then
  begin
    AlertCreate(Self, AlrtNoCheck[CurrLang]);
    exit;
  end;

  tRefresh.Enabled := false;

  DownloadedFirmware := DownloadLatestFirmware(PhysicalDrive, FirmwareGetter);

  if (DownloadedFirmware.IsFirmwareExists = false) or
     ((TRufus.CheckRufus = false) and (DownloadRufus = false)) then
  begin
    tRefresh.Enabled := true;
    exit;
  end;

  if (ExtractFileExt(DownloadedFirmware.FirmwarePath) = '.exe') then
  begin
    ShellExecute(0, 'open', PChar(DownloadedFirmware.FirmwarePath), nil, nil,
      SW_SHOW);
    iFirmUp.OnClick(nil);
  end
  else
  begin
    AlertCreate(Self, AlrtStartFormat[CurrLang]);
    TRufus.RunRufus(Copy(cUSB.Items[cUSB.ItemIndex], 1, 3),
      DownloadedFirmware.FirmwarePath);
    AlertCreate(Self, AlrtFirmEnd[CurrLang]);
    DeleteDirectory(ExtractFilePath(DownloadedFirmware.FirmwarePath));
    DeleteDirectory(DownloadedFirmware.UsedTempFolder);
  end;

  tRefresh.Enabled := true;
end;

procedure TfMain.bStartClick(Sender: TObject);
var
  CurrItem: Integer;
  OptList: TOptList;
begin
  OptList := TOptList.Create;
  for CurrItem := 0 to (lList.Items.Count - 1) do
    OptList.Add(lList.Checked[CurrItem]);
  Optimizer.Optimize(OptList);
  FreeAndNil(OptList);

  RefreshOptimizeList;
  AlertCreate(Self, AlrtOptCmpl[CurrLang]);
end;

procedure TfMain.bRtnClick(Sender: TObject);
begin
  Optimizer.OptimizeReturn;

  RefreshOptimizeList;
  AlertCreate(Self, AlrtOptRetCmpl[CurrLang]);
end;

procedure TfMain.bReturnClick(Sender: TObject);
begin
  gTrim.Visible := true;
  gSchedule.Visible := false;
end;

procedure TfMain.bTrimStartClick(Sender: TObject);
var
  CurrPartition: Integer;
  PartCount: Integer;
  PartToTrim: TTrimList;
begin
  PartCount := 0;
  for CurrPartition := 0 to cTrimList.Items.Count - 1 do
    if cTrimList.Checked[CurrPartition] then
      PartCount := PartCount + 1;

  lDownload.Caption := CapTrimName[CurrLang];
  lProgress.Caption := CapProg1[CurrLang] + '0 / ' + IntToStr(PartCount);
  bCancel.Visible := false;
  lSpeed.Visible := true;

  lSpeed.Font.Color := clRed;
  lSpeed.Font.Style := [fsBold];
  lSpeed.Caption := CapProg2[CurrLang];

  gTrim.Visible := false;
  ShowProgress;
  Application.ProcessMessages;

  CurrDrive := '';
  PartToTrim := TTrimList.Create;
  for CurrPartition := 0 to cTrimList.Items.Count - 1 do
  begin
    if cTrimList.Checked[CurrPartition] then
    begin
      PartToTrim.Add(
        Copy(cTrimList.Items[CurrPartition], 1, 2));
    end;
  end;

  TrimThread := TTrimThread.Create(true, true);
  TrimThread.SetPartitionList(PartToTrim);
  TrimThread.Priority := tpLower;
  TrimThread.Start;
end;

procedure TfMain.CloseDriveList;
begin
  if GSSDSel.Visible = true then
  begin
    GSSDSel.Visible := false;
    SSDSelLbl.Caption := CapSSDSelOpn[CurrLang];
  end;
end;

procedure TfMain.FormClick(Sender: TObject);
begin
  CloseDriveList;
end;

procedure TfMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if TTrimThread.TrimStage = TTrimStage.InProgress then
  begin
    Action := caNone;
    exit;
  end;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  Optimizer := TNSTOptimizer.Create;
  SSDLabel := TSSDLabelList.Create;
  PhysicalDriveList := TPhysicalDriveList.Create;
  FirmwareGetter := TFirmwareGetter.Create;

  CurrDrive := '';
  ShowSerial := false;
  ListEnter := 0;
  FirstiOptLeft := iOptimize.Left;

  if Copy(ParamStr(1), Length(ParamStr(1)) - 3, 4) = '.err' then
    exit;

  InitializeMainForm;
  ApplyLocaleToMainformAndArrangeButton;
  RefreshDrives;

  ReportMemoryLeaksOnShutdown := DebugHook > 0;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FirmwareGetter);

  if PhysicalDrive <> nil then
    FreeAndNil(PhysicalDrive);

  FreeAndNil(PhysicalDriveList);
  FreeAndNil(SSDLabel);
  FreeAndNil(PhysicalDrive);
  FreeAndNil(Optimizer);
  FreeAndNil(ButtonGroup);
  FreeAndNil(WICImage);
end;

procedure TfMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then
    iHelp.OnClick(nil);
end;

procedure TfMain.RefreshDrives;
var
  SSDLabelListRefresher: TSSDLabelListRefresher;
begin
  SSDLabelListRefresher := TSSDLabelListRefresher.Create;
  SSDLabelListRefresher.RefreshDrives;
  FreeAndNil(SSDLabelListRefresher);

  if PhysicalDriveList.Count = 0 then
    exit;
  RefreshByPhysicalDrive;
  SetSelectedDriveLabelBold;
end;

procedure TfMain.FormShow(Sender: TObject);
begin
  PostMessage(Self.Handle, WM_AFTER_SHOW, 0, 0);
end;

procedure TfMain.gInfoClick(Sender: TObject);
begin
  CloseDriveList;
end;

procedure TfMain.DownloaderWork(Sender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
var
  LeftSec: Int64;
begin
  if Aborted then
    TIdHttp(Sender).Disconnect;

  if (AWorkMode <> wmRead) then
    exit;

  pDownload.Position := Round((AWorkCount / Max) * 100);
  CurrDwldCount := AWorkCount;
  lProgress.Caption := CapProg1[CurrLang] +
    Format('%.1fMB', [AWorkCount / 1024 / 1024]) + ' / ' +
    Format('%.1fMB', [Max / 1024 / 1024]) +
    ' (' + IntToStr(Round((AWorkCount / Max) * 100)) + '%)';

  lSpeed.Caption :=
    CapSpeed[CurrLang] +
    Format('%.1f', [(CurrDwldCount - LastDwldCount) * 2 / 1024]) + 'KB/s';

  LeftSec := 0;
  if (CurrDwldCount - LastDwldCount) > 0 then
    LeftSec :=
      round((Max - CurrDwldCount) /
        ((CurrDwldCount - LastDwldCount) * 2));

  lSpeed.Caption := CapTime[CurrLang] + FormatTimeInSecond(LeftSec);
  LastDwldCount := CurrDwldCount;

  Application.ProcessMessages;
end;

procedure TfMain.iAnalyticsClick(Sender: TObject);
begin
  CloseDriveList;
  ButtonGroup.Click(iAnalytics);
end;

procedure TfMain.iFirmUpClick(Sender: TObject);
var
  ifConnected: DWORD;
  Query: TFirmwareQuery;
  QueryResult: TFirmwareQueryResult;
begin
  CloseDriveList;

  if PhysicalDrive.SupportStatus.FirmwareUpdate = false then
  begin
    AlertCreate(Self, AlrtNoFirmSupport[CurrLang]);
    exit;
  end;

  if ButtonGroup.FindEntry(iFirmUp).Selected then
  begin
    ButtonGroup.Click(iFirmUp);
    exit;
  end;

  InternetGetConnectedState(@ifConnected, 0);
  if (ifConnected = INTERNET_CONNECTION_OFFLINE) or
      (ifConnected = 0) then
  begin
    AlertCreate(Self, AlrtNoInternet[CurrLang]);
    exit;
  end;

  GetUSBDrives(cUSB.Items);
  cUSB.ItemIndex := 0;
  if cUSB.Items.Count = 0 then
  begin
    AlertCreate(Self, AlrtNoUSB[CurrLang]);
    exit;
  end;

  lNewFirm.Font.Color := clWindowText;
  lNewFirm.Font.Style := [];
  ButtonGroup.Click(iFirmUp);

  Query.Model := PhysicalDrive.IdentifyDeviceResult.Model;
  Query.Firmware := PhysicalDrive.IdentifyDeviceResult.Firmware;
  QueryResult := FirmwareGetter.CheckFirmware(Query);
  if QueryResult.LatestVersion = '' then
    exit;

  if QueryResult.CurrentVersion = TFirmwareVersion.NewVersion then
  begin
    if lNewFirm.Font.Color <> clRed then
    begin
      lNewFirm.Font.Color := clRed;
      lNewFirm.Font.Style := [fsBold];
      if Pos(CapCurrFirm[CurrLang], lNewFirm.Caption) = 0 then
        lNewFirm.Caption := lNewFirm.Caption + ' ' + CapCurrFirm[CurrLang];
    end;
    AlertCreate(Self, AlrtNoUpdate[CurrLang]);
  end;
end;

procedure TfMain.iEraseClick(Sender: TObject);
begin
  CloseDriveList;

  if ButtonGroup.FindEntry(iErase).Selected then
  begin
    ButtonGroup.Click(iErase);
    exit;
  end;

  GetUSBDrives(cUSBErase.Items);
  cUSBErase.ItemIndex := 0;

  if cUSBErase.Items.Count > 0 then
  begin
    ButtonGroup.Click(iErase);
    exit;
  end;

  AlertCreate(Self, AlrtNoUSB[CurrLang]);
end;

procedure TfMain.InitUIToRefresh;
begin
  if (not gFirmware.Visible) and
     (not gDownload.Visible) then
    lFirmware.Font.Style := [];
  lSectors.Font.Color := clWindowText;
  lPError.Font.Color := clWindowText;
  lNotsafe.Font.Color := clWindowText;

  lPartitionAlign.Font.Color := clWindowText;
  lNotsafe.Caption := CapStatus[CurrLang] + CapSafe[CurrLang];
end;

procedure TfMain.iSCheckClick(Sender: TObject);
begin
  CloseDriveList;

  if ButtonGroup.FindEntry(iFirmUp).Selected then
    ShellExecute(0, 'open', 'http://naraeon.tistory.com/131', '',
      nil, SW_NORMAL)
  else if ButtonGroup.FindEntry(iErase).Selected then
    ShellExecute(0, 'open', 'http://naraeon.tistory.com/144', '',
      nil, SW_NORMAL)
  else if ButtonGroup.FindEntry(iTrim).Selected then
  begin
    if gTrim.Visible then
      ShellExecute(0, 'open', 'http://naraeon.tistory.com/142', '',
        nil, SW_NORMAL)
    else
      ShellExecute(0, 'open', 'http://naraeon.tistory.com/143', '',
        nil, SW_NORMAL);
  end
  else
    ShellExecute(0, 'open', 'http://naraeon.tistory.com/132', '',
      nil, SW_NORMAL);
end;

procedure TfMain.iOptimizeClick(Sender: TObject);
begin
  CloseDriveList;
  ButtonGroup.Click(iOptimize);
end;


procedure TfMain.iTrimClick(Sender: TObject);
var
  CheckedDrives: Integer;
begin
  CloseDriveList;

  if ButtonGroup.Click(iTrim) <> clkOpen then
    exit;

  GetChildDrives(PhysicalDrive.GetPathOfFileAccessingWithoutPrefix,
    cTrimList.Items);
  for CheckedDrives := 0 to cTrimList.Count - 1 do
    cTrimList.Checked[CheckedDrives] := true;
end;

procedure TfMain.lSerialClick(Sender: TObject);
var
  CurrNum: Integer;
begin
  if ShowSerial then
  begin
    ShowSerial := false;
    lSerial.Caption := CapSerial[CurrLang];
    for CurrNum := 0 to
      Length(PhysicalDrive.IdentifyDeviceResult.Serial) - 1 do
        lSerial.Caption := lSerial.Caption + 'X';
  end
  else
  begin
    ShowSerial := true;
    lSerial.Caption := CapSerial[CurrLang] +
      PhysicalDrive.IdentifyDeviceResult.Serial;
  end;
end;

procedure TfMain.SSDLabelClick(Sender: TObject);
begin
  CloseDriveList;

  if CurrDrive =
    TSSDLabel(Sender).PhysicalDrive.GetPathOfFileAccessingWithoutPrefix then
  begin
    gSSDSel.Visible := false;
    exit;
  end;

  lFirmware.Font.Color := clWindowText;
  ButtonGroup.CloseAll;

  CurrDrive :=
    TSSDLabel(Sender).PhysicalDrive.GetPathOfFileAccessingWithoutPrefix;
  tRefreshTimer(Self);

  gSSDSel.Visible := false;
end;

procedure TfMain.SSDSelLblMouseEnter(Sender: TObject);
begin
  if gSSDSel.Visible = false then
  begin
    gSSDSel.Visible := true;
    gSSDSel.BringToFront;
    SSDSelLbl.Caption := CapSSDSelCls[CurrLang];
  end;

  if ListEnter = 0 then
  begin
    ListEnter := ListEnter + 1;
    tListLeave.Enabled := true;
  end;
end;

procedure TfMain.SSDSelLblMouseLeave(Sender: TObject);
begin
  if ListEnter > 0 then
    ListEnter := ListEnter - 1;
end;

procedure TfMain.tListLeaveTimer(Sender: TObject);
begin
  if ListEnter <> 0 then
    exit;

  tListLeave.Enabled := false;
  CloseDriveList;
end;

function TfMain.TryToCreatePhysicalDriveWithEntry(DeviceNumber: Integer):
  Boolean;
begin
  try
    result := true;
    fMain.PhysicalDrive := TPhysicalDrive.Create(DeviceNumber);
  except
    result := false;
    FreeAndNil(fMain.PhysicalDrive);
  end;
end;

procedure TfMain.FindAndSelectValidDrive;
var
  CurrentEntry: TSSDLabel;
begin
  for CurrentEntry in SSDLabel do
  begin
    if TryToCreatePhysicalDriveWithEntry(
      StrToInt(
        CurrentEntry.PhysicalDrive.GetPathOfFileAccessingWithoutPrefix)) then
    begin
      CurrentEntry.OnClick(CurrentEntry);
      exit;
    end;
  end;
end;

procedure TfMain.IfThisDriveNotValidSelectOther;
begin
  if TryToCreatePhysicalDriveWithEntry(StrToInt(fMain.CurrDrive)) = false then
    FindAndSelectValidDrive;
end;

procedure TfMain.CreateNewPhysicalDrive;
begin
  FreeAndNil(fMain.PhysicalDrive);
  IfThisDriveNotValidSelectOther;
end;

procedure TfMain.tRefreshTimer(Sender: TObject);
const
  ORIGINAL_INTERVAL = 60000;
begin
  if tRefresh.Interval < ORIGINAL_INTERVAL then
    tRefresh.Interval := ORIGINAL_INTERVAL;

  if Length(fMain.CurrDrive) = 0 then
    exit;

  CreateNewPhysicalDrive;
  RefreshDrives;
end;

procedure TfMain.SetSelectedDriveLabelBold;
var
  CurrIndex: Integer;
begin
  for CurrIndex := 0 to SSDLabel.Count - 1 do
  begin
    if SSDLabel[CurrIndex].PhysicalDrive.GetPathOfFileAccessing =
       PhysicalDrive.GetPathOfFileAccessing then
    begin
      if SSDLabel[CurrIndex].Font.Style <> [fsBold] then
        SSDLabel[CurrIndex].Font.Style := [fsBold];
    end
    else
      SSDLabel[CurrIndex].Font.Style := [];
  end;
end;

procedure TfMain.RefreshByPhysicalDrive;
var
  MainformPhysicalDriveApplier: TMainformPhysicalDriveApplier;
begin
  MainformPhysicalDriveApplier := TMainformPhysicalDriveApplier.Create;
  MainformPhysicalDriveApplier.ApplyMainformPhysicalDrive(ShowSerial);
  FreeAndNil(MainformPhysicalDriveApplier);
end;

procedure TfMain.WmAfterShow(var Msg: TMessage);
const
  INTERNET_CONNECTION_LAN = 2;
var
  ifConnected: DWORD;
  DesktopPath: array[0..MAX_PATH] of char;
  DeskPath: String;
  ErrList: TStringList;
begin
  if lName.Caption = '' then
  begin
    Close;
    exit;
  end;

  Top := Top - (MinimumSize div 2);

  InternetGetConnectedState(@ifConnected, 0);
  if (ifConnected and INTERNET_CONNECTION_LAN) = INTERNET_CONNECTION_LAN then
  begin
    UpdateThread := TUpdateThread.Create(True);
    UpdateThread.Priority := tpLower;
    UpdateThread.Start;
  end;

  tRefresh.Enabled := false;
  RefreshDrives;
  tRefresh.Enabled := true;

  SHGetFolderPath(0, CSIDL_COMMON_DESKTOPDIRECTORY, 0, 0, @DesktopPath[0]);
  DeskPath := DesktopPath;

  if FileExists(DeskPath + '\!!!SSDError!!!.err') then
    MsgboxCreate(Self, DeskPath + '\!!!SSDError!!!.err');

  ErrList := WriteBufferCheck;
  if ErrList.Count > 0 then
    AlertCreate(Self, ErrCache[CurrLang] + Chr(13) + Chr(10) +  ErrList.Text);
  FreeAndNil(ErrList);
end;

procedure TfMain.WMDeviceChange(var Msg: TMessage);
const
  DBT_DEVICEARRIVAL = $8000;
  DBT_DEVICEREMOVECOMPLETE = $8004;
  DBT_DEVNODES_CHANGED = $0007;
  DBT_STORAGE = $0002;
begin
  if (Msg.WParam = DBT_DEVNODES_CHANGED) or
     (((Msg.WParam = DBT_DEVICEARRIVAL) or
       (Msg.WParam = DBT_DEVICEREMOVECOMPLETE)) and
       (PDevBroadcastHdr(Msg.lParam)^.dbcd_devicetype = DBT_STORAGE)) then
  begin
    tRefresh.Interval := 1;
  end;
end;

procedure TfMain.ShowProgress;
var
  CurrImgLbl: Integer;
begin
  for CurrImgLbl := 0 to SSDLabel.Count - 1 do
    SSDLabel[CurrImgLbl].Enabled := false;

  iFirmUp.Enabled := false;
  lFirmUp.Enabled := false;
  iErase.Enabled := false;
  lErase.Enabled := false;
  iOptimize.Enabled := false;
  lOptimize.Enabled := false;
  iHelp.Enabled := false;
  lHelp.Enabled := false;
  iAnalytics.Enabled := false;
  lAnalytics.Enabled := false;
  iTrim.Enabled := false;
  lTrim.Enabled := false;
  gDownload.Visible := true;

  ButtonGroup.Open;
end;

procedure TfMain.HideProgress;
var
  CurrImgLbl: Integer;
begin
  for CurrImgLbl := 0 to SSDLabel.Count - 1 do
    SSDLabel[CurrImgLbl].Enabled := true;

  iFirmUp.Enabled := true;
  lFirmUp.Enabled := true;
  iErase.Enabled := true;
  lErase.Enabled := true;
  iOptimize.Enabled := true;
  lOptimize.Enabled := true;
  iHelp.Enabled := true;
  lHelp.Enabled := true;
  iAnalytics.Enabled := true;
  lAnalytics.Enabled := true;
  iTrim.Enabled := true;
  lTrim.Enabled := true;
  gDownload.Visible := false;
end;

procedure TfMain.cTrimRunningClick(Sender: TObject);
var
  SchedResult: String;
begin
  if cTrimRunning.Checked then
    if Win32MajorVersion = 5 then
    begin
      SchedResult :=
        string(OpenProcWithOutput(
          TPathManager.WinDir + '\System32',
          'schtasks /create ' +                     //작업 생성
          '/sc onidle ' +                           //유휴시간 작업
          '/i 1' +                                  //아이들 시간
          '/tn "MANTRIM' +                          //이름
          PhysicalDrive.IdentifyDeviceResult.Serial +
          '" ' +
          '/tr "\" ' + Application.ExeName + '\" ' +//경로
            PhysicalDrive.IdentifyDeviceResult.Serial + '" ' +
          '/ru system'));                           //작업할 계정
    end
    else
      SchedResult :=
        string(OpenProcWithOutput(
          TPathManager.WinDir + '\System32',
          'schtasks /create ' +                     //작업 생성
          '/sc onidle ' +                           //유휴시간 작업
          '/i 1 ' +                                 //아이들 시간
          '/tn "MANTRIM' +                          //이름
          PhysicalDrive.IdentifyDeviceResult.Serial +
          '" ' +
          '/tr "''' + Application.ExeName +         //경로
            ''' ''' +
          PhysicalDrive.IdentifyDeviceResult.Serial + '''" ' +
          '/rl HIGHEST'))                           //권한 (Limited/Highest)
  else
    SchedResult :=
      string(OpenProcWithOutput(
        TPathManager.WinDir + '\System32',
        'schtasks /delete ' +                       //작업 삭제
        '/TN "MANTRIM' +                            //작업 이름
        PhysicalDrive.IdentifyDeviceResult.Serial +
        '" ' +
        '/F'));                                     //강제 삭제
end;

function TfMain.DownloadRufus: Boolean;
var
  Src, Dest: TDownloadFile;
  DownloadResult: Boolean;
begin
  result := false;

  AlertCreate(Self, AlrtBootInStart[CurrLang]);

  Src.FBaseAddress := '';
  Src.FFileAddress :=
    'http://nstfirmware.naraeon.net/nst_rufus.htm';
  Src.FType := dftGetFromWeb;

  Dest.FBaseAddress := TPathManager.AppPath;
  Dest.FFileAddress := 'Rufus\rufus.exe_tmp';
  Dest.FType := dftPlain;

  gFirmware.Visible := false;
  DownloadResult :=
    DownloadFile(Src, Dest, CapBootInDwld[CurrLang], bCancel.Caption);
  gFirmware.Visible := true;

  if DownloadResult = false then
  begin
    AlertCreate(Self, AlrtFirmCanc[CurrLang]);
    exit;
  end;
  RenameFile(TPathManager.AppPath + 'Rufus\rufus.exe_tmp',
             TPathManager.AppPath + 'Rufus\rufus.exe');

  result := TRufus.CheckRufus;
end;

procedure TfMain.ProgressDownload;
const
  CancelButton = 1;
var
  Src, Dest: TDownloadFile;
  CodesignVerifier: TCodesignVerifier;
begin
  if Application.MessageBox(
    PChar(UpdateThread.UpdateNotice),
    PChar(AlrtNewVer[CurrLang]),
    MB_OKCANCEL  + MB_IconInformation) <> CancelButton then
      exit;

  Src.FBaseAddress := 'http://nstupdate.naraeon.net';
  Src.FFileAddress := '/Setup.exe';
  Src.FType := dftPlain;

  Dest.FBaseAddress := TPathManager.AppPath;
  Dest.FFileAddress := 'Setup.exe';
  Dest.FType := dftPlain;

  if DownloadFile(Src, Dest, CapUpdDwld[CurrLang], bCancel.Caption) = false then
  begin
    AlertCreate(Self, AlrtVerCanc[CurrLang]);
    exit;
  end;

  ButtonGroup.Close;

  CodesignVerifier := TCodesignVerifier.Create;
  if not CodesignVerifier.IsCodeSigned(TPathManager.AppPath + 'Setup.exe') then
  begin
    AlertCreate(Self, AlrtWrongCodesign[CurrLang]);
    DeleteFile(TPathManager.AppPath + 'Setup.exe');
  end
  else
  begin
    AlertCreate(Self, AlrtUpdateExit[CurrLang]);
    ShellExecute(0, nil,
      PChar(TPathManager.AppPath + 'Setup.exe'), nil, nil, SW_NORMAL);
    Application.Terminate;
  end;
  FreeAndNil(CodesignVerifier);
end;

procedure TfMain.bScheduleClick(Sender: TObject);
var
  DriveList: TPartitionList;
  CurrDrv: Integer;
begin
  gTrim.Visible := false;
  gSchedule.Visible := true;

  lDrives.Caption := CapAppDisk[CurrLang];

  DriveList := PhysicalDrive.GetPartitionList;
  for CurrDrv := 0 to DriveList.Count - 1 do
    lDrives.Caption := lDrives.Caption + DriveList[CurrDrv].Letter + ' ';
  FreeAndNil(DriveList);

  cTrimRunning.Checked :=
    Pos('MANTRIM' + PhysicalDrive.IdentifyDeviceResult.Serial,
      UnicodeString(OpenProcWithOutput(
        TPathManager.WinDir + '\System32',
        'schtasks /query'))) > 0;
end;

function TfMain.DownloadFile(Src: TDownloadFile; Dest: TDownloadFile;
  DownloadCaption, CancelCaption: String): Boolean;
var
  SrcAddress, DestAddress: String;
  Downloader: TIdHttp;
  DownloadStream: TFileStream;
begin
  try
    SrcAddress := GetDownloadPath(Src);
    DestAddress := GetDownloadPath(Dest);
  except
    exit(false);
  end;

  lDownload.Caption := DownloadCaption;
  bCancel.Caption := CancelCaption;

  DownloadStream := TFileStream.Create(DestAddress,
    fmCreate or fmShareExclusive);
  Downloader := TIdHttp.Create;
  Downloader.Request.UserAgent := 'Naraeon SSD Tools';
  Downloader.HandleRedirects := true;

  try
    Aborted := false;
    Max := 0;
    Downloader.OnWork := DownloaderWork;
    Downloader.Head(SrcAddress);
    Max := Downloader.response.ContentLength;

    ShowProgress;
    Downloader.Get(SrcAddress, DownloadStream);
    Application.ProcessMessages;
    HideProgress;
  finally
    FreeAndNil(DownloadStream);
    FreeAndNil(Downloader);
  end;

  result := not Aborted;
end;
end.
