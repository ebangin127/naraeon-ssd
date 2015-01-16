unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.CheckLst, WinInet, URLMon, idHttp,
  Vcl.OleCtrls, uRegFunctions, uDiskFunctions, Vcl.ExtCtrls, ShellApi, Math,
  Vcl.Imaging.pngimage, ShlObj, IdComponent, MMSystem, Vcl.Mask, Vcl.ComCtrls,
  uAlert, uMessage, uSSDSupport, uLogSystem, uSSDInfo, uStrFunctions,
  uTrimThread, uLanguageSettings, uUpdateThread, uBrowser, uSevenZip,
  uSMARTFunctions, uPartitionFunctions, uOptimizer, uExeFunctions, uUSBDrive,
  uFileFunctions, uImager, uDownloadPath, uPlugAndPlay, uFirmware, uRefresh,
  uButtonGroup, uInit, uGetFirm;

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
    tUpdMon: TTimer;

    //생성자와 파괴자
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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

    //펌웨어/Unetbootin 다운로드
    function DownloadUnetbootin: Boolean;

    procedure InitUIToRefresh;
    procedure tRefreshTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tUpdMonTimer(Sender: TObject);
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
    SSDInfo: TSSDInfo_NST;
    ShowSerial: Boolean;
    FirstiOptLeft: Integer;
    ListEnter: Integer;

    //창 관리자
    ButtonGroup: TButtonGroup;

    //최적화 관련
    FirstOpt: String;
    Optimizer: TNSTOptimizer;

    procedure WmAfterShow(var Msg: TMessage); message WM_AFTER_SHOW;
    procedure WMDeviceChange(var Msg: TMessage); message WM_DEVICECHANGE;
  public
    CurrDrive: String;
    SSDLabel: Array of TSSDLabel;

    //현재 드라이브 관련
    CurrUSBMode: Boolean;
    CurrATAorSCSIStatus: TStorInterface;

    procedure ShowProgress;
    procedure HideProgress;
  end;

var
  fMain: TfMain;

  //프로그램 관련 Path들
  AppPath: String;
  WinDir, WinDrive: String;

  //전체 프로그램 공유 내용
  PartCount, CompletedPartition: Integer;
  NeedTrimPartition: Array of String;

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

  FileName := AppPath + 'Erase\pmagic.7z';

  if (FileExists(FileName)) and (CheckUnetBootin) then
  begin
    AlertCreate(Self, AlrtStartFormat[CurrLang]);

    TempFolder :=
      GetEnvironmentVariable('TMP') +
      '\NST' + IntToStr(Random(2147483647)) + '\';
    while DirectoryExists(TempFolder) do
      TempFolder :=
        GetEnvironmentVariable('TMP') +
        '\NST' + IntToStr(Random(2147483647)) + '\';
    CreateDir(TempFolder);

    TSevenZip.Extract(
      AppPath + '7z\7z.exe',
      FileName,
      TempFolder,
      CapTrimName[LANG_ENGLISH] +
      CapStartManTrim[LANG_ENGLISH] +
      BtSemiAutoTrim[LANG_ENGLISH] +
      CapLocalDisk[LANG_ENGLISH] +
      CapRemvDisk[LANG_ENGLISH] +
      CapProg1[LANG_ENGLISH] +
      CapProg2[LANG_ENGLISH] +
      CapProg3[LANG_ENGLISH]
    );

    FileName := TempFolder + 'pmagic.iso';

    ProcessImager(Copy(cUSBErase.Items[cUSBErase.ItemIndex], 1, 3), FileName);
    AlertCreate(Self, AlrtEraEnd[CurrLang]);
    DeleteDirectory(TempFolder);
  end
  else
  begin
    AlertCreate(Self, AlrtBootFail[CurrLang]);
    BrowserCreate(Self);
  end;
end;

procedure TfMain.bFirmStartClick(Sender: TObject);
var
  ChkFrmResult: FirmCheck;
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

  ChkFrmResult.FirmExists := false;
  ChkFrmResult := DownloadFirmware(AppPath, SSDInfo);

  if (ChkFrmResult.FirmExists = false) or
     ((CheckUNetbootin = false) and (DownloadUNetbootin = false)) then
      exit;

  if (ExtractFileExt(ChkFrmResult.FirmPath) = '.exe') then
  begin
    ShellExecute(0, 'open', PChar(ChkFrmResult.FirmPath), nil, nil,
      SW_SHOW);
    iFirmUp.OnClick(nil);
  end
  else
  begin
    AlertCreate(Self, AlrtStartFormat[CurrLang]);
    ProcessImager(Copy(cUSB.Items[cUSB.ItemIndex], 1, 3),
      ChkFrmResult.FirmPath);
    AlertCreate(Self, AlrtFirmEnd[CurrLang]);
    DeleteDirectory(ExtractFilePath(ChkFrmResult.FirmPath));
  end;
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

  RefreshOptList;
  AlertCreate(Self, AlrtOptCmpl[CurrLang]);
end;

procedure TfMain.bRtnClick(Sender: TObject);
begin
  Optimizer.OptimizeReturn;

  RefreshOptList;
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
  CurrDrive: Integer;
begin
  PartCount := 0;
  CompletedPartition := 0;

  for CurrPartition := 0 to cTrimList.Items.Count - 1 do
    if cTrimList.Checked[CurrPartition] then
      PartCount := PartCount + 1;

  lDownload.Caption := CapTrimName[CurrLang];
  lProgress.Caption := CapProg1[CurrLang] + '0 / ' + IntToStr(PartCount) + ' '
                        + CapProg2[CurrLang];
  bCancel.Visible := false;
  lSpeed.Visible := true;

  lSpeed.Font.Color := clRed;
  lSpeed.Font.Style := [fsBold];
  lSpeed.Caption := CapProg3[CurrLang];

  gTrim.Visible := false;
  ShowProgress;
  Application.ProcessMessages;

  pDownload.Height := pDownload.Height + 10;
  pDownload.Top := pDownload.Top + 5;

  SetLength(NeedTrimPartition, PartCount);
  CurrDrive := 0;
  for CurrPartition := 0 to cTrimList.Items.Count - 1 do
  begin
    if cTrimList.Checked[CurrPartition] then
    begin
      NeedTrimPartition[CurrDrive] :=
        Copy(cTrimList.Items[CurrPartition], 1, 2);
      CurrDrive := CurrDrive + 1;
    end;
  end;
  MainLoaded := true;

  if TrimThread <> Nil then FreeAndNil(TrimThread);
  TrimThread := TTrimThread.Create(true);
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
  if ((UpdateThread <> nil) and
      (not UpdFinished)) or
     ((TrimThread <> nil) and
      (TrimStat < 2)) then
  begin
    Action := caNone;
    exit;
  end;

  if (TrimThread <> nil) and (TrimStat >= 2) then
    FreeAndNil(TrimThread);
end;

procedure TfMain.FormCreate(Sender: TObject);
begin
  Optimizer := TNSTOptimizer.Create;
  SSDInfo := TSSDInfo_NST.Create;

  CurrDrive := '';
  ShowSerial := false;
  ListEnter := 0;
  FirstiOptLeft := iOptimize.Left;

  if Copy(ParamStr(1), Length(ParamStr(1)) - 3, 4) = '.err' then
    exit;

  InitMainForm;
  RefreshDrives(SSDInfo);

  ReportMemoryLeaksOnShutdown := DebugHook > 0;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  TGetFirm.DestroyCache;

  FreeAndNil(SSDInfo);
  FreeAndNil(Optimizer);
  FreeAndNil(ButtonGroup);
end;

procedure TfMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then
    iHelp.OnClick(nil);
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

  if LeftSec < 60 then
    lSpeed.Caption :=
      IntToStr(LeftSec mod 60) + CapSec[CurrLang];

  if LeftSec < 3600 then
    lSpeed.Caption :=
      IntToStr(floor(LeftSec / 60)) + CapMin[CurrLang] + ' ' +
      lSpeed.Caption;

  if LeftSec >= 3600 then
    lSpeed.Caption :=
      IntToStr(floor(LeftSec / 3600)) + CapHour[CurrLang] + ' ' +
      lSpeed.Caption;

  lSpeed.Caption := CapTime[CurrLang] + lSpeed.Caption;
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
begin
  CloseDriveList;

  if SSDInfo.SSDSupport.SupportFirmUp = false then
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

  if IsNewVersion(SSDInfo.Model, SSDInfo.Firmware) = NEW_VERSION then
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

  GetChildDrives(ExtractDeviceNum(SSDInfo.DeviceName), cTrimList.Items);
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
    for CurrNum := 0 to Length(SSDInfo.Serial) - 1 do
      lSerial.Caption := lSerial.Caption + 'X';
  end
  else
  begin
    ShowSerial := true;
    lSerial.Caption := CapSerial[CurrLang] + SSDInfo.Serial;
  end;
end;

procedure TfMain.SSDLabelClick(Sender: TObject);
var
  CurrIndex: Integer;
begin
  CloseDriveList;

  if CurrDrive = TSSDLabel(Sender).DriveName then
  begin
    gSSDSel.Visible := false;
    exit;
  end;

  lFirmware.Font.Color := clWindowText;
  ButtonGroup.CloseAll;

  CurrDrive := TSSDLabel(Sender).DriveName;
  CurrUSBMode := TSSDLabel(Sender).USBMode;
  CurrATAorSCSIStatus := TSSDLabel(Sender).ATAorSCSI;
  tRefreshTimer(Self);

  for CurrIndex := 0 to Length(SSDLabel) - 1 do
    if SSDLabel[CurrIndex].DriveName = TSSDLabel(Sender).DriveName then
      SSDLabel[CurrIndex].Font.Style := [fsBold]
    else
      SSDLabel[CurrIndex].Font.Style := [];

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

procedure TfMain.tRefreshTimer(Sender: TObject);
begin
  if RefreshTimer(SSDInfo, CurrUSBMode, CurrATAorSCSIStatus,
                  ShowSerial, firstiOptLeft) = false then
    Application.Terminate;
end;

procedure TfMain.tUpdMonTimer(Sender: TObject);
begin
  if (UpdateThread <> nil) and
     (UpdFinished) then
  begin
    tUpdMon.Enabled := false;
    FreeAndNil(UpdateThread);
  end;
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
    tUpdMon.Enabled := true;
  end;

  tRefresh.Enabled := false;
  RefreshDrives(SSDInfo);
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
    tRefresh.Enabled := false;
    RefreshDrives(SSDInfo);
    tRefresh.Enabled := true;
  end;
end;

procedure TfMain.ShowProgress;
var
  CurrImgLbl: Integer;
begin
  for CurrImgLbl := 0 to Length(SSDLabel) - 1 do
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
  for CurrImgLbl := 0 to Length(SSDLabel) - 1 do
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
          WinDir + '\System32',
          'schtasks /create ' +                     //작업 생성
          '/sc onidle ' +                           //유휴시간 작업
          '/i 1' +                                  //아이들 시간
          '/tn "MANTRIM' + SSDInfo.Serial + '" ' +  //이름
          '/tr "\" ' + Application.ExeName + '\" ' +//경로
            SSDInfo.Serial + '" ' +
          '/ru system'));                           //작업할 계정
    end
    else
      SchedResult :=
        string(OpenProcWithOutput(
          WinDir + '\System32',
          'schtasks /create ' +                     //작업 생성
          '/sc onidle ' +                           //유휴시간 작업
          '/i 1 ' +                                 //아이들 시간
          '/tn "MANTRIM' + SSDInfo.Serial + '" ' +  //이름
          '/tr "''' + Application.ExeName +         //경로
            ''' ''' + SSDInfo.Serial + '''" ' +
          '/rl HIGHEST'))                           //권한 (Limited/Highest)
  else
    SchedResult :=
      string(OpenProcWithOutput(
        WinDir + '\System32',
        'schtasks /delete ' +                       //작업 삭제
        '/TN "MANTRIM' + SSDInfo.Serial + '" ' +    //작업 이름
        '/F'));                                     //강제 삭제
end;

function TfMain.DownloadUnetbootin: Boolean;
var
  Src, Dest: TDownloadFile;
  DownloadResult: Boolean;
begin
  result := false;

  AlertCreate(Self, AlrtBootInStart[CurrLang]);

  Src.FBaseAddress := '';
  Src.FFileAddress :=
    'http://nstfirmware.naraeon.net/nst_unet.htm';
  Src.FType := dftGetFromWeb;

  Dest.FBaseAddress := AppPath;
  Dest.FFileAddress := 'Unetbootin\unetbootin.exe_tmp';
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
  RenameFile(AppPath + 'Unetbootin\unetbootin.exe_tmp',
             AppPath + 'Unetbootin\unetbootin.exe');

  result := CheckUnetbootin;
end;

procedure TfMain.ProgressDownload;
var
  Src, Dest: TDownloadFile;
begin
  if Application.MessageBox(PChar(CapCurrVer[CurrLang] + CurrentVersion
                           + Chr(13) + Chr(10) +
                           CapNewVer[CurrLang] + Copy(ServerVersion, 1, 5)
                           + Chr(13) + Chr(10) + Chr(13) + Chr(10) +
                           Copy(ChangeLog, 1, CurrChr)
                           + Chr(13) + Chr(10) + Chr(13) + Chr(10) +
                           CapUpdQues[CurrLang]), PChar(AlrtNewVer[CurrLang]),
                           MB_OKCANCEL  + MB_IconInformation) <> 1 then
    exit;

  Src.FBaseAddress := 'http://nstupdate.naraeon.net';
  Src.FFileAddress := '/Setup.exe';
  Src.FType := dftPlain;

  Dest.FBaseAddress := AppPath;
  Dest.FFileAddress := 'Setup.exe';
  Dest.FType := dftPlain;

  if DownloadFile(Src, Dest, CapUpdDwld[CurrLang], bCancel.Caption) = false then
  begin
    AlertCreate(Self, AlrtVerCanc[CurrLang]);
    exit;
  end;

  ButtonGroup.Close;
  AlertCreate(Self, AlrtUpdateExit[CurrLang]);
  ShellExecute(0, nil, PChar(AppPath + 'Setup.exe'), nil, nil, SW_NORMAL);
  Application.Terminate;
end;

procedure TfMain.bScheduleClick(Sender: TObject);
var
  Drives: TDriveLetters;
  CurrDrv: Integer;
begin
  gTrim.Visible := false;
  gSchedule.Visible := true;

  Drives := GetPartitionList(ExtractDeviceNum(SSDInfo.DeviceName));
  lDrives.Caption := CapAppDisk[CurrLang];

  for CurrDrv := 0 to Drives.LetterCount - 1 do
    lDrives.Caption := lDrives.Caption + Drives.Letters[CurrDrv] + ' ';

  cTrimRunning.Checked :=
    Pos('MANTRIM' + SSDInfo.Serial,
      UnicodeString(OpenProcWithOutput(
        WinDir + '\System32',
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
