unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.CheckLst, WinInet, URLMon, idHttp,
  Vcl.OleCtrls, uRegFunctions, uDiskFunctions, Vcl.ExtCtrls, ShellApi, Math,
  Vcl.Imaging.pngimage, ShlObj, IdComponent, MMSystem, Vcl.Mask, Vcl.ComCtrls,
  uAlert, uMessage, uSSDVersion, uLogSystem, uSSDInfo, uStrFunctions,
  uTrimThread, uLanguageSettings, uTrimCommand, uUpdateThread, uBrowser,
  uSMARTFunctions, uPartitionFunctions, uOptimizer, uExeFunctions, uUSBDrive,
  uFileFunctions, uImager, uDownloadPath, uPlugAndPlay, uFirmware, uRefresh;

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
    tErrorChk: TTimer;
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
    tGetSSDs: TTimer;
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
    rPartedMagic: TRadioButton;
    rGParted: TRadioButton;
    lName: TLabel;
    iLogo: TImage;
    SSDSelLbl: TLabel;
    bSchedule: TButton;
    GSSDSel: TGroupBox;
    tListLeave: TTimer;
    iBRange: TImage;
    iBG: TImage;
    iHelp: TImage;
    lHelp: TLabel;

    //생성자와 파괴자
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

    //폼 관련 함수
    procedure FormClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    //타이머 관련 함수
    procedure tFindMutexTimer(Sender: TObject);
    procedure tListLeaveTimer(Sender: TObject);
    procedure tGetSSDsTimer(Sender: TObject);
    procedure tDownloadCheckerTimer(Sender: TObject);
    procedure tErrorChkTimer(Sender: TObject);

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
    procedure rPartedMagicClick(Sender: TObject);
    procedure rGPartedClick(Sender: TObject);
    procedure gInfoClick(Sender: TObject);
    procedure cTrimRunningClick(Sender: TObject);

    //엔터/나가기
    procedure SSDSelLblMouseEnter(Sender: TObject);
    procedure SSDSelLblMouseLeave(Sender: TObject);

    //다운로드 진행
    function DownloadFile(Src: TDownloadFile; Dest: TDownloadFile; DownloadCaption, CancelCaption: String): Boolean;
    procedure DownloaderWork(Sender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    procedure ProgressDownload;

    //펌웨어/Unetbootin 다운로드
    function DownloadUnetbootin: Boolean;

    procedure InitUIToRefresh;
    procedure tRefreshTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
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
    FirmForce: Boolean;
    ShowSerial: Boolean;
    firstiOptLeft: Integer;
    ListEnter: Integer;

    //최적화 관련
    FirstOpt: String;
    Optimizer: TNSTOptimizer;

    procedure FontAndSATrimSet;
    procedure SetLanguage;
    procedure LoadBGImage;
    procedure RefreshOptList;

    procedure WmAfterShow(var Msg: TMessage); message WM_AFTER_SHOW;
    procedure WMDeviceChange(var Msg: TMessage); message WM_DEVICECHANGE;
  public
    CurrDrive: String;
    SSDLabel: Array of TLabel;

    //현재 드라이브 관련
    CurrUSBMode: Boolean;
    CurrATAorSCSIStatus: Byte;

    procedure ShowDownloader;
    procedure HideDownloader;
  end;

type
  THackControl = class(TControl);

var
  fMain: TfMain;

  //프로그램 관련 Path들
  AppPath: String;
  WinDir, WinDrive: String;

  //전체 프로그램 공유 내용
  PartCount, CompletedPartition: Integer;
  NeedTrimPartition: Array of String;
  NeedTrimLBASize: Array of Integer;
  USBYN: Boolean;

  //현재 프로세스 뮤텍스 관리
  MutexAppear: LongInt;

const
  MinimumSize = 290;
  MaximumSize = 535;

implementation

{$R *.dfm}

procedure SetFontName(Control: TControl; const FontName: String);
begin
  THackControl(Control).Font.Name := FontName;
end;

procedure TfMain.bCancelClick(Sender: TObject);
begin
  Aborted := true;
end;

procedure TfMain.bEraseUSBStartClick(Sender: TObject);
var
  FileName: String;
begin
  if cEraseAgree.Checked = false then
    AlertCreate(Self, AlrtNoCheck[CurrLang])
  else
  begin
    if rGParted.Checked then
    begin
      FileName := 'erase\gparted.iso';
    end
    else if rPartedMagic.Checked then
    begin
      FileName := 'erase\pmagic.iso';
    end;

    FileName := CheckISOfile(FileName);

    if (Length(FileName) > 0) and (CheckUnetBootin) then
    begin
      gErase.Visible := true;
      Application.ProcessMessages;
      AlertCreate(Self, AlrtStartFormat[CurrLang]);
      ProcessImager(Copy(cUSBErase.Items[cUSBErase.ItemIndex], 1, 3), FileName);
      AlertCreate(Self, AlrtEraEnd[CurrLang]);
    end
    else
    begin
      AlertCreate(Self, AlrtBootFail[CurrLang]);
      BrowserCreate(Self);
    end;
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
  end;

  if (cAgree.Checked = false) and (Sender <> Self)  then
    AlertCreate(Self, AlrtNoCheck[CurrLang])
  else if (ifConnected <> INTERNET_CONNECTION_OFFLINE) and
          (ifConnected <> 0) then
  begin
    ChkFrmResult.FirmExists := false;
    ChkFrmResult := DownloadFirmware(AppPath, SSDInfo);
    if ChkFrmResult.FirmExists then
    begin
      if CheckUNetbootin = false then
      begin
        DownloadUNetbootin;
      end;

      if CheckUNetbootin then
      begin
        if (ExtractFileExt(ChkFrmResult.FirmPath) = '.exe') then
        begin
          ShellExecute(0, 'open', PChar(ChkFrmResult.FirmPath), nil, nil, SW_SHOW);
          if gFirmware.Visible then
          begin
            Constraints.MaxHeight := 0;
            Constraints.MinHeight := 0;
            ClientHeight := MinimumSize;
            gFirmware.Visible := false;
            if FirmForce then
            begin
              lFirmware.Font.Color := clWindowText;
              lFirmware.Font.Style := [];
              FirmForce := false;
            end;
            Constraints.MaxHeight := Height;
            Constraints.MinHeight := Height;
          end;
        end
        else
        begin
          AlertCreate(Self, AlrtStartFormat[CurrLang]);
          ProcessImager(Copy(cUSB.Items[cUSB.ItemIndex], 1, 3), ChkFrmResult.FirmPath);
          AlertCreate(Self, AlrtFirmEnd[CurrLang]);
        end;
      end;
    end;
  end;
end;

procedure TfMain.bStartClick(Sender: TObject);
var
  CurrItem: Integer;
  OptList: TOptList;
begin
  OptList := TOptList.Create;

  for CurrItem := 0 to (lList.Items.Count - 1) do
  begin
    OptList.Add(lList.Checked[CurrItem]);
  end;

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
  lProgress.Caption := CapProg1[CurrLang] + '0 / ' + IntToStr(PartCount) + ' ' + CapProg2[CurrLang];
  bCancel.Visible := false;
  lSpeed.Visible := true;

  lSpeed.Font.Color := clRed;
  lSpeed.Font.Style := [fsBold];
  lSpeed.Caption := CapProg3[CurrLang];

  gTrim.Visible := false;
  ShowDownloader;
  Application.ProcessMessages;
  pDownload.Height := pDownload.Height + 10;
  pDownload.Top := pDownload.Top + 5;

  SetLength(NeedTrimPartition, PartCount);
  SetLength(NeedTrimLBASize, PartCount);
  USBYN := SSDInfo.USBMode;
  CurrDrive := 0;
  for CurrPartition := 0 to cTrimList.Items.Count - 1 do
  begin
    if cTrimList.Checked[CurrPartition] then
    begin
      NeedTrimPartition[CurrDrive] := Copy(cTrimList.Items[CurrPartition], 1, 2);
      NeedTrimLBASize[CurrDrive] := SSDInfo.LBASize;
      CurrDrive := CurrDrive + 1;
    end;
  end;
  MainLoaded := true;

  if TrimThread <> Nil then FreeAndNil(TrimThread);
  TrimThread := TTrimThread.Create(true);
  TrimThread.Priority := tpLower;
  TrimThread.Start;
end;

procedure TfMain.FormClick(Sender: TObject);
begin
  if GSSDSel.Visible = true then
  begin
    GSSDSel.Visible := false;
    SSDSelLbl.Caption := CapSSDSelOpn[CurrLang];
  end;
end;

procedure TfMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if UpdateThread <> nil then
  begin
    if UpdateThread.Finished = false then
    begin
      Action := caNone;
      exit;
    end;
  end;

  if TrimThread <> Nil then
  begin
    if TrimStat < 2 then Action := caNone
    else FreeAndNil(TrimThread);
  end;
  if VersionLoader <> nil then
  begin
    FreeAndNil(VersionLoader);
  end;
end;

procedure TfMain.FormCreate(Sender: TObject);
const
  INTERNET_CONNECTION_OFFLINE = 32;
var
  ifConnected: DWORD;
  SetupPath: String;

begin
  if Win32MajorVersion < 5 then
  begin
    AlertCreate(Self, AlrtOSError[CurrLang]);
    Application.Terminate;
    exit;
  end;

  AppPath := ExtractFilePath(Application.ExeName);
  UpdateThread := nil;
  if Copy(ParamStr(1), Length(ParamStr(1)) - 3, 4) = '.err' then
  begin
    exit;
  end;

  CurrDrive := '';
  FirmForce := false;
  if FileExists(AppPath + 'Setup.exe') then DeleteFile(AppPath + 'Setup.exe');

  if SimulationMode then Caption := 'Naraeon SSD Tools ' + CurrentVersion +
                                     ' - !!! On Simulation Mode !!!';

  Icon := Application.Icon;
  Constraints.MaxHeight := 0;
  Constraints.MinHeight := 0;
  ListEnter := 0;
  ClientHeight := MinimumSize;
  Constraints.MaxHeight := Height;
  Constraints.MinHeight := Height;

  firstiOptLeft := iOptimize.Left;
  ShowSerial := false;

  WinDir := GetEnvironmentVariable('windir');
  WinDrive := ExtractFileDrive(WinDir);

  SetupPath := ExtractFilePath(GetRegStr('LM', 'Software\Microsoft\Windows\CurrentVersion\Uninstall\Naraeon SSD Tools\', 'UninstallString'));
  if DirectoryExists(AppPath + 'Image') = false then CreateDirectory(PChar(AppPath + 'Image'), nil);
  if DirectoryExists(AppPath + 'Erase') = false then CreateDirectory(PChar(AppPath + 'Erase'), nil);
  if DirectoryExists(AppPath + 'Unetbootin') = false then CreateDirectory(PChar(AppPath + 'Unetbootin'), nil);

  Optimizer := TNSTOptimizer.Create;
  SSDInfo := TSSDInfo_NST.Create;

  RefreshOptList;
  SetLanguage;
  FontAndSATrimSet;
  LoadBGImage;
  RefreshDrives(SSDInfo);

  InternetGetConnectedState(@ifConnected, 0);
  if (ifConnected <> INTERNET_CONNECTION_OFFLINE) and
      (ifConnected <> 0) then
  begin
    UpdateThread := TUpdateThread.Create(True);
    UpdateThread.Priority := tpLower;
    UpdateThread.FreeOnTerminate := true;
    UpdateThread.Start;
  end;

  ReportMemoryLeaksOnShutdown := true;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  ReleaseMutex(MutexAppear);
  CloseHandle(MutexAppear);
  FreeAndNil(SSDInfo);
  FreeAndNil(Optimizer);
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
  if GSSDSel.Visible = true then
  begin
    GSSDSel.Visible := false;
    SSDSelLbl.Caption := CapSSDSelOpn[CurrLang];
  end;
end;

procedure TfMain.DownloaderWork(Sender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
var
  LeftSec: Int64;
begin
  if Aborted then
  begin
    TIdHttp(Sender).Disconnect;
  end;

  if (AWorkMode = wmRead) then
  begin
    pDownload.Position := Round((AWorkCount / Max) * 100);
    CurrDwldCount := AWorkCount;
    lProgress.Caption := CapProg1[CurrLang] + Format('%.1fMB', [AWorkCount / 1024 / 1024]) + ' / ' +
                                          Format('%.1fMB', [Max / 1024 / 1024]) +
                                          ' (' + IntToStr(Round((AWorkCount / Max) * 100)) + '%)';

    lSpeed.Caption := CapSpeed[CurrLang] + Format('%.1f', [(CurrDwldCount - LastDwldCount) * 2 / 1024]) + 'KB/s';
    if (CurrDwldCount - LastDwldCount) > 0 then LeftSec := round((Max - CurrDwldCount) / ((CurrDwldCount - LastDwldCount) * 2))
    else LeftSec := 0;
    if LeftSec < 60 then lSpeed.Caption := CapTime[CurrLang] + IntToStr(LeftSec) + CapSec[CurrLang]
    else if LeftSec < 3600 then lSpeed.Caption := CapTime[CurrLang] + IntToStr(floor(LeftSec / 60)) + CapMin[CurrLang] + ' ' + IntToStr(LeftSec mod 60) + CapSec[CurrLang]
    else if LeftSec < 86400 then lSpeed.Caption := CapTime[CurrLang] + IntToStr(floor(LeftSec / 3600)) + CapHour[CurrLang] + ' ' + IntToStr(floor(LeftSec / 60)) + CapMin[CurrLang]
    else lSpeed.Caption := CapTime[CurrLang] + IntToStr(floor(LeftSec / 86400)) + CapDay[CurrLang] + ' ' + IntToStr(floor(LeftSec / 3600)) + CapHour[CurrLang];
    LastDwldCount := CurrDwldCount;

    Application.ProcessMessages;
  end;
end;

procedure TfMain.iAnalyticsClick(Sender: TObject);
begin
  if GSSDSel.Visible = true then
  begin
    GSSDSel.Visible := false;
    SSDSelLbl.Caption := CapSSDSelOpn[CurrLang];
  end;
  if gAnalytics.Visible then
  begin
    Constraints.MaxHeight := 0;
    Constraints.MinHeight := 0;
    ClientHeight := MinimumSize;
    gAnalytics.Visible := false;
    Constraints.MaxHeight := Height;
    Constraints.MinHeight := Height;
  end
  else
  begin
    if gErase.Visible = true then iErase.OnClick(nil);
    if gFirmware.Visible = true then iFirmUp.OnClick(nil);
    if gOpt.Visible = true then iOptimize.OnClick(nil);
    if (gTrim.Visible = true) or (gSchedule.Visible = true) then iTrim.OnClick(nil);
    Constraints.MaxHeight := 0;
    Constraints.MinHeight := 0;
    ClientHeight := MaximumSize;
    gAnalytics.Visible := true;
    Constraints.MaxHeight := Height;
    Constraints.MinHeight := Height;
  end;
end;

procedure TfMain.iEraseClick(Sender: TObject);
begin
  GetUSBDrives(cUSBErase.Items);
  if GSSDSel.Visible = true then
  begin
    GSSDSel.Visible := false;
    SSDSelLbl.Caption := CapSSDSelOpn[CurrLang];
  end;
  if gErase.Visible then
  begin
    Constraints.MaxHeight := 0;
    Constraints.MinHeight := 0;
    ClientHeight := MinimumSize;
    gErase.Visible := false;
    Constraints.MaxHeight := Height;
    Constraints.MinHeight := Height;
  end
  else if cUSBErase.Items.Count = 0 then AlertCreate(Self, AlrtNoUSB[CurrLang])
  else
  begin
    cUSBErase.ItemIndex := 0;
    if gFirmware.Visible = true then iFirmUp.OnClick(nil);
    if gAnalytics.Visible = true then iAnalytics.OnClick(nil);
    if gOpt.Visible = true then iOptimize.OnClick(nil);
    if (gTrim.Visible = true) or (gSchedule.Visible = true) then iTrim.OnClick(nil);
    Constraints.MaxHeight := 0;
    Constraints.MinHeight := 0;
    if ClientHeight = MinimumSize then
    begin
      ClientHeight := MaximumSize;
      gErase.Visible := true;
    end;
    Constraints.MaxHeight := Height;
    Constraints.MinHeight := Height;
  end;
end;

procedure TfMain.iFirmUpClick(Sender: TObject);
begin
  GetUSBDrives(cUSB.Items);
  if GSSDSel.Visible = true then
  begin
    GSSDSel.Visible := false;
    SSDSelLbl.Caption := CapSSDSelOpn[CurrLang];
  end;

  if SSDInfo.SSDSupport.SupportFirmUp = false then
  begin
    ClientHeight := MinimumSize;
    gFirmware.Visible := false;
    AlertCreate(Self, AlrtNoFirmSupport[CurrLang]);
    exit;
  end;

  if gFirmware.Visible then
  begin
    Constraints.MaxHeight := 0;
    Constraints.MinHeight := 0;
    ClientHeight := MinimumSize;
    gFirmware.Visible := false;
    if FirmForce then
    begin
      lFirmware.Font.Color := clWindowText;
      lFirmware.Font.Style := [];
      FirmForce := false;
    end;
    Constraints.MaxHeight := Height;
    Constraints.MinHeight := Height;
  end
  else if cUSB.Items.Count = 0 then
    AlertCreate(Self, AlrtNoUSB[CurrLang])
  else
  begin
    if gErase.Visible = true then iErase.OnClick(nil);
    if gOpt.Visible = true then iOptimize.OnClick(nil);
    if gAnalytics.Visible = true then iAnalytics.OnClick(nil);
    if (gTrim.Visible = true) or (gSchedule.Visible = true) then iTrim.OnClick(nil);
    cUSB.ItemIndex := 0;
    lNewFirm.Font.Color := clWindowText;
    lNewFirm.Font.Style := [];
    Constraints.MaxHeight := 0;
    Constraints.MinHeight := 0;
    if ClientHeight = MinimumSize then
    begin
      ClientHeight := MaximumSize;
      gFirmware.Visible := true;
    end;
    if (IsPlextorNewVer(SSDInfo.Model, SSDInfo.Firmware) = NEW_VERSION) or
        (IsLiteONNewVer(SSDInfo.Model, SSDInfo.Firmware) = NEW_VERSION) then
    begin
        lFirmware.Font.Color := clRed;
        lFirmware.Font.Style := [fsBold];
        FirmForce := true;
        if lNewFirm.Font.Color <> clRed then
        begin
          lNewFirm.Font.Color := clRed;
          lNewFirm.Font.Style := [fsBold];
          if Pos(CapCurrFirm[CurrLang], lNewFirm.Caption) = 0 then lNewFirm.Caption := lNewFirm.Caption + ' ' + CapCurrFirm[CurrLang];
        end;
        AlertCreate(Self, AlrtNoUpdate[CurrLang]);
    end;
    Constraints.MaxHeight := Height;
    Constraints.MinHeight := Height;
  end;
end;

procedure TfMain.InitUIToRefresh;
begin
  if (gFirmware.Visible = false) and
      (gDownload.Visible = false) then
    lFirmware.Font.Style := [];
  lSectors.Font.Color := clWindowText;
  lPError.Font.Color := clWindowText;
  lNotsafe.Font.Color := clWindowText;

  if FirmForce = false then lFirmware.Font.Color := clWindowText;
  lPartitionAlign.Font.Color := clWindowText;
  lNotsafe.Caption := CapStatus[CurrLang] + CapSafe[CurrLang];
end;

procedure TfMain.iSCheckClick(Sender: TObject);
begin
  if GSSDSel.Visible = true then
  begin
    GSSDSel.Visible := false;
    SSDSelLbl.Caption := CapSSDSelOpn[CurrLang];
  end;

  if gFirmware.Visible = true then
    ShellExecute(0, 'open', 'http://naraeon.tistory.com/131', '', nil, SW_NORMAL)
  else if gErase.Visible = true then
    ShellExecute(0, 'open', 'http://naraeon.tistory.com/144', '', nil, SW_NORMAL)
  else if gTrim.Visible = true then
    ShellExecute(0, 'open', 'http://naraeon.tistory.com/142', '', nil, SW_NORMAL)
  else if gSchedule.Visible = true then
    ShellExecute(0, 'open', 'http://naraeon.tistory.com/143', '', nil, SW_NORMAL)
  else
    ShellExecute(0, 'open', 'http://naraeon.tistory.com/132', '', nil, SW_NORMAL);
end;

procedure TfMain.iOptimizeClick(Sender: TObject);
begin
  if GSSDSel.Visible = true then
  begin
    GSSDSel.Visible := false;
    SSDSelLbl.Caption := CapSSDSelOpn[CurrLang];
  end;
  if gErase.Visible = true then iErase.OnClick(nil);
  if gFirmware.Visible = true then iFirmUp.OnClick(nil);
  if gAnalytics.Visible = true then iAnalytics.OnClick(nil);
  if (gTrim.Visible = true) or (gSchedule.Visible = true) then iTrim.OnClick(nil);
  Constraints.MaxHeight := 0;
  Constraints.MinHeight := 0;
  if ClientHeight = MinimumSize then
  begin
    ClientHeight := MaximumSize;
    gOpt.Visible := true;
  end
  else
  begin
    ClientHeight := MinimumSize;
    gOpt.Visible := false;
  end;
  Constraints.MaxHeight := Height;
  Constraints.MinHeight := Height;
end;


procedure TfMain.iTrimClick(Sender: TObject);
var
  CheckedDrives: Integer;
begin
  if GSSDSel.Visible = true then
  begin
    GSSDSel.Visible := false;
    SSDSelLbl.Caption := CapSSDSelOpn[CurrLang];
  end;
  GetChildDrives(ExtractDeviceNum(SSDInfo.DeviceName), cTrimList.Items);
  for CheckedDrives := 0 to cTrimList.Count - 1 do
    cTrimList.Checked[CheckedDrives] := true;

  if gErase.Visible = true then iErase.OnClick(nil);
  if gFirmware.Visible = true then iFirmUp.OnClick(nil);
  if gAnalytics.Visible = true then iAnalytics.OnClick(nil);
  if gOpt.Visible = true then iOptimize.OnClick(nil);
  Constraints.MaxHeight := 0;
  Constraints.MinHeight := 0;
  if ClientHeight = MinimumSize then
  begin
    ClientHeight := MaximumSize;
    gTrim.Visible := true;
  end
  else
  begin
    ClientHeight := MinimumSize;
    gTrim.Visible := false;
    gSchedule.Visible := false;
  end;
  Constraints.MaxHeight := Height;
  Constraints.MinHeight := Height;
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
  if GSSDSel.Visible = true then
  begin
    GSSDSel.Visible := false;
    SSDSelLbl.Caption := CapSSDSelOpn[CurrLang];
  end;
  if CurrDrive <> TLabel(Sender).Hint then
  begin
    if gFirmware.Visible = true then iFirmUp.OnClick(nil);
    if gOpt.Visible = true then iOptimize.OnClick(nil);
    if gErase.Visible = true then iErase.OnClick(nil);
    if gAnalytics.Visible = true then iAnalytics.OnClick(nil);
    if (gTrim.Visible) or (gSchedule.Visible) then iTrim.OnClick(nil);

    CurrDrive := TLabel(Sender).Hint;
    CurrUSBMode := TLabel(Sender).AlignWithMargins;
    CurrATAorSCSIStatus := TLabel(Sender).HelpContext;
    tRefreshTimer(Self);
    for CurrIndex := 0 to Length(SSDLabel) - 1 do
      if SSDLabel[CurrIndex].Hint = TLabel(Sender).Hint then
      begin
        SSDLabel[CurrIndex].Font.Style := [fsBold];
      end
      else
      begin
        SSDLabel[CurrIndex].Font.Style := [];
      end;
  end;
  GSSDSel.Visible := false;
end;

procedure TfMain.tDownloadCheckerTimer(Sender: TObject);
var
  LeftSec: Int64;
begin
  lSpeed.Caption := CapSpeed[CurrLang] + Format('%.1f', [(CurrDwldCount - LastDwldCount) * 2 / 1024]) + 'KB/s';
  if (CurrDwldCount - LastDwldCount) > 0 then LeftSec := round((Max - CurrDwldCount) / ((CurrDwldCount - LastDwldCount) * 2))
  else LeftSec := 0;
  if LeftSec < 60 then lSpeed.Caption := CapTime[CurrLang] + IntToStr(LeftSec) + CapSec[CurrLang]
  else if LeftSec < 3600 then lSpeed.Caption := CapTime[CurrLang] + IntToStr(floor(LeftSec / 60)) + CapMin[CurrLang] + ' ' + IntToStr(LeftSec mod 60) + CapSec[CurrLang]
  else if LeftSec < 86400 then lSpeed.Caption := CapTime[CurrLang] + IntToStr(floor(LeftSec / 3600)) + CapHour[CurrLang] + ' ' + IntToStr(floor(LeftSec / 60)) + CapMin[CurrLang]
  else lSpeed.Caption := CapTime[CurrLang] + IntToStr(floor(LeftSec / 86400)) + CapDay[CurrLang] + ' ' + IntToStr(floor(LeftSec / 3600)) + CapHour[CurrLang];
  LastDwldCount := CurrDwldCount;
end;

procedure TfMain.SSDSelLblMouseEnter(Sender: TObject);
begin
  if GSSDSel.Visible = false then
  begin
    GSSDSel.Visible := true;
    GSSDSel.BringToFront;
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

procedure TfMain.tErrorChkTimer(Sender: TObject);
var
  DesktopPath: array[0..MAX_PATH] of char;
  DeskPath: String;
  ErrList: TStringList;
begin
  if tErrorChk.Interval = 500 then
    tErrorChk.Interval := 5000;
  SHGetFolderPath(0, CSIDL_COMMON_DESKTOPDIRECTORY, 0, 0, @DesktopPath[0]);
  DeskPath := DesktopPath;

  if FileExists(DeskPath + '\!!!SSDError!!!.err') then
  begin
    MsgboxCreate(Self, DeskPath + '\!!!SSDError!!!.err');
  end;

  ErrList := WriteBufferCheck;
  if ErrList.Count > 0 then
    AlertCreate(Self, ErrCache[CurrLang] + Chr(13) + Chr(10) +  ErrList.Text);
  FreeAndNil(ErrList);
end;

procedure TfMain.tFindMutexTimer(Sender: TObject);
var
  TempMutex: Integer;
begin
  TempMutex := OpenMutex(MUTEX_ALL_ACCESS, False, 'NSToolsOpenMainform');
  if TempMutex <> 0 Then
  begin
    Self.Visible := false;
    SetForegroundWindow(Self.Handle);
    Self.Activate;
    Self.WindowState := wsNormal;
    Self.Show;
  end;
  ReleaseMutex(TempMutex);
  CloseHandle(TempMutex);
end;

procedure TfMain.tGetSSDsTimer(Sender: TObject);
begin
  tGetSSDs.Enabled := false;
  tRefresh.Enabled := false;
  RefreshDrives(SSDInfo);
  tRefresh.Enabled := true;
end;

procedure TfMain.tListLeaveTimer(Sender: TObject);
begin
  if ListEnter = 0 then
    if GSSDSel.Visible = true then
    begin
      GSSDSel.Visible := false;
      SSDSelLbl.Caption := CapSSDSelOpn[CurrLang];
      tListLeave.Enabled := false;
    end;
end;

procedure TfMain.tRefreshTimer(Sender: TObject);
begin
  if RefreshTimer(SSDInfo, CurrUSBMode, CurrATAorSCSIStatus,
                  ShowSerial, firstiOptLeft) = false then
    Application.Terminate;
end;

procedure TfMain.WmAfterShow(var Msg: TMessage);
begin
  Top := Top - (MinimumSize div 2);

  if Copy(ParamStr(1), Length(ParamStr(1)) - 3, 4) = '.err' then
  begin
    Visible := false;
    MsgboxCreate(Self, ParamStr(1));
    Application.Terminate;
  end;
  tErrorChkTimer(nil);
end;

procedure TfMain.WMDeviceChange(var Msg: TMessage);
const
  DBT_DEVICEARRIVAL = $8000;
  DBT_DEVICEREMOVECOMPLETE = $8004;
  DBT_DEVNODES_CHANGED = $0007;
  DBT_STORAGE = $0002;
begin
  case Msg.wParam of
    DBT_DEVNODES_CHANGED :
      tGetSSDs.Enabled := true;
    DBT_DEVICEARRIVAL :
    begin
      if PDevBroadcastHdr(Msg.lParam)^.dbcd_devicetype = DBT_STORAGE then
      begin
        tGetSSDs.Enabled := true;
      end;
    end;
    DBT_DEVICEREMOVECOMPLETE :
    begin
      if PDevBroadcastHdr(Msg.lParam)^.dbcd_devicetype = DBT_STORAGE then
      begin
        tGetSSDs.Enabled := true;
      end;
    end;
  end;
end;

procedure TfMain.rGPartedClick(Sender: TObject);
begin
  rGParted.Font.Style := [fsBold];
  rPartedMagic.Font.Style := [];
end;

procedure TfMain.rPartedMagicClick(Sender: TObject);
begin
  rGParted.Font.Style := [];
  rPartedMagic.Font.Style := [fsBold];
end;

procedure TfMain.ShowDownloader;
var
  CurrImgLbl: Integer;
begin
  for CurrImgLbl := 0 to Length(SSDLabel) - 1 do
  begin
    SSDLabel[CurrImgLbl].Enabled := false;
  end;
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

  Constraints.MaxHeight := 0;
  Constraints.MinHeight := 0;
  ClientHeight := MaximumSize;
  Constraints.MaxHeight := Height;
  Constraints.MinHeight := Height;
end;

procedure TfMain.HideDownloader;
var
  CurrImgLbl: Integer;
begin
  for CurrImgLbl := 0 to Length(SSDLabel) - 1 do
  begin
    SSDLabel[CurrImgLbl].Enabled := true;
  end;
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
  resultsched: String;
begin
  if cTrimRunning.Checked then
    if Win32MajorVersion = 5 then
    begin
     resultsched := string(OpenProcWithOutput(WinDir + '\System32',
                    'schtasks /create /sc onidle /i 1 /tn "MANTRIM'
                    + SSDInfo.Serial + '" /tr "\" ' + Application.ExeName
                    + '\" ' + SSDInfo.Serial + '" /ru system'));
    end
    else
     resultsched := string(OpenProcWithOutput(WinDir + '\System32',
                    'schtasks /create /sc onidle /i 1 /tn "MANTRIM'
                    + SSDInfo.Serial + '" /tr "''' + Application.ExeName
                    + ''' ''' + SSDInfo.Serial + '''" /rl HIGHEST'))
  else
    resultsched := string(OpenProcWithOutput(WinDir + '\System32',
                    'schtasks /delete /TN "MANTRIM' + SSDInfo.Serial + '" /F'));
end;

function TfMain.DownloadUnetbootin: Boolean;
var
  Src, Dest: TDownloadFile;
  DownloadResult: Boolean;
begin
  result := false;

  AlertCreate(Self, AlrtBootInStart[CurrLang]);

  Src.FBaseAddress := '';
  Src.FFileAddress := 'http://www.naraeon.net/SSDTools_Common/exec_path/unet.htm';
  Src.FType := dftGetFromWeb;

  Dest.FBaseAddress := AppPath;
  Dest.FFileAddress := 'Unetbootin\unetbootin.exe_tmp';
  Dest.FType := dftPlain;

  gFirmware.Visible := false;
  DownloadResult := DownloadFile(Src, Dest, CapBootInDwld[CurrLang], bCancel.Caption);
  gFirmware.Visible := true;

  if fAlert <> Nil then FreeAndNil(fAlert);
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
  MessageResult: integer;
  Src, Dest: TDownloadFile;
  DownloadResult: Boolean;
begin
  MessageResult :=
    Application.MessageBox(PChar(CapCurrVer[CurrLang] + CurrentVersion
                           + Chr(13) + Chr(10) +
                           CapNewVer[CurrLang] + Copy(ServerVersion, 1, 5)
                           + Chr(13) + Chr(10) + Chr(13) + Chr(10) +
                           Copy(ChangeLog, 1, CurrChr)
                           + Chr(13) + Chr(10) + Chr(13) + Chr(10) +
                           CapUpdQues[CurrLang]), PChar(AlrtNewVer[CurrLang]),
                           MB_OKCANCEL  + MB_IconInformation);

  if MessageResult = 1 then
  begin
    Src.FBaseAddress := 'http://www.naraeon.net';
    Src.FFileAddress := '/SSDTools/Setup.exe';
    Src.FType := dftPlain;

    Dest.FBaseAddress := AppPath;
    Dest.FFileAddress := 'Setup.exe';
    Dest.FType := dftPlain;

    DownloadResult := DownloadFile(Src, Dest, CapUpdDwld[CurrLang],
                                   bCancel.Caption);

    if fAlert <> Nil then FreeAndNil(fAlert);
    if DownloadResult then
    begin
      try
        Constraints.MaxHeight := 0;
        Constraints.MinHeight := 0;
        ClientHeight := MinimumSize;
        gErase.Visible := false;
        Constraints.MaxHeight := Height;
        Constraints.MinHeight := Height;

        AlertCreate(Self, AlrtUpdateExit[CurrLang]);
      finally
        ShellExecute(0, nil, PChar(AppPath + 'Setup.exe'), nil, nil, SW_NORMAL);
        FreeAndNil(VersionLoader);
        Application.Terminate;
      end;
    end
    else
    begin
      AlertCreate(Self, AlrtVerCanc[CurrLang]);
    end;
  end;
end;


procedure TfMain.FontAndSATrimSet;
var
  CurrFont: String;
  CurrCompNum: Integer;
  CurrComponent: TComponent;
begin
  if Win32MajorVersion = 5 then
  begin
    CurrFont := XPFont[CurrLang];

    lAnaly.Font.Style := [fsBold];
    lUpdate.Font.Style := [fsBold];
    lNameOpt.Font.Style := [fsBold];
    lEraseUSB.Font.Style := [fsBold];
    lName.Font.Style := [fsBold];
    lTrimName.Font.Style := [fsBold];
    lSchName.Font.Style := [fsBold];
    lDownload.Font.Style := [fsBold];
  end
  else
  begin
    CurrFont := VistaFont[CurrLang];
    if Win32MinorVersion >= 2 then
    begin
      bSchedule.Visible := false;
      bTrimStart.Width := bFirmStart.Width;
    end;
  end;

  Font.Name := CurrFont;
  for CurrCompNum := 0 to fMain.ComponentCount - 1 do
  begin
    CurrComponent := fMain.Components[CurrCompNum];
    if CurrComponent is TControl then
      SetFontName(TControl(CurrComponent), CurrFont);
  end;

  FirstOpt := lList.Items.Text;
  RefreshOptList;
  Constraints.MaxWidth := Width;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
end;

procedure TfMain.SetLanguage;
begin
  bTrimStart.Caption := CapStartManTrim[CurrLang];

  lFirmUp.Caption := BtFirmware[CurrLang];
  lErase.Caption := BtErase[CurrLang];
  lOptimize.Caption := BtOpt[CurrLang];
  lHelp.Caption := BtHelp[CurrLang];
  lAnalytics.Caption := BtAnaly[CurrLang];
  lTrim.Caption := BtTrim[CurrLang];

  lFirmUp.left    := iFirmUp.Left + (iFirmUp.Width div 2) - (lFirmUp.Width div 2);
  lErase.left     := iErase.Left + (iErase.Width div 2) - (lErase.Width div 2);
  lOptimize.left  := iOptimize.Left + (iOptimize.Width div 2)
                      - (lOptimize.Width div 2);
  lAnalytics.left := iAnalytics.Left + (iAnalytics.Width div 2)
                      - (lAnalytics.Width div 2);
  lHelp.left      := iHelp.Left + (iHelp.Width div 2) - (lHelp.Width div 2);
  lTrim.left      := iTrim.Left + (iTrim.Width div 2) - (lTrim.Width div 2);

  lUpdate.Caption := CapFirm[CurrLang];
  lUSB.Caption := CapSelUSB[CurrLang];
  lNewFirm.Caption := CapNewFirm[CurrLang];
  cAgree.Caption := CapWarnErase[CurrLang];
  bFirmStart.Caption := BtDoUpdate[CurrLang];
  SSDSelLbl.Caption := CapSSDSelOpn[CurrLang];

  lNameOpt.Caption := CapNameOpt[CurrLang];
  bStart.Caption := BtDoOpt[CurrLang];
  bRtn.Caption := BtRollback[CurrLang];

  lAnaly.Caption := CapAnaly[CurrLang];
  lEraseUSB.Caption := CapErase[CurrLang];
  lUSBErase.Caption := CapSelUSB[CurrLang];
  cEraseAgree.Caption := CapWarnErase[CurrLang];
  bEraseUSBStart.Caption := BtDoErase[CurrLang];

  lTrimName.Caption := CapTrimName[CurrLang];
  lAnaly.Caption := CapAnaly[CurrLang];
  bSchedule.Caption := BtSemiAutoTrim[CurrLang];
  bCancel.Caption := BtDnldCncl[CurrLang];

  lSchName.Caption := CapSemiAutoTrim[CurrLang];
  lSchExp.Caption := CapSemiAutoTrimExp[CurrLang];
  cTrimRunning.Caption := ChkSemiAutoTrim[CurrLang];
  bReturn.Caption := BtRtn[CurrLang];
end;

procedure TfMain.LoadBGImage;
begin
  if FileExists(AppPath + 'Image\bg.png') then
    iBG.Picture.LoadFromFile(AppPath + 'Image\bg.png');
  if FileExists(AppPath + 'Image\logo.png') then
    iLogo.Picture.LoadFromFile(AppPath + 'Image\logo.png');
end;

procedure TfMain.RefreshOptList;
var
  CurrItem: Integer;
begin
  lList.Items.Assign(Optimizer.Descriptions);
  for CurrItem := 0 to (Optimizer.Descriptions.Count - 1) do
  begin
    lList.Checked[CurrItem] := (not Optimizer.Optimized[CurrItem])
                                and (not Optimizer.Selective[CurrItem]);
    if Optimizer.Optimized[CurrItem] then
    begin
      lList.Items[CurrItem] := lList.Items[CurrItem]
                                + CapAlreadyCompleted[CurrLang];
    end;
  end;
end;

procedure TfMain.bScheduleClick(Sender: TObject);
var
  Drives: TDriveLetters;
  CurrDrv: Integer;
  resultstring: String;
begin
  gTrim.Visible := false;
  gSchedule.Visible := true;
  Drives := GetPartitionList(ExtractDeviceNum(SSDInfo.DeviceName));
  lDrives.Caption := CapAppDisk[CurrLang];
  for CurrDrv := 0 to Drives.LetterCount - 1 do
    lDrives.Caption := lDrives.Caption + Drives.Letters[CurrDrv] + ' ';
  resultstring := UnicodeString(OpenProcWithOutput(WinDir + '\System32',
                                                   'schtasks /query'));
  if Pos('MANTRIM' + SSDInfo.Serial, resultstring) > 0 then
    cTrimRunning.Checked := true
  else
    cTrimRunning.Checked := false;
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
    result := false;
    exit;
  end;

  lDownload.Caption := DownloadCaption;
  bCancel.Caption := CancelCaption;

  DownloadStream := TFileStream.Create(DestAddress,
                                        fmCreate or fmShareExclusive);
  Downloader := TIdHttp.Create();
  Downloader.Request.UserAgent := 'Naraeon SSD Tools';

  try
    Aborted := false;
    Max := 0;
    Downloader.OnWork := DownloaderWork;
    Downloader.Head(SrcAddress);
    Max := Downloader.response.ContentLength;

    ShowDownloader;

    Downloader.Get(SrcAddress, DownloadStream);
    Application.ProcessMessages;

    HideDownloader;
  finally
    FreeAndNil(DownloadStream);
    FreeAndNil(Downloader);
  end;

  result := not Aborted;
end;
end.
