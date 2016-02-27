unit Form.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.CheckLst, WinInet, UITypes,
  Vcl.OleCtrls, Vcl.ExtCtrls, ShellApi, Math,
  Vcl.Imaging.pngimage, ShlObj, Vcl.ComCtrls,
  Form.Alert, Form.Message, Form.Browser, Global.LanguageString,
  Extern.SevenZip, Optimizer,
  OS.ProcessOpener, OS.DeleteDirectory, OS.PlugAndPlay,
  Component.ButtonGroup, Initializer, Extern.Rufus, OS.EnvironmentVariable,
  Thread.Update, Thread.Trim, TrimList, Initializer.Locale,
  Device.PhysicalDrive, Getter.PhysicalDrive.PartitionList,
  Device.PhysicalDrive.List, Getter.LatestFirmware,
  Component.SSDLabel.List, Component.SSDLabel,
  Initializer.PhysicalDrive, Initializer.SSDLabelListRefresh,
  Downloader.Firmware, Getter.DriveList.Removable, Getter.DriveList,
  Getter.VolumeLabel, Getter.OS.Version, PrerequisiteChecker, BufferInterpreter,
  Global.HelpPage, CommandSet.SAT;

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
    tRefreshList: TTimer;
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

    procedure InitUIToRefresh;
    procedure tRefreshTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tRefreshListTimer(Sender: TObject);
  private
    CurrentDrivePath: String;
    RefreshListDisabled: Boolean;
    ShowSerial: Boolean;
    ListEnter: Integer;
    IsConnected: Boolean;
    FirmwareGetter: TFirmwareGetter;
    Optimizer: TNSTOptimizer;
    UpdateThread: TUpdateThread;
    TrimThread: TTrimThread;
    ButtonGroup: TButtonGroup;
    SSDLabel: TSSDLabelList;
    procedure RefreshByPhysicalDrive;
    procedure RefreshDrives;
    function TryToCreatePhysicalDriveWithEntry(DeviceNumber: Integer): Boolean;
    procedure CreateNewPhysicalDrive;
    procedure FindAndSelectValidDrive;
    procedure SetSelectedDriveLabelBold;
    function FirmwareUpdateNotAvailable: Boolean;
    procedure WmAfterShow(var Msg: TMessage); message WM_AFTER_SHOW;
    procedure WMDeviceChange(var Msg: TMessage); message WM_DEVICECHANGE;
    procedure CreateBasicObjects;
    procedure Erase(const Filename: string);
    procedure RefreshLabelList;
    procedure IfConnectedStartUpdateThread;
    procedure ApplyPartitionCountToTrim;
    procedure PrepareFormToTrim;
    procedure StartTrimThread(const PartitionsToTrim: TTrimList);
    procedure StartUpdateThread;
    procedure SetIsConnected;
    procedure CreateButtonGroup;
  public
    PhysicalDriveList: TPhysicalDriveList;
    PhysicalDrive: IPhysicalDrive;
    OnlineFirmwareUpdateAvailable: Boolean;
    WICImage: TWICImage;
    procedure DisableSSDLabel;
    procedure EnableSSDLabel;
    function GetButtonGroup: TButtonGroup;
    procedure OpenButtonGroup;
    procedure CloseButtonGroup;
    function GetUpdateNotice: String;
    procedure AddEntryToButtonGroup(iSelected: Boolean; iImageButton: TImage;
      iLabelButton: TLabel; iGroupBox: TGroupBox;
      iClickEventProcedure: TClickEventProcedure);
    procedure TerminateUpdateThread;
    function GetFirmwareGetter: TFirmwareGetter;
    function GetOptimizer: TNSTOptimizer;
  end;

var
  fMain: TfMain;

const
  MinimumSize = 290;
  MaximumSize = 535;

implementation

{$R *.dfm}

procedure TfMain.bEraseUSBStartClick(Sender: TObject);
begin
  if cEraseAgree.Checked = false then
  begin
    AlertCreate(Self, AlrtNoCheck[CurrLang]);
    exit;
  end;
  tRefresh.Enabled := false;
  if PhysicalDrive.IdentifyDeviceResult.StorageInterface = NVMe then
    Erase('nvme')
  else
    Erase('pmagic');
  tRefresh.Enabled := true;
end;

procedure TfMain.bFirmStartClick(Sender: TObject);
var
  FirmwareDownloader: TFirmwareDownloader;
begin
  if fMain.cAgree.Checked = false then
  begin
    AlertCreate(Self, AlrtNoCheck[CurrLang]);
    exit;
  end;
  FirmwareDownloader := TFirmwareDownloader.Create;
  FirmwareDownloader.DownloadFirmware;
end;

procedure TfMain.bStartClick(Sender: TObject);
var
  CurrItem: Integer;
  OptList: TOptimizeList;
begin
  OptList := TOptimizeList.Create;
  for CurrItem := 0 to (lList.Items.Count - 1) do
    OptList.Add(lList.Checked[CurrItem]);
  Optimizer.Optimize(OptList);
  FreeAndNil(OptList);

  RefreshOptimizeList;
  AlertCreate(Self, AlrtOptCmpl[CurrLang]);
end;

procedure TfMain.bRtnClick(Sender: TObject);
begin
  Optimizer.CancelOptimization;

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
  PartitionsToTrim: TTrimList;
begin
  ApplyPartitionCountToTrim;
  PrepareFormToTrim;
  CurrentDrivePath := '';
  PartitionsToTrim := TTrimList.Create;
  for CurrPartition := 0 to cTrimList.Items.Count - 1 do
    if cTrimList.Checked[CurrPartition] then
      PartitionsToTrim.Add(
        Copy(cTrimList.Items[CurrPartition], 1, 2));
  StartTrimThread(PartitionsToTrim);
end;

procedure TfMain.CloseButtonGroup;
begin
  ButtonGroup.Close;
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
  CheckPrerequisite;
  SetIsConnected;
  IfConnectedStartUpdateThread;
  CreateBasicObjects;
  InitializeMainForm;
  ApplyLocaleToMainformAndArrangeButton;
  RefreshDrives;
  ReportMemoryLeaksOnShutdown := DebugHook > 0;
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FirmwareGetter);
  FreeAndNil(PhysicalDriveList);
  FreeAndNil(SSDLabel);
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
begin
  if PhysicalDriveList.Count = 0 then
    RefreshLabelList;
  if PhysicalDriveList.Count = 0 then
    exit;
  RefreshByPhysicalDrive;
  SetSelectedDriveLabelBold;
end;

procedure TfMain.FormShow(Sender: TObject);
begin
  PostMessage(Self.Handle, WM_AFTER_SHOW, 0, 0);
end;

function TfMain.GetButtonGroup: TButtonGroup;
begin
  result := ButtonGroup;
end;

function TfMain.GetFirmwareGetter: TFirmwareGetter;
begin
  result := FirmwareGetter;
end;

function TfMain.GetOptimizer: TNSTOptimizer;
begin
  result := Optimizer;
end;

function TfMain.GetUpdateNotice: String;
begin
  result := UpdateThread.UpdateNotice;
end;

procedure TfMain.gInfoClick(Sender: TObject);
begin
  CloseDriveList;
end;

procedure TfMain.iAnalyticsClick(Sender: TObject);
begin
  CloseDriveList;
  ButtonGroup.Click(iAnalytics);
end;

function TfMain.FirmwareUpdateNotAvailable: Boolean;
begin
  result := (not PhysicalDrive.SupportStatus.FirmwareUpdate) or
    (not OnlineFirmwareUpdateAvailable);
end;

procedure TfMain.iFirmUpClick(Sender: TObject);
var
  Query: TFirmwareQuery;
  QueryResult: TFirmwareQueryResult;
  RemovableDriveListGetter: TRemovableDriveListGetter;
  RemovableDriveList: TDriveList;
begin
  CloseDriveList;

  if FirmwareUpdateNotAvailable then
  begin
    AlertCreate(Self, AlrtNoFirmSupport[CurrLang]);
    exit;
  end;

  if ButtonGroup.FindEntry(iFirmUp).Selected then
  begin
    ButtonGroup.Click(iFirmUp);
    exit;
  end;

  SetIsConnected;
  if not IsConnected then
  begin
    AlertCreate(Self, AlrtNoInternet[CurrLang]);
    exit;
  end;

  RemovableDriveListGetter := TRemovableDriveListGetter.Create('');
  RemovableDriveList := RemovableDriveListGetter.GetDriveList;
  PathListToVolumeLabel(RemovableDriveList, CapRemvDisk[CurrLang]);
  cUSB.Items.Assign(RemovableDriveList);
  FreeAndNil(RemovableDriveListGetter);
  FreeAndNil(RemovableDriveList);
  
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
var
  RemovableDriveListGetter: TRemovableDriveListGetter;
  RemovableDriveList: TDriveList;
begin
  CloseDriveList;

  if ButtonGroup.FindEntry(iErase).Selected then
  begin
    ButtonGroup.Click(iErase);
    exit;
  end;

  RemovableDriveListGetter := TRemovableDriveListGetter.Create('');
  RemovableDriveList := RemovableDriveListGetter.GetDriveList;
  PathListToVolumeLabel(RemovableDriveList, CapRemvDisk[CurrLang]);
  cUSBErase.Items.Assign(RemovableDriveList);
  FreeAndNil(RemovableDriveListGetter);
  FreeAndNil(RemovableDriveList);
  
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
    ShellExecute(0, 'open',
      PChar(HelpHeader + HelpLanguage[CurrLang] + FirmwareUpdatePage), '',
      nil, SW_NORMAL)
  else if ButtonGroup.FindEntry(iErase).Selected then
    if PhysicalDrive.IdentifyDeviceResult.StorageInterface =
      TStorageInterface.NVMe then
      ShellExecute(0, 'open',
        PChar(HelpHeader + HelpLanguage[CurrLang] + NVMeSecureErasePage), '',
        nil, SW_NORMAL)
    else
      ShellExecute(0, 'open',
        PChar(HelpHeader + HelpLanguage[CurrLang] + SecureErasePage), '',
        nil, SW_NORMAL)
  else if ButtonGroup.FindEntry(iTrim).Selected then
    ShellExecute(0, 'open',
      PChar(HelpHeader + HelpLanguage[CurrLang] + ManualTrimPage), '',
      nil, SW_NORMAL)
  else if ButtonGroup.FindEntry(iOptimize).Selected then
    ShellExecute(0, 'open',
      PChar(HelpHeader + HelpLanguage[CurrLang] + OptimizePage), '',
      nil, SW_NORMAL)
  else
    ShellExecute(0, 'open',
      PChar(HelpHeader + HelpLanguage[CurrLang] + MainPage), '',
      nil, SW_NORMAL)
end;

procedure TfMain.iOptimizeClick(Sender: TObject);
begin
  CloseDriveList;
  ButtonGroup.Click(iOptimize);
end;


procedure TfMain.iTrimClick(Sender: TObject);
var
  CheckedDrives: Integer;
  PartitionList: TPartitionList;
  VolumeLabelGetter: TVolumeLabelGetter;
  CurrentDrive: Integer;
begin
  CloseDriveList;

  if ButtonGroup.Click(iTrim) <> clkOpen then
    exit;

  cTrimList.Clear;
  PartitionList := PhysicalDrive.GetPartitionList;
  for CurrentDrive := 0 to PartitionList.Count - 1 do
  begin
    VolumeLabelGetter := TVolumeLabelGetter.Create(
      PartitionList[CurrentDrive].Letter);
    cTrimList.Items.Add(VolumeLabelGetter.GetVolumeLabel(
      CapLocalDisk[CurrLang]));
    FreeAndNil(VolumeLabelGetter);
  end;
  FreeAndNil(PartitionList);
  
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

procedure TfMain.OpenButtonGroup;
begin
  ButtonGroup.Open;
end;

procedure TfMain.SSDLabelClick(Sender: TObject);
begin
  CloseDriveList;

  if CurrentDrivePath =
    TSSDLabel(Sender).PhysicalDrive.GetPathOfFileAccessingWithoutPrefix then
  begin
    gSSDSel.Visible := false;
    exit;
  end;

  lFirmware.Font.Color := clWindowText;
  ButtonGroup.CloseAll;

  CurrentDrivePath :=
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
    fMain.PhysicalDrive :=
      TPhysicalDrive.Create(
          TPhysicalDrive.BuildFileAddressByNumber(DeviceNumber));
  except
    result := false;
    fMain.PhysicalDrive := nil;
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

procedure TfMain.CreateNewPhysicalDrive;
begin
  if not TryToCreatePhysicalDriveWithEntry(
    StrToInt(fMain.CurrentDrivePath)) then
      FindAndSelectValidDrive;
end;

procedure TfMain.tRefreshListTimer(Sender: TObject);
begin
  RefreshListDisabled := true;
  tRefreshList.Enabled := false;
  RefreshLabelList;
  RefreshByPhysicalDrive;
  SetSelectedDriveLabelBold;
  RefreshListDisabled := false;
end;

procedure TfMain.tRefreshTimer(Sender: TObject);
const
  ORIGINAL_INTERVAL = 60000;
begin
  if tRefresh.Interval < ORIGINAL_INTERVAL then
    tRefresh.Interval := ORIGINAL_INTERVAL;

  if Length(fMain.CurrentDrivePath) = 0 then
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
begin
  if lName.Caption = '' then
  begin
    Close;
    exit;
  end;

  Top := Top - (MinimumSize div 2);
end;

procedure TfMain.WMDeviceChange(var Msg: TMessage);
const
  DBT_DEVICEARRIVAL = $8000;
  DBT_DEVICEREMOVECOMPLETE = $8004;
  DBT_DEVNODES_CHANGED = $0007;
  DBT_STORAGE = $0002;
begin
  if ((Msg.WParam = DBT_DEVICEARRIVAL) or
      (Msg.WParam = DBT_DEVICEREMOVECOMPLETE)) and
     (PDevBroadcastHdr(Msg.lParam)^.dbcd_devicetype = DBT_STORAGE) then
    tRefreshList.Enabled := not RefreshListDisabled;
end;

procedure TfMain.CreateBasicObjects;
begin
  Optimizer := TNSTOptimizer.Create;
  SSDLabel := TSSDLabelList.Create;
  PhysicalDriveList := TPhysicalDriveList.Create;
  FirmwareGetter := TFirmwareGetter.Create;
  CreateButtonGroup;
end;

procedure TfMain.CreateButtonGroup;
begin
  ButtonGroup :=
    TButtonGroup.Create(fMain, MaximumSize, MinimumSize,
      ClientWidth, ClientWidth);
end;

procedure TfMain.Erase(const Filename: string);
var
  FullPath: string;
  TempFolder: string;
begin
  FullPath := EnvironmentVariable.AppPath + 'Erase\' + Filename + '.7z';
  if (FileExists(FullPath)) and (Rufus.CheckRufus) then
  begin
    AlertCreate(Self, AlrtStartFormat[CurrLang]);
    TempFolder := EnvironmentVariable.TempFolder(true);
    CreateDir(TempFolder);
    SevenZip.Extract(
      EnvironmentVariable.AppPath + '7z\7z.exe',
      FullPath, TempFolder,
      CapTrimName[LANG_ENGLISH] + CapStartManTrim[LANG_ENGLISH] +
      BtSemiAutoTrim[LANG_ENGLISH] + CapLocalDisk[LANG_ENGLISH] +
      CapRemvDisk[LANG_ENGLISH] + CapProg1[LANG_ENGLISH] +
      CapProg3[LANG_ENGLISH] + CapProg2[LANG_ENGLISH]);
    FullPath := TempFolder + Filename + '.iso';
    Rufus.RunRufus(Copy(cUSBErase.Items[cUSBErase.ItemIndex], 1, 3), FullPath);
    AlertCreate(Self, AlrtEraEnd[CurrLang]);
    DeleteDirectory(TempFolder);
  end
  else
  begin
    AlertCreate(Self, AlrtBootFail[CurrLang]);
    BrowserCreate(Self);
  end;
end;

procedure TfMain.RefreshLabelList;
var
  SSDLabelListRefresher: TSSDLabelListRefresher;
begin
  SSDLabelListRefresher := TSSDLabelListRefresher.Create;
  SSDLabelListRefresher.RefreshDrives(SSDLabel);
  FreeAndNil(SSDLabelListRefresher);
end;

procedure TfMain.IfConnectedStartUpdateThread;
begin
  if IsConnected then
    StartUpdateThread;
end;

procedure TfMain.AddEntryToButtonGroup(iSelected: Boolean; iImageButton: TImage;
  iLabelButton: TLabel; iGroupBox: TGroupBox;
  iClickEventProcedure: TClickEventProcedure);
begin
  ButtonGroup.AddEntry(iSelected, iImageButton, iLabelButton, iGroupBox,
    iClickEventProcedure);
end;

procedure TfMain.ApplyPartitionCountToTrim;
var
  PartitionCount: Integer;
  CurrentPartitionIndex: Integer;
begin
  PartitionCount := 0;
  for CurrentPartitionIndex := 0 to cTrimList.Items.Count - 1 do
    if cTrimList.Checked[CurrentPartitionIndex] then
      PartitionCount := PartitionCount + 1;
  lProgress.Caption := CapProg1[CurrLang] + '0 / ' + IntToStr(PartitionCount);
end;

procedure TfMain.PrepareFormToTrim;
begin
  lDownload.Caption := CapTrimName[CurrLang];
  bCancel.Visible := false;
  lSpeed.Visible := true;
  lSpeed.Font.Color := clRed;
  lSpeed.Font.Style := [fsBold];
  lSpeed.Caption := CapProg2[CurrLang];
  gTrim.Visible := false;
end;

procedure TfMain.StartTrimThread(const PartitionsToTrim: TTrimList);
begin
  TrimThread := TTrimThread.Create(true, true);
  TrimThread.SetPartitionList(PartitionsToTrim);
  TrimThread.Priority := tpLower;
  TrimThread.Start;
end;

procedure TfMain.StartUpdateThread;
begin
  UpdateThread := TUpdateThread.Create(True);
  UpdateThread.Priority := tpLower;
  UpdateThread.Start;
end;

procedure TfMain.TerminateUpdateThread;
begin
  UpdateThread.Terminate;
end;

procedure TfMain.SetIsConnected;
var
  IsConnectedDWORD: Cardinal;
begin
  InternetGetConnectedState(@IsConnectedDWORD, 0);
  IsConnected :=
    (IsConnectedDWORD and INTERNET_CONNECTION_LAN) = INTERNET_CONNECTION_LAN;
end;

procedure TfMain.cTrimRunningClick(Sender: TObject);
var
  SchedResult: String;
begin
  if cTrimRunning.Checked then
    if VersionHelper.Version.FMajorVer = 5 then
    begin
      SchedResult :=
        string(ProcessOpener.OpenProcWithOutput(
          EnvironmentVariable.WinDir + '\System32',
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
        string(ProcessOpener.OpenProcWithOutput(
          EnvironmentVariable.WinDir + '\System32',
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
      string(ProcessOpener.OpenProcWithOutput(
        EnvironmentVariable.WinDir + '\System32',
        'schtasks /delete ' +                       //작업 삭제
        '/TN "MANTRIM' +                            //작업 이름
        PhysicalDrive.IdentifyDeviceResult.Serial +
        '" ' +
        '/F'));                                     //강제 삭제
end;

procedure TfMain.DisableSSDLabel;
var
  CurrentSSDLabel: TSSDLabel;
begin
  for CurrentSSDLabel in SSDLabel do
    CurrentSSDLabel.Enabled := false;
end;

procedure TfMain.EnableSSDLabel;
var
  CurrentSSDLabel: TSSDLabel;
begin
  for CurrentSSDLabel in SSDLabel do
    CurrentSSDLabel.Enabled := true;
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
      UnicodeString(ProcessOpener.OpenProcWithOutput(
        EnvironmentVariable.WinDir + '\System32',
        'schtasks /query'))) > 0;
end;
end.
