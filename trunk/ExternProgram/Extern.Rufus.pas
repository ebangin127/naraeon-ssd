unit Extern.Rufus;

interface

uses
  Windows, SysUtils, Messages, Dialogs, IOUtils, Math, ShellAPI,
  OS.EnvironmentVariable, Getter.CodesignVerifier, AsciiCheck;

type
  TRufus = class
  public
    function CheckRufus: Boolean;
    procedure RunRufus(const DestDrive, FromISO: String;
      const IsISOHybrid: Boolean);
    procedure SetRufus(const RufusPath, DriveName, ISOPath: String;
      const IsISOHybrid: Boolean);
    class function Create: TRufus;
  private
    function FindMainWindow: THandle;
    function FindFTCombo(const MWHandle: THandle): THandle;
    function GetFTComboText(const EDTHandle: THandle): String;
    function FindDriveCombo(const MWHandle: THandle): THandle;
    function EditDriveCombo(const EDTHandle: THandle;
      const Text: String): Boolean;
    function FindStartButton(const MWHandle: THandle): THandle;
    function ClickStartButton(const MWHandle, BTHandle: THandle): Boolean;
    function FindConfirmWindow: THandle;
    function FindOKButton(const SWHandle: THandle): THandle;
    function ClickOKButton(const SWHandle, BTHandle: THandle): Boolean;
    function FindCloseButton(const MWHandle: THandle): THandle;
    function ClickCloseButton(const MWHandle, BTHandle: THandle): Boolean;
    procedure SetRufusToHandle(const MWHandle: THandle;
      const DriveName: String; const IsISOHybrid: Boolean);
    function IsValidHandle(const Handle: THandle): Boolean;
    function FindISOHybridWindow: THandle;
    function FindISOHybridOKButton(const SWHandle: THandle): THandle;
    procedure WaitForISOHybrid;
  end;

var
  Rufus: TRufus;
  GlobalISOHybridHandleResult: THandle;

implementation

{ TRufus }

const
  RufusWinCap = 'Rufus 2.10.973';
  RufusWinCap2 = 'Rufus 2.10.973 ';
  ISOHybridStr = 'ISOHybrid';
  MAX_LENGTH = 1024;
  ISOPosition = 5;
  StartPosition = 16;
  ClosePosition = 17;

var
  EnumWindowsWC: Array of WideChar;

function EnumWindowsCallback(Handle: THandle;
  LParamAsParameter: DWORD): Boolean; stdcall;
var
  ReceivedCaptionStr: String;
  SendMessageResult: DWORD;
begin
  if SendMessageTimeout(Handle, WM_GETTEXT,
    MAX_LENGTH, LParam(@EnumWindowsWC[0]), SMTO_ABORTIFHUNG, 100,
    @SendMessageResult) > 0 then
  begin
    ReceivedCaptionStr := PWideChar(EnumWindowsWC);
    if Pos(ISOHybridStr, ReceivedCaptionStr, 1) > 0 then
      GlobalISOHybridHandleResult := Handle;
  end;
  result := true;
end;

function TRufus.CheckRufus: Boolean;
var
  CodesignVerifier: TCodesignVerifier;
begin
  result := FileExists(EnvironmentVariable.AppPath + 'Rufus\rufus.exe');
  if not result then
    exit;

  CodesignVerifier := TCodesignVerifier.Create;
  result := CodesignVerifier.VerifySignByPublisher(
    EnvironmentVariable.AppPath + 'Rufus\rufus.exe', 'Akeo Consulting');
  FreeAndNil(CodesignVerifier);
end;

procedure TRufus.RunRufus(const DestDrive, FromISO: String;
  const IsISOHybrid: Boolean);
var
  CutDestDrive: String;
begin
  CutDestDrive := Copy(DestDrive, 1, 2);

  SetRufus(
    EnvironmentVariable.AppPath + '\Rufus\rufus.exe',
    CutDestDrive, FromISO, Pos('all.iso', FromISO, 1) > 0);

  DeleteFile(FromISO);
end;

function TRufus.IsValidHandle(const Handle: THandle): Boolean;
begin
  result := (Handle <> INVALID_HANDLE_VALUE) and (Handle <> 0);
end;

function TRufus.FindMainWindow: THandle;
begin
  repeat
    Sleep(100);
    result := FindWindow(nil, RufusWinCap);
    if not IsValidHandle(result) then
      result := FindWindow(nil, RufusWinCap2);
  until IsValidHandle(result);
end;

function TRufus.FindFTCombo(const MWHandle: THandle): THandle;
var
  CurrentCount: Integer;
begin
  result := 0;
  for CurrentCount := 0 to ISOPosition do
  begin
    result :=
      FindWindowEx(MWHandle, result, 'ComboBox', nil);
  end;
end;

function TRufus.GetFTComboText(const EDTHandle: THandle): String;
var
  ReceivedCaptionWC: Array of WideChar;
  SendMessageResult: DWORD;
begin
  SetLength(ReceivedCaptionWC, MAX_LENGTH);
  SendMessageTimeout(EDTHandle, WM_GETTEXT,
    MAX_LENGTH, LParam(@ReceivedCaptionWC[0]), SMTO_ABORTIFHUNG, 100,
    @SendMessageResult);
  result := PWideChar(ReceivedCaptionWC);
  SetLength(ReceivedCaptionWC, 0);
end;

function TRufus.FindDriveCombo(const MWHandle: THandle): THandle;
var
  ReceivedCaptionWC: Array of WideChar;
  ReceivedCaptionStr: String;
  SendMessageResult: DWORD;
begin
  result := 0;
  SetLength(ReceivedCaptionWC, MAX_LENGTH);

  repeat
    result :=
      FindWindowEx(MWHandle, result, 'ComboBox', nil);
    SendMessageTimeout(result, WM_GETTEXT,
      MAX_LENGTH, LParam(@ReceivedCaptionWC[0]), SMTO_ABORTIFHUNG, 100,
      @SendMessageResult);
    ReceivedCaptionStr := PWideChar(ReceivedCaptionWC);
  until Pos(':)', ReceivedCaptionStr) > 0;
end;

function TRufus.EditDriveCombo(const EDTHandle: THandle;
  const Text: String): Boolean;
var
  ReceivedCaptionWC: Array of WideChar;
  ReceivedCaptionStr: String;
  CurrItem, AllCount: Integer;
  UpperCaseText: String;
  SendMessageResult: DWORD;
begin
  UpperCaseText := UpperCase(Text);

  SendMessageTimeout(EDTHandle, WM_LBUTTONDOWN, 0, 0, SMTO_ABORTIFHUNG, 100,
    @SendMessageResult);
  Sleep(0);
  SendMessageTimeout(EDTHandle, WM_LBUTTONUP, 0, 0, SMTO_ABORTIFHUNG, 100,
    @SendMessageResult);
  Sleep(0);
  SendMessageTimeout(EDTHandle, WM_LBUTTONDOWN, 0, 0, SMTO_ABORTIFHUNG, 100,
    @SendMessageResult);
  Sleep(0);
  SendMessageTimeout(EDTHandle, WM_LBUTTONUP, 0, 0, SMTO_ABORTIFHUNG, 100,
    @SendMessageResult);

  AllCount := SendMessageTimeout(EDTHandle, CB_GETCOUNT, 0, 0, SMTO_ABORTIFHUNG,
    100, @SendMessageResult);
  SetLength(ReceivedCaptionWC, MAX_LENGTH);

  for CurrItem := 0 to AllCount - 1 do
  begin
    SendMessageTimeout(EDTHandle, CB_SETCURSEL, CurrItem, 0, SMTO_ABORTIFHUNG,
      100, @SendMessageResult);
    SendMessageTimeout(EDTHandle, CB_GETITEMDATA, CurrItem, 0, SMTO_ABORTIFHUNG,
      100, @SendMessageResult);
    SendMessageTimeout(EDTHandle, WM_GETTEXT,
      MAX_LENGTH, LParam(@ReceivedCaptionWC[0]), SMTO_ABORTIFHUNG,
      100, @SendMessageResult);
    ReceivedCaptionStr := PWideChar(ReceivedCaptionWC);

    if Pos(UpperCaseText, ReceivedCaptionStr) > 0 then
      break;

    SendMessageTimeout(EDTHandle, WM_KEYDOWN, VK_DOWN, 0, SMTO_ABORTIFHUNG,
      100, @SendMessageResult);
    SendMessageTimeout(EDTHandle, WM_KEYUP, VK_DOWN, 0, SMTO_ABORTIFHUNG,
      100, @SendMessageResult);
  end;
  result := GetLastError = 0;
end;

function TRufus.FindStartButton(const MWHandle: THandle): THandle;
var
  CurrentCount: Integer;
begin
  result := 0;

  for CurrentCount := 0 to StartPosition do
  begin
    result :=
      FindWindowEx(MWHandle, result, 'Button', nil);
  end;
end;

function TRufus.ClickStartButton(const MWHandle, BTHandle: THandle): Boolean;
begin
  exit(
    PostMessage(MWHandle, WM_COMMAND,
      MakeLong(GetDlgCtrlID(BTHandle), BN_CLICKED), LPARAM(BTHandle)));
end;

class function TRufus.Create: TRufus;
begin
  if Rufus = nil then
    result := inherited Create as self
  else
    result := Rufus;
end;

function TRufus.FindConfirmWindow: THandle;
begin
  repeat
    Sleep(100);
    result := FindWindow(Nil, 'Rufus');
  until (result <> INVALID_HANDLE_VALUE) and
        (result <> 0);
end;

function TRufus.FindISOHybridWindow: THandle;
begin
  GlobalISOHybridHandleResult := INVALID_HANDLE_VALUE;
  SetLength(EnumWindowsWC, MAX_LENGTH);
  ZeroMemory(EnumWindowsWC, MAX_LENGTH);
  while GlobalISOHybridHandleResult = INVALID_HANDLE_VALUE do
    EnumWindows(@EnumWindowsCallback, 0);
  result := GlobalISOHybridHandleResult;
end;

function TRufus.FindISOHybridOKButton(const SWHandle: THandle): THandle;
begin
  result :=
    FindWindowEx(SWHandle, 0, 'Button', nil);
  result :=
    FindWindowEx(SWHandle, result, 'Button', nil);
end;

procedure TRufus.WaitForISOHybrid;
var
  ISOHybridHandleResult: THandle;
  OKButtonHandle: THandle;
begin
  ISOHybridHandleResult := FindISOHybridWindow;
  while ISOHybridHandleResult <> INVALID_HANDLE_VALUE do
  begin
    OKButtonHandle := FindISOHybridOKButton(ISOHybridHandleResult);
    ClickOKButton(ISOHybridHandleResult, OKButtonHandle);
    GlobalISOHybridHandleResult := INVALID_HANDLE_VALUE;
    ZeroMemory(EnumWindowsWC, MAX_LENGTH);
    EnumWindows(@EnumWindowsCallback, 0);
    ISOHybridHandleResult := GlobalISOHybridHandleResult;
  end;
end;

function TRufus.FindOKButton(const SWHandle: THandle): THandle;
begin
  result :=
    FindWindowEx(SWHandle, 0, 'Button', nil);
end;

function TRufus.ClickOKButton(const SWHandle, BTHandle: THandle): Boolean;
begin
  exit(
    PostMessage(SWHandle, WM_COMMAND,
      MakeLong(GetDlgCtrlID(BTHandle), BN_CLICKED), LPARAM(BTHandle)));
end;

function TRufus.FindCloseButton(const MWHandle: THandle): THandle;
var
  CurrentCount: Integer;
begin
  result := 0;

  for CurrentCount := 0 to ClosePosition do
  begin
    result :=
      FindWindowEx(MWHandle, result, 'Button', nil);
  end;
end;

function TRufus.ClickCloseButton(const MWHandle, BTHandle: THandle): Boolean;
begin
  exit(
    PostMessage(MWHandle, WM_COMMAND,
      MakeLong(GetDlgCtrlID(BTHandle), BN_CLICKED), LPARAM(BTHandle)));
end;

procedure TRufus.SetRufusToHandle(const MWHandle: THandle;
  const DriveName: String; const IsISOHybrid: Boolean);
var
  FileTypeComboHandle: THandle;
  DriveComboHandle: THandle;
  StartButtonHandle: THandle;
  SWHandle: THandle;
  OKButtonHandle: THandle;
  CloseButtonHandle: THandle;
begin
  repeat
    FileTypeComboHandle := FindFTCombo(MWHandle);
    Sleep(10);
  until Pos('ISO', GetFTComboText(FileTypeComboHandle), 1) > 0;
  DriveComboHandle := FindDriveCombo(MWHandle);
  EditDriveCombo(DriveComboHandle, DriveName);
  StartButtonHandle := FindStartButton(MWHandle);
  ClickStartButton(MWHandle, StartButtonHandle);
  if IsISOHybrid then
    WaitForISOHybrid;
  repeat
    SWHandle := FindConfirmWindow;
    OKButtonHandle := FindOKButton(SWHandle);
    ClickOKButton(SWHandle, OKButtonHandle);
    Sleep(100);
  until not IsWindowEnabled(DriveComboHandle);
  repeat
    Sleep(100);
  until IsWindowEnabled(DriveComboHandle);
  CloseButtonHandle := FindCloseButton(MWHandle);
  ClickCloseButton(MWHandle, CloseButtonHandle);
end;

procedure TRufus.SetRufus(const RufusPath, DriveName, ISOPath: String;
  const IsISOHybrid: Boolean);
var
  MWHandle: THandle;
begin
  if not StringHelper.IsAscii(ISOPath) then
    raise EInvalidArgument.Create(
      'Invalid argument: Don''t use unicode to rufus');

  ShellExecute(0, 'open',
    PChar(RufusPath),
    PChar('--iso="' + ISOPath + '"'),
    PChar(ExtractFilePath(RufusPath)), SW_NORMAL);

  MWHandle := FindMainWindow;
  SetRufusToHandle(MWHandle, DriveName, IsISOHybrid);
end;

initialization
  Rufus := TRufus.Create;
finalization
  Rufus.Free;
end.
