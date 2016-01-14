unit Extern.Rufus;

interface

uses
  Windows, SysUtils, Messages, Dialogs, IOUtils, Math, ShellAPI,
  uPathManager, Getter.CodesignVerifier, AsciiCheck;

type
  TRufus = class
  public
    function CheckRufus: Boolean;
    procedure RunRufus(DestDrive, FromISO: String);
    procedure SetRufus
      (RufusPath, DriveName, ISOPath: String);
    class function Create: TRufus;

  private
    function FindMainWindow: THandle;

    function FindFTCombo(MWHandle: THandle): THandle;
    function GetFTComboText(EDTHandle: THandle): String;

    function FindDriveCombo(MWHandle: THandle): THandle;
    function EditDriveCombo(EDTHandle: THandle; Text: String): Boolean;

    function FindStartButton(MWHandle: THandle): THandle;
    function ClickStartButton(MWHandle, BTHandle: THandle): Boolean;

    function FindSubWindow(MWHandle: THandle): THandle;
    function FindOKButton(SWHandle: THandle): THandle;
    function ClickOKButton(SWHandle, BTHandle: THandle): Boolean;

    function FindCloseButton(MWHandle: THandle): THandle;
    function ClickCloseButton(MWHandle, BTHandle: THandle): Boolean;
  end;

var
  Rufus: TRufus;

implementation

{ TRufus }

const
  RufusWinCap = 'Rufus 2.5.799';
  MAX_LENGTH = 1024;
  ComboDefaultText = 'FreeDOS';
  ComboISOText = 'ISO Image';

function TRufus.CheckRufus: Boolean;
var
  CodesignVerifier: TCodesignVerifier;
begin
  result := FileExists(PathManager.AppPath + 'Rufus\rufus.exe');
  if not result then
    exit;

  CodesignVerifier := TCodesignVerifier.Create;
  result := CodesignVerifier.VerifySignByPublisher(
    PathManager.AppPath + 'Rufus\rufus.exe', 'Akeo Consulting');
  FreeAndNil(CodesignVerifier);
end;

procedure TRufus.RunRufus(DestDrive, FromISO: String);
begin
  DestDrive := Copy(DestDrive, 1, 2);

  SetRufus(
    PathManager.AppPath + '\Rufus\rufus.exe',
    DestDrive, FromISO);

  DeleteFile(FromISO);
end;

function TRufus.FindMainWindow: THandle;
begin
  repeat
    Sleep(100);
    result := FindWindow(Nil, RufusWinCap);
  until (result <> INVALID_HANDLE_VALUE) and
        (result <> 0);
end;

function TRufus.FindFTCombo(MWHandle: THandle): THandle;
var
  ReceivedCaptionWC: Array of WideChar;
  ReceivedCaptionStr: String;
begin
  result := 0;
  SetLength(ReceivedCaptionWC, MAX_LENGTH);

  repeat
    result :=
      FindWindowEx(MWHandle, result, 'ComboBox', nil);
    SendMessage(result, WM_GETTEXT,
      MAX_LENGTH, LParam(@ReceivedCaptionWC[0]));
    ReceivedCaptionStr := PWideChar(ReceivedCaptionWC);
  until (ReceivedCaptionStr = ComboDefaultText) or
        (ReceivedCaptionStr = ComboISOText);
end;

function TRufus.GetFTComboText(EDTHandle: THandle): String;
var
  ReceivedCaptionWC: Array of WideChar;
begin
  SetLength(ReceivedCaptionWC, MAX_LENGTH);
  SendMessage(EDTHandle, WM_GETTEXT,
    MAX_LENGTH, LParam(@ReceivedCaptionWC[0]));
  result := PWideChar(ReceivedCaptionWC);
  SetLength(ReceivedCaptionWC, 0);
end;

function TRufus.FindDriveCombo(MWHandle: THandle): THandle;
var
  ReceivedCaptionWC: Array of WideChar;
  ReceivedCaptionStr: String;
begin
  result := 0;
  SetLength(ReceivedCaptionWC, MAX_LENGTH);

  repeat
    result :=
      FindWindowEx(MWHandle, result, 'ComboBox', nil);
    SendMessage(result, WM_GETTEXT,
      MAX_LENGTH, LParam(@ReceivedCaptionWC[0]));
    ReceivedCaptionStr := PWideChar(ReceivedCaptionWC);
  until Pos(':)', ReceivedCaptionStr) > 0;
end;

function TRufus.EditDriveCombo(EDTHandle: THandle; Text: String): Boolean;
var
  ReceivedCaptionWC: Array of WideChar;
  ReceivedCaptionStr: String;
  CurrItem, AllCount: Integer;
begin
  Text := UpperCase(Text);

  SendMessage(EDTHandle, WM_LBUTTONDOWN, 0, 0);
  Sleep(0);
  SendMessage(EDTHandle, WM_LBUTTONUP, 0, 0);
  Sleep(0);
  SendMessage(EDTHandle, WM_LBUTTONDOWN, 0, 0);
  Sleep(0);
  SendMessage(EDTHandle, WM_LBUTTONUP, 0, 0);

  AllCount := SendMessage(EDTHandle, CB_GETCOUNT, 0, 0);
  SetLength(ReceivedCaptionWC, MAX_LENGTH);

  for CurrItem := 0 to AllCount - 1 do
  begin
    SendMessage(EDTHandle, CB_SETCURSEL, CurrItem, 0);
    SendMessage(EDTHandle, CB_GETITEMDATA, CurrItem, 0);
    SendMessage(EDTHandle, WM_GETTEXT,
      MAX_LENGTH, LParam(@ReceivedCaptionWC[0]));
    ReceivedCaptionStr := PWideChar(ReceivedCaptionWC);

    if Pos(Text, ReceivedCaptionStr) > 0 then
      break;

    SendMessage(EDTHandle, WM_KEYDOWN, VK_DOWN, 0);
    SendMessage(EDTHandle, WM_KEYUP, VK_DOWN, 0);
  end;
  result := GetLastError = 0;
end;

function TRufus.FindStartButton(MWHandle: THandle): THandle;
var
  ReceivedCaptionWC: Array of WideChar;
  ReceivedCaptionStr: String;
begin
  result := 0;
  SetLength(ReceivedCaptionWC, MAX_LENGTH);

  repeat
    result :=
      FindWindowEx(MWHandle, result, 'Button', nil);
    SendMessage(result, WM_GETTEXT,
      MAX_LENGTH, LParam(@ReceivedCaptionWC[0]));
    ReceivedCaptionStr := PWideChar(ReceivedCaptionWC);
  until ReceivedCaptionStr = 'Start';
end;

function TRufus.ClickStartButton(MWHandle, BTHandle: THandle): Boolean;
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

function TRufus.FindSubWindow(MWHandle: THandle): THandle;
begin
  repeat
    Sleep(100);
    result := FindWindow(Nil, 'Rufus');
  until (result <> INVALID_HANDLE_VALUE) and
        (result <> 0);
end;

function TRufus.FindOKButton(SWHandle: THandle): THandle;
begin
  result :=
    FindWindowEx(SWHandle, 0, 'Button', nil);
end;

function TRufus.ClickOKButton(SWHandle, BTHandle: THandle): Boolean;
begin
  exit(
    PostMessage(SWHandle, WM_COMMAND,
      MakeLong(GetDlgCtrlID(BTHandle), BN_CLICKED), LPARAM(BTHandle)));
end;

function TRufus.FindCloseButton(MWHandle: THandle): THandle;
var
  ReceivedCaptionWC: Array of WideChar;
  ReceivedCaptionStr: String;
begin
  result := 0;
  SetLength(ReceivedCaptionWC, MAX_LENGTH);

  repeat
    result :=
      FindWindowEx(MWHandle, result, 'Button', nil);
    SendMessage(result, WM_GETTEXT,
      MAX_LENGTH, LParam(@ReceivedCaptionWC[0]));
    ReceivedCaptionStr := PWideChar(ReceivedCaptionWC);
  until ReceivedCaptionStr = 'Close';
end;

function TRufus.ClickCloseButton(MWHandle, BTHandle: THandle): Boolean;
begin
  exit(
    PostMessage(MWHandle, WM_COMMAND,
      MakeLong(GetDlgCtrlID(BTHandle), BN_CLICKED), LPARAM(BTHandle)));
end;

procedure TRufus.SetRufus
  (RufusPath, DriveName, ISOPath: String);
var
  MWHandle, SWHandle: THandle;
  DriveComboHandle,
  FileTypeComboHandle: THandle;
  StartButtonHandle,
  CloseButtonHandle,
  OKButtonHandle: THandle;
begin
  if not StringHelper.IsAscii(ISOPath) then
    raise EInvalidArgument.Create(
      'Invalid argument: Don''t use unicode to rufus');

  ShellExecute(0, 'open',
    PChar(RufusPath),
    PChar('-l en_US --iso="' + ISOPath + '"'),
    nil, SW_NORMAL);

  MWHandle := FindMainWindow;
  FileTypeComboHandle := FindFTCombo(MWHandle);
  repeat
    Sleep(10);
  until GetFTComboText(FileTypeComboHandle) = ComboISOText;

  DriveComboHandle := FindDriveCombo(MWHandle);
  EditDriveCombo(DriveComboHandle, DriveName);

  StartButtonHandle := FindStartButton(MWHandle);
  ClickStartButton(MWHandle, StartButtonHandle);

  SWHandle := FindSubWindow(MWHandle);
  OKButtonHandle := FindOKButton(SWHandle);
  ClickOKButton(SWHandle, OKButtonHandle);

  repeat
    Sleep(100);
  until IsWindowEnabled(DriveComboHandle);

  CloseButtonHandle := FindCloseButton(MWHandle);
  ClickCloseButton(MWHandle, CloseButtonHandle);
end;

initialization
  Rufus := TRufus.Create;
finalization
  Rufus.Free;
end.
