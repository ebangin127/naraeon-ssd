unit uRufus;

interface

uses
  Windows, SysUtils, Messages, Dialogs, IOUtils, Math, ShellAPI,
  uPathManager;

type
  TRufus = class
  public
    class function CheckRufus: Boolean;
    class procedure SetRufus
      (RufusPath, DriveName, ISOPath: String);
  private
    class function FindMainWindow: THandle;

    class function FindFTCombo(MWHandle: THandle): THandle;
    class function GetFTComboText(EDTHandle: THandle): String;

    class function FindDriveCombo(MWHandle: THandle): THandle;
    class function EditDriveCombo(EDTHandle: THandle; Text: String): Boolean;

    class function FindStartButton(MWHandle: THandle): THandle;
    class function ClickStartButton(MWHandle, BTHandle: THandle): Boolean;

    class function FindSubWindow(MWHandle: THandle): THandle;
    class function FindOKButton(SWHandle: THandle): THandle;
    class function ClickOKButton(SWHandle, BTHandle: THandle): Boolean;

    class function FindCloseButton(MWHandle: THandle): THandle;
    class function ClickCloseButton(MWHandle, BTHandle: THandle): Boolean;
  end;

implementation

{ TRufus }

const
  RufusWinCap = 'Rufus 1.4.12.535';
  MAX_LENGTH = 1024;
  ComboDefaultText = 'FreeDOS';
  ComboISOText = 'ISO Image';


class function TRufus.CheckRufus: Boolean;
begin
  result := FileExists(TPathManager.AppPath + 'Rufus\rufus.exe');
end;

class function TRufus.FindMainWindow: THandle;
begin
  repeat
    Sleep(100);
    result := FindWindow(Nil, RufusWinCap);
  until (result <> INVALID_HANDLE_VALUE) and
        (result <> 0);
end;

class function TRufus.FindFTCombo(MWHandle: THandle): THandle;
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

class function TRufus.GetFTComboText(EDTHandle: THandle): String;
var
  ReceivedCaptionWC: Array of WideChar;
begin
  SetLength(ReceivedCaptionWC, MAX_LENGTH);
  SendMessage(EDTHandle, WM_GETTEXT,
    MAX_LENGTH, LParam(@ReceivedCaptionWC[0]));
  result := PWideChar(ReceivedCaptionWC);
  SetLength(ReceivedCaptionWC, 0);
end;

class function TRufus.FindDriveCombo(MWHandle: THandle): THandle;
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

class function TRufus.EditDriveCombo(EDTHandle: THandle; Text: String): Boolean;
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

class function TRufus.FindStartButton(MWHandle: THandle): THandle;
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

class function TRufus.ClickStartButton(MWHandle, BTHandle: THandle): Boolean;
begin
  exit(
    PostMessage(MWHandle, WM_COMMAND,
      MakeLong(GetDlgCtrlID(BTHandle), BN_CLICKED), LPARAM(BTHandle)));
end;

class function TRufus.FindSubWindow(MWHandle: THandle): THandle;
begin
  repeat
    Sleep(100);
    result := FindWindow(Nil, 'Rufus');
  until (result <> INVALID_HANDLE_VALUE) and
        (result <> 0);
end;

class function TRufus.FindOKButton(SWHandle: THandle): THandle;
begin
  result :=
    FindWindowEx(SWHandle, 0, 'Button', nil);
end;

class function TRufus.ClickOKButton(SWHandle, BTHandle: THandle): Boolean;
begin
  exit(
    PostMessage(SWHandle, WM_COMMAND,
      MakeLong(GetDlgCtrlID(BTHandle), BN_CLICKED), LPARAM(BTHandle)));
end;

class function TRufus.FindCloseButton(MWHandle: THandle): THandle;
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

class function TRufus.ClickCloseButton(MWHandle, BTHandle: THandle): Boolean;
begin
  exit(
    PostMessage(MWHandle, WM_COMMAND,
      MakeLong(GetDlgCtrlID(BTHandle), BN_CLICKED), LPARAM(BTHandle)));
end;

class procedure TRufus.SetRufus
  (RufusPath, DriveName, ISOPath: String);
var
  MWHandle, SWHandle: THandle;
  DriveComboHandle,
  FileTypeComboHandle: THandle;
  StartButtonHandle,
  CloseButtonHandle,
  OKButtonHandle: THandle;
begin
  ShellExecute(0, 'open',
    PChar(RufusPath),
    PChar('-l en_US --iso="' + ISOPath + '"')
    //한글경로 안됨.
    , nil, SW_NORMAL);

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

end.
