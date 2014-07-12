unit uMessage;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Vcl.ExtCtrls, uSSDInfo, uSMARTFunctions;

procedure MsgboxCreate(Sender: TForm; FileName: String);

type
  TfMessage = class(TForm)
    MMessage: TMemo;
    tLogger: TTimer;
    procedure FormResize(Sender: TObject);
    procedure tLoggerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    LogDevice: String;
  end;

var
  fMessage: TfMessage;
  Logger: Array[2..3] of UInt64;

implementation

{$R *.dfm}

procedure MsgboxCreate(Sender: TForm; FileName: String);
begin
  if fMessage <> Nil then FreeAndNil(fMessage);
  try
    fMessage := TfMessage.Create(Sender);
    fMessage.MMessage.Lines.LoadFromFile(FileName);
    DeleteFile(FileName);
    fMessage.ShowModal;
  finally
    FreeAndNil(fMessage);
  end;
  Logger[2] := 0;
  Logger[3] := 0;
end;

procedure TfMessage.FormResize(Sender: TObject);
begin
  MMessage.Width := ClientWidth - 16;
  MMessage.Height := ClientHeight - 16;
end;

procedure TfMessage.tLoggerTimer(Sender: TObject);
var
  TempSSDInfo: TSSDInfo_NST;
  i: integer;
begin
  TempSSDInfo := TSSDInfo_NST.Create;
  for i := 2 to 3 do
  begin
    TempSSDInfo.ATAorSCSI := DetermineModel;
    TempSSDInfo.SetDeviceName(i);
    TempSSDInfo.CollectAllSmartData;
    if Logger[i] <> ExtractSMART(TempSSDInfo.SMARTData, 9) then
    begin
      MMessage.Lines.Add('[' + FormatDateTime('MM/DD hh:mm:ss', Now) + ']' + TempSSDInfo.Model + '/' + UIntToStr(ExtractSMART(TempSSDInfo.SMARTData, 9)));
      Logger[i] := ExtractSMART(TempSSDInfo.SMARTData, 9);
    end;
  end;
  FreeAndNil(TempSSDInfo);
end;

end.
