unit uProgressGroup;

interface

uses
  IdHttp, IdComponent,
  uDownloadPath;

type
  TProgressGroup = class
  public
    procedure Abort;
  private
    FAborted: Boolean;
    FFileSizeInByte: Int64;
    FPreviousPosition: Integer;

    //�ٿ��ε� ����
    procedure Disconnect(Sender: TObject);
    function DownloadFile(Src: TDownloadFile; Dest: TDownloadFile;
      DownloadCaption, CancelCaption: String): Boolean;
    procedure DownloaderWork(Sender: TObject; CurrentWorkMode: TWorkMode;
      CurrentDownloadedSizeInByte: Int64);
    procedure ProgressDownload;
  end;

implementation

uses uMain;

procedure TProgressGroup.Abort;
begin
  FAborted := true;
end;

procedure TProgressGroup.Disconnect(Sender: TObject);
begin
  TIdHttp(Sender).Disconnect;
end;

procedure TProgressGroup.DownloaderWork(Sender: TObject;
  CurrentWorkMode: TWorkMode; CurrentDownloadedSizeInByte: Int64);
var
  CurrentPosition: Integer;
  LeftSec: Int64;
begin
  if FAborted then
    Disconnect(Sender);

  if CurrentWorkMode <> wmRead then
    exit;

  CurrentPosition :=
    Round((CurrentDownloadedSizeInByte / FFileSizeInByte) * 100);

  if FPreviousPosition = CurrentPosition then
    exit;

  GetTBStr(1024, [FFileSizeInByte / 1024 / 1024]);

  CurrDwldCount := FFileSizeInByte;
  pDownload.Position := CurrentPosition;
  lProgress.Caption := CapProg1[CurrLang] +
    Format('%.1fMB', [FFileSize / 1024 / 1024]) + ' / ' +
    Format('%.1fMB', [Max / 1024 / 1024]) +
    ' (' + IntToStr(Round((FFileSize / Max) * 100)) + '%)';

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

function TProgressGroup.DownloadFile(Src: TDownloadFile; Dest: TDownloadFile;
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

  FPreviousPosition := 0;
  FAborted := false;

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

procedure TProgressGroup.ProgressDownload;
begin

end;

end.
