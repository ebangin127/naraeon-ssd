unit uDownloader;

interface

uses
  SysUtils, Classes, Windows, IdHttp, IdComponent,
  uDatasizeUnit, uTimeUnit, uDeleteDirectory, uDownloadPath, uProgressSection,
  uLanguageSettings;

type
  TDownloadModelStrings = record
    Download: String;
    Cancel: String;
  end;

  TDownloadRequest = record
    Source: TDownloadFile;
    Destination: TDownloadFile;
    DownloadModelStrings: TDownloadModelStrings;
  end;

  TDownloader = class
  private
    type
      TRAWDownloadRequest = record
        Source: String;
        Destination: String;
      end;
  private
    ThreadToSynchronize: TThread;
    OriginalRequest: TDownloadRequest;
    RAWRequest: TRAWDownloadRequest;
    Aborted: Boolean;
    DestinationFileStream: TFileStream;
    Downloader: TIdHttp;
    SourceSize: Int64;
    ProgressSynchronizer: TProgressSection;
    LastTick: Cardinal;
    LastWorkCount: Int64;
    procedure SetRAWDownloadRequest;
    procedure SetMainformCaption;
    procedure SynchronizedSetMainformCaption;
    procedure CreateDownloader;
    procedure CreateDestinationFileStream;
    procedure FreeDownloader;
    procedure FreeDestinationFileStream;
    procedure TryToDownload;
    procedure DownloaderWork(Sender: TObject; AWorkMode: TWorkMode;
      AWorkCount: Int64);
    procedure CreateProgressSynchronizer;
    procedure FreeProgressSynchronizer;
    function GetElapsedTick: Cardinal;
    function GetAddedByte(CurrentWorkCount: Int64): Cardinal;
    function GetProgressCaption(CurrentWorkCount: Int64): String;
    function GetLeftTimeInSecond(RemainingByte, BytePerSecond: Int64):
      Int64;
    function GetBytePerSecond(AddedByte, ElapsedTick: Int64): Int64;
    procedure StartDownload;
    procedure PrepareDownload;
    procedure SetCancelAbort;
    procedure SynchronizedSetCancelAbort;
  public
    procedure DownloadFile(DownloadRequest: TDownloadRequest;
      ThreadToSynchronize: TThread);
    procedure Abort(Sender: TObject);
  end;

implementation

uses
  uMain;

procedure TDownloader.SetRAWDownloadRequest;
begin
  RAWRequest.Source := GetDownloadPath(OriginalRequest.Source);
  RAWRequest.Destination := GetDownloadPath(OriginalRequest.Destination);
end;

procedure TDownloader.SetMainformCaption;
begin
  if ThreadToSynchronize <> nil then
    TThread.Synchronize(ThreadToSynchronize, SynchronizedSetMainformCaption)
  else
    SynchronizedSetMainformCaption;
end;

procedure TDownloader.SynchronizedSetMainformCaption;
begin
  fMain.lDownload.Caption := OriginalRequest.DownloadModelStrings.Download;
  fMain.bCancel.Caption := OriginalRequest.DownloadModelStrings.Cancel;
end;

procedure TDownloader.Abort(Sender: TObject);
begin
  Aborted := true;
  Downloader.Disconnect;
  FreeDestinationFileStream;
  DeleteFile(PChar(RAWRequest.Destination));
  DeleteDirectory(ExtractFileDir(RAWRequest.Destination));
end;

procedure TDownloader.CreateDestinationFileStream;
begin
  DestinationFileStream := TFileStream.Create(RAWRequest.Destination,
    fmCreate or fmShareExclusive);
end;

procedure TDownloader.CreateDownloader;
begin
  Downloader := TIdHttp.Create;
  Downloader.Request.UserAgent := 'Naraeon SSD Tools';
  Downloader.HandleRedirects := true;
end;

procedure TDownloader.CreateProgressSynchronizer;
begin
  ProgressSynchronizer := TProgressSection.Create(ThreadToSynchronize);
end;

procedure TDownloader.FreeProgressSynchronizer;
begin
  FreeAndNil(ProgressSynchronizer);
end;

procedure TDownloader.TryToDownload;
begin
  Aborted := false;
  SourceSize := 0;
  LastTick := 0;
  LastWorkCount := 0;
  Downloader.OnWork := DownloaderWork;
  Downloader.Head(RAWRequest.Source);
  SourceSize := Downloader.response.ContentLength;

  ProgressSynchronizer.ShowProgress;
  Downloader.Get(RAWRequest.Source, DestinationFileStream);
  ProgressSynchronizer.HideProgress;
end;

procedure TDownloader.PrepareDownload;
begin
  SetRAWDownloadRequest;
  SetMainformCaption;
  SetCancelAbort;
  CreateDestinationFileStream;
  CreateDownloader;
  CreateProgressSynchronizer;
end;

procedure TDownloader.StartDownload;
begin
  try
    TryToDownload;
  finally
    FreeDestinationFileStream;
    FreeDownloader;
    FreeProgressSynchronizer;
  end;
end;

procedure TDownloader.DownloadFile(DownloadRequest: TDownloadRequest;
  ThreadToSynchronize: TThread);
begin
  self.OriginalRequest := DownloadRequest;
  self.ThreadToSynchronize := ThreadToSynchronize;
  PrepareDownload;
  StartDownload;
end;

procedure TDownloader.SynchronizedSetCancelAbort;
begin
  fMain.bCancel.OnClick := Abort;
end;

procedure TDownloader.SetCancelAbort;
begin
  TThread.Synchronize(ThreadToSynchronize, SynchronizedSetCancelAbort);
end;

procedure TDownloader.FreeDestinationFileStream;
begin
  if DestinationFileStream <> nil then
    FreeAndNil(DestinationFileStream);
end;

procedure TDownloader.FreeDownloader;
begin
  FreeAndNil(Downloader);
end;

function TDownloader.GetElapsedTick: Cardinal;
var
  CurrentTick: Cardinal;
begin
  CurrentTick := GetTickCount;
  if CurrentTick < LastTick then
    result := 1
  else
    result := CurrentTick - LastTick;
  LastTick := CurrentTick;
end;

function TDownloader.GetAddedByte(CurrentWorkCount: Int64): Cardinal;
begin
  if CurrentWorkCount < LastWorkCount then
    result := 1
  else
    result := CurrentWorkCount - LastWorkCount;
  LastWorkCount := CurrentWorkCount;
end;

function TDownloader.GetProgressCaption(CurrentWorkCount: Int64): String;
var
  BinaryBtoMB: TDatasizeUnitChangeSetting;
begin
  BinaryBtoMB.FNumeralSystem := Binary;
  BinaryBtoMB.FFromUnit := ByteUnit;
  BinaryBtoMB.FToUnit := MegaUnit;

  result := CapProg1[CurrLang];
  result := result +
    Format('%.1fMB', [ChangeDatasizeUnit(CurrentWorkCount, BinaryBtoMB)]) +
    ' / ';
  result := result +
    Format('%.1fMB', [ChangeDatasizeUnit(SourceSize, BinaryBtoMB)]);
  result := result +
    ' (' + IntToStr(Round((CurrentWorkCount / SourceSize) * 100)) + '%)';
end;

function TDownloader.GetBytePerSecond(AddedByte, ElapsedTick: Int64):
  Int64;
const
  TickToSecond = 1000;
begin
  if ElapsedTick > 0 then
    result := round(AddedByte / (ElapsedTick / TickToSecond))
  else
    result := 0;
end;

function TDownloader.GetLeftTimeInSecond(RemainingByte, BytePerSecond: Int64):
  Int64;
begin
  result := 0;
  if BytePerSecond > 0 then
    result :=
      round((SourceSize - RemainingByte) / BytePerSecond);
end;

procedure TDownloader.DownloaderWork(Sender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
var
  ProgressToApply: TProgressToApply;
  BytePerSecond: Int64;
  RemainingCount: Int64;
  ElapsedTick: Int64;
  LeftTimeInSecond: Double;
begin
  if (AWorkMode <> wmRead) then
    exit;

  ProgressToApply.ProgressValue := Round((AWorkCount / SourceSize) * 100);
  ProgressToApply.ProgressCaption := GetProgressCaption(AWorkCount);
  ElapsedTick := GetElapsedTick;
  BytePerSecond := GetBytePerSecond(GetAddedByte(AWorkCount), ElapsedTick);
  if (ElapsedTick <= 0) or (BytePerSecond <= 0) then
    exit;
  RemainingCount := SourceSize - AWorkCount;
  LeftTimeInSecond := GetLeftTimeInSecond(RemainingCount, BytePerSecond);
  ProgressToApply.SpeedCaption := CapTime[CurrLang] +
    FormatTimeInSecond(LeftTimeInSecond);

  ProgressSynchronizer.ChangeProgress(ProgressToApply);
end;

end.
