unit Thread.Download;

interface

uses
  Classes, SysUtils,
  Thread.Download.Helper;

type
  TDownloadThread = class(TThread)
  private
    Downloader: TDownloader;
    Request: TDownloadRequest;
    IsRequestSet: Boolean;
    PostDownloadMethod: TThreadMethod;
    IsPostDownloadMethodSet: Boolean;
  protected
    procedure Execute; override;
    procedure PostDownloadMethodWithSynchronization;
  public
    constructor Create; overload;
    constructor Create(CreateSuspended: Boolean); overload;
    destructor Destroy; override;
    procedure SetRequest(Request: TDownloadRequest);
    procedure SetPostDownloadMethod(PostDownloadMethod: TThreadMethod);
  end;

implementation




constructor TDownloadThread.Create;
begin
  Create(true);
end;

constructor TDownloadThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  Downloader := TDownloader.Create;
  IsRequestSet := false;
  IsPostDownloadMethodSet := false;
  FreeOnTerminate := true;
end;

destructor TDownloadThread.Destroy;
begin
  FreeAndNil(Downloader);
  inherited Destroy;
end;

procedure TDownloadThread.Execute;
begin
  if IsRequestSet then
  begin
    Downloader.DownloadFile(Request, self);
    PostDownloadMethodWithSynchronization;
  end;
end;
procedure TDownloadThread.PostDownloadMethodWithSynchronization;
begin
  Synchronize(PostDownloadMethod);
end;

procedure TDownloadThread.SetPostDownloadMethod(
  PostDownloadMethod: TThreadMethod);
begin
  IsPostDownloadMethodSet := true;
  self.PostDownloadMethod := PostDownloadMethod;
end;

procedure TDownloadThread.SetRequest(Request: TDownloadRequest);
begin
  IsRequestSet := true;
  self.Request := Request;
end;

end.
