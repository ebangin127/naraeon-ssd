unit OS.Partition.Lock;

interface

uses
  OSFile.IoControl, OSFile.Handle, OS.Handle;

type
  TPartitionLock = class(TIoControlFile)
  public
    constructor Create(const FileToGetAccess: String); override;
    procedure Lock;
    procedure Unlock;
    function GetHandle: THandle;
  protected
    function GetMinimumPrivilege: TCreateFileDesiredAccess; override;
  end;

implementation

{ TPartitionLock }

constructor TPartitionLock.Create(const FileToGetAccess: String);
begin
  inherited Create(FileToGetAccess);
  CreateHandle(FileToGetAccess, DesiredReadWrite);
end;

function TPartitionLock.GetHandle: THandle;
begin
  result := GetFileHandle;
end;

function TPartitionLock.GetMinimumPrivilege: TCreateFileDesiredAccess;
begin
  result := TCreateFileDesiredAccess.DesiredReadWrite;
end;

procedure TPartitionLock.Lock;
begin
  IoControl(TIoControlCode.LockVolume, NullOSBuffer);
end;

procedure TPartitionLock.Unlock;
begin
  IoControl(TIoControlCode.UnlockVolume, NullOSBuffer);
end;

end.
