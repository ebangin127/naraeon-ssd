unit uMotherDriveGetter;

interface

uses
  Windows, SysUtils, Generics.Collections,
  uOSFileWithHandle, uIoControlFile;

type
  TMotherDriveEntry = record
    DriveNumber: DWORD;
    StartingOffset: TLargeInteger;
    ExtentLength: TLargeInteger;
  end;

  TMotherDriveList = TList<TMotherDriveEntry>;

  TMotherDriveGetter = class sealed(TIoControlFile)
  public
    constructor Create(FileToGetAccess: String); override;

    function GetMotherDriveList: TMotherDriveList;

  protected
    function GetMinimumPrivilege: TCreateFileDesiredAccess; reintroduce;

  private
    MotherDriveList: TMotherDriveList;
    VolumeName: String;

    type
      TVolumeNameBuffer = Array[0..MAX_PATH] of Char;

      DISK_EXTENT = TMotherDriveEntry;

      VOLUME_DISK_EXTENTS = record
        NumberOfDiskExtents: DWORD;
        Extents: Array[0..50] of DISK_EXTENT;
      end;

    function IsRAMDrive: Boolean;
    procedure IfRAMDriveRaiseException;
    function GetMotherDrive: TMotherDriveList;
    procedure GetMotherDriveAndIfNotReturnedRaiseException(
      IOBuffer: TIoControlIOBuffer);
    function SetIOBufferToGetMotherDrive(
      OutputBufferPointer: Pointer): TIoControlIOBuffer;
    procedure ExtentsToTMotherDriveList(DiskExtents: VOLUME_DISK_EXTENTS);

    procedure SetVolumeNameBuffer;
    function QueryDosDeviceSystemCall
      (VolumePath: String; VolumeNameBuffer: TVolumeNameBuffer): String;
  end;

  ERAMDrive = class(Exception);

implementation

{ TMotherDrive }

constructor TMotherDriveGetter.Create(FileToGetAccess: String);
begin
  inherited Create(FileToGetAccess, GetMinimumPrivilege);
end;

function TMotherDriveGetter.QueryDosDeviceSystemCall
  (VolumePath: String; VolumeNameBuffer: TVolumeNameBuffer): String;
begin
  QueryDosDevice(PChar(VolumePath), VolumeNameBuffer, MAX_PATH);
  IfOSErrorRaiseException;
  result := String(PChar(@VolumeName));
end;

procedure TMotherDriveGetter.SetVolumeNameBuffer;
var
  VolumePath: String;
  VolumeNameBuffer: TVolumeNameBuffer;
begin
  VolumePath := GetPathOfFileAccessingWithoutPrefix;
  ZeroMemory(@VolumeNameBuffer[0], MAX_PATH);
  self.VolumeName := QueryDosDeviceSystemCall(VolumePath, VolumeNameBuffer);
end;

function GetMinimumPrivilege: TCreateFileDesiredAccess;
begin
  result := DesiredReadOnly;
end;

function TMotherDriveGetter.IsRAMDrive: Boolean;
const
  QSoftRamdriveVolumeName = 'ramdriv';
begin
  result := Pos(QSoftRamdriveVolumeName, lowercase(VolumeName)) > 0;
end;

procedure TMotherDriveGetter.IfRAMDriveRaiseException;
begin
  if IsRAMDrive then
    raise ERAMDrive.Create('RAMDrive: MotherDrive can''t target RAMDrive.');
end;

function TMotherDriveGetter.SetIOBufferToGetMotherDrive
  (OutputBufferPointer: Pointer): TIoControlIOBuffer;
const
  NullInputBuffer = nil;
  NullInputBufferSize = 0;
begin
  result.InputBuffer.Buffer := NullInputBuffer;
  result.InputBuffer.Size := NullInputBufferSize;

  result.OutputBuffer.Buffer := OutputBufferPointer;
  result.OutputBuffer.Size := SizeOf(VOLUME_DISK_EXTENTS);
end;

procedure TMotherDriveGetter.GetMotherDriveAndIfNotReturnedRaiseException
  (IOBuffer: TIoControlIOBuffer);
var
  ReturnedBytes: Cardinal;
begin
  ReturnedBytes := IoControl(TIoControlCode.GetVolumeDiskExtents, IOBuffer);
  if ReturnedBytes = 0 then
    ENoDataReturnedFromIO.Create
      ('NoDataReturnedFromIO: No data returned from GetVolumeDiskExtents');
end;

procedure TMotherDriveGetter.ExtentsToTMotherDriveList
  (DiskExtents: VOLUME_DISK_EXTENTS);
var
  CurrentExtent: Integer;
begin
  MotherDriveList := TMotherDriveList.Create;
  for CurrentExtent := 0 to DiskExtents.NumberOfDiskExtents - 1 do
    MotherDriveList.Add(DiskExtents.Extents[CurrentExtent]);
end;

function TMotherDriveGetter.GetMotherDrive: TMotherDriveList;
var
  IOBuffer: TIoControlIOBuffer;
  OSVolumeDiskExtents: VOLUME_DISK_EXTENTS;
begin
  IOBuffer := SetIOBufferToGetMotherDrive(@OSVolumeDiskExtents);
  GetMotherDriveAndIfNotReturnedRaiseException(IOBuffer);
  ExtentsToTMotherDriveList(OSVolumeDiskExtents);
  exit(MotherDriveList);
end;

procedure TMotherDriveGetter.TryToGetMotherDriveList;
begin
  SetVolumeNameBuffer;
  IfRAMDriveRaiseException;
  result := GetMotherDrive;
end;

function TMotherDriveGetter.GetMotherDriveList: TMotherDriveList;
begin
  try
    result := TryToGetMotherDriveList;
  except
    FreeAndNil(MotherDriveList);
    result := nil;
  end;
end;

function TMotherDriveGetter.GetMinimumPrivilege: TCreateFileDesiredAccess;
begin
  exit(DesiredReadOnly);
end;

end.
