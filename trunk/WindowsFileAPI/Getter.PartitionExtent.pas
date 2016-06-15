unit Getter.PartitionExtent;

interface

uses
  Windows, SysUtils, Generics.Collections,
  OSFile.Handle, OSFile.IoControl, OS.Handle;

type
  TPartitionExtentEntry = record
    DriveNumber: DWORD;
    StartingOffset: TLargeInteger;
    ExtentLength: TLargeInteger;
  end;

  TPartitionExtentList = TList<TPartitionExtentEntry>;

  TPartitionExtentGetter = class sealed(TIoControlFile)
  public
    constructor Create(const FileToGetAccess: String); override;
    function GetPartitionExtentList: TPartitionExtentList;
  protected
    function GetMinimumPrivilege: TCreateFileDesiredAccess; override;
  private
    PartitionExtentList: TPartitionExtentList;
    VolumeName: String;
    type
      TVolumeNameBuffer = Array[0..MAX_PATH] of Char;
      DISK_EXTENT = TPartitionExtentEntry;
      VOLUME_DISK_EXTENTS = record
        NumberOfDiskExtents: DWORD;
        Extents: Array[0..50] of DISK_EXTENT;
      end;
    function TryToGetPartitionExtentList: TPartitionExtentList;
    function IsRAMDrive: Boolean;
    procedure IfRAMDriveRaiseException;
    function GetPartitionExtent: TPartitionExtentList;
    procedure GetPartitionExtentAndIfFailedRaiseException(
      const IOBuffer: TIoControlIOBuffer);
    function SetIOBufferToGetPartitionExtent(
      const OutputBufferPointer: Pointer): TIoControlIOBuffer;
    procedure ExtentsToTPartitionExtentList(
      const DiskExtents: VOLUME_DISK_EXTENTS);
    procedure SetVolumeNameBuffer;
    function QueryDosDeviceSystemCall
      (const VolumePath: String;
       const VolumeNameBuffer: TVolumeNameBuffer): String;
  end;

  ERAMDrive = class(Exception);

implementation

{ TPartitionExtent }

constructor TPartitionExtentGetter.Create(const FileToGetAccess: String);
begin
  inherited;
  CreateHandle(FileToGetAccess, DesiredReadWrite);
end;

function TPartitionExtentGetter.QueryDosDeviceSystemCall(
  const VolumePath: String; const VolumeNameBuffer: TVolumeNameBuffer): String;
begin
  QueryDosDevice(PChar(VolumePath), VolumeNameBuffer, MAX_PATH);
  IfOSErrorRaiseException;
  result := String(PChar(@VolumeName));
end;

procedure TPartitionExtentGetter.SetVolumeNameBuffer;
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

function TPartitionExtentGetter.IsRAMDrive: Boolean;
const
  QSoftRamdriveVolumeName = 'ramdriv';
begin
  result := Pos(QSoftRamdriveVolumeName, lowercase(VolumeName)) > 0;
end;

procedure TPartitionExtentGetter.IfRAMDriveRaiseException;
begin
  if IsRAMDrive then
    raise ERAMDrive.Create('RAMDrive: PartitionExtent can''t target RAMDrive.');
end;

function TPartitionExtentGetter.SetIOBufferToGetPartitionExtent
  (const OutputBufferPointer: Pointer): TIoControlIOBuffer;
const
  NullInputBuffer = nil;
  NullInputBufferSize = 0;
begin
  result.InputBuffer.Buffer := NullInputBuffer;
  result.InputBuffer.Size := NullInputBufferSize;

  result.OutputBuffer.Buffer := OutputBufferPointer;
  result.OutputBuffer.Size := SizeOf(VOLUME_DISK_EXTENTS);
end;

procedure TPartitionExtentGetter.GetPartitionExtentAndIfFailedRaiseException
  (const IOBuffer: TIoControlIOBuffer);
var
  ReturnedBytes: Cardinal;
begin
  ReturnedBytes := IoControl(TIoControlCode.GetVolumeDiskExtents, IOBuffer);
  if ReturnedBytes = 0 then
    ENoDataReturnedFromIO.Create
      ('NoDataReturnedFromIO: No data returned from GetVolumeDiskExtents');
end;

procedure TPartitionExtentGetter.ExtentsToTPartitionExtentList
  (const DiskExtents: VOLUME_DISK_EXTENTS);
var
  CurrentExtent: Integer;
begin
  PartitionExtentList := TPartitionExtentList.Create;
  for CurrentExtent := 0 to DiskExtents.NumberOfDiskExtents - 1 do
    PartitionExtentList.Add(DiskExtents.Extents[CurrentExtent]);
end;

function TPartitionExtentGetter.GetPartitionExtent: TPartitionExtentList;
var
  IOBuffer: TIoControlIOBuffer;
  OSVolumeDiskExtents: VOLUME_DISK_EXTENTS;
begin
  IOBuffer := SetIOBufferToGetPartitionExtent(@OSVolumeDiskExtents);
  GetPartitionExtentAndIfFailedRaiseException(IOBuffer);
  ExtentsToTPartitionExtentList(OSVolumeDiskExtents);
  exit(PartitionExtentList);
end;

function TPartitionExtentGetter.TryToGetPartitionExtentList:
  TPartitionExtentList;
begin
  SetVolumeNameBuffer;
  IfRAMDriveRaiseException;
  result := GetPartitionExtent;
end;

function TPartitionExtentGetter.GetPartitionExtentList: TPartitionExtentList;
begin
  try
    result := TryToGetPartitionExtentList;
  except
    FreeAndNil(PartitionExtentList);
    result := nil;
  end;
end;

function TPartitionExtentGetter.GetMinimumPrivilege: TCreateFileDesiredAccess;
begin
  exit(DesiredReadOnly);
end;

end.
