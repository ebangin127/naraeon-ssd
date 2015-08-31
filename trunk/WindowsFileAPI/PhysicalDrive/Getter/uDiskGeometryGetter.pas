unit uDiskGeometryGetter;

interface

uses
  Windows,
  uOSFileWithHandle, uIoControlFile;

type
  TMediaType =
    (Unknown, FloppyMedia, RemovableMedia, FixedMedia);

  TDiskGeometryGetter = class sealed(TIoControlFile)
  public
    constructor Create(FileToGetAccess: String); override;

    function GetMediaType: TMediaType;
    function GetDiskSizeInByte: TLargeInteger;

  protected
    function GetMinimumPrivilege: TCreateFileDesiredAccess; override;

  private
    type
      DISK_GEOMETRY = record
        Cylinders: TLargeInteger;
        MediaType: Byte;
        TracksPerCylinder: DWORD;
        SectorsPerTrack: DWORD;
        BytesPerSector: DWORD;
      end;

      DISK_GEOMETRY_EX = record
        Geometry: DISK_GEOMETRY;
        DiskSize: TLargeInteger;
        Data: Array[0..1] of UChar;
      end;

      TDiskGeometryResult = record
        MediaType: TMediaType;
        DiskSizeInByte: TLargeInteger;
      end;

    function GetDiskGeometry: TDiskGeometryResult;
    function GetIOBufferToGetGeometry(
      OutputBufferPointer: Pointer): TIoControlIOBuffer;
    procedure GetDiskGeometryAndIfNotReturnedRaiseException(
      IOBuffer: TIoControlIOBuffer);
    function OSMediaTypeToTMediaType(MediaType: Byte): TMediaType;
  end;


implementation

{ TDiskGeometry }

constructor TDiskGeometryGetter.Create(FileToGetAccess: String);
begin
  CreateHandle(FileToGetAccess, GetMinimumPrivilege);
end;

function TDiskGeometryGetter.GetIOBufferToGetGeometry
  (OutputBufferPointer: Pointer): TIoControlIOBuffer;
const
  NullInputBuffer = nil;
  NullInputBufferSize = 0;
begin
  result.InputBuffer.Buffer := NullInputBuffer;
  result.InputBuffer.Size := NullInputBufferSize;

  result.OutputBuffer.Buffer := OutputBufferPointer;
  result.OutputBuffer.Size := SizeOf(DISK_GEOMETRY_EX);
end;

procedure TDiskGeometryGetter.GetDiskGeometryAndIfNotReturnedRaiseException
  (IOBuffer: TIoControlIOBuffer);
var
  ReturnedBytes: Cardinal;
begin
  ReturnedBytes := IoControl(TIoControlCode.GetDriveGeometryEX, IOBuffer);
  if ReturnedBytes = 0 then
    ENoDataReturnedFromIO.Create
      ('NoDataReturnedFromIO: No data returned from GetDriveGeometryEX');
end;

function TDiskGeometryGetter.OSMediaTypeToTMediaType(MediaType: Byte):
  TMediaType;
const
  OSUnknown = $00;
  OSRemovableMedia = $0B;
  OSFixedMedia = $0C;
begin
  case MediaType of
    OSUnknown:
      exit(TMediaType.Unknown);
    OSRemovableMedia:
      exit(TMediaType.RemovableMedia);
    OSFixedMedia:
      exit(TMediaType.FixedMedia);
    else
      exit(TMediaType.FloppyMedia);
  end;
end;

function TDiskGeometryGetter.GetDiskGeometry: TDiskGeometryResult;
var
  IOBuffer: TIoControlIOBuffer;
  OSGeometryResult: DISK_GEOMETRY_EX;
begin
  IOBuffer := GetIOBufferToGetGeometry(@OSGeometryResult);
  GetDiskGeometryAndIfNotReturnedRaiseException(IOBuffer);

  result.DiskSizeInByte := OSGeometryResult.DiskSize;
  result.MediaType :=
    OSMediaTypeToTMediaType(OSGeometryResult.Geometry.MediaType);
end;

function TDiskGeometryGetter.GetDiskSizeInByte: TLargeInteger;
begin
  exit(GetDiskGeometry.DiskSizeInByte);
end;

function TDiskGeometryGetter.GetMediaType: TMediaType;
begin
  exit(GetDiskGeometry.MediaType);
end;

function TDiskGeometryGetter.GetMinimumPrivilege: TCreateFileDesiredAccess;
begin
  exit(DesiredReadOnly);
end;

end.
