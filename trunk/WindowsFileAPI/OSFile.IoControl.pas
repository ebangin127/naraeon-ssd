unit OSFile.IoControl;

interface

uses
  Windows, Classes, SysUtils,
  OSFile.Handle;

type
  TIoControlSingleBuffer = record
    Size: Cardinal;
    Buffer: Pointer;
  end;
  TIoControlIOBuffer = record
    InputBuffer: TIoControlSingleBuffer;
    OutputBuffer: TIoControlSingleBuffer;
  end;
  TIoControlCode =
    (ATAPassThrough,
     ATAPassThroughDirect,
     SCSIPassThrough,
     StorageProtocolCommand,
     StorageQueryProperty,
     StorageCheckVerify,
     GetVolumeBitmap,
     GetVolumeDiskExtents,
     GetNTFSVolumeData,
     GetDriveGeometryEX,
     OSLevelTrim,
     ScsiMiniport,
     GetScsiAddress,
     LockVolume,
     UnlockVolume,
     LoadMedia,
     Unknown);

  EInvalidIoControlCode = class(Exception);
  ENoDataReturnedFromIO = class(Exception);
  TIoControlFile = class abstract(TOSFileWithHandle)
  protected
    function IoControl(
      ControlCode: TIoControlCode;
      IOBuffer: TIoControlIOBuffer): Cardinal;
    function ExceptionFreeIoControl(
      ControlCode: TIoControlCode;
      IOBuffer: TIoControlIOBuffer): Cardinal;
    function BuildOSBufferBy<InputType, OutputType>(var InputBuffer: InputType;
      var OutputBuffer: OutputType): TIoControlIOBuffer;
    function BuildOSBufferByOutput<OutputType>(var OutputBuffer: OutputType):
      TIoControlIOBuffer;
    function NullOSBuffer: TIoControlIOBuffer;
  private
    function DeviceIoControlSystemCall(
      OSControlCode: Integer;
      IOBuffer: TIoControlIOBuffer): Cardinal;
    function IsOutOfRange(const ControlCode: TIoControlCode): Boolean;
  {$IfDef UNITTEST}
  public
  {$EndIf}
    function TDeviceIoControlCodeToOSControlCode
      (ControlCode: TIoControlCode): Integer;
  end;

implementation

{ TIoControlFile }

function TIoControlFile.ExceptionFreeIoControl(ControlCode: TIoControlCode;
  IOBuffer: TIoControlIOBuffer): Cardinal;
var
  OSControlCode: Integer;
begin
  OSControlCode := TDeviceIoControlCodeToOSControlCode(ControlCode);
  SetLastError(ERROR_SUCCESS);
  DeviceIoControlSystemCall(OSControlCode, IOBuffer);
  result := GetLastError;
end;

function TIoControlFile.IoControl(ControlCode: TIoControlCode;
  IOBuffer: TIoControlIOBuffer): Cardinal;
var
  OSControlCode: Integer;
begin
  OSControlCode := TDeviceIoControlCodeToOSControlCode(ControlCode);
  SetLastError(ERROR_SUCCESS);
  result := DeviceIoControlSystemCall(OSControlCode, IOBuffer);
  IfOSErrorRaiseException;
end;

function TIoControlFile.DeviceIoControlSystemCall(
  OSControlCode: Integer;
  IOBuffer: TIoControlIOBuffer): Cardinal;
const
  NotOverlappedIO = nil;
begin
  Windows.DeviceIoControl(GetFileHandle,
    OSControlCode,
    IOBuffer.InputBuffer.Buffer, IOBuffer.InputBuffer.Size,
    IOBuffer.OutputBuffer.Buffer, IOBuffer.OutputBuffer.Size,
    result, NotOverlappedIO);
end;

function TIoControlFile.BuildOSBufferBy<InputType, OutputType>(
  var InputBuffer: InputType; var OutputBuffer: OutputType): TIoControlIOBuffer;
begin
  result.InputBuffer.Size := SizeOf(InputBuffer);
  result.InputBuffer.Buffer := @InputBuffer;
  result.OutputBuffer.Size := SizeOf(OutputBuffer);
  result.OutputBuffer.Buffer := @OutputBuffer;
end;

function TIoControlFile.BuildOSBufferByOutput<OutputType>(
  var OutputBuffer: OutputType): TIoControlIOBuffer;
begin
  FillChar(result, SizeOf(result), #0);
  result.OutputBuffer.Size := SizeOf(OutputBuffer);
  result.OutputBuffer.Buffer := @OutputBuffer;
end;

function TIoControlFile.IsOutOfRange(
  const ControlCode: TIoControlCode): Boolean;
begin
  result :=
    (Integer(ControlCode) > Integer(High(TIoControlCode))) or
    (Integer(ControlCode) < Integer(Low(TIoControlCode)));
end;

function TIoControlFile.NullOSBuffer: TIoControlIOBuffer;
begin
  result.InputBuffer.Size := 0;
  result.InputBuffer.Buffer := nil;
  result.OutputBuffer.Size := 0;
  result.OutputBuffer.Buffer := nil;
end;

function TIoControlFile.TDeviceIoControlCodeToOSControlCode(
  ControlCode: TIoControlCode): Integer;
const
  IOCTL_SCSI_BASE = FILE_DEVICE_CONTROLLER;
  IOCTL_STORAGE_BASE = $2D;
  IOCTL_ATA_PASS_THROUGH =
    (IOCTL_SCSI_BASE shl 16) or
    ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($040B shl 2) or
    (METHOD_BUFFERED);
  IOCTL_ATA_PASS_THROUGH_DIRECT = $4D030;
  IOCTL_SCSI_PASS_THROUGH =
    (IOCTL_SCSI_BASE shl 16) or
    ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($0401 shl 2) or
    (METHOD_BUFFERED);
  IOCTL_STORAGE_PROTOCOL_COMMAND =
    (IOCTL_SCSI_BASE shl 16) or
    ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($04F0 shl 2) or
    (METHOD_BUFFERED);
  IOCTL_SCSI_MINIPORT =
    (IOCTL_SCSI_BASE shl 16) or
    ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($0402 shl 2) or
    (METHOD_BUFFERED);
  IOCTL_STORAGE_MANAGE_DATA_SET_ATTRIBUTES =
    (IOCTL_STORAGE_BASE shl 16) or
    (FILE_WRITE_ACCESS shl 14) or ($0501 shl 2) or
    (METHOD_BUFFERED);
  IOCTL_SCSI_GET_ADDRESS =
    (IOCTL_SCSI_BASE shl 16) or
    (FILE_ANY_ACCESS shl 14) or ($0406 shl 2) or
    (METHOD_BUFFERED);

  OSControlCodeOfIoControlCode: Array[TIoControlCode] of Integer =
    (IOCTL_ATA_PASS_THROUGH,
     IOCTL_ATA_PASS_THROUGH_DIRECT,
     IOCTL_SCSI_PASS_THROUGH,
     IOCTL_STORAGE_PROTOCOL_COMMAND,
     IOCTL_STORAGE_QUERY_PROPERTY,
     IOCTL_STORAGE_CHECK_VERIFY,
     FSCTL_GET_VOLUME_BITMAP,
     IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS,
     FSCTL_GET_NTFS_VOLUME_DATA,
     IOCTL_DISK_GET_DRIVE_GEOMETRY_EX,
     IOCTL_STORAGE_MANAGE_DATA_SET_ATTRIBUTES,
     IOCTL_SCSI_MINIPORT,
     IOCTL_SCSI_GET_ADDRESS,
     FSCTL_LOCK_VOLUME,
     FSCTL_UNLOCK_VOLUME,
     IOCTL_STORAGE_LOAD_MEDIA,
     0);
begin
  if (ControlCode = TIoControlCode.Unknown) or (IsOutOfRange(ControlCode)) then
    raise EInvalidIoControlCode.Create
      ('InvalidIoControlCode: There''s no such IoControlCode - ' +
       IntToStr(Cardinal(ControlCode)));
  exit(OSControlCodeOfIoControlCode[ControlCode]);
end;

end.
