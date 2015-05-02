unit uIoControlFile;

interface

uses
  Windows, Classes, SysUtils,
  uOSFileWithHandle;

type
  TIoControlSingleBuffer = record
    Size: Cardinal;
    Buffer: Pointer;
  end;

  TIoControlIOBuffer = record
    InputBuffer: TIoControlSingleBuffer;
    OutputBuffer: TIoControlSingleBuffer;
  end;

  EInvalidIoControlCode = class(Exception);

  EWrongIoControlCode = class(Exception);

  TIoControlFile = class abstract(TOSFileWithHandle)
  protected
    type
      TIoControlCode =
        (ATAPassThrough,
         ATAPassThroughDirect,
         SCSIPassThrough,
         StorageQueryProperty,
         StorageCheckVerify,
         GetVolumeBitmap,
         GetVolumeDiskExtents,
         GetNTFSVolumeData,
         GetDriveGeometryEX,
         Unknown);

    function IoControl(
      ControlCode: TIoControlCode;
      IOBuffer: TIoControlIOBuffer): Cardinal;
    function ExceptionFreeIoControl(
      ControlCode: TIoControlCode;
      IOBuffer: TIoControlIOBuffer): Cardinal;

  private
    function DeviceIoControlSystemCall(
      OSControlCode: Integer;
      IOBuffer: TIoControlIOBuffer): Cardinal;
    function TDeviceIoControlCodeToOSControlCode
      (ControlCode: TIoControlCode): Integer;
  end;

  ENoDataReturnedFromIO = class(Exception);

implementation

{ TDeviceFile }

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

function TIoControlFile.TDeviceIoControlCodeToOSControlCode
  (ControlCode: TIoControlCode): Integer;
const
  IOCTL_SCSI_BASE = FILE_DEVICE_CONTROLLER;
  IOCTL_ATA_PASS_THROUGH = 
    (IOCTL_SCSI_BASE shl 16) or 
    ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($040B shl 2) or
    (METHOD_BUFFERED);
  IOCTL_ATA_PASS_THROUGH_DIRECT = $4D030;
  IOCTL_SCSI_PASS_THROUGH =
    (IOCTL_SCSI_BASE shl 16) or
    ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or ($0401 shl 2) or
    (METHOD_BUFFERED);
  
  OSControlCodeOfIoControlCode: Array[TIoControlCode] of Integer =
    (IOCTL_ATA_PASS_THROUGH,
     IOCTL_ATA_PASS_THROUGH_DIRECT,
     IOCTL_SCSI_PASS_THROUGH,
     IOCTL_STORAGE_QUERY_PROPERTY,
     IOCTL_STORAGE_CHECK_VERIFY,
     FSCTL_GET_VOLUME_BITMAP,
     IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS,
     FSCTL_GET_NTFS_VOLUME_DATA,
     IOCTL_DISK_GET_DRIVE_GEOMETRY_EX,
     0);
begin
  if ControlCode = TIoControlCode.Unknown then
    raise EInvalidIoControlCode.Create
      ('InvalidIoControlCode: There''s no such IoControlCode - ' +
       IntToStr(Cardinal(ControlCode)));
  exit(OSControlCodeOfIoControlCode[ControlCode]);
end;

end.
