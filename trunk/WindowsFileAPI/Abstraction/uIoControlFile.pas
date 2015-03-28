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

  private
    function DeviceIoControlSystemCall(
      OSControlCode: Cardinal;
      IOBuffer: TIoControlIOBuffer): Cardinal;
    function TDeviceIoControlCodeToOSControlCode
      (ControlCode: TIoControlCode): Cardinal;
  end;

  ENoDataReturnedFromIO = class(Exception);

implementation

{ TDeviceFile }

procedure TIoControlFile.IoControl(ControlCode: TIoControlCode;
  IOBuffer: TIoControlIOBuffer);
var
  OSControlCode: Cardinal;
begin
  OSControlCode := TDeviceIoControlCodeToOSControlCode(ControlCode);
  result := DeviceIoControlSystemCall(OSControlCode, IOBuffer);
  IfOSErrorRaiseException;
end;

procedure TIoControlFile.DeviceIoControlSystemCall(
  OSControlCode: Cardinal;
  IOBuffer: TIoControlIOBuffer);
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
  (ControlCode: TIoControlCode): Cardinal;
const
  IOCTL_SCSI_BASE = FILE_DEVICE_CONTROLLER;
  IOCTL_ATA_PASS_THROUGH = (IOCTL_SCSI_BASE shl 16)
                            or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14)
                            or ($040B shl 2) or (METHOD_BUFFERED);
  IOCTL_ATA_PASS_THROUGH_DIRECT = $4D030;
  IOCTL_SCSI_PASS_THROUGH      =  $0004D004;
begin
  case ControlCode of
    ATAPassThrough:
      exit(IOCTL_ATA_PASS_THROUGH);
    ATAPassThroughDirect:
      exit(IOCTL_ATA_PASS_THROUGH_DIRECT);
    SCSIPassThrough:
      exit(IOCTL_SCSI_PASS_THROUGH);
    StorageQueryProperty:
      exit(IOCTL_STORAGE_QUERY_PROPERTY);
    StorageCheckVerify:
      exit(IOCTL_STORAGE_CHECK_VERIFY);
    GetVolumeBitmap:
      exit(FSCTL_GET_VOLUME_BITMAP);
    GetVolumeDiskExtents:
      exit(IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS);
    GetNTFSVolumeData:
      exit(FSCTL_GET_NTFS_VOLUME_DATA);
    GetDriveGeometryEX:
      exit(IOCTL_DISK_GET_DRIVE_GEOMETRY_EX);
    else
      raise EInvalidIoControlCode.Create
        ('InvalidIoControlCode: There''s no such IoControlCode - ' +
         IntToStr(Cardinal(ControlCode)));
  end;
end;

end.
