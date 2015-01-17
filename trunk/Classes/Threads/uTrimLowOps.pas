unit uTrimLowOps;

interface

uses
  Windows, uDiskFunctions;

type
  //---Trim Command--//
  PSTARTING_LCN_INPUT_BUFFER = ^STARTING_LCN_INPUT_BUFFER;
  {$EXTERNALSYM PSTARTING_LCN_INPUT_BUFFER}
  STARTING_LCN_INPUT_BUFFER = record
    StartingLcn: LARGE_INTEGER;
  end;

  PVOLUME_BITMAP_BUFFER = ^VOLUME_BITMAP_BUFFER;
  {$EXTERNALSYM PVOLUME_BITMAP_BUFFER}
  VOLUME_BITMAP_BUFFER = record
    StartingLcn: LARGE_INTEGER;
    BitmapSize: LARGE_INTEGER;
    Buffer: array [0..4095] of Byte;
  end;
  //---Trim Command--//

  TTrimLowOps = class
    class function CreateHandle(const DriveLetter: String): THandle;
    class function GetVolumeBitmap(
      PartHandle: THandle;
      var StartingBuffer: STARTING_LCN_INPUT_BUFFER;
      var BitmapBuffer: VOLUME_BITMAP_BUFFER;
      var BytesRead: Cardinal): Cardinal;
  end;

implementation

{ TTrimLowOps }

class function TTrimLowOps.CreateHandle(const DriveLetter: String): THandle;
begin
  exit(CreateFile(
        PChar('\\.\' + DriveLetter),
        GENERIC_READ,
        FILE_SHARE_READ or FILE_SHARE_WRITE,
        nil, OPEN_EXISTING, 0, 0));
end;

class function TTrimLowOps.GetVolumeBitmap(
  PartHandle: THandle;
  var StartingBuffer: STARTING_LCN_INPUT_BUFFER;
  var BitmapBuffer: VOLUME_BITMAP_BUFFER;
  var BytesRead: Cardinal): Cardinal;
begin
  DeviceIoControl(
    PartHandle,
    FSCTL_GET_VOLUME_BITMAP,
    @StartingBuffer, SizeOf(STARTING_LCN_INPUT_BUFFER),
    @BitmapBuffer, SizeOf(VOLUME_BITMAP_BUFFER),
    BytesRead, nil);

  exit(GetLastError);
end;

end.
