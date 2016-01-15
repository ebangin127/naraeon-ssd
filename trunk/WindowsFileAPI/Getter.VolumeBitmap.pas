unit VolumeLabelGetter;

interface

uses
  SysUtils, Windows,
  OSFile.Handle, OSFile.IoControl;

const
  BitmapSizePerBuffer = 16384;

type
  TBitmapBuffer = Array[0..BitmapSizePerBuffer - 1] of Cardinal;
  TBitmapPositionSize = record
    StartingLCN: LARGE_INTEGER;
    BitmapSize: LARGE_INTEGER;
  end;
  TVolumeBitmapBufferWithErrorCode = record
    PositionSize: TBitmapPositionSize;
    Buffer: TBitmapBuffer;
    LastError: Cardinal;
  end;
  
  TVolumeBitmapGetter = class(TIoControlFile)
  public
    constructor Create(const FileToGetAccess: String); override;
    function GetVolumeBitmap(const StartingLCN: LARGE_INTEGER):
      TVolumeBitmapBufferWithErrorCode;
  protected
    function GetMinimumPrivilege: TCreateFileDesiredAccess; override;
  private
    type
      TVolumeBitmapInput = record
        StartingLCN: LARGE_INTEGER;
      end;
  private
    InnerInput: TVolumeBitmapInput;
    function GetIOBuffer(const ResultBufferPointer: Pointer):
      TIoControlIOBuffer;
  end;

implementation

{ TVolumeBitmapGetter }

constructor TVolumeBitmapGetter.Create(const FileToGetAccess: String);
begin
  CreateHandle(FileToGetAccess, DesiredReadOnly);
end;

function TVolumeBitmapGetter.GetIOBuffer(const ResultBufferPointer: Pointer):
  TIoControlIOBuffer;
begin
  result.InputBuffer.Buffer := @InnerInput;
  result.InputBuffer.Size := SizeOf(InnerInput);

  result.OutputBuffer.Buffer := ResultBufferPointer;
  result.OutputBuffer.Size := SizeOf(TBitmapPositionSize) +
    SizeOf(TBitmapBuffer);
end;

function TVolumeBitmapGetter.GetVolumeBitmap(
  const StartingLCN: LARGE_INTEGER): TVolumeBitmapBufferWithErrorCode;
begin
  InnerInput.StartingLCN := StartingLCN;
  result.LastError :=
    ExceptionFreeIoControl(
      TIoControlCode.GetVolumeBitmap, GetIOBuffer(@result));
end;

function TVolumeBitmapGetter.GetMinimumPrivilege: TCreateFileDesiredAccess;
begin
  result := TCreateFileDesiredAccess.DesiredReadOnly;
end;

end.
