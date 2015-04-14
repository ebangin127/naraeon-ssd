unit uVolumeBitmapGetter;

interface

uses
  SysUtils, Windows,
  uOSFileWithHandle, uIoControlFile;

const
  BitmapSizePerBuffer = 4096;

type
  TBitmapBuffer = Array[0..BitmapSizePerBuffer - 1] of Byte;

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
    constructor Create(FileToGetAccess: String); override;
    function GetVolumeBitmap(StartingLCN: LARGE_INTEGER):
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
    function GetIOBuffer(ResultBufferPointer: Pointer): TIoControlIOBuffer;
  end;

implementation

{ TVolumeBitmapGetter }

constructor TVolumeBitmapGetter.Create(FileToGetAccess: String);
begin
  inherited Create(FileToGetAccess, DesiredReadOnly);
end;

function TVolumeBitmapGetter.GetIOBuffer(ResultBufferPointer: Pointer):
  TIoControlIOBuffer;
begin
  result.InputBuffer.Buffer := @InnerInput;
  result.InputBuffer.Size := SizeOf(InnerInput);

  result.OutputBuffer.Buffer := ResultBufferPointer;
  result.OutputBuffer.Size := SizeOf(TBitmapPositionSize) +
    SizeOf(TBitmapBuffer);
end;

function TVolumeBitmapGetter.GetVolumeBitmap(
  StartingLCN: LARGE_INTEGER): TVolumeBitmapBufferWithErrorCode;
begin
  result.LastError := ERROR_SUCCESS;
  try
    InnerInput.StartingLCN := StartingLCN;
    IoControl(TIoControlCode.GetVolumeBitmap, GetIOBuffer(@result));
  except
    on E: EOSError do
      result.LastError := E.ErrorCode;
  end;
end;

function TVolumeBitmapGetter.GetMinimumPrivilege: TCreateFileDesiredAccess;
begin
  result := TCreateFileDesiredAccess.DesiredReadOnly;
end;

end.
