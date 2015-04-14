unit uVolumeBitmapGetter;

interface

uses
  Windows,
  uOSFileWithHandle, uIoControlFile;

const
  BitmapSizePerBuffer = 4096;

type
  TBitmapBuffer = Array[0..BitmapSizePerBuffer - 1] of Byte;

  TVolumeBitmapBuffer = record
    StartingLCN: LARGE_INTEGER;
    BitmapSize: LARGE_INTEGER;
    Buffer: TBitmapBuffer;
  end;

  TVolumeBitmapGetter = class(TIoControlFile)
  public
    constructor Create(FileToGetAccess: String); override;
    function GetVolumeBitmap(StartingLCN: LARGE_INTEGER): TVolumeBitmapBuffer;

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
  result.OutputBuffer.Size := SizeOf(TVolumeBitmapBuffer);
end;

function TVolumeBitmapGetter.GetVolumeBitmap(
  StartingLCN: LARGE_INTEGER): TVolumeBitmapBuffer;
begin
  InnerInput.StartingLCN := StartingLCN;
  IoControl(TIoControlCode.GetVolumeBitmap, GetIOBuffer(@result));
end;

function TVolumeBitmapGetter.GetMinimumPrivilege: TCreateFileDesiredAccess;
begin
  result := TCreateFileDesiredAccess.DesiredReadOnly;
end;

end.
