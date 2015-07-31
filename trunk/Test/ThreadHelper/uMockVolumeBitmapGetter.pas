unit uMockVolumeBitmapGetter;

interface

uses
  SysUtils, Windows, Generics.Collections,
  uOSFile;

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

  TVolumeBitmapGetter = class(TOSFile)
  public
    function GetVolumeBitmap(StartingLCN: LARGE_INTEGER):
      TVolumeBitmapBufferWithErrorCode;
      
    class procedure CreateBitmapStorage;
    class procedure FreeBitmapStorage;
    class procedure AddAtBitmapStorage(BitmapBuffer: TBitmapBuffer);
    class procedure ClearBitmapStorage;
    class procedure SetLength(NewLength: Int64);

  private
    type
      TBitmapBufferStorage = TList<TBitmapBuffer>;
      
  private
    class var BitmapBufferLength: Int64;
    class var BitmapBufferStorage: TBitmapBufferStorage;
  end;

implementation

{ TVolumeBitmapGetter }

function TVolumeBitmapGetter.GetVolumeBitmap(
  StartingLCN: LARGE_INTEGER): TVolumeBitmapBufferWithErrorCode;
var
  RemainingLength: Int64;
begin
  RemainingLength :=
    BitmapBufferLength - StartingLCN.QuadPart - BitmapSizePerBuffer;
  result.PositionSize.StartingLCN := StartingLCN;
  result.PositionSize.BitmapSize := (RemainingLength + BitmapSizePerBuffer) *
    BitmapSizePerBuffer;
  result.Buffer := BitmapBufferStorage[StartingLCN.QuadPart div
    BitmapSizePerBuffer];
      
  if RemainingLength > 0 then
    result.LastError := ERROR_MORE_DATA
  else if RemainingLength = 0 then
    result.LastError := ERROR_SUCCESS
  else
    result.LastError := ERROR_NO_MORE_ITEMS; 
end;

class procedure TVolumeBitmapGetter.SetLength(NewLength: Int64);
begin
  BitmapBufferLength := NewLength;
end;

class procedure TVolumeBitmapGetter.CreateBitmapStorage;
begin
  BitmapBufferStorage := TBitmapBufferStorage.Create;
end;

class procedure TVolumeBitmapGetter.FreeBitmapStorage;
begin
  FreeAndNil(BitmapBufferStorage);
end;

class procedure TVolumeBitmapGetter.AddAtBitmapStorage(
  BitmapBuffer: TBitmapBuffer);
begin
  BitmapBufferStorage.Add(BitmapBuffer);
end;

class procedure TVolumeBitmapGetter.ClearBitmapStorage;
begin
  BitmapBufferStorage.Clear;
end;

end.
