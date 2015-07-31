unit uMockAutoTrimBasicsGetter;

interface

uses
  SysUtils, Windows,
  uPartition, uIoControlFile, uTrimBasicsGetter, uVolumeBitmapGetter,
  uFATTrimBasicsGetter, uNTFSTrimBasicsGetter, uPartitionExtentGetter;

type
  TAutoTrimBasicsGetter = class(TTrimBasicsGetter)
  public
    function IsPartitionMyResponsibility: Boolean; override;
    function GetTrimBasicsToInitialize: TTrimBasicsToInitialize; override;
  end;

implementation

{ TAutoTrimBasicsGetter }

function TAutoTrimBasicsGetter.IsPartitionMyResponsibility: Boolean;
begin
  result := true;
end;

function TAutoTrimBasicsGetter.GetTrimBasicsToInitialize:
  TTrimBasicsToInitialize;
begin
  result.StartLBA := 0;
  result.LBAPerCluster := 1;
  result.PaddingLBA := 0;
end;
end.
