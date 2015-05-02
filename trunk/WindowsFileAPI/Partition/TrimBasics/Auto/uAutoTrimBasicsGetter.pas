unit uAutoTrimBasicsGetter;

interface

uses
  SysUtils, Windows,
  uPartition, uIoControlFile, uTrimBasicsGetter, uVolumeBitmapGetter,
  uFATTrimBasicsGetter, uNTFSTrimBasicsGetter, uPartitionExtentGetter;

type
  TAutoTrimBasicsGetter = class(TTrimBasicsGetter)
  public
    destructor Destroy; override;
    function IsPartitionMyResponsibility: Boolean; override;
    function GetTrimBasicsToInitialize: TTrimBasicsToInitialize; override;

  private
    TrimBasicsGetter: TTrimBasicsGetter;
    procedure RequestTrimBasicsGetter;
    procedure CheckResponsibilityAndIfFitSetClassField(
      TrimBasicsGetterToCheck: TTrimBasicsGetter);
    function GetStartLBA: UInt64;
  end;

implementation

{ TAutoTrimBasicsGetter }

procedure TAutoTrimBasicsGetter.CheckResponsibilityAndIfFitSetClassField
  (TrimBasicsGetterToCheck: TTrimBasicsGetter);
var
  IsResponsible: Boolean;
begin
  if TrimBasicsGetterToCheck = nil then
    exit;

  IsResponsible := TrimBasicsGetterToCheck.IsPartitionMyResponsibility;

  if IsResponsible then
    TrimBasicsGetter := TrimBasicsGetterToCheck
  else
    FreeAndNil(TrimBasicsGetterToCheck);
end;

procedure TAutoTrimBasicsGetter.RequestTrimBasicsGetter;
var
  TestingTrimBasicsGetter: TTrimBasicsGetter;
begin
  try
    TestingTrimBasicsGetter :=
      TFATTrimBasicsGetter.Create(GetPathOfFileAccessing);
    CheckResponsibilityAndIfFitSetClassField(TestingTrimBasicsGetter);

    if TrimBasicsGetter <> nil then
      exit;
    TestingTrimBasicsGetter :=
      TNTFSTrimBasicsGetter.Create(GetPathOfFileAccessing);
    CheckResponsibilityAndIfFitSetClassField(TestingTrimBasicsGetter);
  except
    FreeAndNil(TestingTrimBasicsGetter);
  end;
end;

function TAutoTrimBasicsGetter.IsPartitionMyResponsibility: Boolean;
begin
  if TrimBasicsGetter = nil then
    RequestTrimBasicsGetter;
  result := TrimBasicsGetter <> nil;
end;

destructor TAutoTrimBasicsGetter.Destroy;
begin
  FreeAndNil(TrimBasicsGetter);
  inherited;
end;

function TAutoTrimBasicsGetter.GetStartLBA: UInt64;
var
  Partition: TPartition;
  PartitionExtentList: TPartitionExtentList;
begin
  Partition := TPartition.Create(GetPathOfFileAccessing);
  PartitionExtentList := Partition.GetPartitionExtentList;
  result := PartitionExtentList[0].StartingOffset div BytePerLBA;
  FreeAndNil(PartitionExtentList);
  FreeAndNil(Partition);
end;

function TAutoTrimBasicsGetter.GetTrimBasicsToInitialize:
  TTrimBasicsToInitialize;
begin
  if TrimBasicsGetter = nil then
    RequestTrimBasicsGetter;
  result := TrimBasicsGetter.GetTrimBasicsToInitialize;
  result.StartLBA := GetStartLBA;
end;
end.
