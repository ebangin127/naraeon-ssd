unit uTrimThread;

interface

uses
  Classes, SysUtils, uDiskFunctions, Math, Dialogs, Windows,
  uATALowOps, uLanguageSettings, uPartitionFunctions, uTrimList;

type
  TTrimStage = (TRIMSTAGE_NONE, TRIMSTAGE_INPROGRESS,
    TRIMSTAGE_END, TRIMSTAGE_ERROR);

  TTrimThread = class(TThread)
  public
    class var IsSemiAuto: Boolean;
    class var TrimStage: TTrimStage;

    destructor Destroy; override;

    function ApplyPartList(PartListToTrim: TTrimList): Boolean;
  private
    PartToTrim: TTrimList;
    AllClusters: _LARGE_INTEGER;
    Progress: Cardinal;
    procedure ApplyTrimProgressToUI;
    procedure ApplyTrimStageToUI;
    function CheckPartitionInputValidity: Boolean;
    procedure SetTrimError;
    procedure SetTrimInProgress;
    procedure SetTrimIsEnd;
    procedure InitializeTrim;
    procedure ApplyEndStateOfTrim;
    procedure SetThisPartitionAsCompleted;
    procedure CleanupTrim;
    procedure TrimNextPartition;
    procedure TrimEachPartition;
    procedure TrimAppliedPartitions;

    procedure TryToTrimPartitions;  protected
    procedure Execute; override;
    procedure TrimPartition(const DriveLetter: String);

    //Main과의 Sync함수
    procedure ApplyProgress;
    procedure ApplyStage;
    procedure EndTrim;
  end;

implementation

uses uMain;

const
  LBASize = 512;

destructor TTrimThread.Destroy;
begin
  if PartToTrim <> nil then
    FreeAndNil(PartToTrim);

  inherited Destroy;
end;

function TTrimThread.ApplyPartList(PartListToTrim: TTrimList): Boolean;
begin
  result := PartListToTrim <> nil;

  if not result then
    exit;

  PartToTrim := PartListToTrim;
end;

procedure TTrimThread.ApplyTrimStageToUI;
begin
  if not IsSemiAuto then
    Synchronize(ApplyStage);
end;

procedure TTrimThread.ApplyTrimProgressToUI;
begin
  if not IsSemiAuto then
    Synchronize(ApplyProgress);
end;

procedure TTrimThread.ApplyEndStateOfTrim;
begin
  if not IsSemiAuto then
    Synchronize(EndTrim);
end;

function TTrimThread.CheckPartitionInputValidity: Boolean;
begin
  raise EArgumentNilException.Create('TrimList = Nil');
end;

procedure TTrimThread.SetTrimError;
begin
  TrimStage := TRIMSTAGE_ERROR;
end;

procedure TTrimThread.SetTrimInProgress;
begin
  TrimStage := TRIMSTAGE_INPROGRESS;
end;

procedure TTrimThread.SetTrimIsEnd;
begin
  TrimStage := TRIMSTAGE_END;
end;

procedure TTrimThread.InitializeTrim;
begin
  CheckPartitionInputValidity;
  PartToTrim.PointerToFirst;
  SetTrimInProgress;
end;

procedure TTrimThread.CleanupTrim;
begin
  SetTrimIsEnd;
  ApplyEndStateOfTrim;
end;

procedure TTrimThread.SetThisPartitionAsCompleted;
begin
  PartToTrim.MarkAsCompleted;
  ApplyTrimStageToUI;
end;

procedure TTrimThread.TrimNextPartition;
begin
  TrimPartition(PartToTrim.GetNextPartition.PartitionName);
  SetThisPartitionAsCompleted;
end;

procedure TTrimThread.TrimPartition(const DriveLetter: String);
begin

end;

procedure TTrimThread.TrimEachPartition;
var
  CurrDrive: Integer;
begin
  for CurrDrive := 0 to PartToTrim.Count - 1 do
    TrimNextPartition;
end;

procedure TTrimThread.TrimAppliedPartitions;
begin
  InitializeTrim;
  TrimEachPartition;
  CleanupTrim;
end;

procedure TTrimThread.TryToTrimPartitions;
begin
  try
    TrimAppliedPartitions;
  except
    SetTrimError;
  end;
end;

procedure TTrimThread.Execute;
begin
  TryToTrimPartitions;
end;

procedure TTrimThread.ApplyProgress;
begin
  fMain.pDownload.Position := Progress;
end;

procedure TTrimThread.ApplyStage;
begin
  fMain.pDownload.Position := Progress;

  if PartToTrim.CompletedPartition < 0 then
    exit;

  if PartToTrim.CompletedPartition < PartToTrim.Count then
    fMain.lProgress.Caption :=
      CapProg1[CurrLang] +
      PartToTrim[PartToTrim.CompletedPartition] + ' ' +
      CapProg2[CurrLang] + ' (' +
      IntToStr(PartToTrim.CompletedPartition + 1) + '/' +
      IntToStr(PartToTrim.Count) + ')';
end;

procedure TTrimThread.EndTrim;
begin
  fMain.pDownload.Height := fMain.pDownload.Height - 10;
  fMain.pDownload.Top := fMain.pDownload.Top - 5;
  fMain.pDownload.Position := 0;
  fMain.gTrim.Visible := true;
  fMain.HideProgress;
end;
end.

