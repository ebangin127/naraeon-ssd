unit Thread.Trim;

interface

uses
  Classes, SysUtils,
  TrimList, Thread.Trim.Helper.List;

type
  TTrimStage = (Initial, InProgress, Finished, Error);

  TTrimThread = class(TThread)
  private
    class var InnerTrimStage: TTrimStage;
    ListTrimmer: TListTrimmer;

  public
    class property TrimStage: TTrimStage read InnerTrimStage;

    constructor Create
      (CreateSuspended: Boolean; IsUIInteractionNeeded: Boolean);
    destructor Destroy; override;
    function SetPartitionList(PartitionsToTrim: TTrimList): Boolean;

  protected
    procedure Execute; override;

  end;

implementation

constructor TTrimThread.Create(CreateSuspended, IsUIInteractionNeeded: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := true;
  ListTrimmer := TListTrimmer.Create(IsUIInteractionNeeded);
  InnerTrimStage := TTrimStage.Initial;
end;

destructor TTrimThread.Destroy;
begin
  FreeAndNil(ListTrimmer);
  inherited Destroy;
end;

function TTrimThread.SetPartitionList(PartitionsToTrim: TTrimList): Boolean;
begin
  result := ListTrimmer.SetPartitionList(PartitionsToTrim);
end;

procedure TTrimThread.Execute;
begin
  InnerTrimStage := TTrimStage.InProgress;
  if ListTrimmer.IsUIInteractionNeeded then
    ListTrimmer.TrimAppliedPartitionsWithUI(Self)
  else
    ListTrimmer.TrimAppliedPartitionsWithoutUI;
  InnerTrimStage := TTrimStage.Finished;
end;
end.

