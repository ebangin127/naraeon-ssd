unit uMockAutoTrimBasicsGetter;

interface

uses
  Classes, SysUtils, Windows,
  uOSFile;

type
  TTrimProgress = record
    CurrentPartition: Integer;
    PartitionCount: Integer;
  end;

  TTrimSynchronization = record
    IsUIInteractionNeeded: Boolean;
    ThreadToSynchronize: TThread;
    Progress: TTrimProgress;
  end;

  TTrimBasicsToInitialize = record
    PaddingLBA: Integer;
    LBAPerCluster: Cardinal;
    StartLBA: UInt64;
  end;

  TAutoTrimBasicsGetter = class(TOSFile)
  public
    function IsPartitionMyResponsibility: Boolean;
    function GetTrimBasicsToInitialize: TTrimBasicsToInitialize;
  end;

  EUnknownPartition = class(Exception);

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
