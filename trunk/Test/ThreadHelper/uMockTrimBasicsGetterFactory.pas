unit uMockTrimBasicsGetterFactory;

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

  TTrimBasicsGetter = class(TOSFile)
  public
    function IsPartitionMyResponsibility: Boolean;
    function GetTrimBasicsToInitialize: TTrimBasicsToInitialize;
  end;

  TTrimBasicsGetterFactory = class
  public
    class function GetSuitableTrimBasicsGetter(FileToGetAccess: String):
      TTrimBasicsGetter;
  end;

  EUnknownPartition = class(Exception);

implementation

function TTrimBasicsGetter.IsPartitionMyResponsibility: Boolean;
begin
  result := true;
end;

function TTrimBasicsGetter.GetTrimBasicsToInitialize:
  TTrimBasicsToInitialize;
begin
  result.StartLBA := 0;
  result.LBAPerCluster := 1;
  result.PaddingLBA := 0;
end;

class function TTrimBasicsGetterFactory.GetSuitableTrimBasicsGetter(
  FileToGetAccess: String): TTrimBasicsGetter;
begin
  result := TTrimBasicsGetter.Create(FileToGetAccess);
end;

end.
