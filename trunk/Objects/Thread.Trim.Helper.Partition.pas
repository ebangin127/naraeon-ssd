unit Thread.Trim.Helper.Partition;

interface

uses
  OSFile
  {$IfNDef UNITTEST}, ThreadToView.Trim
  {$Else}, Mock.Getter.TrimBasics.Factory{$EndIf};

type
  TPartitionTrimmer = class abstract(TOSFile)
  public
    procedure TrimPartition(
      const TrimSynchronizationToApply: TTrimSynchronization);
      virtual; abstract;
    function GetPathOfFileAccessing: String; override;
  end;
  TCurrentPartitionTrimProgress = record
    Progress: Integer;
    BaseProgress: Integer;
    ProgressPerPartition: Integer;
  end;

implementation

function TPartitionTrimmer.GetPathOfFileAccessing: String;
begin
  result := inherited GetPathOfFileAccessing;
  result := ThisComputerPrefix + result;
end;

end.
