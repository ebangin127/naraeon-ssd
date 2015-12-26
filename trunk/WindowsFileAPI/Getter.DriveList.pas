unit Getter.DriveList;

interface

uses
  Windows, SysUtils, Classes,
  OSFile;

type
  TDriveList = TStringList;

  TDriveListGetter = class abstract(TOSFile)
  public
    function GetDriveList: TDriveList;
  
  protected
    function GetDriveTypeToGet: Cardinal; virtual; abstract;

  private
    SpecifiedDriveList: TDriveList;
    LogicalDriveString: Array of WideChar;

    CurrentCharPosition: Cardinal;

    procedure SetLogicalDriveInConcatString;
    procedure ConcatStringToTDriveList;
    procedure AddNextDrive;
    function GoToNextCharIfInDriveOrFalse: Boolean;
    function IsThisCharNullChar: Boolean;
    function IsThisPointOverLimit: Boolean;
    procedure IfNotFixedDelete(var CurrentDrive: Cardinal);
    procedure LeaveOnlySpecifiedDrives;
    procedure TryToGetDriveList;
  end;

implementation

procedure TDriveListGetter.SetLogicalDriveInConcatString;
var
  LengthOfLogicalDriveString: Cardinal;
begin
  SetLength(LogicalDriveString, 1);
  LengthOfLogicalDriveString :=
    GetLogicalDriveStrings(0, @LogicalDriveString[0]);
  SetLength(LogicalDriveString, LengthOfLogicalDriveString);
  GetLogicalDriveStrings(LengthOfLogicalDriveString, @LogicalDriveString[0]);
end;

function TDriveListGetter.IsThisPointOverLimit: Boolean;
begin
  result :=
    CurrentCharPosition = Cardinal(Length(LogicalDriveString));
end;

function TDriveListGetter.IsThisCharNullChar: Boolean;
begin
  result := LogicalDriveString[CurrentCharPosition] = #0;
end;

function TDriveListGetter.GoToNextCharIfInDriveOrFalse: Boolean;
begin
  result := true;

  if IsThisPointOverLimit then
    result := false
  else if IsThisCharNullChar then
    result := false
  else
    Inc(CurrentCharPosition, 1);
end;

procedure TDriveListGetter.AddNextDrive;
  procedure Nothing;
  begin
    //Nothing, but NOP procedure for infinite loop
  end;
var
  StartingPoint: Cardinal;
begin
  StartingPoint := CurrentCharPosition;
  while GoToNextCharIfInDriveOrFalse do
    Nothing;
  if CurrentCharPosition <> StartingPoint then
    SpecifiedDriveList.Add(
      PChar(@LogicalDriveString[StartingPoint]));
  Inc(CurrentCharPosition, 1);
end;

procedure TDriveListGetter.ConcatStringToTDriveList;
begin
  SpecifiedDriveList := TDriveList.Create;

  CurrentCharPosition := 0;
  while CurrentCharPosition <
    Cardinal(Length(LogicalDriveString)) do
      AddNextDrive;
end;

procedure TDriveListGetter.IfNotFixedDelete(var CurrentDrive: Cardinal);
begin
  if GetDriveType(PChar(SpecifiedDriveList[CurrentDrive])) <> 
     GetDriveTypeToGet then
      SpecifiedDriveList.Delete(CurrentDrive)
  else
    CurrentDrive := CurrentDrive + 1;
end;

procedure TDriveListGetter.LeaveOnlySpecifiedDrives;
var
  CurrentDrive: Cardinal;
begin
  CurrentDrive := 0;
  while Integer(CurrentDrive) <= SpecifiedDriveList.Count - 1 do
    IfNotFixedDelete(CurrentDrive);
end;

procedure TDriveListGetter.TryToGetDriveList;
begin
  SetLogicalDriveInConcatString;
  ConcatStringToTDriveList;
  LeaveOnlySpecifiedDrives;
end;

function TDriveListGetter.GetDriveList: TDriveList;
begin
  try
    TryToGetDriveList;
  except
    FreeAndNil(SpecifiedDriveList);
  end;
  result := SpecifiedDriveList;
end;

end.

