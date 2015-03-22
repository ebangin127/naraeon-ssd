unit uFixedDriveListGetter;

interface

uses
  Windows, SysUtils, Classes,
  uOSFile;

type
  TFixedDriveList = TStringList;

  TFixedDriveListGetter = class sealed(TOSFile)
  public
    function GetFixedDriveList: TFixedDriveList;

  private
    FixedDriveList: TFixedDriveList;
    FixedDriveListInConcatString: String;
    ConcatStringArray: Array of PChar;

    CurrentCharPosition: Cardinal;

    function GetLogicalDriveInConcatString: String;
    procedure ConcatStringToTFixedDriveList;
    procedure AddNextDrive;
    function GoToNextCharIfInDriveOrFalse: Boolean;
    function IsThisCharNullChar: Boolean;
    function IsThisPointOverLimit: Boolean;
    procedure IfNotFixedDelete(CurrentDrive: Cardinal);
    procedure LeaveOnlyFixedDrives;
  end;

implementation

function TFixedDriveListGetter.GetLogicalDriveInConcatString: String;
var
  LengthOfLogicalDriveString: Cardinal;
begin
  SetLength(ConcatStringArray, 1);
  LengthOfLogicalDriveString :=
    GetLogicalDriveStrings(0, @ConcatStringArray[0]);
  SetLength(ConcatStringArray, LengthOfLogicalDriveString);
  GetLogicalDriveStrings(LengthOfLogicalDriveString, @ConcatStringArray[0]);
  IfOSErrorRaiseException;
  exit(String(ConcatStringArray));
end;

function TFixedDriveListGetter.IsThisPointOverLimit: Boolean;
begin
  result :=
    CurrentCharPosition = Cardinal(Length(FixedDriveListInConcatString));
end;

function TFixedDriveListGetter.IsThisCharNullChar: Boolean;
begin
  result := FixedDriveListInConcatString[CurrentCharPosition] = #0;
end;

function TFixedDriveListGetter.GoToNextCharIfInDriveOrFalse: Boolean;
begin
  result := true;

  if IsThisPointOverLimit then
    result := false
  else if IsThisCharNullChar then
    result := false
  else
    Inc(CurrentCharPosition, 1);
end;

procedure TFixedDriveListGetter.AddNextDrive;
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
    FixedDriveList.Add(
      Copy(FixedDriveListInConcatString, StartingPoint,
        CurrentCharPosition - StartingPoint));
  Inc(CurrentCharPosition, 1);
end;

procedure TFixedDriveListGetter.ConcatStringToTFixedDriveList;
begin
  FixedDriveList := TFixedDriveList.Create;

  CurrentCharPosition := 1;
  while CurrentCharPosition <
    Cardinal(Length(FixedDriveListInConcatString)) do
      AddNextDrive;
end;

procedure TFixedDriveListGetter.IfNotFixedDelete(CurrentDrive: Cardinal);
begin
  if GetDriveType(PChar(FixedDriveList[CurrentDrive])) <> DRIVE_FIXED then
    FixedDriveList.Delete(CurrentDrive);
end;

procedure TFixedDriveListGetter.LeaveOnlyFixedDrives;
var
  CurrentDrive: Cardinal;
begin
  for CurrentDrive := 0 to (FixedDriveList.Count - 1) do
    IfNotFixedDelete(CurrentDrive);
end;

function TFixedDriveListGetter.GetFixedDriveList: TFixedDriveList;
begin
  FixedDriveListInConcatString := GetLogicalDriveInConcatString;
  ConcatStringToTFixedDriveList;
  LeaveOnlyFixedDrives;
  result := FixedDriveList;
end;

end.

