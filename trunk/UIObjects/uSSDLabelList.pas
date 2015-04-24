unit uSSDLabelList;

interface

uses
  SysUtils, Generics.Collections,
  uSSDLabel, uPhysicalDrive;

type
  TSSDLabelList = class(TList<TSSDLabel>)
  private
    const
      LabelPerLine = 10;
      HorizontalPadding = 10;
      VerticalPadding = 10;
    function GetMaxHeight: Integer;
    function GetMaxWidth: Integer;
    procedure SetFirstLabelLeft(Index: Integer);
    procedure SetFirstLabelTop(Index: Integer);
    procedure SetGroupboxSize;
    procedure SetLabelPosition;
    procedure SetMidLabelLeft(Index: Integer);
    procedure SetMidLabelTop(Index: Integer);
    procedure SetSingleLabelLeft(Index: Integer);
    procedure SetSingleLabelPosition(Index: Integer);
    procedure SetSingleLabelTop(Index: Integer);
  public
    destructor Destroy; override;
    procedure Delete(Index: Integer);
    procedure Add(SSDLabel: TSSDLabel);
    function IndexOf(Entry: TPhysicalDrive): Integer;
    function IsExists(Entry: TPhysicalDrive): Boolean;
  end;

  EIndexOfNotFound = class(Exception);
  
implementation

uses uMain;

procedure TSSDLabelList.Delete(Index: Integer);
begin
  Self[Index].Free;
  inherited Delete(Index);
end;

procedure TSSDLabelList.Add(SSDLabel: TSSDLabel);
begin
  inherited Add(SSDLabel);
  SetLabelPosition;
  SetGroupboxSize;
end;

function TSSDLabelList.GetMaxWidth: Integer;
var
  CurrentEntry: TSSDLabel;
begin
  if Self.Count = 0 then
    exit(0);

  result := 0;
  for CurrentEntry in Self do
    if result < CurrentEntry.Left + CurrentEntry.Width then
      result := CurrentEntry.Left + CurrentEntry.Width;
end;

function TSSDLabelList.GetMaxHeight: Integer;
var
  CurrentEntry: TSSDLabel;
begin
  if Self.Count = 0 then
    exit(0);
    
  result := 0;
  for CurrentEntry in Self do
    if result < CurrentEntry.Top + CurrentEntry.Height then
      result := CurrentEntry.Top + CurrentEntry.Height;
end;

procedure TSSDLabelList.SetGroupboxSize;
begin
  fMain.gSSDSel.Width := GetMaxWidth + (HorizontalPadding * 2);
  fMain.gSSDSel.Height := GetMaxHeight + (VerticalPadding * 2);
end;

procedure TSSDLabelList.SetLabelPosition;
var
  CurrentLabel: Integer;
begin
  for CurrentLabel := 0 to fMain.SSDLabel.Count - 1 do
    SetSingleLabelPosition(CurrentLabel);
end;

procedure TSSDLabelList.SetFirstLabelTop(Index: Integer);
begin
  Self[Index].Top := VerticalPadding;
end;

procedure TSSDLabelList.SetFirstLabelLeft(Index: Integer);
begin
  Self[Index].Left := HorizontalPadding;
end;

procedure TSSDLabelList.SetMidLabelTop(Index: Integer);
begin
  Self[Index].Top := Self[Index - 1].Top + Self[Index - 1].Height +
    VerticalPadding;
end;

procedure TSSDLabelList.SetMidLabelLeft(Index: Integer);
begin
  Self[Index].Left := 
    Self[Index - LabelPerLine].Left +
    Self[Index - LabelPerLine].Width +
    HorizontalPadding;
end;

procedure TSSDLabelList.SetSingleLabelTop(Index: Integer);
begin
  if ((Index + 1) mod (LabelPerLine)) = 1 then
    SetFirstLabelTop(Index)
  else
    SetMidLabelTop(Index);
end;

procedure TSSDLabelList.SetSingleLabelLeft(Index: Integer);
begin
  if ((Index + 1) div (LabelPerLine)) = 0 then
    SetFirstLabelLeft(Index)
  else
    SetMidLabelLeft(Index);
end;

procedure TSSDLabelList.SetSingleLabelPosition(Index: Integer);
begin
  SetSingleLabelLeft(Index);
  SetSingleLabelTop(Index);
end;

destructor TSSDLabelList.Destroy;
var
  CurrentItem: Integer;
begin
  for CurrentItem := 0 to Count - 1 do
    Delete(0);
  inherited;
end;

function TSSDLabelList.IndexOf(Entry: TPhysicalDrive): Integer;
var
  CurrentEntry: Integer;
begin
  for CurrentEntry := 0 to Count - 1 do
    if Entry.IsPathEqual(self[CurrentEntry].PhysicalDrive) then
      exit(CurrentEntry);
      
  raise EIndexOfNotFound.Create('EIndexOfNotFound: This list does not contain' +
    ' that item');
end;

function TSSDLabelList.IsExists(Entry: TPhysicalDrive): Boolean;
begin
  try
    result := true;
    IndexOf(Entry);
  except
    result := false;
  end;
end;

end.