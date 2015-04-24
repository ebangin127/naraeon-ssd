unit uSSDLabelList;

interface

uses
  SysUtils, Generics.Collections,
  uSSDLabel;

const
  EIndexOfNotFound = class(Exception);

type
  TSSDLabelList = class(TList<TSSDLabel>)
  private
    const
      LabelPerLine = 10;
      HorizontalPadding = 10;
      VerticalPadding = 10;
  public
    destructor Destroy; override;
    procedure Delete(Index: Integer); override;
    procedure Add(SSDLabel: TSSDLabel); override;
    function IndexOf(Entry: TPhysicalDrive): Integer;
  end;
  
implementation

uses uMain;

procedure TSSDLabelList.Delete(Index: Integer);
begin
  Self[Index].Free;
  inherited Delete(Index);
end;

procedure TSSDLabelList.Add(SSDLabel: TSSDLabel);
begin
  inherited Add(Index);
  SetLabelPosition;
  SetGroupboxSize;
end;

function TSSDLabelList.GetMaxWidth: Integer;
begin
  if Self.Count = 0 then
    exit(0);

  result :=
    Self[Self.Count - 1].Left +
    Self[Self.Count - 1].Width +
    HorizontalPadding;
end;

function TSSDLabelList.GetMaxHeight: Integer;
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

procedure TSSDLabelList.SetGroupboxSize;
begin
  fMain.gSSDSel.Width := GetMaxWidth; 
  fMain.gSSDSel.Height := GetMaxHeight;
end;

procedure TSSDLabelList.SetLabelPosition;
var
  CurrentLabel: Integer;
begin
  for CurrentLabel := 0 to SSDLabel.Count - 1 do
    SetSingleLabelPosition(CurrentLabel);

  gSSDSel.Height :=
    SSDLabel[SSDLabel.Count - 1].Top +
    SSDLabel[SSDLabel.Count - 1].Height + 5;
end;

procedure TSSDLabelList.SetFirstLabelTop(Index: Integer);
begin
  Self[Index].Top := VerticalPadding;
end;

procedure TSSDLabelList.SetFirstLabelLeft(Index: Integer);
const
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
  if ((CurrLabel + 1) mod (LabelPerLine)) = 0 then
    SetFirstLabelTop(Index)
  else
    SetMidLabelTop(Index);
end;

procedure TSSDLabelList.SetSingleLabelLeft(Index: Integer);
begin
  if ((CurrLabel + 1) div (LabelPerLine)) = 0 then
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

end.