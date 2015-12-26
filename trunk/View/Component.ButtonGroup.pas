unit Component.ButtonGroup;

interface

uses
  Forms, StdCtrls, ExtCtrls, Generics.Collections;

type
  TClickEventProcedure =
    procedure (IsOpening: Boolean; Sender: TObject; GroupBox: TGroupBox);

  TButtonGroupEntry = record
    Selected: Boolean;
    ImageButton: TImage;
    LabelButton: TLabel;
    GroupBox: TGroupBox;
    ClickEventProcedure: TClickEventProcedure;
  end;

  TClickResult = (clkError, clkOpen, clkClose);

  TButtonGroup = class(TList<TButtonGroupEntry>)
  private
    FForm: TForm;
    FMaxHeight, FMinHeight: Integer;
    FMaxWidth, FMinWidth: Integer;

    procedure FindJobProcedure
      (IsRight: Boolean; Sender: TObject; Action: TClickResult;
       Entry: TButtonGroupEntry);
    function FindEntryAndDo(Sender: TObject): TClickResult;

  public
    constructor Create(iForm: TForm;
      iMaxHeight, iMinHeight, iMaxWidth, iMinWidth: Integer);
    function Click(Sender: TObject): TClickResult;
    procedure AddEntry(iSelected: Boolean; iImageButton: TImage;
      iLabelButton: TLabel; iGroupBox: TGroupBox;
      iClickEventProcedure: TClickEventProcedure);
    function FindEntry(Sender: TObject): TButtonGroupEntry;

    procedure Open;
    procedure Close;
    procedure CloseAll;

    property Form: TForm read FForm;
    property MaxHeight: Integer read FMaxHeight;
    property MinHeight: Integer read FMinHeight;
    property MaxWidth: Integer read FMaxWidth;
    property MinWidth: Integer read FMinWidth;
  end;

implementation

{ TButtonGroup }

procedure TButtonGroup.Open;
begin
  if FForm = nil then
    exit;

  with FForm do
  begin
    Constraints.MaxHeight := 0;
    Constraints.MinHeight := 0;
    Constraints.MaxWidth := 0;
    Constraints.MaxWidth := 0;

    ClientHeight := MaxHeight;
    ClientWidth := MaxWidth;

    Constraints.MaxHeight := Height;
    Constraints.MinHeight := Height;
    Constraints.MaxWidth := Width;
    Constraints.MaxWidth := Width;
  end;
end;

procedure TButtonGroup.Close;
begin
  if FForm = nil then
    exit;

  with FForm do
  begin
    Constraints.MaxHeight := 0;
    Constraints.MinHeight := 0;
    Constraints.MaxWidth := 0;
    Constraints.MaxWidth := 0;

    ClientHeight := MinHeight;
    ClientWidth := MinWidth;

    Constraints.MaxHeight := Height;
    Constraints.MinHeight := Height;
    Constraints.MaxWidth := Width;
    Constraints.MaxWidth := Width;
  end;
end;

procedure TButtonGroup.CloseAll;
var
  CurrEntry: Integer;
begin
  for CurrEntry := 0 to (self.Count - 1) do
  begin
    List[CurrEntry].Selected := false;
    List[CurrEntry].GroupBox.Visible := false;
  end;

  Close;
end;

procedure TButtonGroup.AddEntry(iSelected: Boolean; iImageButton: TImage;
  iLabelButton: TLabel; iGroupBox: TGroupBox;
  iClickEventProcedure: TClickEventProcedure);
var
  TempEntry: TButtonGroupEntry;
begin
  TempEntry.Selected := iSelected;
  TempEntry.ImageButton := iImageButton;
  TempEntry.LabelButton := iLabelButton;
  TempEntry.GroupBox := iGroupBox;
  TempEntry.ClickEventProcedure := @iClickEventProcedure;

  Add(TempEntry);
end;

function TButtonGroup.Click(Sender: TObject): TClickResult;
begin
  exit(FindEntryAndDo(Sender));
end;

constructor TButtonGroup.Create(iForm: TForm; iMaxHeight, iMinHeight, iMaxWidth,
  iMinWidth: Integer);
begin
  inherited Create;

  FForm := iForm;
  FMaxHeight := iMaxHeight;
  FMinHeight := iMinHeight;
  FMaxWidth := iMaxWidth;
  FMinWidth := iMinWidth;
end;

procedure TButtonGroup.FindJobProcedure
  (IsRight: Boolean; Sender: TObject; Action: TClickResult;
   Entry: TButtonGroupEntry);
const
  Closing = False;
  Opening = True;
begin
  Entry.GroupBox.Visible := IsRight;
  if IsRight then
    Entry.GroupBox.BringToFront;

  if not IsRight then
    exit;

  if IsRight then
    Close;

  if @Entry.ClickEventProcedure <> nil then
    Entry.ClickEventProcedure(Closing, Sender, Entry.GroupBox);

  if (IsRight) and (Action = clkClose) then
    exit;

  if @Entry.ClickEventProcedure <> nil then
    Entry.ClickEventProcedure(Opening, Sender, Entry.GroupBox);

  Open;
end;

function TButtonGroup.FindEntry(Sender: TObject): TButtonGroupEntry;
var
  CurrEntry: Integer;
begin
  FillChar(result, SizeOf(TButtonGroupEntry), 0);

  for CurrEntry := 0 to (self.Count - 1) do
    if (Sender = List[CurrEntry].ImageButton) or
       (Sender = List[CurrEntry].LabelButton) then
      exit(List[CurrEntry]);
end;

function TButtonGroup.FindEntryAndDo(Sender: TObject): TClickResult;
var
  CurrEntry: Integer;
  IsRight: Boolean;
begin
  result := clkError;

  for CurrEntry := 0 to (self.Count - 1) do
  begin
    IsRight := (Sender = List[CurrEntry].ImageButton) or
               (Sender = List[CurrEntry].LabelButton);


    if IsRight then
    begin
      if List[CurrEntry].Selected then
        result := clkClose
      else
        result := clkOpen;
    end;

    FindJobProcedure(IsRight, Sender, result, List[CurrEntry]);

    List[CurrEntry].Selected :=
      (IsRight) and (result = clkOpen);
  end;
end;


end.
