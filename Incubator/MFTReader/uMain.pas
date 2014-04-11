unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uSSDInfo, uDiskFunctions;

type
  TfMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.dfm}

procedure TfMain.FormCreate(Sender: TObject);
var
  PartList: TDriveLetters;
begin
  PartList := GetPartitionList('1');
  ShowMessage(IntToStr(PartList.StartOffset[0]));
end;

end.
