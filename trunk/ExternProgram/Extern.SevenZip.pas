unit Extern.SevenZip;

interface

uses
  SysUtils,
  OS.ProcessOpener, Getter.CodesignVerifier;

type
  TSevenZip = class
  private
    function BuildCommand
      (SzipPath, SrcFile, DestFolder, Password: String): String;
    function VerifySevenZip(SzipPath: String): Boolean;
  public
    function Extract
      (SzipPath, SrcFile, DestFolder: String;
       Password: String = ''): AnsiString;
    class function Create: TSevenZip;
  end;

var
  SevenZip: TSevenZip;

implementation

{ TSevenZip }

function TSevenZip.BuildCommand(SzipPath, SrcFile, DestFolder,
  Password: String): String;
begin
  result :=
    '"' + SzipPath + '" e -y ' +
    '-o"' + DestFolder + '\" ' +
    '"' + SrcFile + '"';

  if Length(Password) = 0 then
    exit;

  result :=
    result + ' -p"' + Password + '"';
end;

function TSevenZip.VerifySevenZip(SzipPath: String): Boolean;
var
  CodesignVerifier: TCodesignVerifier;
begin
  CodesignVerifier := TCodesignVerifier.Create;
  result := CodesignVerifier.VerifySignByPublisher(SzipPath, 'Minkyu Kim');
  FreeAndNil(CodesignVerifier);
end;

class function TSevenZip.Create: TSevenZip;
begin
  if SevenZip = nil then
    result := inherited Create as self
  else
    result := SevenZip;
end;

function TSevenZip.Extract
  (SzipPath, SrcFile, DestFolder, Password: String): AnsiString;
begin
  if VerifySevenZip(SzipPath) then
    result :=
      ProcessOpener.OpenProcWithOutput('C:\',
        BuildCommand(SzipPath, SrcFile, DestFolder, Password));
end;

initialization
  SevenZip := TSevenZip.Create;
finalization
  SevenZip.Free;
end.
