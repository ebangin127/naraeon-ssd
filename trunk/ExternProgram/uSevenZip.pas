unit uSevenZip;

interface

uses
  SysUtils,
  uProcessOpener, uCodesignVerifier;

type
  TSevenZip = class
  private
    class function BuildCommand
      (SzipPath, SrcFile, DestFolder, Password: String): String;
    class function VerifySevenZip(SzipPath: String): Boolean; static;
  public
    class function Extract
      (SzipPath, SrcFile, DestFolder: String;
       Password: String = ''): AnsiString;
  end;

implementation

{ TSevenZip }

class function TSevenZip.BuildCommand(SzipPath, SrcFile, DestFolder,
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

class function TSevenZip.VerifySevenZip(SzipPath: String): Boolean;
var
  CodesignVerifier: TCodesignVerifier;
begin
  CodesignVerifier := TCodesignVerifier.Create;
  result := CodesignVerifier.VerifySignByPublisher(SzipPath, 'Minkyu Kim');
  FreeAndNil(CodesignVerifier);
end;

class function TSevenZip.Extract
  (SzipPath, SrcFile, DestFolder, Password: String): AnsiString;
begin
  if VerifySevenZip(SzipPath) then
    result :=
      TProcessOpener.OpenProcWithOutput('C:\',
        BuildCommand(SzipPath, SrcFile, DestFolder, Password));
end;

end.
