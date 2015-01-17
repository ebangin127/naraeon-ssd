unit uSevenZip;

interface

uses uExeFunctions;

type
  TSevenZip = class
  private
    class function BuildCommand
      (SzipPath, SrcFile, DestFolder, Password: String): String;
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

class function TSevenZip.Extract
  (SzipPath, SrcFile, DestFolder, Password: String): AnsiString;
begin
  result :=
    OpenProcWithOutput('C:\',
      BuildCommand(SzipPath, SrcFile, DestFolder, Password));
end;

end.
