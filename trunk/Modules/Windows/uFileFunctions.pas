unit uFileFunctions;

interface

uses SysUtils, ShellAPI;

function DeleteDirectory(Const DirPath: String): Boolean;

implementation

function DeleteDirectory(Const DirPath: String): Boolean;
var
  SHFileOpStruct: TSHFileOpStruct;
  DirBuf: array [0..255] of char;
  Directory: string;
  iFindResult: integer;
 srSchRec : TSearchRec;
begin
  Result := False;
  iFindResult := FindFirst(DirPath + '*.*', faAnyFile, srSchRec);
  while iFindResult = 0 do
  begin
    try
      Directory := ExcludeTrailingPathDelimiter(DirPath + srSchRec.Name);
      Fillchar(SHFileOpStruct, sizeof(SHFileOpStruct), 0);
      FillChar(DirBuf, sizeof(DirBuf), 0);
      StrPCopy(DirBuf, Directory);
      with SHFileOpStruct do
      begin
        Wnd := 0;
        pFrom := @DirBuf;
        wFunc := FO_DELETE;
        fFlags := fFlags or FOF_NOCONFIRMATION;
        fFlags := fFlags or FOF_SILENT;
      end;
      Result := (SHFileOperation(SHFileOpStruct) = 0);
    except
      Result := False;
    end;
    iFindResult := FindNext(srSchRec);
  end;
  FindClose(srSchRec);
end;

end.
