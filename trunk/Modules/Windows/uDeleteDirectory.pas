unit uFileFunctions;

interface

uses SysUtils, ShellAPI;

function DeleteDirectory(const DirectoryToDelete: String): Boolean;

implementation

function GetSHFileOpStruct(const DirectoryToDelete: String): TSHFileOpStruct;
begin
  FillChar(SHFileOpStruct, sizeof(SHFileOpStruct), 0);
  result.Wnd := 0;
  result.pFrom := PChar(DirectoryToDelete);
  result.wFunc := FO_DELETE;
  result.fFlags := result.fFlags or FOF_NOCONFIRMATION;
  result.fFlags := result.fFlags or FOF_SILENT;
end;
    
function TryToDeleteFoundDirectory(const DirectoryToDelete,
  FoundFile: String): Boolean;
begin
  try
    CurrentDirectoryToDelete := ExcludeTrailingPathDelimiter(
      DirectoryToDelete + srSchRec.Name);
    result := (SHFileOperation(GetSHFileOpStruct(
      CurrentDirectoryToDelete)) = 0);
  except
    result := False;
  end;
end;

function DeleteDirectory(const DirectoryToDelete: String): Boolean;
var
  CurrentDirectoryToDelete: string;
  ZeroFoundElseNotFound: integer;
  SearchRecord: TSearchRec;
begin
  result := false;
  ZeroFoundElseNotFound := FindFirst(DirectoryToDelete + '*.*', faAnyFile,
    SearchRecord);
    
  while ZeroFoundElseNotFound = 0 do
  begin
    TryToDeleteFoundDirectory(DirectoryToDelete, SearchRecord.Name);
    ZeroFoundElseNotFound := FindNext(srSchRec);
  end;
  
  FindClose(srSchRec);
end;

end.
