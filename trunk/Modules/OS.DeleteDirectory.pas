unit OS.DeleteDirectory;

interface

uses SysUtils, ShellAPI;

function DeleteDirectory(const DirectoryToDelete: String): Boolean;

implementation

function GetSHFileOpStruct(const DirectoryToDelete: String): TSHFileOpStruct;
begin
  FillChar(result, sizeof(SHFileOpStruct), 0);
  result.Wnd := 0;
  result.pFrom := PChar(DirectoryToDelete);
  result.wFunc := FO_DELETE;
  result.fFlags := result.fFlags or FOF_NOCONFIRMATION;
  result.fFlags := result.fFlags or FOF_SILENT;
end;
    
function TryToDeleteFoundDirectory(const DirectoryToDelete,
  FoundFile: String): Boolean;
var
  CurrentDirectoryToDelete: String;
begin
  CurrentDirectoryToDelete := ExcludeTrailingPathDelimiter(
    DirectoryToDelete + FoundFile);
  result := (SHFileOperation(GetSHFileOpStruct(
    CurrentDirectoryToDelete)) = 0);
end;

function DeleteDirectory(const DirectoryToDelete: String): Boolean;
var
  ZeroFoundElseNotFound: integer;
  SearchRecord: TSearchRec;
begin
  result := false;
  ZeroFoundElseNotFound := FindFirst(DirectoryToDelete + '*.*', faAnyFile,
    SearchRecord);
    
  while ZeroFoundElseNotFound = 0 do
  begin
    TryToDeleteFoundDirectory(DirectoryToDelete, SearchRecord.Name);
    ZeroFoundElseNotFound := FindNext(SearchRecord);
  end;
  
  FindClose(SearchRecord);
end;

end.
