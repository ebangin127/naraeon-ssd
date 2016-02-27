unit Getter.Filesystem.Name;

interface

uses
  Windows,
  OSFile.ForInternal;

type
  TFileSystemNameGetter = class(TOSFileForInternal)
  private
    FileSystemNameInCharArray: Array[0..MAX_PATH - 1] of Char;
  public
    function GetFileSystemName: String;
  end;

implementation

{ TFileSystemNameGetter }

function TFileSystemNameGetter.GetFileSystemName: String;
var
  Useless: DWORD;
  PathToGetFileSystemName: String;
begin
  Useless := 0;
  PathToGetFileSystemName := GetPathOfFileAccessing + '\';
  GetVolumeInformation(PChar(PathToGetFileSystemName), nil, 0, nil, Useless,
    Useless, FileSystemNameInCharArray, SizeOf(FileSystemNameInCharArray));
  result := PChar(@FileSystemNameInCharArray[0]);
end;

end.
