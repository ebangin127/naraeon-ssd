unit Getter.CodesignVerifier;

interface

uses
  SysUtils, Windows,
  Getter.CodesignVerifier.Publisher;

type
  TCodesignVerifier = class
  private
    type
      TWinTrustFileInfo = record
        cbStruct: DWORD;
        pcwszFilePath: PWideChar;
        hFile: THandle;
        pgKnownSubject: PGUID;
      end;

      TWinTrustData = record
        cbStruct: DWORD;
        pPolicyCallbackData: Pointer;
        pSIPClientData: Pointer;
        dwUIChoice: DWORD;
        fdwRevocationChecks: DWORD;
        dwUnionChoice: DWORD;
        pFile: Pointer;
        dwStateAction: DWORD;
        hWVTStateData: THandle;
        pwszURLReference: PWideChar;
        dwProvFlags: DWORD;
        dwsUIContext: DWORD;
      end;

    const
      WINTRUST_ACTION_GENERIC_VERIFY_V2: TGUID =
        '{00AAC56B-CD44-11d0-8CC2-00C04FC295EE}';

    const
      WTD_REVOKE_NONE = 0;
      WTD_REVOKE_WHOLECHAIN = 1;

    const
      WTD_UI_ALL = 1;
      WTD_UI_NONE = 2;
      WTD_UI_NOBAD = 3;
      WTD_UI_NOGOOD = 4;

    const
      WTD_CHOICE_FILE = 1;
      WTD_CHOICE_CATALOG = 2;
      WTD_CHOICE_BLOB = 3;
      WTD_CHOICE_SIGNER = 4;
      WTD_CHOICE_CERT = 5;

  private
    WinTrustFileInfo: TWinTrustFileInfo;
    WinTrustData: TWinTrustData;
    procedure SetWinTrustData;
    procedure SetWinTrustFileInfo(const PathToVerify: String);
    function VerifyAndReturnResult: Boolean;
    function IsCodeSigned(const PathToVerify: String): Boolean;
    function VerifyPublisher(const PathToVerify, ExpectedPublisher: string):
      Boolean;

  public
    function VerifySignByPublisher(const PathToVerify,
      ExpectedPublisher: string): Boolean;
  end;

implementation

{ TCodesignVerifier }

procedure TCodesignVerifier.SetWinTrustFileInfo(const PathToVerify: String);
begin
  ZeroMemory(@WinTrustFileInfo, SizeOf(WinTrustFileInfo));
  WinTrustFileInfo.cbStruct := sizeof(WinTrustFileInfo);
  WinTrustFileInfo.pcwszFilePath := PWideChar(WideString(PathToVerify));
end;

procedure TCodesignVerifier.SetWinTrustData;
begin
  ZeroMemory(@WinTrustData, SizeOf(WinTrustData));
  WinTrustData.cbStruct := sizeof(WinTrustData);
  WinTrustData.dwUIChoice := WTD_UI_NONE;
  WinTrustData.fdwRevocationChecks := WTD_REVOKE_NONE;
  WinTrustData.dwUnionChoice := WTD_CHOICE_FILE;
  WinTrustData.pFile := @WinTrustFileInfo;
end;

function TCodesignVerifier.VerifyAndReturnResult: Boolean;
var
  ErrorCode: Cardinal;
begin
  ErrorCode :=
    WinVerifyTrust(INVALID_HANDLE_VALUE, WINTRUST_ACTION_GENERIC_VERIFY_V2,
      @WinTrustData);
  result := ErrorCode = ERROR_SUCCESS;
end;

function TCodesignVerifier.VerifyPublisher(const PathToVerify,
  ExpectedPublisher: string): Boolean;
var
  CodesignPublisherVerifier: TCodesignPublisherVerifier;
begin
  CodesignPublisherVerifier := TCodesignPublisherVerifier.Create;
  result := CodesignPublisherVerifier.VerifySignByPublisher(PathToVerify,
    ExpectedPublisher);
  FreeAndNil(CodesignPublisherVerifier);
end;

function TCodesignVerifier.VerifySignByPublisher(const PathToVerify,
  ExpectedPublisher: string): Boolean;
begin
  exit(true);
  result := IsCodeSigned(PathToVerify);
  if ExtractFileExt(PathToVerify) <> '.exe' then
    exit;
  if not result then
    exit
  else
    result := VerifyPublisher(PathToVerify, ExpectedPublisher);
end;

function TCodesignVerifier.IsCodeSigned(const PathToVerify: String): Boolean;
begin
  SetWinTrustFileInfo(PathToVerify);
  SetWinTrustData;
  result := VerifyAndReturnResult;
end;

end.
