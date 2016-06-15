unit Getter.CodesignVerifier.Publisher;

interface

uses
  Windows,
  OS.Codesign;

type
  TCodesignPublisherVerifier = class
  private
    FileHandle: THandle;
    ExpectedPublisher: String;
    PathToVerify: String;
    Certificate: PWinCertificate;
    function OpenFileHandleAndReturnResult: Boolean;
    function VerifyPublisherByHandle: Boolean;
    function TryToVerifyPublisherByHandle: Boolean;
    function ValidateCertificationCount: Boolean;
    function TryToVerifyPublisherWithCertificate: Boolean;
    function GetCertificateHeader: Boolean;
    function GetCertificateBody: Boolean;
    function GetCertificateContext: TCertificateContext;
    function TryToComparePublisherWithContext(ValidContext: PCCERT_CONTEXT):
      Boolean;
  public
    function VerifySignByPublisher(
      const PathToVerify, ExpectedPublisher: string): Boolean;
  end;

implementation

function TCodesignPublisherVerifier.ValidateCertificationCount: Boolean;
var
  CertificateCount: DWORD;
begin
  result :=
    (ImageEnumerateCertificates(FileHandle, CERT_SECTION_TYPE_ANY,
     CertificateCount, nil, 0)) and (CertificateCount = 1);
end;

function TCodesignPublisherVerifier.GetCertificateHeader: Boolean;
begin
  result := ImageGetCertificateHeader(FileHandle, 0, Certificate^);
end;

function TCodesignPublisherVerifier.GetCertificateBody: Boolean;
begin
  result := ImageGetCertificateData(FileHandle, 0, Certificate,
    Certificate.dwLength);
end;

function TCodesignPublisherVerifier.GetCertificateContext:
  TCertificateContext;
var
  VerifyParameter: CRYPT_VERIFY_MESSAGE_PARA;
begin
  ZeroMemory(@VerifyParameter, SizeOf(VerifyParameter));
  VerifyParameter.cbSize := SizeOf(VerifyParameter);
  VerifyParameter.dwMsgAndCertEncodingType :=
    X509_ASN_ENCODING or PKCS_7_ASN_ENCODING;
  result.IsContextValid :=
    CryptVerifyMessageSignature(VerifyParameter, 0, @Certificate.bCertificate,
      Certificate.dwLength, nil, nil, @result.Context)
end;

function TCodesignPublisherVerifier.TryToComparePublisherWithContext(
  ValidContext: PCCERT_CONTEXT): Boolean;
const
  CERT_NAME_SIMPLE_DISPLAY_TYPE = 4;
var
  Publisher: String;
  PublisherLength: DWORD;
begin
  PublisherLength := CertGetNameStringW(ValidContext,
    CERT_NAME_SIMPLE_DISPLAY_TYPE, 0, nil, nil, 0);
  SetLength(Publisher, PublisherLength - 1);
  CertGetNameStringW(ValidContext, CERT_NAME_SIMPLE_DISPLAY_TYPE, 0,
    nil, PChar(Publisher), PublisherLength);
  result := ExpectedPublisher = Publisher;
end;

function TCodesignPublisherVerifier.TryToVerifyPublisherWithCertificate:
  Boolean;
var
  ResultOfGettingContext: TCertificateContext;
begin
  result := false;

  if GetCertificateHeader then
    ReallocMem(Certificate, SizeOf(TWinCertificate) + Certificate.dwLength)
  else
    exit;

  if not GetCertificateBody then
    Exit;

  ResultOfGettingContext := GetCertificateContext;
  if not ResultOfGettingContext.IsContextValid then
    exit;

  try
    result := TryToComparePublisherWithContext(ResultOfGettingContext.Context);
  finally
    CertFreeCertificateContext(ResultOfGettingContext.Context);
  end;
end;

function TCodesignPublisherVerifier.TryToVerifyPublisherByHandle: Boolean;
const
  Padding = 3;
begin
  result := false;
  if not ValidateCertificationCount then
    Exit;

  GetMem(Certificate, SizeOf(TWinCertificate) + Padding);
  try
    Certificate.dwLength := 0;
    Certificate.wRevision := WIN_CERT_REVISION_1_0;
    result := TryToVerifyPublisherWithCertificate;
  finally
    FreeMem(Certificate);
  end;
end;

function TCodesignPublisherVerifier.VerifyPublisherByHandle: Boolean;
begin
  try
    result := TryToVerifyPublisherByHandle;
  finally
    CloseHandle(FileHandle);
  end;
end;

function TCodesignPublisherVerifier.OpenFileHandleAndReturnResult: Boolean;
begin
  FileHandle := CreateFile(PChar(PathToVerify), GENERIC_READ, FILE_SHARE_READ,
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_RANDOM_ACCESS, 0);
  result := (FileHandle <> INVALID_HANDLE_VALUE) and
    (FileHandle <> 0);
end;

function TCodesignPublisherVerifier.VerifySignByPublisher(const PathToVerify,
  ExpectedPublisher: string): Boolean;
begin
  Result := false;
  Self.ExpectedPublisher := ExpectedPublisher;
  Self.PathToVerify := PathToVerify;
  if not OpenFileHandleAndReturnResult then
    exit;
  Result := VerifyPublisherByHandle;
end;
end.
