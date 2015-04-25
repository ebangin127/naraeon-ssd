unit uCodesignExtern;

interface

uses Windows;

type
  PCCERT_CONTEXT = Pointer;
  HCRYPTPROV_LEGACY = Pointer;
  PFN_CRYPT_GET_SIGNER_CERTIFICATE = Pointer;
  CRYPT_VERIFY_MESSAGE_PARA = record
    cbSize: DWORD;
    dwMsgAndCertEncodingType: DWORD;
    hCryptProv: HCRYPTPROV_LEGACY;
    pfnGetSignerCertificate: PFN_CRYPT_GET_SIGNER_CERTIFICATE;
    pvGetArg: Pointer;
  end;
  TCertificateContext = record
    Context: PCCERT_CONTEXT;
    IsContextValid: Boolean;
  end;

const
  PKCS_7_ASN_ENCODING = $00010000;
  X509_ASN_ENCODING = $00000001;
  CERT_SECTION_TYPE_ANY = $FF;

function ImageEnumerateCertificates(FileHandle: THandle; TypeFilter: WORD;
  out CertificateCount: DWORD; Indicies: PDWORD; IndexCount: Integer): BOOL;
  stdcall; external 'Imagehlp.dll';
function ImageGetCertificateHeader(FileHandle: THandle;
  CertificateIndex: Integer; var CertificateHeader: TWinCertificate): BOOL;
  stdcall; external 'Imagehlp.dll';
function ImageGetCertificateData(FileHandle: THandle;
  CertificateIndex: Integer; Certificate: PWinCertificate;
  var RequiredLength: DWORD): BOOL; stdcall; external 'Imagehlp.dll';
function CryptVerifyMessageSignature(
  const pVerifyPara: CRYPT_VERIFY_MESSAGE_PARA;
  dwSignerIndex: DWORD; pbSignedBlob: PByte; cbSignedBlob: DWORD;
  pbDecoded: PBYTE; pcbDecoded: PDWORD; ppSignerCert: PCCERT_CONTEXT): BOOL;
  stdcall; external 'Crypt32.dll';
function CertGetNameStringW(pCertContext: PCCERT_CONTEXT; dwType: DWORD;
  dwFlags: DWORD; pvTypePara: Pointer; pszNameString: PChar;
  cchNameString: DWORD): DWORD; stdcall; external 'Crypt32.dll';
function CertFreeCertificateContext(pCertContext: PCCERT_CONTEXT): BOOL;
  stdcall; external 'Crypt32.dll';

implementation

end.
