unit OS.SecurityDescriptor;

interface

uses
  AccCtrl, AclApi, SysUtils, Windows;

type
  TFixedSIDArray = Array[0..7] of Cardinal;

  TSecurityDescriptorManipulator = class
  private
    ACLPointer: PACL;
    SecurityDescriptor: SECURITY_DESCRIPTOR;
    FExplicitAccess: Array of EXPLICIT_ACCESS;
    procedure AllocateSid(
      RIDs: Array of Cardinal;
      var IdentifierAuthority: _SID_IDENTIFIER_AUTHORITY;
      var SID: PSID);
    function CombineAccessTemplate(const AccessTemplate: EXPLICIT_ACCESS;
      const TrusteeTemplate: TRUSTEE): EXPLICIT_ACCESS;
    procedure ApplyToAcl(const ExplicitAccess: EXPLICIT_ACCESS);
    procedure CopyRIDs(
      var FixedLengthRIDs: TFixedSIDArray;
      var RIDs: Array of Cardinal);
    procedure SetSecurityForEveryone;
    procedure SetSecurityForAdministrator;
    procedure SetACLToSecurityDescriptor;
    procedure AddNewAccessToArray(ExplicitAccess: EXPLICIT_ACCESS);

  public
    function GetSecurityDescriptor: PSECURITY_DESCRIPTOR;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TSecurityDescriptor }

function TSecurityDescriptorManipulator.GetSecurityDescriptor:
  PSECURITY_DESCRIPTOR;
begin
  SetSecurityForEveryone;
  SetACLToSecurityDescriptor;
  SetSecurityForAdministrator;
  SetACLToSecurityDescriptor;
  exit(@SecurityDescriptor);
end;

procedure TSecurityDescriptorManipulator.CopyRIDs(
  var FixedLengthRIDs: TFixedSIDArray;
  var RIDs: Array of Cardinal);
var
  CurrentValidItem: Integer;
begin
  ZeroMemory(@FixedLengthRIDs, SizeOf(Cardinal) * 8);
  for CurrentValidItem := 0 to Length(RIDs) - 1 do
    FixedLengthRIDs[CurrentValidItem] := RIDs[CurrentValidItem];
end;

constructor TSecurityDescriptorManipulator.Create;
begin
  InitializeSecurityDescriptor(@SecurityDescriptor,
    SECURITY_DESCRIPTOR_REVISION);
end;

destructor TSecurityDescriptorManipulator.Destroy;
var
  CurrentItem: EXPLICIT_ACCESS;
begin
  for CurrentItem in FExplicitAccess do
    FreeSid(CurrentItem.Trustee.ptstrName);
  if ACLPointer <> nil then
    LocalFree(NativeUInt(ACLPointer));
  inherited;
end;

procedure TSecurityDescriptorManipulator.AddNewAccessToArray(ExplicitAccess:
  EXPLICIT_ACCESS);
begin
  SetLength(FExplicitAccess, Length(FExplicitAccess) + 1);
  FExplicitAccess[Length(FExplicitAccess) - 1] := ExplicitAccess;
end;

procedure TSecurityDescriptorManipulator.AllocateSid(
  RIDs: Array of Cardinal;
  var IdentifierAuthority: _SID_IDENTIFIER_AUTHORITY;
  var SID: PSID);
var
  FixedLengthRIDs: TFixedSIDArray;
begin
  CopyRIDs(FixedLengthRIDs, RIDs);
  if not AllocateAndInitializeSid(
    IdentifierAuthority, Length(RIDs),
    FixedLengthRIDs[0], FixedLengthRIDs[1],
    FixedLengthRIDs[2], FixedLengthRIDs[3],
    FixedLengthRIDs[4], FixedLengthRIDs[5],
    FixedLengthRIDs[6], FixedLengthRIDs[7],
    SID) then
  begin
    raise EOSError.Create('Allocate And Initialize SID Failed' +
      'by (' + IntToStr(GetLastError) + ')');
  end;
end;

function TSecurityDescriptorManipulator.CombineAccessTemplate(
  const AccessTemplate: EXPLICIT_ACCESS;
  const TrusteeTemplate: TRUSTEE): EXPLICIT_ACCESS;
begin
  result := AccessTemplate;
  result.Trustee := TrusteeTemplate;
end;

procedure TSecurityDescriptorManipulator.ApplyToAcl(const ExplicitAccess:
  EXPLICIT_ACCESS);
begin
  AddNewAccessToArray(ExplicitAccess);
  if SetEntriesInAcl(Length(FExplicitAccess), @FExplicitAccess[0],
    ACLPointer, ACLPointer)
      <> ERROR_SUCCESS then
  begin
    raise EOSError.Create('Set entry in ACL failed' +
      ' (' + IntToStr(GetLastError) + ')');
  end;
end;

procedure TSecurityDescriptorManipulator.SetSecurityForEveryone;
const
  NoAccess = 0;
  SECURITY_WORLD_SID_AUTHORITY: TSIDIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 1));
  SECURITY_WORLD_RID = 0;
  EveryoneRIDs: Array[0..0] of Cardinal = (SECURITY_WORLD_RID);
  EveryoneAccessTemplate: EXPLICIT_ACCESS =
    (grfAccessPermissions: NoAccess;
     grfAccessMode: SET_ACCESS;
     grfInheritance: NO_INHERITANCE);
  EveryoneAccessTrusteeTemplate: TRUSTEE =
    (pMultipleTrustee: nil;
     TrusteeForm: TRUSTEE_IS_SID;
     TrusteeType: TRUSTEE_IS_WELL_KNOWN_GROUP);
var
  WorldAuthority: _SID_IDENTIFIER_AUTHORITY;
  EveryoneSID: PSID;
  EveryoneAccess: EXPLICIT_ACCESS;
begin
  try
    WorldAuthority := SECURITY_WORLD_SID_AUTHORITY;
    AllocateSid(EveryoneRIDs, WorldAuthority, EveryoneSID);
    EveryoneAccess :=
      CombineAccessTemplate(
        EveryoneAccessTemplate, EveryoneAccessTrusteeTemplate);
    EveryoneAccess.Trustee.ptstrName := EveryoneSID;
    ApplyToAcl(EveryoneAccess);
  except
    if EveryoneSID <> nil then
      FreeSid(EveryoneSID);
  end;
end;

procedure TSecurityDescriptorManipulator.SetACLToSecurityDescriptor;
begin
  SetSecurityDescriptorDacl(
    @SecurityDescriptor, true, ACLPointer, false);
end;

procedure TSecurityDescriptorManipulator.SetSecurityForAdministrator;
const
  NoAccess = 0;
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
  AdminRIDs: Array[0..1] of Cardinal = (
    SECURITY_BUILTIN_DOMAIN_RID,
    DOMAIN_ALIAS_RID_ADMINS);
  AdminAccessTemplate: EXPLICIT_ACCESS =
    (grfAccessPermissions: GENERIC_ALL;
     grfAccessMode: SET_ACCESS;
     grfInheritance: NO_INHERITANCE);
  AdminAccessTrusteeTemplate: TRUSTEE =
    (pMultipleTrustee: nil;
     TrusteeForm: TRUSTEE_IS_SID;
     TrusteeType: TRUSTEE_IS_GROUP);
var
  NTAuthority: _SID_IDENTIFIER_AUTHORITY;
  AdminSID: PSID;
  AdminAccess: EXPLICIT_ACCESS;
begin
  try
    NTAuthority := SECURITY_NT_AUTHORITY;
    AllocateSid(AdminRIDs, NTAuthority, AdminSID);
    AdminAccess :=
      CombineAccessTemplate(
        AdminAccessTemplate, AdminAccessTrusteeTemplate);
    AdminAccess.Trustee.ptstrName := AdminSID;
    ApplyToAcl(AdminAccess);
  except
    if AdminSID <> nil then
      FreeSid(AdminSID);
  end;
end;

end.
