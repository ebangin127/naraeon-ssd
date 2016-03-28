unit TestOSFile.Handle;

interface

uses
  Windows, OSFile, Dialogs, SysUtils, TestFramework,
  OSFile.Handle, OS.Handle;

type
  // Test methods for class TOSFileWithHandle
  TConcreteOSFileWithHandle = class(TOSFileWithHandle)
  public
    function IsHandleValid(const HandleToCheck: THandle): Boolean;
  protected
    function GetMinimumPrivilege: TCreateFileDesiredAccess; override;
  end;

  TestTOSFileWithHandle = class(TTestCase)
  strict private
    FOSFileWithHandle: TConcreteOSFileWithHandle;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestIsHandleValid;
    procedure TestGetDesiredAccessFromTCreateFileDesiredAccess;
  end;

implementation

procedure TestTOSFileWithHandle.SetUp;
begin
  FOSFileWithHandle := TConcreteOSFileWithHandle.Create('');
end;

procedure TestTOSFileWithHandle.TearDown;
begin
  FOSFileWithHandle.Free;
  FOSFileWithHandle := nil;
end;

procedure TestTOSFileWithHandle.TestIsHandleValid;
begin
  CheckEquals(false, IsHandleValid(INVALID_HANDLE_VALUE),
    'Handle test failure - INVALID_HANDLE_VALUE');
  CheckEquals(false, IsHandleValid(0),
    'Handle test failure - 0');
  CheckEquals(true, IsHandleValid(1),
    'Handle test failure - 1');
end;

procedure TestTOSFileWithHandle.
  TestGetDesiredAccessFromTCreateFileDesiredAccess;
begin
  CheckEquals(0,
    GetDesiredAccessFromTCreateFileDesiredAccess(
      TCreateFileDesiredAccess.DesiredNone),
      'TCreateFileDesiredAccess.DesiredNone');
  CheckEquals(GENERIC_READ,
    GetDesiredAccessFromTCreateFileDesiredAccess(
      TCreateFileDesiredAccess.DesiredReadOnly),
      'TCreateFileDesiredAccess.DesiredReadOnly');
  CheckEquals(GENERIC_READ or GENERIC_WRITE,
    GetDesiredAccessFromTCreateFileDesiredAccess(
      TCreateFileDesiredAccess.DesiredReadWrite),
      'TCreateFileDesiredAccess.DesiredReadWrite');
  StartExpectingException(EArgumentOutOfRangeException);
  GetDesiredAccessFromTCreateFileDesiredAccess(
    TCreateFileDesiredAccess(5));
  StopExpectingException('Wrong TCreateFileDesiredAccess (5)');
end;

{ TConcreteOSFileWithHandle }

function TConcreteOSFileWithHandle.GetMinimumPrivilege:
  TCreateFileDesiredAccess;
begin
  result := TCreateFileDesiredAccess.DesiredNone;
end;

function TConcreteOSFileWithHandle.IsHandleValid(
  const HandleToCheck: THandle): Boolean;
begin
  result := IsHandleValid(HandleToCheck);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTOSFileWithHandle.Suite);
end.
