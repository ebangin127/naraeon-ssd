unit OSFile.Interfaced;

interface

uses
  OSFile;

type
  TInterfacedOSFile = class(TOSFile)
  protected
    [Volatile] FRefCount: Integer;
    class procedure __MarkDestroying(const Obj); static; inline;

    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  private const
    objDestroyingFlag = Integer($80000000);
  end;

implementation

{ TInterfacedOSFile }

class procedure TInterfacedOSFile.__MarkDestroying(const Obj);
var
  LRef: Integer;
begin
  repeat
    LRef := TInterfacedOSFile(Obj).FRefCount;
  until AtomicCmpExchange(TInterfacedOSFile(Obj).FRefCount,
    LRef or objDestroyingFlag, LRef) = LRef;
end;

function TInterfacedOSFile.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TInterfacedOSFile._AddRef: Integer;
begin
  Result := AtomicIncrement(FRefCount);
end;

function TInterfacedOSFile._Release: Integer;
begin
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
  begin
    __MarkDestroying(Self);
    Destroy;
  end;
end;
end.
