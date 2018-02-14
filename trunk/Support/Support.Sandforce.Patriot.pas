unit Support.Sandforce.Patriot;

interface

uses
  SysUtils,
  Support, Support.Sandforce;

type
  TPatriotSandforceNSTSupport = class sealed(TSandforceNSTSupport)
  private
    function IsPatriotSandforceProduct: Boolean;

  public
    function GetSupportStatus: TSupportStatus; override;
  end;

implementation

{ TPatriotSandforceNSTSupport }

function TPatriotSandforceNSTSupport.GetSupportStatus: TSupportStatus;
begin
  FillChar(result, SizeOf(result), #0);
  if IsPatriotSandforceProduct then
    result := GetSemiSupport;
end;

function TPatriotSandforceNSTSupport.IsPatriotSandforceProduct: Boolean;
begin
  result := 'PATRIOT PYRO' = Identify.Model;
end;

end.
