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
  if IsPatriotSandforceProduct then
    result := GetSemiSupport;
end;

function TPatriotSandforceNSTSupport.IsPatriotSandforceProduct: Boolean;
begin
  result := 'PATRIOT PYRO' = Model;
end;

end.
