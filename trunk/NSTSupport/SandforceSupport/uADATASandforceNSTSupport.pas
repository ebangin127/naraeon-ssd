unit uADATASandforceNSTSupport;

interface

uses
  SysUtils,
  uNSTSupport, uSandforceNSTSupport;

type
  TADATASandforceNSTSupport = class sealed(TSandforceNSTSupport)
  private
    function IsADATASandforceProduct: Boolean;

  public
    function GetSupportStatus: TSupportStatus; override;
  end;

implementation

{ TADATASandforceNSTSupport }

function TADATASandforceNSTSupport.GetSupportStatus: TSupportStatus;
begin
  if IsADATASandforceProduct then
    result := GetSemiSupport;
end;

function TADATASandforceNSTSupport.IsADATASandforceProduct: Boolean;
begin
  result := 'ADATA SP900' = Model;
end;

end.
