unit Support.Sandforce.ADATA;

interface

uses
  SysUtils,
  Support, Support.Sandforce;

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
  FillChar(result, SizeOf(result), #0);
  if IsADATASandforceProduct then
    result := GetSemiSupport;
end;

function TADATASandforceNSTSupport.IsADATASandforceProduct: Boolean;
begin
  result := 'ADATA SP900' = Model;
end;

end.
