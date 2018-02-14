unit Support.Sandforce.Toshiba;

interface

uses
  SysUtils,
  Support, Support.Sandforce;

type
  TToshibaSandforceNSTSupport = class sealed(TSandforceNSTSupport)
  private
    function IsToshibaSandforceProduct: Boolean;

  public
    function GetSupportStatus: TSupportStatus; override;
  end;

implementation

{ TToshibaSandforceNSTSupport }

function TToshibaSandforceNSTSupport.GetSupportStatus: TSupportStatus;
begin
  FillChar(result, SizeOf(result), #0);
  if IsToshibaSandforceProduct then
    result := GetSemiSupport;
end;

function TToshibaSandforceNSTSupport.IsToshibaSandforceProduct: Boolean;
begin
  result :=
    (Pos('TOSHIBA', UpperCase(Identify.Model)) > 0) and
    (Pos('THNSNS', UpperCase(Identify.Model)) > 0);
end;

end.
