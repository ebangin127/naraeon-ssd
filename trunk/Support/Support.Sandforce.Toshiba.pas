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
  if IsToshibaSandforceProduct then
    result := GetSemiSupport;
end;

function TToshibaSandforceNSTSupport.IsToshibaSandforceProduct: Boolean;
begin
  result :=
    (Pos('TOSHIBA', UpperCase(Model)) > 0) and
    (Pos('THNSNS', UpperCase(Model)) > 0);
end;

end.
