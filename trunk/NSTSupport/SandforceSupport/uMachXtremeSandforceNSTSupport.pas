unit uMachXtremeSandforceNSTSupport;

interface

uses
  SysUtils,
  uNSTSupport, uSandforceNSTSupport;

type
  TMachXtremeSandforceNSTSupport = class sealed(TSandforceNSTSupport)
  private
    function IsMachXtremeSandforceProduct: Boolean;

  public
    function GetSupportStatus: TSupportStatus; override;
  end;

implementation

{ TMachXtremeSandforceNSTSupport }

function TMachXtremeSandforceNSTSupport.GetSupportStatus: TSupportStatus;
begin
  if IsMachXtremeSandforceProduct then
    result := GetSemiSupport;
end;

function TMachXtremeSandforceNSTSupport.IsMachXtremeSandforceProduct: Boolean;
begin
  result :=
    (Pos('MXSSD', UpperCase(Model)) > 0) and
    (Pos('SF', UpperCase(Model)) > 0);
end;

end.
