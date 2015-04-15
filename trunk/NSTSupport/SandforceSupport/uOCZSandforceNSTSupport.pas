unit uOCZSandforceNSTSupport;

interface

uses
  SysUtils,
  uNSTSupport, uSandforceNSTSupport;

type
  TOCZSandforceNSTSupport = class sealed(TSandforceNSTSupport)
  private
    function IsOCZSandforceProduct: Boolean;
    function IsAgility3: Boolean;

    function IsVertex3: Boolean;
  public
    function GetSupportStatus: TSupportStatus; override;
  end;

implementation

{ TOCZSandforceNSTSupport }

function TOCZSandforceNSTSupport.GetSupportStatus: TSupportStatus;
begin
  if IsOCZSandforceProduct then
    result := GetSemiSupport;
end;

function TOCZSandforceNSTSupport.IsVertex3: Boolean;
begin
  result := (Model = 'OCZ-VERTEX3') or (Model = 'OCZ-VERTEX3 MI');
end;

function TOCZSandforceNSTSupport.IsAgility3: Boolean;
begin
  result := Model = 'OCZ-AGILITY3';
end;

function TOCZSandforceNSTSupport.IsOCZSandforceProduct: Boolean;
begin
  result := IsVertex3 or IsAgility3;
end;

end.
