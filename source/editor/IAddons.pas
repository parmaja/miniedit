unit IAddons;

{$mode objfpc}

{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{
  Addon main unit
}
interface

uses
  SysUtils, Forms, StrUtils, Variants, Classes, Controls, Graphics, Contnrs;

type
  IAddon = interface(IInterface)
    ['{D87616D9-5B2E-464A-BBB1-5881D1F0FEA7}']
    function GetObject: TObject;
  end;

  { TAddonObject }

  TAddonObject = class(TObject, IInterface)
  protected
    function _AddRef: integer; stdcall;
    function _Release: integer; stdcall;
  public
    function GetObject: TObject;
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
  end;

  TAddon = class(TAddonObject, IAddon)
  public
  end;

  TAddonClass = class of TAddon;

  { TAddonItem }

  TAddonItem = class(TObject)
  protected
    FAddon: IAddon;
    FAddonObject: TObject;//kill this object when free
  public
    ID: int64;
    Category: string;
    Name: string;
    destructor Destroy; override;
    property Addon: IAddon read FAddon;
  end;

  { TAddons }

  TAddons = class(TObjectList)
  private
    function GetItem(Index: integer): TAddonItem;
  public
    function Add(Category, Name: string; vAddon: IAddon): TAddonItem; overload;
    function Add(Category, Name: string; vAddon: TAddonClass): TAddonItem; overload;
    property Items[Index: integer]: TAddonItem read GetItem; default;
  end;

{
  Most common addons
}
  IClickAddon = interface(IAddon)
    ['{780D2A49-8F64-45A3-8F2C-96DE2318DB90}']
    procedure Click(Sender: TObject);
  end;

  IMenuAddon = interface(IClickAddon)
    ['{23FA4EA8-C13E-4110-A6AC-B97489E9E10A}']
    function GetCaption: string;
  end;

  IEditorFile = interface(IAddon)
    ['{40A84C67-30B5-4A38-98B5-B56AE3842625}']
  end;

function Addons: TAddons;

implementation

var
  FAddons: TAddons = nil;

function Addons: TAddons;
begin
  if FAddons = nil then
    FAddons := TAddons.Create;
  Result := FAddons;
end;

{ TAddonItem }

destructor TAddonItem.Destroy;
begin
  FreeAndNil(FAddonObject);
  inherited Destroy;
end;

{ TAddons }

function TAddons.GetItem(Index: integer): TAddonItem;
begin
  Result := inherited Items[Index] as TAddonItem;
end;

function TAddons.Add(Category, Name: string; vAddon: IAddon): TAddonItem;
begin
  Result := TAddonItem.Create;
  Result.Category := Category;
  Result.Name := Name;
  Result.FAddon := vAddon;
  Inherited Add(Result);
end;

function TAddons.Add(Category, Name: string; vAddon: TAddonClass): TAddonItem;
var
  AO: TAddon;
begin
  AO := vAddon.Create;
  Result := Add(Category, Name, AO);
  Result.FAddonObject := AO;//to kill the object when free the list
end;

function TAddonObject._AddRef: integer; stdcall;
begin
  Result := 0;
end;

function TAddonObject._Release: integer; stdcall;
begin
  Result := 0;
end;

function TAddonObject.GetObject: TObject;
begin
  Result := Self;
end;

function TAddonObject.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

end.

