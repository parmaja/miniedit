unit DebugClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  TDebugWatchInfo = record
    VarName: string;
    VarType: string;
    Value: variant;
  end;


  TCallStackItem = class(TObject)
  private
    FLine: Integer;
    FFileName: string;
  public
  published
    property FileName: string read FFileName write FFileName;
    property Line: integer read FLine write FLine;
  end;

  { TCallStackItem }

  { TCallStackItems }

  TCallStackItems = class(TObjectList)
  private
    function GetItem(Index: integer): TCallStackItem;
  protected
  public
    function Add(vItem: TCallStackItem): integer; overload;
    function Add(FileName: string; Line: integer): integer; overload;
    procedure AssignFrom(vItems: TCallStackItems);
    property Items[Index: integer]: TCallStackItem read GetItem; default;
  end;

implementation

{ TCallStackItems }

function TCallStackItems.GetItem(Index: integer): TCallStackItem;
begin
  Result := inherited Items[Index] as TCallStackItem;
end;

function TCallStackItems.Add(vItem: TCallStackItem): integer;
begin
  Result := inherited Add(vItem);
end;

function TCallStackItems.Add(FileName: string; Line: integer): integer;
var
  aItem: TCallStackItem;
begin
  aItem := TCallStackItem.Create;
  aItem.FileName := FileName;
  aItem.Line := Line;
  Result := Add(aItem);
end;

procedure TCallStackItems.AssignFrom(vItems: TCallStackItems);
var
  i: Integer;
begin
  Clear;
  if vItems <> nil then
    for i := 0 to vItems.Count -1 do
    begin
       Add(vItems[i].FileName, vItems[i].Line);
    end;
end;

end.

