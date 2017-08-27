unit mneBoardComponents;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics, Contnrs,
  LCLintf, LCLType, ExtCtrls,
  mnClasses,
  Dialogs, EditorEngine, EditorClasses, EditorOptions, ntvBoard, SynEditHighlighter, SynEditSearch, SynEdit;

type

  { TBoardComponent }

  TBoardComponent = class(TPersistent)
  public
    Info: TStringList;
    Name: string;
    Caption: string;
    ImageFile: string;
    Image: TGraphic;
    procedure LoadFromFile(FileName: String);
    procedure CheckImage;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TBoardComponents }

  TBoardComponents = class(specialize GNamedItems<TBoardComponent>)
  private
  public
  end;

  { TComponentElement }

  TComponentElement = class(TSizableElement)
  public
    Component: TBoardComponent;
    constructor Create(AOwner: TContainer); override;
    procedure AfterCreate(X, Y: Integer; Dummy: Boolean); override;
    procedure CorrectSize; override;
    destructor Destroy; override;
    procedure Paint(vCanvas: TCanvas); override;
  end;

function BoardComponents: TBoardComponents;

implementation

{ TComponentElement }

constructor TComponentElement.Create(AOwner: TContainer);
begin
  inherited;
end;

procedure TComponentElement.AfterCreate(X, Y: Integer; Dummy: Boolean);
begin
  inherited AfterCreate(X, Y, Dummy);
end;

procedure TComponentElement.CorrectSize;
begin
  Component.CheckImage;
  Width := Component.Image.Width;
  Height := Component.Image.Height;
end;

destructor TComponentElement.Destroy;
begin
  inherited;
end;

procedure TComponentElement.Paint(vCanvas: TCanvas);
var
  aRect: TRect;
begin
  inherited;
  Component.CheckImage;
  aRect := BoundRect;
  aRect.Left := aRect.Left + (((aRect.Right - aRect.Left) - Component.Image.Width) div 2);
  aRect.Top := aRect.Top + (((aRect.Bottom - aRect.Top) - Component.Image.Height) div 2);
  vCanvas.Draw(aRect.Left, aRect.Top, Component.Image);
end;

{ TBoardComponent }

procedure TBoardComponent.LoadFromFile(FileName: String);
var
  Path: string;
begin
  Path := IncludeTrailingPathDelimiter(FileName); //TODO maybe it is zip file
  Info.LoadFromFile(Path + 'component.properties');
  ImageFile := Path + Info.Values['Image'];
  Name := Info.Values['Name'];
  Caption := Info.Values['Title'];
end;

procedure TBoardComponent.CheckImage;
begin
  if Image = nil then
  begin
    Image := TPortableNetworkGraphic.Create;
    Image.LoadFromFile(ImageFile);
  end;
end;

constructor TBoardComponent.Create;
begin
  inherited;
  Info := TStringList.Create;
end;

destructor TBoardComponent.Destroy;
begin
  FreeAndNil(Info);
  FreeAndNil(Image);
  inherited Destroy;
end;


procedure EnumFilesCallback(AObject: TObject; const FileName: string; Count, Level:Integer; IsDirectory: Boolean; var Resume: Boolean);
var
  Elements: TBoardComponents;
  Item: TBoardComponent;
  Path: string;
begin
  Path := IncludeTrailingPathDelimiter(FileName);
  Elements := (AObject as TBoardComponents);
  Item := TBoardComponent.Create;
  Item.LoadFromFile(Path);
  Elements.Add(Item);
end;

var
  FBoardComponents: TBoardComponents = nil;

function BoardComponents: TBoardComponents;
begin
  if FBoardComponents = nil then
  begin
    FBoardComponents := TBoardComponents.Create;
    EnumFileList(Application.Location + 'components\', '*.*', '', @EnumFilesCallback, FBoardComponents, 0, 1, true, [fftDir]);
  end;
  Result := FBoardComponents;
end;

initialization
end.

