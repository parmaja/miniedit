unit mneViewClasses;
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
  Dialogs, EditorEngine, EditorClasses, EditorOptions, SynEditHighlighter, SynEditSearch, SynEdit;

type

  { TImagePanel }

  TImagePanel = class(TPanel)
  protected
  public
    Image: TImage;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TImageFile }

  TImageFile = class(TEditorFile, IFileEditor)
  private
    FContents: TImagePanel;
    function GetContents: TImagePanel;
  protected
    property Contents: TImagePanel read GetContents;
    function GetControl: TWinControl; override;
    function GetIsReadonly: Boolean; override;
    procedure DoLoad(FileName: string); override;
    procedure DoSave(FileName: string); override;
  public
    destructor Destroy; override;
  end;

  { TImageFileCategory }

  TImageFileCategory = class(TFileCategory)
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
    function GetIsText: Boolean; override;
  public
  end;

implementation

{ TImagePanel }

constructor TImagePanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Caption := '';
  Image := TImage.Create(Self);
  Image.Parent := Self;
  Image.Align := alClient;
  Image.Center := True;
  BevelOuter := bvNone;
end;

destructor TImagePanel.Destroy;
begin
  inherited Destroy;
end;

{ TImageFile }

function TImageFile.GetContents: TImagePanel;
begin
  if FContents = nil then
  begin
    FContents := TImagePanel.Create(Engine.Container);
    FContents.Parent := Engine.Container;
    FContents.Align := alClient;
  end;
  Result := FContents;
end;

function TImageFile.GetControl: TWinControl;
begin
  Result := Contents;
end;

function TImageFile.GetIsReadonly: Boolean;
begin
  Result := True;
end;

procedure TImageFile.DoLoad(FileName: string);
begin
  Contents.Image.Picture.LoadFromFile(FileName);
end;

procedure TImageFile.DoSave(FileName: string);
begin
  Contents.Image.Picture.SaveToFile(FileName);
end;

destructor TImageFile.Destroy;
begin
  FreeAndNil(FContents);
  inherited Destroy;
end;

{ TImageFileCategory }

function TImageFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := nil;
end;

procedure TImageFileCategory.InitMappers;
begin
end;

function TImageFileCategory.GetIsText: Boolean;
begin
  Result := False;
end;

initialization
  with Engine do
  begin
    Categories.Add(TImageFileCategory.Create('Image'));
    Groups.Add(TImageFile, 'png', 'PNG files', 'Image', ['png'], [fgkBrowsable]);
    Groups.Add(TImageFile, 'jpg', 'Jpg files', 'Image', ['jpg'], [fgkBrowsable]);
    Groups.Add(TImageFile, 'bmp', 'BMP files', 'Image', ['bmp'], [fgkBrowsable]);
  end;
end.

