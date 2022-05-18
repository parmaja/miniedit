unit mneViewClasses;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
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
    FContent: TImagePanel;
  protected
    procedure InitContents; override;
    function GetContent: TWinControl; override;
    function GetIsReadonly: Boolean; override;
    procedure DoLoad(AFileName: string); override;
    procedure DoSave(AFileName: string); override;
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

procedure TImageFile.InitContents;
begin
  inherited InitContents;
  if FContent = nil then
  begin
    FContent := TImagePanel.Create(Engine.FilePanel);
    FContent.Parent := Engine.FilePanel;
    FContent.Align := alClient;
  end;
end;

function TImageFile.GetContent: TWinControl;
begin
  Result := FContent;
end;

function TImageFile.GetIsReadonly: Boolean;
begin
  Result := True;
end;

procedure TImageFile.DoLoad(AFileName: string);
begin
  try
    FContent.Image.Picture.LoadFromFile(AFileName);
  except
    on E: Exception do
    begin
      FContent.Image.Picture.Clear;
      raise;
    end;
  end;
end;

procedure TImageFile.DoSave(AFileName: string);
begin
  FContent.Image.Picture.SaveToFile(AFileName);
end;

destructor TImageFile.Destroy;
begin
  FreeAndNil(FContent);
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
    Categories.Add(TImageFileCategory.Create(DefaultProject.Tendency, 'Image', 'Images JPG, PNG'));
    Groups.Add(TImageFile, 'png', 'PNG', TImageFileCategory, ['.png'], [fgkUneditable, fgkBrowsable]);
    Groups.Add(TImageFile, 'jpg', 'Jpg', TImageFileCategory, ['.jpg'], [fgkUneditable, fgkBrowsable]);
    Groups.Add(TImageFile, 'bmp', 'BMP', TImageFileCategory, ['.bmp'], [fgkUneditable, fgkBrowsable]);
  end;
end.

