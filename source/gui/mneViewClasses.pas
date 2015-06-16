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

  { TPNGFile }

  TPNGFile = class(TEditorFile, IFileEditor)
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

  { TPNGFileCategory }

  TPNGFileCategory = class(TFileCategory)
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

{ TPNGFile }

function TPNGFile.GetContents: TImagePanel;
begin
  if FContents = nil then
    FContents := TImagePanel.Create(Engine.Container);
  Result := FContents;
  FContents.Parent := Engine.Container;
end;

function TPNGFile.GetControl: TWinControl;
begin
  Result := Contents;
end;

function TPNGFile.GetIsReadonly: Boolean;
begin
  Result := True;
end;

procedure TPNGFile.DoLoad(FileName: string);
begin
  Contents.Image.Picture.LoadFromFile(FileName);
end;

procedure TPNGFile.DoSave(FileName: string);
begin
  Contents.Image.Picture.SaveToFile(FileName);
end;

destructor TPNGFile.Destroy;
begin
  FreeAndNil(FContents);
  inherited Destroy;
end;

{ TPNGFileCategory }

function TPNGFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := nil;
end;

procedure TPNGFileCategory.InitMappers;
begin
end;

function TPNGFileCategory.GetIsText: Boolean;
begin
  Result := False;
end;

initialization
  with Engine do
  begin
    Categories.Add(TPNGFileCategory.Create('png'));
    Groups.Add(TPNGFile, 'png', 'PNG files', 'png', ['png'], [fgkBrowsable]);
  end;
end.

