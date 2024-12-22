unit mneBoardClasses;
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
  Dialogs, EditorEngine, EditorClasses, EditorOptions, SynEditHighlighter, SynEditSearch, SynEdit,
  mneBoardForms;

type

  { TBoardFile }

  TBoardFile = class(TEditorFile, IFileEditor)
  private
    FContent: TBoardForm;
  protected
    procedure InitContents; override;
    function GetContent: TWinControl; override;
    function GetControl: TWinControl; override;
    function GetIsReadonly: Boolean; override;
    procedure DoLoad(AFileName: string); override;
    procedure DoSave(AFileName: string); override;
  public
    destructor Destroy; override;
  end;

  { TBoardFileCategory }

  TBoardFileCategory = class(TFileCategory)
  protected
    procedure InitMappers; override;
    function GetIsText: Boolean; override;
  public
    function CreateHighlighter: TSynCustomHighlighter; override;
  end;

implementation

{ TBoardFile }

procedure TBoardFile.InitContents;
begin
  inherited InitContents;
  if FContent = nil then
  begin
    FContent := TBoardForm.Create(Engine.FilePanel);
    FContent.Parent := Engine.FilePanel;
    FContent.Align := alClient;
  end;
end;

function TBoardFile.GetContent: TWinControl;
begin
  Result := FContent;
end;

function TBoardFile.GetControl: TWinControl;
begin
  Result := FContent.Board;
end;

function TBoardFile.GetIsReadonly: Boolean;
begin
  Result := True;
end;

procedure TBoardFile.DoLoad(AFileName: string);
begin
  //Contents.Board.Picture.LoadFromFile(AFileName);
end;

procedure TBoardFile.DoSave(AFileName: string);
begin
  //Contents.Board.Picture.SaveToFile(AFileName);
end;

destructor TBoardFile.Destroy;
begin
  FreeAndNil(FContent);
  inherited Destroy;
end;

{ TBoardFileCategory }

function TBoardFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := nil;
end;

procedure TBoardFileCategory.InitMappers;
begin
end;

function TBoardFileCategory.GetIsText: Boolean;
begin
  Result := False;
end;

initialization
  with Engine do
  begin
    Categories.Add(TBoardFileCategory.Create(DefaultProject.Tendency, 'Board', 'Board diagram'));
    Groups.Add(TBoardFile, 'board', 'board', TBoardFileCategory, ['.board'], [fgkAssociated, fgkBrowsable], []);
  end;
end.
