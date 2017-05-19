unit mneBoardClasses;
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
  Dialogs, EditorEngine, EditorClasses, EditorOptions, SynEditHighlighter, SynEditSearch, SynEdit,
  DesignerBoard;

type

  { TBoardPanel }

  TBoardPanel = class(TPanel)
  protected
  public
    Board: TDesignerBoard;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TBoardFile }

  TBoardFile = class(TEditorFile, IFileEditor)
  private
    FContents: TBoardPanel;
    function GetContents: TBoardPanel;
  protected
    property Contents: TBoardPanel read GetContents;
    function GetControl: TWinControl; override;
    function GetIsReadonly: Boolean; override;
    procedure DoLoad(FileName: string); override;
    procedure DoSave(FileName: string); override;
  public
    destructor Destroy; override;
  end;

  { TBoardFileCategory }

  TBoardFileCategory = class(TFileCategory)
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
    function GetIsText: Boolean; override;
  public
  end;

implementation

{ TBoardPanel }

constructor TBoardPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Caption := '';
  Board := TDesignerBoard.Create(Self);
  Board.Parent := Self;
  Board.Align := alClient;
  //Board.Center := True;
  BevelOuter := bvNone;
end;

destructor TBoardPanel.Destroy;
begin
  inherited Destroy;
end;

{ TBoardFile }

function TBoardFile.GetContents: TBoardPanel;
begin
  if FContents = nil then
  begin
    FContents := TBoardPanel.Create(Engine.Container);
    FContents.Parent := Engine.Container;
    FContents.Align := alClient;
  end;
  Result := FContents;
end;

function TBoardFile.GetControl: TWinControl;
begin
  Result := Contents;
end;

function TBoardFile.GetIsReadonly: Boolean;
begin
  Result := True;
end;

procedure TBoardFile.DoLoad(FileName: string);
begin
  //Contents.Board.Picture.LoadFromFile(FileName);
end;

procedure TBoardFile.DoSave(FileName: string);
begin
  //Contents.Board.Picture.SaveToFile(FileName);
end;

destructor TBoardFile.Destroy;
begin
  FreeAndNil(FContents);
  inherited Destroy;
end;

{ TBoardFileCategory }

function TBoardFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
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
    Categories.Add(TBoardFileCategory.Create(DefaultProject.Tendency, 'Board'));
    Groups.Add(TBoardFile, 'board', 'board', TBoardFileCategory, ['board'], [fgkAssociated, fgkExecutable, fgkBrowsable]);
  end;
end.

