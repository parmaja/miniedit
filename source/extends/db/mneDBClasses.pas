unit mneDBClasses;
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
  sqlvManager;

type

  { TDBFile }

  TDBFile = class(TEditorFile, IFileEditor)
  private
    FContents: TsqlvManagerForm;
    function GetContents: TsqlvManagerForm;
  protected
    property Contents: TsqlvManagerForm read GetContents;
    function GetControl: TWinControl; override;
    function GetIsReadonly: Boolean; override;
    procedure DoLoad(FileName: string); override;
    procedure DoSave(FileName: string); override;
  public
    destructor Destroy; override;
  end;

  { TDBFileCategory }

  TDBFileCategory = class(TFileCategory)
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
    function GetIsText: Boolean; override;
  public
  end;

implementation

{ TDBFile }

function TDBFile.GetContents: TsqlvManagerForm;
begin
  if FContents = nil then
  begin
    FContents := TsqlvManagerForm.Create(Engine.ProjectPanel);
    FContents.Parent := Engine.ProjectPanel;
    FContents.Align := alClient;
  end;
  Result := FContents;
end;

function TDBFile.GetControl: TWinControl;
begin
  Result := Contents;
end;

function TDBFile.GetIsReadonly: Boolean;
begin
  Result := True;
end;

procedure TDBFile.DoLoad(FileName: string);
begin
  //Contents.DB.Picture.LoadFromFile(FileName);
end;

procedure TDBFile.DoSave(FileName: string);
begin
  //Contents.DB.Picture.SaveToFile(FileName);
end;

destructor TDBFile.Destroy;
begin
  FreeAndNil(FContents);
  inherited Destroy;
end;

{ TDBFileCategory }

function TDBFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := nil;
end;

procedure TDBFileCategory.InitMappers;
begin
end;

function TDBFileCategory.GetIsText: Boolean;
begin
  Result := False;
end;

initialization
  with Engine do
  begin
    Categories.Add(TDBFileCategory.Create(DefaultProject.Tendency, 'DB', 'Database connection'));
    //Groups.Add(TDBFile, 'DB', 'DB', TDBFileCategory, ['DB'], [fgkAssociated, fgkExecutable, fgkBrowsable]);
    Groups.Add(TDBFile, 'SQLite', 'SQLite', TDBFileCategory, ['sqlite'], [fgkAssociated, fgkBrowsable]);
    //Groups.Add(TDBFile, 'Firebird', 'Firebird', TDBFileCategory, ['firebird'], [fgkAssociated, fgkBrowsable]);
  end;
end.
