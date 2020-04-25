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
  LCLintf, LCLType, ExtCtrls, SynHighlighterSQL, EditorProfiles,
  Dialogs, EditorEngine, EditorClasses, EditorOptions, SynEditHighlighter, SynEditSearch, SynEdit, EditorRun,
  sqlvManager, SQLEditForms;

type

  { TSQLFile }

  TSQLFile = class(TSyntaxEditorFile, IExecuteEditor)
  private
    FContents: TSQLEditForm;
    function GetContents: TSQLEditForm;
  protected
    procedure InitContents; override;
    function GetControl: TWinControl; override;
    function GetSynEdit: TSynEdit; override;
    function GetIsReadonly: Boolean; override;
    property Contents: TSQLEditForm read GetContents;
    function Execute(RunInfo: TmneRunInfo): Boolean; override;
  public
    destructor Destroy; override;
    function Run: Boolean;
  end;

  { TSQLFileCategory }

  TSQLFileCategory = class(TTextFileCategory)
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
  public
  end;

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

uses
  mnSynHighlighterStdSQL;

{ TSQLFile }

function TSQLFile.GetContents: TSQLEditForm;
begin
  Result := FContents;
end;

procedure TSQLFile.InitContents;
begin
  inherited;
  FContents := TSQLEditForm.CreateParented(Engine.FilePanel.Handle);
  FContents.Parent := Engine.FilePanel;
  FContents.Align := alClient;
  FContents.OnChanged := @DoEdit;
end;

function TSQLFile.Run: Boolean;
begin
  Result := True; //TODO we will run SQL
end;

function TSQLFile.GetSynEdit: TSynEdit;
begin
  Result := FContents.SQLEdit;
end;

function TSQLFile.GetControl: TWinControl;
begin
  Result := Contents;
end;

function TSQLFile.GetIsReadonly: Boolean;
begin
  Result := False;
end;

function TSQLFile.Execute(RunInfo: TmneRunInfo): Boolean;
begin
  FContents.Execute;
  Result := True;
end;

destructor TSQLFile.Destroy;
begin
  FreeAndNil(FContents);
  inherited Destroy;
end;

{ TSQLFileCategory }

function TSQLFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  {Result := TSynSQLSyn.Create(nil);
  (Result as TSynSQLSyn).SQLDialect := sqlMySQL;}
  Result := TmnSynStdSQLSyn.Create(nil);
end;

procedure TSQLFileCategory.InitMappers;
begin
  with Highlighter as TmnSynStdSQLSyn do
  begin
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(TypeAttri, attDataType);
    Mapper.Add(ObjectAttri, attDataName);
    Mapper.Add(FunctionAttri, attStandard);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(KeyAttri, attKeyword);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(SpaceAttri, attDefault);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(VariableAttri, attVariable);
  end;
end;

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
    Categories.Add(TSQLFileCategory.Create(DefaultProject.Tendency, 'sql', 'SQL'));
    Groups.Add(TSQLFile, 'sql', 'SQL', TSQLFileCategory, ['sql'], [fgkAssociated, fgkExecutable, fgkBrowsable]);
    Categories.Add(TDBFileCategory.Create(DefaultProject.Tendency, 'DB', 'Database connection'));
    Groups.Add(TDBFile, 'SQLite', 'SQLite', TDBFileCategory, ['sqlite'], [fgkAssociated, fgkBinary, fgkBrowsable]);
    Groups.Add(TDBFile, 'Firebird', 'Firebird', TDBFileCategory, ['firebird'], [fgkAssociated, fgkBinary, fgkBrowsable]);
  end;
end.
