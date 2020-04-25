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
  sqlvManagerForms, sqlvSQLForms;

type

  { TSQLFile }

  TSQLFile = class(TSyntaxEditorFile, IExecuteEditor)
  private
    FContent: TSQLEditForm;
  protected
    procedure InitContents; override;
    function GetContent: TWinControl; override;
    function GetControl: TWinControl; override;
    function GetSynEdit: TSynEdit; override;
    function GetIsReadonly: Boolean; override;
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

procedure TSQLFile.InitContents;
begin
  inherited;
  FContent := TSQLEditForm.CreateParented(Engine.FilePanel.Handle);
  FContent.Parent := Engine.FilePanel;
  FContent.Align := alClient;
  FContent.OnChanged := @DoEdit;
end;

function TSQLFile.GetContent: TWinControl;
begin
  Result := FContent;
end;

function TSQLFile.GetControl: TWinControl;
begin
  if FContent.PageControl.ActiveControl = FContent.SQLPnl then
    Result := FContent.SQLEdit
  else if FContent.PageControl.ActiveControl = FContent.DataPnl then
    Result := FContent.DataGrid
  else
    Result := FContent;
end;

function TSQLFile.Run: Boolean;
begin
  Result := True; //TODO we will run SQL
end;

function TSQLFile.GetSynEdit: TSynEdit;
begin
  Result := FContent.SQLEdit;
end;

function TSQLFile.GetIsReadonly: Boolean;
begin
  Result := False;
end;

function TSQLFile.Execute(RunInfo: TmneRunInfo): Boolean;
begin
  FContent.Execute;
  Result := True;
end;

destructor TSQLFile.Destroy;
begin
  FreeAndNil(FContent);
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
    //Groups.Add(TDBFile, 'SQLite', 'SQLite', TDBFileCategory, ['sqlite'], [fgkAssociated, fgkBinary, fgkBrowsable]);
    //Groups.Add(TDBFile, 'Firebird', 'Firebird', TDBFileCategory, ['firebird'], [fgkAssociated, fgkBinary, fgkBrowsable]);
  end;
end.
