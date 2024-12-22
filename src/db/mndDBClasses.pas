unit mndDBClasses;
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
  LCLintf, LCLType, ExtCtrls, SynHighlighterSQL, EditorProfiles, GUIMsgBox, mnMsgBox,
  Dialogs, EditorEngine, EditorClasses, EditorOptions, SynCompletion, SynEditHighlighter, SynHighlighterHashEntries, SynEditSearch,
  SynEdit, EditorRun, mnSQLProcessor,
  mnSynHighlighterMultiProc, mndManagerForms, mndSQLForms, mndEngines;

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
    procedure EnumSwitchControls(vList: TSwitchControls); override;
    procedure Show; override;
    procedure Update; override;
    function Run: Boolean;
  end;

  { TSQLFileCategory }

  TSQLFileCategory = class(TTextFileCategory)
  protected
    procedure InitMappers; override;
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
    procedure DoPrepareCompletion(AEditor: TCustomSynEdit); override;
  public
    function CreateHighlighter: TSynCustomHighlighter; override;
  end;

  { TDBFileCategory }

  TDBFileCategory = class(TVirtualCategory)
  protected
    procedure InitMappers; override;
    function GetIsText: Boolean; override;
    function OpenFile(vGroup: TFileGroup; vFiles: TEditorFiles; vFileName, vFileParams: string): TEditorFile; override;
  public
    function CreateHighlighter: TSynCustomHighlighter; override;
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
  Result := FContent.SQLEdit
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
  inherited;
end;

procedure TSQLFile.EnumSwitchControls(vList: TSwitchControls);
begin
  inherited;
  if FContent.PageControl.ActiveControl = FContent.GridPnl then
    vList.Add(FContent.DataGrid);
end;

procedure TSQLFile.Show;
begin
  inherited;
  FContent.UpdateControls;
end;

procedure TSQLFile.Update;
begin
  FContent.UpdateControls;
end;

{ TSQLFileCategory }

function TSQLFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  {Result := TSynSQLSyn.Create(nil);
  (Result as TSynSQLSyn).SQLDialect := sqlMySQL;}
  Result := TmnSynStdSQLSyn.Create(nil);
end;

procedure TSQLFileCategory.InitMappers;
begin
  with Highlighter as TmnSynStdSQLSyn do
  begin
    Mapper.Add(WhitespaceAttri, attDefault);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(KeywordAttri, attKeyword);
    Mapper.Add(DocumentAttri, attDocument);
    Mapper.Add(TypeAttri, attDataType);
    Mapper.Add(FunctionAttri, attCommon);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(TextAttri, attText);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(VariableAttri, attVariable);
    Mapper.Add(ProcessorAttri, attDirective);

{    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(TypeAttri, attDataType);
    Mapper.Add(ObjectAttri, attVariable);
    Mapper.Add(FunctionAttri, attCommon);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(KeyAttri, attKeyword);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(SpaceAttri, attDefault);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(VariableAttri, attVariable);}
  end;
end;

procedure TSQLFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  inherited;
  Completion.EndOfTokenChr := '"{}()[].<>/\:!&*+-=%;"';//what about notice "
end;

procedure TSQLFileCategory.DoPrepareCompletion(AEditor: TCustomSynEdit);
var
  i: Integer;
begin
  inherited;
  Screen.Cursor := crHourGlass;
  Completion.BeginUpdate;
  try
    Completion.Clear;
    EnumerateKeywords(Ord(attKeyword), StdSQLKeywords, Highlighter.IdentChars, @AddKeyword);
    EnumerateKeywords(Ord(attDataType), StdSQLTypes, Highlighter.IdentChars, @AddKeyword);
    EnumerateKeywords(Ord(attCommon), StdSQLFunctions, Highlighter.IdentChars, @AddKeyword);
    for i := 0 to DBEngine.DB.Tables.Count -1 do
      AddKeyword(DBEngine.DB.Tables[i].Name, 'variable', attVariable, TRUE);
  finally
    Completion.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

{ TDBFileCategory }

function TDBFileCategory.CreateHighlighter: TSynCustomHighlighter;
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

function TDBFileCategory.OpenFile(vGroup: TFileGroup; vFiles: TEditorFiles; vFileName, vFileParams: string): TEditorFile;
begin
  Result := nil;
  //MsgBox.Show(vFileName + ' is a database'); //TODO open database
  DBEngine.OpenDatabase(vFileName, 'sqlite', '', '', '', '', '');
end;

initialization
  with Engine do
  begin
    Categories.Add(TSQLFileCategory.Create(DefaultProject.Tendency, 'sql', 'SQL'));
    Groups.Add(TSQLFile, 'sql', 'SQL', TSQLFileCategory, ['.sql'], [fgkAssociated, fgkBrowsable], [capTransaction, capExecute]);
    Categories.Add(TDBFileCategory.Create(DefaultProject.Tendency, 'DB', 'Database connection'));
    Groups.Add(nil, 'SQLite', 'SQLite', TDBFileCategory, ['.sqlite'], [fgkAssociated, fgkBinary, fgkBrowsable]);
    //Groups.Add(TDBFile, 'Firebird', 'Firebird', TDBFileCategory, ['.firebird'], [fgkAssociated, fgkBinary, fgkBrowsable]);
  end;
end.
