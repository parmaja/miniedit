unit mnePASClasses;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics,
  Contnrs, LCLintf, LCLType, Dialogs, EditorOptions, SynEditHighlighter,
  SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  EditorDebugger, gdbClasses, EditorRun, mneCompilerProjectFrames, FileUtil,
  SynHighlighterPas, SynHighlighterLFM;

type

  { TmneSynPASSyn }

  TmneSynPASSyn = class(TSynPASSyn) //Only need for add sample source
  public
    function GetSampleSource: string; override;
  end;

  { TPASFile }

  TPASFile = class(TSourceEditorFile)
  protected
    procedure NewContent; override;
  public
  end;

  TLFMFile = class(TTextEditorFile)
  protected
    //procedure NewContent; override;
  public
  end;

  { TPASFileCategory }

  TPASFileCategory = class(TCodeFileCategory)
  private
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
    procedure DoAddKeywords; override;
  public
  end;

  { TLFMFileCategory }

  TLFMFileCategory = class(TTextFileCategory)
  private
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
  public
  end;

  { TPasProjectOptions }

  TPasProjectOptions = class(TCompilerProjectOptions)
  private
    FUseCFG: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack); override;
  published
    property UseCFG: Boolean read FUseCFG write FUseCFG default True;
  end;

  { TPasTendency }

  TPasTendency = class(TEditorTendency)
  private
    FCompiler: string;
  protected
    function CreateDebugger: TEditorDebugger; override;
    procedure Init; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    procedure CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack); override;
    function CreateOptions: TEditorProjectOptions; override;
    property Compiler: string read FCompiler write FCompiler;
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils, mnePasProjectFrames, mneCompilerTendencyFrames;

{ TPasProjectOptions }

constructor TPasProjectOptions.Create;
begin
  inherited Create;
  FUseCFG := True;
end;

destructor TPasProjectOptions.Destroy;
begin
  inherited Destroy;
end;

procedure TPasProjectOptions.CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack);
var
  aFrame: TFrame;
begin
  aFrame := TCompilerProjectFrame.Create(AOwner);
  (aFrame as TCompilerProjectFrame).FProject := AProject;
  aFrame.Caption := 'Compiler';
  AddFrame(aFrame);
  aFrame := TPasProjectFrame.Create(AOwner);
  (aFrame as TPasProjectFrame).Project := AProject;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

{ TmneSynPASSyn }

function TmneSynPASSyn.GetSampleSource: string;
begin
  Result := '{ Syntax highlighting }'#13#10 +
             'procedure TForm1.Button1Click(Sender: TObject);'#13#10 +
             'var'#13#10 +
             '  Number, I, X: Integer;'#13#10 +
             'begin'#13#10 +
             '  Number := 123456;'#13#10 +
             '  Caption := ''The Number is'' + #32 + IntToStr(Number);'#13#10 +
             '  for I := 0 to Number do'#13#10 +
             '  begin'#13#10 +
             '    Inc(X);'#13#10 +
             '    Dec(X);'#13#10 +
             '    X := X + 1.0;'#13#10 +
             '    X := X - $5E;'#13#10 +
             '  end;'#13#10 +
             '  {$R+}'#13#10 +
             '  asm'#13#10 +
             '    mov AX, 1234H'#13#10 +
             '    mov Number, AX'#13#10 +
             '  end;'#13#10 +
             '  {$R-}'#13#10 +
             'end;';
end;

{ TLFMFileCategory }

function TLFMFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynLFMSyn.Create(nil);
end;

procedure TLFMFileCategory.InitMappers;
begin
  with Highlighter as TSynLFMSyn do
  begin
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(KeyAttri, attKeyword);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(SpaceAttri, attDefault);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttribute, attSymbol);
  end;
end;

{ TPasTendency }

function TPasTendency.CreateDebugger: TEditorDebugger;
begin
  Result := TGDBDebug.Create;
end;

function TPasTendency.CreateOptions: TEditorProjectOptions;
begin
  Result := TPasProjectOptions.Create;;
end;

procedure TPasTendency.Init;
begin
  FCapabilities := [capRun, capDebug, capTrace, capCompile, capLink, capOptions];
  FName := 'Pascal';
  FTitle := 'Pascal project';
  FDescription := 'Pascal/FPC/Lazarus Files, *.pas, *.pp *.inc';
  FImageIndex := -1;
end;

procedure TPasTendency.DoRun(Info: TmneRunInfo);
var
  i: Integer;
  aPath: string;
  Options: TPasProjectOptions;
  aRunItem: TmneRunItem;
  p: string;
begin
  if (Engine.Session.Project.Options is TPasProjectOptions) then
    Options := (Engine.Session.Project.Options as TPasProjectOptions)
  else
    Options := TPasProjectOptions.Create;

  if rnaCompile in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;

    aRunItem.Info.Command := Info.Command;
    if aRunItem.Info.Command = '' then
      aRunItem.Info.Command := 'fpc.exe';

    aRunItem.Info.Mode := runOutput;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;

    aRunItem.Info.Params := Info.MainFile + #13;
    if Info.OutputFile <> '' then
      aRunItem.Info.Params := aRunItem.Info.Params + '-o' + Info.OutputFile + #13;

    aRunItem.Info.Message := 'Compiling ' + Info.OutputFile;

    p := '-Fu';
    for i := 0 to Options.Paths.Count - 1 do
    begin
      aPath := Trim(Options.Paths[i]);
      if aPath <>'' then
      begin
        if Options.ExpandPaths then
          aPath := Engine.ExpandFile(aPath);
        if p <> '' then
          p := p + ';';
        p := p + aPath;
      end;
    end;
    if p <> '' then
      aRunItem.Info.Params := aRunItem.Info.Params + p + #13;
  end;

  if rnaDebug in Info.Actions then
  begin
    aRunItem.Info.Params := aRunItem.Info.Params + '-gw'#13;
    aRunItem.Info.Params := aRunItem.Info.Params + '-dDebug'#13;
  end
  else
    aRunItem.Info.Params := aRunItem.Info.Params + '-dRelease'#13;

  if rnaExecute in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;
    aRunItem.Info.Message := 'Running ' + Info.OutputFile;
    aRunItem.Info.Mode := Info.Mode;
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.Pause := Info.Pause;
    aRunItem.Info.DebugIt := rnaDebug in Info.Actions;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.OutputFile);
    aRunItem.Info.Command := Info.RunFile;
    if Options.RunParams <> '' then
      aRunItem.Info.Params := aRunItem.Info.Params + Options.RunParams + #13;
  end;

  Engine.Session.Run.Start;
end;

procedure TPasTendency.CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack);
var
  aFrame: TCompilerTendencyFrame;
begin
  aFrame := TCompilerTendencyFrame.Create(AOwner);
  aFrame.FTendency := ATendency;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

{ TPASFileCategory }

function TPASFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TmneSynPASSyn.Create(nil);
end;

procedure TPASFileCategory.InitMappers;
begin
  with Highlighter as TSynPasSyn do
  begin
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(KeyAttri, attKeyword);
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(ASMAttri, attEmbedText);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(IDEDirectiveAttri, attDirective);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(SpaceAttri, attDefault);
    Mapper.Add(CaseLabelAttri, attSymbol);
    Mapper.Add(DirectiveAttri, attDirective);
  end;
end;

procedure TPASFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  inherited;
  Completion.EndOfTokenChr := '${}()[].<>/\:!&*+-=%;';
  IdentifierID := ord(SynHighlighterPas.tkIdentifier);
end;

procedure TPASFileCategory.DoAddKeywords;
begin
  inherited DoAddKeywords;
  //EnumerateKeywords(Ord(tkKeyword), sPasKeywords, Highlighter.IdentChars, @DoAddCompletion);
  //EnumerateKeywords(Ord(tkFunction), sDFunctions, Highlighter.IdentChars, @DoAddCompletion);
end;

{ TPASFile }

procedure TPASFile.NewContent;
begin
  inherited NewContent;
  SynEdit.Text := 'unit ';
  SynEdit.Lines.Add('');
  SynEdit.Lines.Add('interface');
  SynEdit.Lines.Add('');
  SynEdit.Lines.Add('uses');
  SynEdit.Lines.Add('  SysUtils;');
  SynEdit.Lines.Add('');
  SynEdit.Lines.Add('implementation');
  SynEdit.Lines.Add('');
  SynEdit.Lines.Add('end.');
  SynEdit.CaretY := 1;
  SynEdit.CaretX := 5;
end;

initialization
  with Engine do
  begin
    Tendencies.Add(TPasTendency);
    Categories.Add(TPASFileCategory.Create(TPasTendency, 'pas'));
    Categories.Add(TLFMFileCategory.Create(TPasTendency, 'lfm'));

    Groups.Add(TPASFile, 'pas', 'Pascal', TPASFileCategory, ['pas', 'pp', 'p', 'inc'], [fgkAssociated, fgkExecutable, fgkBrowsable], [fgsFolding]);
    Groups.Add(TPASFile, 'ppr', 'Pascal Project', TPASFileCategory, ['ppr'], [fgkAssociated, fgkMain, fgkExecutable, fgkBrowsable], [fgsFolding]);//PPR meant Pascal project
    Groups.Add(TPASFile, 'lpr', 'Lazarus Project', TPASFileCategory, ['lpr'], [fgkAssociated, fgkMain,fgkExecutable, fgkBrowsable], [fgsFolding]);
    Groups.Add(TPASFile, 'dpr', 'Delphi Project', TPASFileCategory, ['dpr'], [fgkAssociated, fgkMain, fgkExecutable, fgkBrowsable], [fgsFolding]);
    Groups.Add(TLFMFile, 'lfm', 'Lazarus Form', TLFMFileCategory, ['lfm'], [fgkAssociated, fgkBrowsable], [fgsFolding]);
  end;
end.
