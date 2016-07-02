unit mneVerilogClasses;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *}

interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics,
  Contnrs, LCLintf, LCLType, Dialogs, EditorOptions, SynEditHighlighter,
  SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  EditorDebugger, EditorRun, DebugClasses, mneCompileProjectOptions, LazFileUtils,
  SynHighlighterVerilog;

type

  { TmneSynVerilogSyn }

  TmneSynVerilogSyn = class(TSynVerilogSyn) //Only need for add sample source
  public
  end;

  { TVerilogFile }

  TVerilogFile = class(TSourceEditorFile)
  protected
    procedure NewContent; override;
  public
  end;

  { TVerilogFileCategory }

  TVerilogFileCategory = class(TCodeFileCategory)
  private
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
    procedure DoAddKeywords; override;
  public
  end;
(* We dont have Project/Tendency yet, TODO

  { TVerilogProjectOptions }

  TVerilogProjectOptions = class(TCompilerProjectOptions)
  private
    FUseCFG: Boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack); override;
  published
    property UseCFG: Boolean read FUseCFG write FUseCFG default True;
  end;

  { TVerilogTendency }

  TVerilogTendency = class(TEditorTendency)
  private
    FCompiler: string;
  protected
    function CreateDebugger: TEditorDebugger; override;
    function CreateOptions: TEditorProjectOptions; override;
    procedure Init; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    procedure CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack); override;
    property Compiler: string read FCompiler write FCompiler;
  end;
*)

implementation

uses
  IniFiles, mnStreams, mnUtils(*, mneVerilogProjectFrames, mneVerilogConfigForms*);

{ TmneSynVerilogSyn }

(* not yet

{ TVerilogProjectOptions }

constructor TVerilogProjectOptions.Create;
begin
  inherited Create;
  FUseCFG := True;
end;

destructor TVerilogProjectOptions.Destroy;
begin
  inherited Destroy;
end;

procedure TVerilogProjectOptions.CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack);
var
  aFrame: TFrame;
begin
  aFrame := TCompilerProjectOptionsForm.Create(AOwner);
  (aFrame as TCompilerProjectOptionsForm).Project := AProject;
  aFrame.Caption := 'Compiler';
  AddFrame(aFrame);
  aFrame := TVerilogProjectFrame.Create(AOwner);
  (aFrame as TVerilogProjectFrame).Project := AProject;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

{ TVerilogTendency }

function TVerilogTendency.CreateDebugger: TEditorDebugger;
begin
  Result := nil;
end;

function TVerilogTendency.CreateOptions: TEditorProjectOptions;
begin
  Result := TVerilogProjectOptions.Create;;
end;

procedure TVerilogTendency.Init;
begin
  FCapabilities := [capRun, capCompile, capLink, capOptions];
  FName := 'Verilog';
  FTitle := 'Verilog project';
  FDescription := 'Verilog/FPC/Lazarus Files, *.v, *.vh';
  FImageIndex := -1;
  AddGroup('Verilog', 'Verilog');
  AddGroup('dpr', 'Verilog');
  AddGroup('lpr', 'Verilog');
  AddGroup('ppr', 'Verilog');
  AddGroup('lfm', 'lfm');
  //AddGroup('inc');
end;

procedure TVerilogTendency.DoRun(Info: TmneRunInfo);
var
  aParams: string;
  s: string;
  i: Integer;
  aPath: string;
  Options: TVerilogProjectOptions;
  aRunItem: TmneRunItem;
  p: string;
begin
  if (Engine.Session.IsOpened) then
    Options := (Engine.Session.Project.Options as TVerilogProjectOptions)
  else
    Options := TVerilogProjectOptions.Create;

  if rnaCompile in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;

    aRunItem.Info.Command := Info.Command;
    if aRunItem.Info.Command = '' then
      aRunItem.Info.Command := 'fpc.exe';

    aRunItem.Info.Mode := runOutput;
    aRunItem.Info.Title := ExtractFileNameOnly(Info.MainFile);
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

  if rnaExecute in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;
    aRunItem.Info.Message := 'Running ' + Info.OutputFile;
    aRunItem.Info.Mode := Info.Mode;
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.Pause := Info.Pause;
    aRunItem.Info.Title := ExtractFileNameOnly(Info.OutputFile);
    aRunItem.Info.Command := Info.RunFile;
    if Options.RunParams <> '' then
      aRunItem.Info.Params := aRunItem.Info.Params + Options.RunParams + #13;
  end;

  Engine.Session.Run.Start;
end;

procedure TVerilogTendency.CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack);
var
  aFrame: TVerilogConfigForm;
begin
  aFrame := TVerilogConfigForm.Create(AOwner);
  aFrame.FTendency := ATendency;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;
*)

{ TVerilogFileCategory }

function TVerilogFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TmneSynVerilogSyn.Create(nil);
end;

procedure TVerilogFileCategory.InitMappers;
begin
  with Highlighter as TSynVerilogSyn do
  begin
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(KeywordAttribute, attKeyword);
    Mapper.Add(DirectiveAttri, attDirective);
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(SpaceAttri, attDefault);
  end;
end;

procedure TVerilogFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  inherited;
  Completion.EndOfTokenChr := '${}()[].<>/\:!&*+-=%;';
  IdentifierID := ord(SynHighlighterVerilog.tkIdentifier);
end;

procedure TVerilogFileCategory.DoAddKeywords;
begin
  inherited DoAddKeywords;
  EnumerateKeywords(Ord(tkKeyword), sVerilogKeywords, Highlighter.IdentChars, @DoAddCompletion);
  EnumerateKeywords(Ord(tkDirective), sVerilogDirectives, Highlighter.IdentChars, @DoAddCompletion);
end;

{ TVerilogFile }

procedure TVerilogFile.NewContent;
begin
  inherited NewContent;
  SynEdit.Text := '// hello.v ';
  SynEdit.Lines.Add('module hello;');
  SynEdit.Lines.Add('  initial');
  SynEdit.Lines.Add('    begin');
  SynEdit.Lines.Add('      $display("Hello Verilog");');
  SynEdit.Lines.Add('    end');
  SynEdit.Lines.Add('endmodule // hello');
  SynEdit.CaretY := 1;
  SynEdit.CaretX := 5;
end;

initialization
  with Engine do
  begin
    Categories.Add(TVerilogFileCategory.Create('Verilog'));
    Groups.Add(TVerilogFile, 'Verilog', 'Verilog Files', 'Verilog', ['v', 'vh'], [fgkAssociated, fgkExecutable, fgkMember, fgkBrowsable], [fgsFolding]);
    //Tendencies.Add(TVerilogTendency);
  end;
end.
