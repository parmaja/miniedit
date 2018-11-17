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
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics, mneClasses,
  Contnrs, LCLintf, LCLType, Dialogs, EditorOptions, SynEditHighlighter,
  SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  EditorDebugger, gdbClasses, EditorRun, mneCompilerProjectFrames, mneRunFrames,
  LazFileUtils, SynHighlighterPas, SynHighlighterLFM;

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
    function GetColorPrefix: string; override;
    function FormatColor(Color: TColor): string; override;
    function DeformatColor(Str: string): TColor; override;
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

  TPasProjectOptions = class(TEditorProjectOptions)
  private
  public
    constructor Create; override;
    procedure CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack); override;
  published
  end;

  { TPasTendency }

  TPasTendency = class(TEditorTendency)
  private
    FUseCFG: Boolean;
    FCompiler: string;
  protected
    function CreateDebugger: TEditorDebugger; override;
    procedure Init; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    procedure CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack); override;
    function CreateOptions: TEditorProjectOptions; override;
    property Compiler: string read FCompiler write FCompiler;
    property UseCFG: Boolean read FUseCFG write FUseCFG default True;
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils, mnePasProjectFrames;

{ TPasProjectOptions }

constructor TPasProjectOptions.Create;
begin
  inherited Create;
end;

procedure TPasProjectOptions.CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack);
var
  aFrame: TFrame;
begin
  aFrame := TCompilerProjectFrame.Create(AOwner);
  (aFrame as TCompilerProjectFrame).Project := AProject;
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
  FUseCFG := True;
  {$ifdef windows}
  OutputExtension := '.exe';
  {$endif}
  FImageIndex := -1;
end;

procedure TPasTendency.DoRun(Info: TmneRunInfo);
var
  i: Integer;
  aPath: string;
  aRunItem: TmneRunItem;
  p: string;
begin
  aRunItem := Engine.Session.Run.Add;
  if rnaCompile in Info.Actions then
  begin
    aRunItem.Info.Run.Command := Info.Command;
    if aRunItem.Info.Run.Command = '' then
      aRunItem.Info.Run.Command := 'fpc.exe';

    aRunItem.Info.Run.Silent := True;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;

    aRunItem.Info.Run.AddParam(Info.MainFile);
    if Info.OutputFile <> '' then
      aRunItem.Info.Run.AddParam('-o' + Info.OutputFile);

    aRunItem.Info.Run.AddParam('-MObjFPC');

    if RunOptions.ConfigFile <> '' then
      aRunItem.Info.Run.AddParam('@' + Engine.EnvReplace(RunOptions.ConfigFile))
    else if UseCfg then
    begin
      if FileExists(ChangeFileExt(Info.MainFile, '.cfg')) then
        aRunItem.Info.Run.AddParam('@' + ExtractFileNameWithoutExt(ExtractFileName(Info.MainFile))+'.cfg');
    end;

    p := '';
    for i := 0 to RunOptions.Paths.Count - 1 do
    begin
      aPath := Trim(RunOptions.Paths[i]);
      if aPath <>'' then
      begin
        if RunOptions.ExpandPaths then
          aPath := Engine.ExpandFile(aPath);
        if p <> '' then
          p := p + ';';
        p := p + aPath;
      end;
    end;
    if p <> '' then
      aRunItem.Info.Run.AddParam('-Fu'+p);

    if rnaDebug in Info.Actions then
    begin
      aRunItem.Info.Run.AddParam('-gw');
      aRunItem.Info.Run.AddParam('-dDebug');
    end
    else
      aRunItem.Info.Run.AddParam('-dRelease');

    aRunItem.Info.StatusMessage := 'Compiling ' + Info.OutputFile;
  end;

  if (rnaExecute in Info.Actions) then
  begin
    aRunItem := Engine.Session.Run.Add;
    aRunItem.Info.Run.Pause := Info.Pause;
    aRunItem.Info.Run.Console := Info.Console;
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.StartDebug := rnaDebug in Info.Actions;
    aRunItem.Info.Title := ExtractFileName(Info.OutputFile);
    aRunItem.Info.Run.Command := Info.RunFile;
    aRunItem.Info.Run.AddParam(RunOptions.Params);
    aRunItem.Info.Run.AddParam(Engine.Session.Project.RunOptions.Params);
    aRunItem.Info.StatusMessage := 'Running ' + Info.OutputFile;
  end;

  Engine.Session.Run.Start(Self);
end;

procedure TPasTendency.CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack);
var
  aFrame: TRunFrameOptions;
begin
  aFrame := TRunFrameOptions.Create(AOwner);
  aFrame.Options := ATendency.RunOptions;
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

function TPASFileCategory.GetColorPrefix: string;
begin
  Result := '$';
end;

function TPASFileCategory.FormatColor(Color: TColor): string;
begin
  Result := ColorToRGBHex(Color, GetColorPrefix, true);
end;

function TPASFileCategory.DeformatColor(Str: string): TColor;
begin
  Result := RGBHexToColor(str, GetColorPrefix, true);
end;

{ TPASFile }

procedure TPASFile.NewContent;
begin
  inherited NewContent;
  SynEdit.Text := 'program newprogram;';
  SynEdit.Lines.Add('');
  SynEdit.Lines.Add('uses');
  SynEdit.Lines.Add('  SysUtils;');
  SynEdit.Lines.Add('');
  SynEdit.Lines.Add('begin');
  SynEdit.Lines.Add('');
  SynEdit.Lines.Add('end.');
  SynEdit.CaretX := 1;
  SynEdit.CaretY := 7;
end;

initialization
  with Engine do
  begin
    Tendencies.Add(TPasTendency);
    Categories.Add(TPASFileCategory.Create(TPasTendency, 'pas', 'Pascal'));
    Categories.Add(TLFMFileCategory.Create(TPasTendency, 'lfm', 'Lazarus form lfm'));

    Groups.Add(TPASFile, 'pas', 'Pascal', TPASFileCategory, ['pas', 'pp', 'p', 'dpk', 'inc'], [fgkAssociated, fgkExecutable, fgkBrowsable], [fgsFolding]);
    Groups.Add(TPASFile, 'ppr', 'Pascal Project', TPASFileCategory, ['dpr', 'lpr', 'ppr'], [fgkAssociated, fgkMain, fgkExecutable, fgkBrowsable], [fgsFolding]);//PPR meant Pascal project
    Groups.Add(TLFMFile, 'lfm', 'Delphi, Lazarus Form', TLFMFileCategory, ['dfm', 'lfm'], [fgkAssociated, fgkBrowsable], [fgsFolding]);
  end;
end.
