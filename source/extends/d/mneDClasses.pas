unit mneDClasses;
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
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics, FileUtil,
  Contnrs, LCLintf, LCLType, Dialogs, EditorOptions, SynEditHighlighter,
  SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  mnSynHighlighterD, EditorDebugger, gdbClasses,
  EditorClasses, mneClasses, MsgBox,
  mneCompilerProjectFrames, mneDTendencyFrames, EditorRun, mneConsoleClasses,
  mneConsoleForms, mneRunFrames;

type

  { TDFile }

  TDFile = class(TSourceEditorFile)
  protected
  public
    procedure NewContent; override;
    procedure OpenInclude; override;
    function CanOpenInclude: Boolean; override;
  end;

  { TDFileCategory }

  TDFileCategory = class(TCodeFileCategory)
  private
  protected
    procedure InitMappers; override;
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
    procedure DoAddKeywords; override;
  public
  end;

  { TDProjectOptions }

  TDProjectOptions = class(TEditorProjectOptions)
  private
    FCompilerType: Integer;
  public
    procedure CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack); override;
  published
    property CompilerType: Integer read FCompilerType write FCompilerType default 0;
  end;

  { TDTendency }

  TDTendency = class(TEditorTendency)
  private
    FCompilerType: Integer;
  protected
    function CreateDebugger: TEditorDebugger; override;
    procedure Init; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    constructor Create; override;
    function CreateOptions: TEditorProjectOptions; override;
    procedure CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack); override;
  published
    property CompilerType: Integer read FCompilerType write FCompilerType default 0;
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils, mnSynHighlighterMultiProc, SynEditStrConst, mneDProjectFrames, LCLProc;

{ TDProject }

procedure TDProjectOptions.CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack);
var
  aFrame: TFrame;
begin
  aFrame := TCompilerProjectFrame.Create(AOwner);
  (aFrame as TCompilerProjectFrame).Project := AProject;
  aFrame.Caption := 'Compiler';
  AddFrame(aFrame);

  aFrame := TDProjectFrame.Create(AOwner);
  (aFrame as TDProjectFrame).Project := AProject;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

{ TDFile }

procedure TDFile.NewContent;
begin
  SynEdit.Text := cDSample;
end;

{ TDFile }

procedure TDFile.OpenInclude;
var
  P: TPoint;
  Attri: TSynHighlighterAttributes;
  aToken: string;
  aTokenType: integer;
  aStart: integer;

  function TryOpen: boolean;
  begin
    if (aToken[1] = '/') or (aToken[1] = '\') then
      aToken := RightStr(aToken, Length(aToken) - 1);
    aToken := Engine.ExpandFile(aToken);
    Result := FileExists(aToken);
    if Result then
      Engine.Files.OpenFile(aToken);
  end;

begin
  inherited;
  if Engine.Files.Current <> nil then
  begin
    if Engine.Files.Current.Group.Category is TDFileCategory then
    begin
      P := SynEdit.CaretXY;
      SynEdit.GetHighlighterAttriAtRowColEx(P, aToken, aTokenType, aStart, Attri);
      aToken := DequoteStr(aToken);
      //if (aToken <> '') and (TtkTokenKind(aTokenType) = tkString) then
      begin
        aToken := StringReplace(aToken, '/', '\', [rfReplaceAll, rfIgnoreCase]);
        if not TryOpen then
        begin
          aToken := ExtractFileName(aToken);
          TryOpen;
        end;
      end;
    end;
  end;
end;

function TDFile.CanOpenInclude: Boolean;
var
  P: TPoint;
  Attri: TSynHighlighterAttributes;
  aToken: string;
  aTokenType: integer;
  aStart: integer;
begin
  Result := False;
  if (Group <> nil) then
  begin
    if Group.Category is TDFileCategory then
    begin
      P := SynEdit.CaretXY;
      aToken := '';
      SynEdit.GetHighlighterAttriAtRowColEx(P, aToken, aTokenType, aStart, Attri);
      //Result := (aToken <> '') and (TtkTokenKind(aTokenType) = tkString);
    end;
  end;
end;

{ TDTendency }

procedure TDTendency.DoRun(Info: TmneRunInfo);
var
  aParams: string;
  i: Integer;
  aPath: string;
  aRunItem: TmneRunItem;
begin
  Engine.Session.Run.Clear;

  if rnaCompile in Info.Actions then
  begin
    aParams := '';
    aRunItem := Engine.Session.Run.Add;

    aRunItem.Info.Run.Command := Info.Command;
    if aRunItem.Info.Run.Command = '' then
    begin
      case CompilerType of
        0: aRunItem.Info.Run.Command := 'dmd.exe';
        1:
        {$ifdef windows}
        aRunItem.Info.Run.Command := 'gdc.exe';
        {$else}
        aRunItem.Info.Run.Command := 'gdc';
        {$endif}
      end;
    end;

    aRunItem.Info.Run.Silent := True;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;

    aPath := Info.MainFile;
    if RunOptions.ExpandPaths then
      aPath := Engine.ExpandFile(aPath);
    if not FileExists(aPath) then
      raise EEditorException.Create('File not exists: ' + aParams);

    aRunItem.Info.Run.Params := aPath + #13;

    if Info.OutputFile <> '' then
      case CompilerType of
        0: aRunItem.Info.Run.Params := aRunItem.Info.Run.Params + '-of' + Info.OutputFile + #13; //dmd
        1: aRunItem.Info.Run.Params := aRunItem.Info.Run.Params + '-o' + Info.OutputFile + #13; //gdc
      end;

    if rnaDebug in Info.Actions then
    begin
      case CompilerType of
        0: aRunItem.Info.Run.Params := aRunItem.Info.Run.Params + '-g'#13;
        1: aRunItem.Info.Run.Params := aRunItem.Info.Run.Params + '-g'#13;
      end;
    end;

    aRunItem.Info.Message := 'Compiling ' + Info.OutputFile;
    //aRunItem.Info.Params := aRunItem.Info.Params + '-color=on' + #13; //not work :(

    for i := 0 to RunOptions.Paths.Count - 1 do
    begin
      aPath := Trim(RunOptions.Paths[i]);
      if aPath <>'' then
      begin
        if RunOptions.ExpandPaths then
          aPath := Engine.ExpandFile(aPath);
        if not DirectoryExists(aPath) then
          raise EEditorException.Create('Path not exists: ' + aParams);

        case CompilerType of
          0: aRunItem.Info.Run.Params := aRunItem.Info.Run.Params + '-I' +aPath + #13;
          1: aRunItem.Info.Run.Params := aRunItem.Info.Run.Params + '-B' +aPath + #13;
        end;
      end;
    end;

    //aRunItem.Info.Params := aRunItem.Info.Params + '-v'#13;

    if RunOptions.ConfigFile <> '' then
      aRunItem.Info.Run.Params := aRunItem.Info.Run.Params + '@' + Engine.EnvReplace(RunOptions.ConfigFile) + #13;
  end;

  if rnaExecute in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;
    aRunItem.Info.Message := 'Running ' + Info.OutputFile;
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.Run.Pause := Info.Pause;
    aRunItem.Info.DebugIt := rnaDebug in Info.Actions;
    aRunItem.Info.Title := ExtractFileName(Info.OutputFile);;
    aRunItem.Info.Run.Command := Info.RunFile;
    if RunOptions.Params <> '' then
      aRunItem.Info.Run.Params := aRunItem.Info.Run.Params + RunOptions.Params + #13;
  end;

  Engine.Session.Run.Start;
end;

constructor TDTendency.Create;
begin
  inherited Create;
end;

procedure TDTendency.CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack);
var
  aFrame: TRunFrameOptions;
  aTendencyFrame: TDTendencyFrame;
begin
  aFrame := TRunFrameOptions.Create(AOwner);
  aFrame.Options := ATendency.RunOptions;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);

  aTendencyFrame := TDTendencyFrame.Create(AOwner);
  aTendencyFrame.Tendency := ATendency;
  aTendencyFrame.Caption := 'D Options';
  AddFrame(aTendencyFrame);
end;

function TDTendency.CreateDebugger: TEditorDebugger;
begin
  Result := TGDBDebug.Create;
end;

function TDTendency.CreateOptions: TEditorProjectOptions;
begin
  Result := TDProjectOptions.Create;
end;

procedure TDTendency.Init;
begin
  FCapabilities := [capRun, capDebug, capTrace, capCompile, capLink, capOptions];
  FTitle := 'D Lang';
  FDescription := 'D Files, *.D, *.inc';
  FName := 'D';
  FImageIndex := -1;
  AddGroup('cfg', 'cfg');
  AddGroup('ini', 'ini');
  AddGroup('txt', 'txt');
end;

{ TDFileCategory }

function TDFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynDSyn.Create(nil);
end;

procedure TDFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  inherited;
  FCompletion.EndOfTokenChr := '${}()[].<>/\:!&*+-=%;';
  IdentifierID := ord(mnSynHighlighterMultiProc.tkIdentifier);
end;

procedure TDFileCategory.DoAddKeywords;
begin
  EnumerateKeywords(Ord(tkKeyword), sDKeywords, Highlighter.IdentChars, @DoAddCompletion);
  EnumerateKeywords(Ord(tkFunction), sDFunctions, Highlighter.IdentChars, @DoAddCompletion);
end;

procedure TDFileCategory.InitMappers;
begin
  with Highlighter as TSynDSyn do
  begin
    Mapper.Add(WhitespaceAttri, attDefault);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(KeywordAttri, attKeyword);
    Mapper.Add(DocumentAttri, attDocument);
    Mapper.Add(ValueAttri, attValue);
    Mapper.Add(FunctionAttri, attStandard);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(TextAttri, attText);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(VariableAttri, attVariable);
    Mapper.Add(ProcessorAttri, attDirective);
  end;
end;

initialization
  with Engine do
  begin
    Tendencies.Add(TDTendency);
    Categories.Add(TDFileCategory.Create(TDTendency, 'D', [fckPublish]));
    Groups.Add(TDFile, 'd', 'D', TDFileCategory, ['d', 'inc'], [fgkAssociated, fgkExecutable, fgkBrowsable, fgkMain]);
  end;
end.
