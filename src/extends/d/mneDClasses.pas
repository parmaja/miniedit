unit mneDClasses;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *
 *}

interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics, LazFileUtils,
  Contnrs, LCLintf, LCLType, Dialogs, EditorOptions, SynEditHighlighter,
  SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  gdbClasses, EditorClasses, mneClasses, mnMsgBox,
  mneCompilerProjectFrames, mneDTendencyFrames, EditorRun, mneRunFrames;

type

  TDDebugger = class(TGDBDebug)
  public
  end;

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
    procedure DoAddKeywords; override;
  public
    function CreateHighlighter: TSynCustomHighlighter; override;
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
  end;


  TDCompilerType = (cmpkDMD, cmpkGDC, cmpkLDC);

  { TDProjectOptions }

  TDProjectOptions = class(TEditorProjectOptions)
  private
    FCompilerType: TDCompilerType;
  public
    procedure CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack); override;
  published
    property CompilerType: TDCompilerType read FCompilerType write FCompilerType default cmpkDMD;
  end;

  { TDTendency }

  TDTendency = class(TEditorTendency)
  private
    FCompilerType: Integer;
    FUseCfg: boolean;
  protected
    function CreateDebugger: TEditorDebugger; override;
    procedure Created; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    constructor Create; override;
    function CreateProjectOptions: TEditorProjectOptions; override;
    procedure CreateOptionsFrame(AOwner: TComponent; AddFrame: TAddFrameCallBack); override;
    procedure SendMessage(S: string; vMessageType: TNotifyMessageType); override; //Please handle errors format in RunItems
  published
    property CompilerType: Integer read FCompilerType write FCompilerType default 0;
    property UseCfg: boolean read FUseCfg write FUseCfg default false;
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils, mnSynHighlighterD, mnSynHighlighterMultiProc, SynEditStrConst, mneDProjectFrames, LCLProc;

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
  i: Integer;
  aPath: string;
  aRunItem: TmneRunItem;
begin
  Engine.Session.Run.Clear;
  Engine.SendAction(eaClearOutput);

  if rnaCompile in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;
    aRunItem.Info.Run.Silent := True;
    aRunItem.Info.Run.CatchOutput := True;
    aRunItem.MessageType := msgtInteractive;

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
    aRunItem.Info.Run.CatchOutput := True;
    aRunItem.MessageType := msgtInteractive;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;

    aPath := Info.MainFile;
    if RunOptions.ExpandPaths then
      aPath := Engine.ExpandFile(aPath);
    if not FileExists(aPath) then
      raise EEditorException.Create('File not exists: ' + aPath);

    aRunItem.Info.Run.AddParam(aPath);

    if RunOptions.ConfigFile <> '' then
      aRunItem.Info.Run.AddParam('@' + ReplaceVariables(RunOptions.ConfigFile, []))
    else if UseCfg then
    begin
      if FileExists(ChangeFileExt(Info.MainFile, '.cfg')) then
        aRunItem.Info.Run.AddParam('@' + ExtractFileNameWithoutExt(ExtractFileName(Info.MainFile))+'.cfg');
    end;

    if Info.OutputFile <> '' then
      case CompilerType of
        0: aRunItem.Info.Run.AddParam('-of' + Info.OutputFile); //dmd
        1: aRunItem.Info.Run.AddParam('-o' + Info.OutputFile); //gdc
      end;

    if rnaDebug in Info.Actions then
    begin
      case CompilerType of
        0: aRunItem.Info.Run.AddParam('-g');
        1: aRunItem.Info.Run.AddParam('-g');
      end;
    end;

    for i := 0 to RunOptions.Paths.Count - 1 do
    begin
      aPath := Trim(RunOptions.Paths[i]);
      if aPath <>'' then
      begin
        if RunOptions.ExpandPaths then
          aPath := Engine.ExpandFile(aPath);
        if not DirectoryExists(aPath) then
          raise EEditorException.Create('Path not exists: ' + aPath);

        case CompilerType of
          0: aRunItem.Info.Run.AddParam('-I' + aPath);
          1: aRunItem.Info.Run.AddParam('-B' + aPath);
        end;
      end;
    end;

    //aRunItem.Info.AddParam('-v');
    aRunItem.Info.StatusMessage := 'Compiling ' + Info.OutputFile;
  end;

  if rnaExecute in Info.Actions then
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
end;

constructor TDTendency.Create;
begin
  inherited Create;
end;

procedure TDTendency.CreateOptionsFrame(AOwner: TComponent; AddFrame: TAddFrameCallBack);
var
  aFrame: TRunFrameOptions;
  aTendencyFrame: TDTendencyFrame;
begin
  aFrame := TRunFrameOptions.Create(AOwner);
  aFrame.Options := RunOptions;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);

  aTendencyFrame := TDTendencyFrame.Create(AOwner);
  aTendencyFrame.Tendency := Self;
  aTendencyFrame.Caption := 'D Options';
  AddFrame(aTendencyFrame);
end;

function PosForward(S: string; vChar: string): Integer;
begin
  Result := PosEx(vChar, S);
end;

function PosBackword(S: string; vChar: Char): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Length(S) downto 1 do
  begin
    if S[i] = vChar then
    begin
      Result := i;
      break;
    end;
  end;
end;

procedure TDTendency.SendMessage(S: string; vMessageType: TNotifyMessageType);
var
  aMsg: TMessageInfo;
  p: Integer;
  m, t : string;
begin
  if (S <> '') and (vMessageType = msgtInteractive) then
  begin
    p := PosForward(S, '):');
    if p > 0 then
    begin
      aMsg := Default(TMessageInfo);
      aMsg.Processed := True;
      aMsg.MessageType := vMessageType;
      t := MidStr(S, 1, p - 1);
      m := Trim(MidStr(S, p + 2, MaxInt));
      p := PosBackword(t, '(');
      if p > 0 then
      begin
        aMsg.FileName := ExpandToPath(Trim(MidStr(t, 1, p - 1)), Engine.Session.Run.CurrentDirectory);
        t := MidStr(t, p + 1, MaxInt);
        aMsg.Line := StrToIntDef(t, 0);
      end;
      p := PosForward(m, ':');
      if p > 0 then
      begin
        aMsg.Name := Trim(MidStr(m, 1, p - 1));
        m := Trim(MidStr(m, p + 1, MaxInt));
      end;
      aMsg.Message := m;
      Engine.SendMessage(S, aMsg);
    end
    else
      inherited;
  end
  else
    inherited;
end;

function TDTendency.CreateDebugger: TEditorDebugger;
begin
  Result := TGDBDebug.Create;
end;

function TDTendency.CreateProjectOptions: TEditorProjectOptions;
begin
  Result := TDProjectOptions.Create;
end;

procedure TDTendency.Created;
begin
  FCapabilities := [capExecute, capDebug, capTrace, capCompile, capLink];
  FHaveOptions := True;
  FTitle := 'D Lang';
  FDescription := 'D Files, *.D, *.inc';
  FName := 'D';
  {$ifdef windows}
  OutputExtension := '.exe';
  {$endif}
  AddGroup('cfg', 'cfg');
  AddGroup('ini', 'ini');
  AddGroup('txt', 'txt');
end;

{ TDFileCategory }

function TDFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynDSyn.Create(nil);
end;

procedure TDFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  inherited;
  FCompletion.EndOfTokenChr := '${}()[].<>/\:!&*+-=%;';
end;

procedure TDFileCategory.DoAddKeywords;
begin
  EnumerateKeywords(Ord(attKeyword), sDKeywords, Highlighter.IdentChars, @AddKeyword);
  EnumerateKeywords(Ord(attCommon), sDFunctions, Highlighter.IdentChars, @AddKeyword);
end;

procedure TDFileCategory.InitMappers;
begin
  with Highlighter as TSynDSyn do
  begin
    Mapper.Add(WhitespaceAttri, attDefault);
    Mapper.Add(CommentAttri, attComment, ord(tkComment));
    Mapper.Add(KeywordAttri, attKeyword, ord(tkKeyword));
    Mapper.Add(DocumentAttri, attDocument, ord(tkDocument));
    Mapper.Add(TypeAttri, attDataType);
    Mapper.Add(FunctionAttri, attCommon);
    Mapper.Add(IdentifierAttri, attIdentifier, ord(tkIdentifier));
    Mapper.Add(TextAttri, attText);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(VariableAttri, attVariable, ord(tkVariable));
    Mapper.Add(ProcessorAttri, attDirective);
  end;
end;

initialization
  with Engine do
  begin
    Tendencies.Add(TDTendency);
    Categories.Add(TDFileCategory.Create(TDTendency, 'D', 'D lang', [fckPublish]));
    Groups.Add(TDFile, 'd', 'D', TDFileCategory, ['.d', '.inc'], [fgkAssociated, fgkBrowsable, fgkMain], [capExecute, capDebug]);
  end;
end.
