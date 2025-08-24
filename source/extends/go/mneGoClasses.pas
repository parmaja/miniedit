unit mneGoClasses;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Goirkey 
 *
 *}

interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics, LazFileUtils,
  Contnrs, LCLintf, LCLType, Dialogs, EditorOptions, SynEditHighlighter,
  SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  LazEditTextAttributes,
  gdbClasses, mnSynHighlighterGo,
  EditorClasses, mneClasses, mnMsgBox,
  mneCompilerProjectFrames, mneGoTendencyFrames, EditorRun,
  mneRunFrames;

type

  { TGoFile }

  TGoFile = class(TSourceEditorFile)
  protected
  public
    procedure NewContent; override;
    procedure OpenInclude; override;
    function CanOpenInclude: Boolean; override;
  end;

  { TGoFileCategory }

  TGoFileCategory = class(TCodeFileCategory)
  private
  protected
    procedure InitMappers; override;
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
    procedure DoAddKeywords; override;
  public
    function CreateHighlighter: TSynCustomHighlighter; override;
  end;

  { TGoProjectOptions }

  TGoProjectOptions = class(TEditorProjectOptions)
  private
    FCompilerType: Integer;
  public
    procedure CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack); override;
  published
    property CompilerType: Integer read FCompilerType write FCompilerType default 0;
  end;

  { TGoTendency }

  TGoTendency = class(TEditorTendency)
  private
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
    property UseCfg: boolean read FUseCfg write FUseCfg default false;
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils, mnSynHighlighterMultiProc, SynEditStrConst, mneGoProjectFrames, LCLProc;

{ TGoProject }

procedure TGoProjectOptions.CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack);
var
  aFrame: TFrame;
begin
  aFrame := TCompilerProjectFrame.Create(AOwner);
  (aFrame as TCompilerProjectFrame).Project := AProject;
  aFrame.Caption := 'Compiler';
  AddFrame(aFrame);

  aFrame := TGoProjectFrame.Create(AOwner);
  (aFrame as TGoProjectFrame).Project := AProject;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

{ TGoFile }

procedure TGoFile.NewContent;
begin
  SynEdit.Text := cGoSample;
end;

{ TGoFile }

procedure TGoFile.OpenInclude;
var
  P: TPoint;
  Attri: TLazEditTextAttribute;
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
    if Engine.Files.Current.Group.Category is TGoFileCategory then
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

function TGoFile.CanOpenInclude: Boolean;
var
  P: TPoint;
  Attri: TLazEditTextAttribute;
  aToken: string;
  aTokenType: integer;
  aStart: integer;
begin
  Result := False;
  if (Group <> nil) then
  begin
    if Group.Category is TGoFileCategory then
    begin
      P := SynEdit.CaretXY;
      aToken := '';
      SynEdit.GetHighlighterAttriAtRowColEx(P, aToken, aTokenType, aStart, Attri);
      //Result := (aToken <> '') and (TtkTokenKind(aTokenType) = tkString);
    end;
  end;
end;

{ TGoTendency }

procedure TGoTendency.DoRun(Info: TmneRunInfo);
var
  //i: Integer;
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

    aRunItem.Info.Run.Command := Info.Command;

    if aRunItem.Info.Run.Command = '' then
    begin
      {$ifdef windows}
      aRunItem.Info.Run.Command := 'go.exe';
      {$else}
      aRunItem.Info.Run.Command := 'go';
      {$endif}
    end;

    aRunItem.Info.Run.Silent := True;
    aRunItem.Info.Run.CatchOutput := True;
    aRunItem.MessageType := msgtInteractive;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;

    aRunItem.Info.Run.AddParam('build');

    if Info.OutputFile <> '' then
      aRunItem.Info.Run.AddParam('-o "' + Info.OutputFile+'"');

    if rnaDebug in Info.Actions then
      aRunItem.Info.Run.AddParam('-tags debug')
    else
    begin
      aRunItem.Info.Run.AddParam('-tags release');
      aRunItem.Info.Run.AddParam('-ldflags "-s -w"');
    end;

    {
    for i := 0 to RunOptions.Paths.Count - 1 do
    begin
      aPath := Trim(RunOptions.Paths[i]);
      if aPath <>'' then
      begin
        if RunOptions.ExpandPaths then
          aPath := Engine.ExpandFile(aPath);
        if not DirectoryExists(aPath) then
          raise EEditorException.Create('Path not exists: ' + aPath);

         aRunItem.Info.Run.AddParam('-I' + aPath);
      end;
    end;}

    aPath := Info.MainFile;
    if RunOptions.ExpandPaths then
      aPath := Engine.ExpandFile(aPath);
    if not FileExists(aPath) then
      raise EEditorException.Create('File not exists: ' + aPath);

    aRunItem.Info.Run.AddParam('"'+aPath+'"');

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

constructor TGoTendency.Create;
begin
  inherited Create;
end;

procedure TGoTendency.CreateOptionsFrame(AOwner: TComponent; AddFrame: TAddFrameCallBack);
var
  aFrame: TRunFrameOptions;
  aTendencyFrame: TGoTendencyFrame;
begin
  aFrame := TRunFrameOptions.Create(AOwner);
  aFrame.Options := RunOptions;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);

  aTendencyFrame := TGoTendencyFrame.Create(AOwner);
  aTendencyFrame.Tendency := Self;
  aTendencyFrame.Caption := 'Go Options';
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

procedure TGoTendency.SendMessage(S: string; vMessageType: TNotifyMessageType);
var
  aMsg: TMessageInfo;
  p: Integer;
begin
  if (S <> '') and (vMessageType = msgtInteractive)
    and  (S[1] <> '#') then
  begin
    begin
      aMsg := Default(TMessageInfo);
      aMsg.Processed := True;
      aMsg.MessageType := vMessageType;
      p := PosForward(S, ':');
      if p > 0 then
      begin
        aMsg.FileName := ExpandToPath(Trim(MidStr(s, 1, p - 1)), Engine.Session.Run.CurrentDirectory);
        s := Trim(MidStr(S, p + 1, MaxInt));
        p := PosForward(s, ':');
        if p > 0 then
        begin
          aMsg.Line := StrToIntDef(MidStr(s, 1, p - 1), 0);
          s := MidStr(s, p + 1, MaxInt);
          p := PosForward(s, ':');
          if p > 0 then
          begin
            aMsg.Column := StrToIntDef(MidStr(s, 1, p - 1), 0);
          end;
        end;
        aMsg.Message := s;
      end;
    end;
    Engine.SendMessage(S, aMsg);
  end
  else
    inherited;
end;

function TGoTendency.CreateDebugger: TEditorDebugger;
begin
  Result := TGDBDebug.Create;
end;

function TGoTendency.CreateProjectOptions: TEditorProjectOptions;
begin
  Result := TGoProjectOptions.Create;
end;

procedure TGoTendency.Created;
begin
  FCapabilities := [capExecute, capDebug, capTrace, capCompile, capLink];
  FHaveOptions := True;
  FTitle := 'Go Lang';
  FDescription := 'Go Files, *.Go';
  FName := 'Go';
  {$ifdef windows}
  OutputExtension := '.exe';
  {$endif}
  FImageIndex := -1;
  AddGroup('cfg', 'cfg');
  AddGroup('ini', 'ini');
  AddGroup('txt', 'txt');
end;

{ TGoFileCategory }

function TGoFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynGoSyn.Create(nil);
end;

procedure TGoFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  inherited;
  FCompletion.EndOfTokenChr := '${}()[].<>/\:!&*+-=%;';
end;

procedure TGoFileCategory.DoAddKeywords;
begin
  EnumerateKeywords(Ord(attKeyword), sGoKeywords, Highlighter.IdentChars, @AddKeyword);
  EnumerateKeywords(Ord(attCommon), sGoFunctions, Highlighter.IdentChars, @AddKeyword);
end;

procedure TGoFileCategory.InitMappers;
begin
  with Highlighter as TSynGoSyn do
  begin
    Mapper.Add(WhitespaceAttri, attDefault);
    Mapper.Add(CommentAttri, attComment, ord(tkComment));
    Mapper.Add(KeywordAttri, attKeyword, ord(tkKeyword));
    Mapper.Add(DocumentAttri, attDocument, ord(tkDocument));
    Mapper.Add(TypeAttri, attDataType);
    Mapper.Add(FunctionAttri, attCommon, ord(tkFunction));
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
    Tendencies.Add(TGoTendency);
    Categories.Add(TGoFileCategory.Create(TGoTendency, 'Go', 'Go language',[fckPublish]));
    Groups.Add(TGoFile, 'Go', 'Go', TGoFileCategory, ['.go'], [fgkAssociated, fgkBrowsable, fgkMain], [capExecute, capDebug]);
  end;
end.
