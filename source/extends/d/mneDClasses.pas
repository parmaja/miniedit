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
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics, Contnrs,
  LCLintf, LCLType,
  Dialogs, EditorOptions, SynEditHighlighter, SynEditSearch, SynEdit,
  Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  SynHighlighterD,
  EditorDebugger, EditorClasses,
  mneClasses;

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

  TDFileCategory = class(TTextFileCategory)
  private
    //procedure ExtractKeywords(Files, Variables, Identifiers: TStringList);
  protected
    procedure InitMappers; override;
    function DoCreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  TDRunMode = (prunConsole, prunUrl);

  { TDProjectOptions }

  TDProjectOptions = class(TEditorProjectOptions)
  private
    FRootUrl: string;
    FRunMode: TDRunMode;
  public
    constructor Create; override;
    function Show: Boolean; override;
  published
    property RunMode: TDRunMode read FRunMode write FRunMode;
    property RootUrl: string read FRootUrl write FRootUrl;
  end;

  { TDTendency }

  TDTendency = class(TEditorTendency)
  private
    FHTMLHelpFile: string;
    FDHelpFile: string;
    FDPath: string;
  protected
    function CreateDebugger: TEditorDebugger; override;
    function CreateOptions: TEditorProjectOptions; override;
    procedure Init; override;
  public
    procedure Run; override;
    constructor Create; override;
    procedure Show; override;
  published
    property DPath: string read FDPath write FDPath;
    property DHelpFile: string read FDHelpFile write FDHelpFile;
    property HTMLHelpFile: string read FHTMLHelpFile write FHTMLHelpFile;
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils, HTMLProcessor, SynEditStrConst{, mneDConfigForms};

{ TDProject }

constructor TDProjectOptions.Create;
begin
  inherited;
end;

function TDProjectOptions.Show: Boolean;
begin
  Result :=inherited Show;
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
    aToken := Engine.ExpandFileName(aToken);
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

procedure TDTendency.Run;
var
  aFile: string;
  aRoot: string;
  aUrlMode: TDRunMode;
begin
  //if (Engine.Files.Current <> nil) and (fgkExecutable in Engine.Files.Current.Group.Kind) then

  if Engine.Tendency.Debug <> nil then
  if Engine.Tendency.Debug.Running then
    Engine.Tendency.Debug.Action(dbaRun)
  else
  begin
    if (Engine.Session.IsOpened) then
    begin
      aFile := Engine.Session.Project.RootDir;
      aFile := ExpandToPath(aFile, Engine.Session.Project.RootDir);
      aUrlMode := (Engine.Session.Project.Options as TDProjectOptions).RunMode;
      aRoot := IncludeTrailingPathDelimiter(Engine.Session.Project.RootDir);
    end
    else
    begin
      //Check the file is executable
      if (Engine.Files.Current <> nil) and (fgkExecutable in Engine.Files.Current.Group.Kind) then
      begin
        aFile := Engine.Files.Current.Name;
        aUrlMode := prunConsole;
      end
    end;

    if aFile <> '' then
    begin
      case aUrlMode of
        prunUrl:
        begin
          if Engine.Session.IsOpened then
          begin
            if SameText((MidStr(aFile, 1, Length(aRoot))), aRoot) then
            begin
              aFile := MidStr(aFile, Length(aRoot) + 1, MaxInt);
              aFile := IncludeSlash((Engine.Session.Project.Options as TDProjectOptions).RootUrl) + aFile;
              //ShellExecute(0, 'open', PChar(aFile), '', PChar(ExtractFilePath(aFile)), SW_SHOWNOACTIVATE);
            end;
          end;
        end;
        prunConsole:
        begin
          {$ifdef windows}
          ExecuteProcess('cmd ',['/c "D.exe "' + aFile + '" & pause'], []);
          {$endif}

          {$ifdef linux}
          {$endif}

          {$ifdef macos}
          {$endif}
        end;
      end;
    end;
  end;
end;

constructor TDTendency.Create;
begin
  inherited Create;
end;

procedure TDTendency.Show;
begin
  {with TDConfigForm.Create(Application) do
  begin
    FTendency := Self;
    Retrive;
    if ShowModal = mrOK then
    begin
      Apply;
    end;
  end;}
end;

function TDTendency.CreateDebugger: TEditorDebugger;
begin
  Result := nil;
end;

function TDTendency.CreateOptions: TEditorProjectOptions;
begin
  Result := TDProjectOptions.Create;
end;

procedure TDTendency.Init;
begin
  FTitle := 'D project';
  FDescription := 'D Files, *.D, *.inc';
  FName := 'D';
  FImageIndex := -1;
  AddGroup('D', 'html');
  AddGroup('html', 'html');
  AddGroup('css', 'css');
  AddGroup('js', 'js');
end;

{ TDFileCategory }

function TDFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynDSyn.Create(nil);
end;

{procedure TDFileCategory.DoAddCompletion(AKeyword: string; AKind: integer);
begin
  Completion.ItemList.Add(AKeyword);
end;}

procedure TDFileCategory.InitMappers;
begin
  with Highlighter as TSynDSyn do
  begin
    Mapper.Add(WhitespaceAttri, attWhitespace);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(KeywordAttri, attKeyword);
    Mapper.Add(DocumentAttri, attDocument);
    Mapper.Add(ValueAttri, attValue);
    Mapper.Add(FunctionAttri, attStandard);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(HtmlAttri, attOutter);
    Mapper.Add(TextAttri, attText);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attString);
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(VariableAttri, attVariable);
    Mapper.Add(ProcessorAttri, attDirective);
  end;
end;

initialization
  with Engine do
  begin
    Categories.Add(TDFileCategory.Create('D', [fckPublish]));

    Groups.Add(TDFile, 'D', 'D Files', 'D', ['D', 'inc'], [fgkAssociated, fgkExecutable, fgkMember, fgkBrowsable, fgkMain]);

    Tendencies.Add(TDTendency);
  end;
end.
