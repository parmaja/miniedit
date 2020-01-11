unit mneVerilogClasses;
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

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
  Contnrs, LazFileUtils, LCLintf, LCLType, Dialogs, EditorOptions, SynEditHighlighter, LCLProc,
  SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  EditorDebugger, EditorRun, mneCompilerProjectFrames,
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
    procedure CreateInstantiation(AWires: boolean);
    procedure ImplementClick(Sender: TObject);
    procedure ImplementWiresClick(Sender: TObject);
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
    procedure DoAddKeywords; override;

  public
    procedure EnumMenuItems(AddItems: TAddClickCallBack); override;
  end;

  { TVerilogProjectOptions }

  TVerilogProjectOptions = class(TEditorProjectOptions)
  private
    FFileList: TStringList;
    function GetFileList: string;
    procedure SetFileList(const AValue: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack); override;
    procedure CreateProjectPanel(AOwner: TComponent; AProject: TEditorProject; var AFrame: TFrame); override;

    property Files: TStringList read FFileList;
  published
    property FileList: string read GetFileList write SetFileList;
  end;

  { TVerilogTendency }

  TVerilogTendency = class(TEditorTendency)
  private
    FCompiler: string;
  protected
    function CreateDebugger: TEditorDebugger; override;
    procedure Created; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    function CreateOptions: TEditorProjectOptions; override;
    procedure CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack); override;
    property Compiler: string read FCompiler write FCompiler;
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils, mneVerilogProjectPanel,
  verilogparser, busintf;

{ TVerilogProjectOptions }

function TVerilogProjectOptions.GetFileList: string;
begin
  result:=FFileList.Text;
end;

procedure TVerilogProjectOptions.SetFileList(const AValue: string);
begin
  FFileList.Text:=AValue;
end;

constructor TVerilogProjectOptions.Create;
begin
  inherited Create;
  FFileList:=TStringList.Create;
end;

destructor TVerilogProjectOptions.Destroy;
begin
  FFileList.Free;
  inherited Destroy;
end;

procedure TVerilogProjectOptions.CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack);
var
  aFrame: TFrame;
begin
  aFrame := TCompilerProjectFrame.Create(AOwner);
  (aFrame as TCompilerProjectFrame).Project := AProject;
  aFrame.Caption := 'Compiler';
  AddFrame(aFrame);
end;

procedure TVerilogProjectOptions.CreateProjectPanel(AOwner: TComponent; AProject: TEditorProject; var AFrame: TFrame);
begin
  aFrame := TVerilogProjectPanel.Create(AOwner);
  (aFrame as TVerilogProjectPanel).Project := AProject;
  aFrame.Caption := 'Files';
end;

{ TVerilogTendency }

function TVerilogTendency.CreateDebugger: TEditorDebugger;
begin
  Result := nil;
end;

function TVerilogTendency.CreateOptions: TEditorProjectOptions;
begin
  Result := TVerilogProjectOptions.Create;
end;

procedure TVerilogTendency.Created;
begin
  FCapabilities := [capRun, capCompile, capLink, capOptions];
  FName := 'Verilog';
  FTitle := 'Verilog project';
  FDescription := 'Verilog Files, *.v, *.vh';
  FImageIndex := -1;
end;

procedure TVerilogTendency.DoRun(Info: TmneRunInfo);
var
  i: Integer;
  aPath: string;
  aRunItem: TmneRunItem;
  p: string;
begin
  if rnaCompile in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;

    aRunItem.Info.Run.Command := Info.Command;
    if aRunItem.Info.Run.Command = '' then
      aRunItem.Info.Run.Command := 'iverilog'{$ifdef windows}+'.exe'{$endif};

    aRunItem.Info.Run.Silent := True;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;

    aRunItem.Info.Run.AddParam('-s' + Info.MainFile);
    if Info.OutputFile <> '' then
      aRunItem.Info.Run.AddParam('-o' + Info.OutputFile);

    aRunItem.Info.StatusMessage := 'Compiling ' + Info.OutputFile;

    p := '';
    for i := 0 to RunOptions.Paths.Count - 1 do
    begin
      aPath := Trim(RunOptions.Paths[i]);
      if aPath <>'' then
      begin
        if RunOptions.ExpandPaths then
          aPath := Engine.ExpandFile(aPath);
        if p <> '' then
          p := p + PathSeparator;
        p := p + aPath;
      end;
    end;
    if p <> '' then
      aRunItem.Info.Run.AddParam('-I ' + p);

    for i := 0 to Engine.Files.Count-1 do
      if Engine.Files[i].Extension='.v' then
        aRunItem.Info.Run.AddParam(Engine.Files[i].Name);
  end;

  if rnaExecute in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;
    aRunItem.Info.StatusMessage := 'Running ' + Info.OutputFile;
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.Run.Pause := Info.Pause;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.OutputFile);
    aRunItem.Info.Run.Command := Info.RunFile;
    aRunItem.Info.Run.AddParam(RunOptions.Params);
    aRunItem.Info.Run.AddParam(Engine.Session.Project.RunOptions.Params);
  end;

  Engine.Session.Run.Start(Self);
end;

procedure TVerilogTendency.CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack);
{var
  aFrame: TVerilogTendencyFrame;}
begin
  {aFrame := TVerilogTendencyFrame.Create(AOwner);
  aFrame.FTendency := ATendency;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);}
end;

type

  { TPoint_Helper }

  TPoint_Helper = record helper for TPoint
  public
    procedure SetLocation(const X, Y: Integer); overload;
    procedure SetLocation(const P: TPoint); overload;
    function Subtract(const Point: TPoint): TPoint;
  end;

{ TPoint_Helper }

procedure TPoint_Helper.SetLocation(const X, Y: Integer);
begin
  Self.X := X;
  Self.Y := Y;
end;

procedure TPoint_Helper.SetLocation(const P: TPoint);
begin
  Self := P;
end;

function TPoint_Helper.Subtract(const Point: TPoint): TPoint;
begin
  Result.SetLocation(Self.X - Point.X, Self.Y - Point.Y);
end;

{ TVerilogFileCategory }
                           
procedure TVerilogFileCategory.CreateInstantiation(AWires: boolean);
var
  CurrentFile: TTextEditorFile;
  ai, ns: Boolean;
  id, f, inst: String;
  vp: TVerilogParser;
  proj: TVerilogProjectOptions;
  i, sx, ex: Integer;
  st: TStringList;
  cr: TPoint;
begin
  if (Engine.Session.Active) and (Engine.Files.Current <> nil) and (Engine.Session.Project.Options is TVerilogProjectOptions) then
  begin
    CurrentFile := Engine.Files.Current as TTextEditorFile;
    proj:= Engine.Session.Project.Options as TVerilogProjectOptions;

    if Assigned(CurrentFile) and
       Assigned(CurrentFile.SynEdit) and
       Assigned(CurrentFile.SynEdit.Highlighter) then
    begin                             
      cr:=CurrentFile.SynEdit.CaretXY; //TODO: maybe LogicalCaretXY
      CurrentFile.SynEdit.CaretAtIdentOrString(cr, ai,ns);

      if (not ai) and
         (cr.x>0) and
         (length(CurrentFile.SynEdit.TextBetweenPoints[cr.Subtract(point(1,0)), cr])=1) and
         (CurrentFile.SynEdit.IsIdentChar(CurrentFile.SynEdit.TextBetweenPoints[cr.Subtract(point(1,0)), cr][1])) then
      begin
        cr:=cr.Subtract(point(1,0));
        CurrentFile.SynEdit.CaretAtIdentOrString(cr, ai,ns);
      end;

      if ai then
      begin
        id:=CurrentFile.SynEdit.GetWordAtRowCol(cr);

        vp:=TVerilogParser.Create;
        try
          for i:=0 to proj.Files.Count-1 do
          begin
            f:=ConcatPaths([Engine.GetRoot, proj.Files[i]]);

            if LowerCase(ExtractFileExt(f))='.v' then
            begin
              st:=TStringList.Create;
              try
                st.LoadFromFile(f);
                vp.Parse(st.Text);
              finally
                st.Free;
              end;
            end;
          end;

          for i:=0 to vp.ModuleCount-1 do
          begin
            if vp.Module[i].Name=id then
            begin
              CurrentFile.SynEdit.GetWordBoundsAtRowCol(CurrentFile.SynEdit.CaretXY, sx,ex);

              inst:=GenerateInstance(vp.Module[i], sx-1);
              if AWires then inst:=GenerateWires(vp.Module[i], sx-1)+LineEnding+LineEnding+inst;

              CurrentFile.SynEdit.SetTextBetweenPoints(Point(sx,CurrentFile.SynEdit.CaretY),Point(ex,CurrentFile.SynEdit.CaretY),TrimLeft(inst));

              break;
            end;
          end;
        finally
          vp.free;
        end;
      end;
    end;
  end;
end;

procedure TVerilogFileCategory.ImplementClick(Sender: TObject);
begin
  CreateInstantiation(False);
end;

procedure TVerilogFileCategory.ImplementWiresClick(Sender: TObject);
begin
  CreateInstantiation(true);
end;

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
    Mapper.Add(InbuiltFuncAttri, attStandard);
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
  //EnumerateKeywords(Ord(tkKeyword), sVerilogKeywords, Highlighter.IdentChars, @DoAddCompletion);
  //EnumerateKeywords(Ord(tkDirective), sVerilogDirectives, Highlighter.IdentChars, @DoAddCompletion);
end;

procedure TVerilogFileCategory.EnumMenuItems(AddItems: TAddClickCallBack);
begin
  inherited EnumMenuItems(AddItems);
  AddItems('Implement','Implement', @ImplementClick, TextToShortCut('Ctrl+W'));
  AddItems('ImplementWires','Implement+Wires', @ImplementWiresClick, TextToShortCut('Ctrl+Shift+W'));
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
    Tendencies.Add(TVerilogTendency);
    Categories.Add(TVerilogFileCategory.Create(TVerilogTendency, 'Verilog', 'Verilog'));
    Groups.Add(TVerilogFile, 'v', 'Verilog', TVerilogFileCategory, ['v', 'vh'], [fgkAssociated, fgkExecutable, fgkBrowsable, fgkMain], [fgsFolding]);
  end;
end.
