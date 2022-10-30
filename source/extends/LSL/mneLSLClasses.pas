unit mneLSLClasses;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey
 *
 * https://github.com/Makopo/lslint/issues/102
 * https://github.com/Sei-Lisa/kwdb
 *}

interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics, Contnrs, Dialogs,
  LCLintf, LCLType, LazFileUtils, mnSynHighlighterBVH,
  EditorOptions, EditorRun, EditorClasses, mneRunFrames,
  SynEditHighlighter, SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  mnSynHighlighterMultiProc, mnSynHighlighterLSL;

const
  sl_script_: string = 'sl_script_';

type

  { TmneSynLSLSyn }

  TmneSynLSLSyn = class(TSynLSLSyn)
  public
  end;

  { TLSLFile }

  TLSLFile = class(TSourceEditorFile)
  protected
    procedure NewContent; override;
  public
    function CanAddRecentFiles: Boolean; override;
  end;

  { TLSLFileCategory }

  TLSLFileCategory = class(TCodeFileCategory)
  private
    procedure FormatCodeClick(Sender: TObject);
    procedure MangleCodeClick(Sender: TObject);
  protected
    function GetFileCaption(AFile: TEditorFile; FileName: string): string; override;
    procedure GetHintString(Token: string; ParamIndex: Integer; out AHint: String); override;
    procedure InitMappers; override;
    procedure DoAddKeywords; override;
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
  public
    procedure EnumMenuItems(AddItems: TAddClickCallBack); override;
    function CreateHighlighter: TSynCustomHighlighter; override;
  end;

  { TBVHFile }

  TBVHFile = class(TSourceEditorFile)
  protected
    procedure NewContent; override;
  public
  end;

  { TBVHFileCategory }

  TBVHFileCategory = class(TTextFileCategory)
  private
  protected
    procedure InitMappers; override;
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
    procedure DoPrepareCompletion(AEditor: TCustomSynEdit); override;
  public
    function CreateHighlighter: TSynCustomHighlighter; override;
  end;

  TLSLEditorDebugger = class(TEditorDebugger)
  end;

  { TLSLTendency }

  TLSLTendency = class(TEditorTendency)
  protected
    procedure Created; override;
    function CreateDebugger: TEditorDebugger; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    procedure SendMessage(S: string; vMessageType: TNotifyMessageType); override;
    procedure HelpKeyword(AWord:string); override;
    procedure CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack); override;
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils;

{ TBVHFileCategory }

function TBVHFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynBVHSyn.Create(nil);
end;

procedure TBVHFileCategory.InitMappers;
begin
  with Highlighter as TSynBVHSyn do
  begin
    Mapper.Add(WhitespaceAttri, attDefault);
    Mapper.Add(CommentAttri, attComment, ord(tkComment));
    Mapper.Add(ProcessorAttri, attDirective);
    Mapper.Add(KeywordAttri, attKeyword);
    Mapper.Add(DocumentAttri, attDocument);
    Mapper.Add(IdentifierAttri, attIdentifier, ord(tkIdentifier) );
    Mapper.Add(VariableAttri, attVariable);
    Mapper.Add(TypeAttri, attDataType);
    Mapper.Add(ValueAttri, attDataValue);
    Mapper.Add(FunctionAttri, attCommon);
    Mapper.Add(TextAttri, attText);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
  end
end;

procedure TBVHFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  inherited;
  Completion.EndOfTokenChr := '{}()[].<>/\:!&*+-=%;,';//what about $?
end;

procedure TBVHFileCategory.DoPrepareCompletion(AEditor: TCustomSynEdit);
begin
  inherited;
  Screen.Cursor := crHourGlass;
  Completion.BeginUpdate;
  try
    //Completion.ItemList.Clear;
    EnumerateKeywords(Ord(attKeyword), sBVHKeywords, Highlighter.IdentChars, @AddKeyword);
    EnumerateKeywords(Ord(attDataType), sBVHTypes, Highlighter.IdentChars, @AddKeyword);
  finally
    Completion.EndUpdate;
    Screen.Cursor := crDefault;
  end;
end;

{ TBVHFile }

procedure TBVHFile.NewContent;
begin
  inherited NewContent;
end;

{ TLSLTendency }

procedure TLSLTendency.Created;
begin
  FCapabilities := [{capExecute, } capLint];
  FHaveOptions := True;
  FName := 'LSL';
  FTitle := 'LSL/OSSL project';
  FDescription := 'LSL/OSSL Files, *.lsl;*.ossl';
  FImageIndex := -1;
end;

function TLSLTendency.CreateDebugger: TEditorDebugger;
begin
  //Result := TLSLEditorDebugger.Create;
  Result := inherited CreateDebugger;
end;

procedure TLSLTendency.CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack);
var
  aFrame: TRunFrameOptions;
begin
  aFrame := TRunFrameOptions.Create(AOwner);
  aFrame.Options := ATendency.RunOptions;
  EnumRunCommands(aFrame.CommandEdit.Items);
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

procedure TLSLTendency.DoRun(Info: TmneRunInfo);
var
  aRunItem: TmneRunItem;
begin
  Engine.Session.Run.Clear;

  if rnaExecute in Info.Actions then //not yet
  begin
    Engine.SendAction(eaClearOutput);

    aRunItem := Engine.Session.Run.Add;
    aRunItem.MessageType := msgtInteractive;
    aRunItem.Info.Run.Pause := Info.Pause;
    aRunItem.Info.Run.Console := Info.Console;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.StatusMessage := 'Runing ' + Info.MainFile;
    {
    if RunOptions.Require <> '' then
      RunItem.Info.Run.AddParam('-l '+ RunOptions.Require);
    if rnaDebug in Info.Actions then
      aRunItem.Info.Run.AddParam('-e '+ '"require(''mobdebug'').start()"'); //using mobdebug
    }
    aRunItem.Info.Run.Command := Info.Command;
    if Info.Command = '' then
    begin
      {$ifdef windows}
        aRunItem.Info.Run.Command := 'LSL.exe';
      {$else}
        aRunItem.Info.Run.Command := 'LSL';
      {$endif}
    end;
    aRunItem.Info.Run.AddParam(' "' + Info.MainFile + '"');
  end
  else if (rnaLint in Info.Actions) then
  begin
    Engine.SendAction(eaClearOutput);

    aRunItem := Engine.Session.Run.Add;
    aRunItem.MessageType := msgtInteractive;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.Run.Silent := True;
    aRunItem.Info.Run.Console := False;

    aRunItem.Info.StatusMessage := 'Linting ' + Info.MainFile;
    aRunItem.Info.Run.AddParam('-p ');
    //aRunItem.Info.Run.AddParam('-l ');

    {$ifdef windows}
      aRunItem.Info.Run.Command := 'lslint.exe';
    {$else}
      aRunItem.Info.Run.Command := 'lslint';
    {$endif}
    aRunItem.Info.Run.AddParam(' -b '+Application.Location+'lsl.keywords "' + Info.MainFile + '"');
  end;

  if Engine.Session.Run.Active then //if there is items ready to run
    Engine.Session.Run.Start(Debugger);
end;

function PosForward(S: string; vChar: string): Integer;
begin
  Result := PosEx(vChar, S);
end;

procedure TLSLTendency.SendMessage(S: string; vMessageType: TNotifyMessageType);
var
  aMsg: TMessageInfo;
  p: Integer;
  m, t : string;
  list: TStringList;
  function GetStr(Index: Integer): string;
  begin
    if Index >= list.Count then
      Result := ''
    else
      Result := List[Index];
  end;

//D:\lab\pascal\miniEdit\test\teleport.lsl::ERROR:: ( 21, 95): `OS_LTPAG_FORCEFLY' is undeclared.
//TOTAL:: Errors: 4  Warnings: 0

begin
  if (S <> '') and (vMessageType = msgtInteractive) then
  begin
    aMsg := Default(TMessageInfo);
    list := TStringList.Create;
    try
      StrToStringsEx(Trim(S), List, ['::']);
      t := GetStr(2);
      if t <> '' then
      begin
        aMsg.Processed := True;
        aMsg.MessageType := vMessageType;
        aMsg.FileName := GetStr(0);
        aMsg.Name := GetStr(1);
        aMsg.Message := Trim(SubStr(t, ':', 1));
        t := Trim(SubStr(t, ':', 0));

        if LeftStr(t, 1) = '(' then
          t := Trim(MidStr(t, 2, Length(t) - 1));
        if RightStr(t, 1) = ')' then
          t := Trim(MidStr(t, 1, Length(t) - 1));

        p := Pos(',', t);
        if p > 0 then
        begin
          m := Trim(MidStr(t, 1, p - 1));
          t := Trim(MidStr(t, p + 1, MaxInt));
        end
        else
          m := t;
        aMsg.Line := StrToIntDef(m, 0);
        aMsg.Column := StrToIntDef(t, 0);
        Engine.SendMessage(S, aMsg);
      end
      else
        inherited;
    finally
      list.Free;
    end;
  end
  else
    inherited;
end;

procedure TLSLTendency.HelpKeyword(AWord: string);
begin
  inherited;
  if SameText(LeftStr(AWord, 2), 'os') then
    OpenURL('http://opensimulator.org/wiki/' + AWord)
  else
    OpenURL('http://wiki.secondlife.com/wiki/' + AWord);
end;

{ TLSLFileCategory }

procedure TLSLFileCategory.FormatCodeClick(Sender: TObject);
begin

end;

procedure TLSLFileCategory.MangleCodeClick(Sender: TObject);
begin

end;

function TLSLFileCategory.GetFileCaption(AFile: TEditorFile; FileName: string): string;
var
  p: Integer;
  ext: string;
begin
  if LeftStr(FileName, Length(sl_script_)) = sl_script_ then
  begin
    ext := ExtractFileExt(FileName);
    Result := MidStr(FileName, Length(sl_script_) + 1, MaxInt);
    p := ReversePos('_', Result);
    if p>0 then
    begin
      Result := MidStr(Result, 1, p + 4); //* we take 4 char after _
      Result := ChangeFileExt(Result, ext);
    end;
  end
  else
    Result := FileName;
end;

procedure TLSLFileCategory.GetHintString(Token: string; ParamIndex: Integer; out AHint: String);
var
  KeywordItem: TKeywordItem;
  i: Integer;
begin
  KeywordItem := Keywords.Find(Token);
  if KeywordItem <> nil then
  begin
    AHint := KeywordItem.AttributeName + ' ' + KeywordItem.Name;
    if KeywordItem.Params.Count > 0 then
    begin
      for i := 0 to KeywordItem.Params.Count -1 do
      begin
        if i = 0 then
          AHint := AHint + '('
        else
          AHint := AHint + ', ';
        if i = ParamIndex then
          AHint := AHint + '\style{+B}' + KeywordItem.Params[i] + '\style{-B}'
        else
          AHint := AHint + KeywordItem.Params[i];
      end;
      AHint := AHint + ')';
    end;
  end
  else
    AHint := '';
end;

function TLSLFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TmneSynLSLSyn.Create(nil);
end;

procedure TLSLFileCategory.InitMappers;
begin
  with Highlighter as TSynLSLSyn do
  begin
    Mapper.Add(WhitespaceAttri, attDefault);
    Mapper.Add(CommentAttri, attComment, ord(tkComment));
    Mapper.Add(ProcessorAttri, attDirective);
    Mapper.Add(KeywordAttri, attKeyword);
    Mapper.Add(DocumentAttri, attDocument, ord(tkDocument));
    Mapper.Add(IdentifierAttri, attIdentifier, ord(tkIdentifier));
    Mapper.Add(VariableAttri, attVariable, ord(tkVariable));
    Mapper.Add(TypeAttri, attDataType);
    Mapper.Add(ValueAttri, attDataValue);
    Mapper.Add(FunctionAttri, attCommon);
    Mapper.Add(TextAttri, attText);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
  end
end;

procedure TLSLFileCategory.DoAddKeywords;
var
  stream: TmnWrapperStream;
  line: utf8string;
  declare, subdeclare, token: string;
  CharIndex, NextIndex: Integer;
  att: TAttributeType;
begin
  inherited;

  EnumerateKeywords(Ord(attKeyword), sLSLKeywords, Highlighter.IdentChars, @AddKeyword);
  EnumerateKeywords(Ord(attDataType), sLSLTypes, Highlighter.IdentChars, @AddKeyword);

  if FileExistsUTF8(Application.Location + 'lsl.keywords') then
  begin
    stream := TmnWrapperStream.Create(TFileStream.Create(Application.Location + 'lsl.keywords', fmOpenRead), True);
    try
      while stream.ReadLine(line) do
      begin
        line := trim(line);
        if LeftStr(line, 2) <> '//' then
        begin
          if StrScanTo(line, 1, declare, CharIndex, NextIndex, [' ', '(', ')', ',', ';']) then
          begin
            if SameText(declare, 'const') then
              StrScanTo(line, NextIndex, subdeclare, CharIndex, NextIndex, [' ', '(', ')', ',', ';']);
            StrScanTo(line, NextIndex, token, CharIndex, NextIndex, [' ', '(', ')', ',', ';']);
            if SameText(declare, 'const') then
              att := attDataValue
            else
              att := attCommon;
            end;
            with AddKeyword(token, declare, att, False) do
            begin
              if StrScanTo(line, 0, token, CharIndex, NextIndex, ['(']) then
              begin
                line := MidStr(line, NextIndex, MaxInt);
                if RightStr(line, 1) = ')' then
                  line := MidStr(line, 1, Length(line) - 1);
                StrToStrings(Trim(line), Params, [','], [' ']);
              end;
            end;
          end;
        end;
    finally
      stream.Free;
    end;
  end
  else
  begin
    EnumerateKeywords(Ord(attDataValue), sLSLValues, Highlighter.IdentChars, @AddKeyword);
    EnumerateKeywords(Ord(attCommon), sLSLFunctions, Highlighter.IdentChars, @AddKeyword);
    EnumerateKeywords(Ord(attCommon), sOpenSIMFunctions, Highlighter.IdentChars, @AddKeyword);
  end;
end;

procedure TLSLFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  inherited;
  Completion.EndOfTokenChr := '{}()[].<>/\:!&*+-=%;,';//what about $
end;

procedure TLSLFileCategory.EnumMenuItems(AddItems: TAddClickCallBack);
begin
  inherited;
//  AddItems('FormatCode', 'Format Code', 'Edit', @FormatCodeClick, scCtrl + scAlt + VK_F);
  AddItems('MangleCode', 'Mangle Code', 'Edit', @MangleCodeClick);
end;

{ TLSLFile }

procedure TLSLFile.NewContent;
begin
  inherited NewContent;
end;

function TLSLFile.CanAddRecentFiles: Boolean;
begin
  Result := not SameText(LeftStr(BaseName, Length(sl_script_)), sl_script_);
end;

initialization
  with Engine do
  begin
    Tendencies.Add(TLSLTendency);
    Categories.Add(TLSLFileCategory.Create(TLSLTendency, 'LSL', 'SecondLife/OpenSIM Script'));
    Groups.Add(TLSLFile, 'LSL', 'OpenSIM Script', TLSLFileCategory, ['.lsl', '.ossl'], [fgkAssociated, fgkFolding, fgkBrowsable], [capLint]);
    Categories.Add(TBVHFileCategory.Create(TLSLTendency, 'BVH', 'SecondLife/OpenSIM Animation'));
    Groups.Add(TBVHFile, 'BVH', 'Biovision Animation', TBVHFileCategory, ['.bvh'], [fgkAssociated, fgkBrowsable], []);
  end;
end.
