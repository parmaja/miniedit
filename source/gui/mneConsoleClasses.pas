unit mneConsoleClasses;
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
  mneConsoleForms, SynHighlighterAny, SynHighlighterLFM;

type

  { TmneSynConsoleSyn }

  TSynConsoleSyn = class(TSynAnySyn)
  public
    function GetSampleSource: string; override;
    class function GetLanguageName: string; override;
  end;

  { TConsoleFile }

  TConsoleFile = class(TSourceEditorFile)
  private
    FContents: TConsoleForm;
    function GetContents: TConsoleForm;
  protected
    property Contents: TConsoleForm read GetContents;
    function GetControl: TWinControl; override;
    function GetIsReadonly: Boolean; override;
    procedure DoLoad(FileName: string); override;
    procedure DoSave(FileName: string); override;
  public
    destructor Destroy; override;
  end;

  { TConsoleFileCategory }

  TConsoleFileCategory = class(TTextFileCategory)
  private
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
  public
  end;

  { TConsolePerspective }

  TConsoleTendency = class(TEditorTendency)
  protected
    procedure Init; override;
  public
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils;

{ TConsoleFile }

function TConsoleFile.GetContents: TConsoleForm;
begin
  if FContents = nil then
  begin
    FContents := TConsoleForm.Create(nil);
    FContents.Parent := Engine.Container;
    FContents.Align := alClient;
    FContents.OnChanged := @DoEdit;
  end;
  Result := FContents;
end;

function TConsoleFile.GetControl: TWinControl;
begin
  Result := Contents;
end;

function TConsoleFile.GetIsReadonly: Boolean;
begin
  Result := True;
end;

procedure TConsoleFile.DoLoad(FileName: string);
begin
end;

procedure TConsoleFile.DoSave(FileName: string);
begin
end;

destructor TConsoleFile.Destroy;
begin
  FreeAndNil(FContents);
  inherited;
end;

{ TmneSynPASSyn }

function TSynConsoleSyn.GetSampleSource: string;
begin
  Result := 'echo %temp%'#13#10;
end;

class function TSynConsoleSyn.GetLanguageName: string;
begin
  Result := 'Console';
end;

{ TConsoleTendency }

procedure TConsoleTendency.Init;
begin
  FName := 'Console';
  FTitle := 'Console project';
  FDescription := 'Comma seperator values Files, *.Console';
  FImageIndex := -1;
  AddGroup('Console', 'Console');
end;

{ TConsoleFileCategory }

function TConsoleFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynConsoleSyn.Create(nil);
end;

procedure TConsoleFileCategory.InitMappers;
begin
  with Highlighter as TSynConsoleSyn do
  begin
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(KeyAttri, attKeyword);
    Mapper.Add(ConstantAttri, attName);
    Mapper.Add(ObjectAttri, attIdentifier);
    Mapper.Add(EntityAttri, attValue);
    Mapper.Add(DotAttri, attIdentifier);
    Mapper.Add(VariableAttri, attVariable);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(PreprocessorAttri, attDirective);
    Mapper.Add(SpaceAttri, attWhitespace);
    Mapper.Add(StringAttri, attString);
    Mapper.Add(SymbolAttri, attSymbol);
  end;
end;

initialization
  with Engine do
  begin
    Categories.Add(TConsoleFileCategory.Create('Console'));
    Groups.Add(TConsoleFile, 'Console', 'Console Files', 'Console', ['Console'], [fgkVirtual, fgkText], []);
    Tendencies.Add(TConsoleTendency);
  end;
end.
