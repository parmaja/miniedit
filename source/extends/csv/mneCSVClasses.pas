unit mneCSVClasses;
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
  mneCSVForms, SynHighlighterAny, SynHighlighterLFM;

type

  { TmneSynCSVSyn }

  TSynCSVSyn = class(TSynAnySyn)
  public
    function GetSampleSource: string; override;
    class function GetLanguageName: string; override;
  end;

  { TCSVFile }

  TCSVFile = class(TSourceEditorFile)
  private
    FContents: TCSVForm;
    function GetContents: TCSVForm;
  protected
    property Contents: TCSVForm read GetContents;
    function GetControl: TWinControl; override;
    function GetIsReadonly: Boolean; override;
    procedure DoLoad(FileName: string); override;
    procedure DoSave(FileName: string); override;
  public
    destructor Destroy; override;
  end;

  { TCSVFileCategory }

  TCSVFileCategory = class(TTextFileCategory)
  private
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
  public
  end;

  { TCSVPerspective }

  TCSVTendency = class(TEditorTendency)
  protected
    procedure Init; override;
  public
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils;

{ TCSVFile }

function TCSVFile.GetContents: TCSVForm;
begin
  if FContents = nil then
  begin
    FContents := TCSVForm.Create(nil);
    FContents.Parent := Engine.FilePanel;
    FContents.Align := alClient;
    FContents.OnChanged := @DoEdit;
  end;
  Result := FContents;
end;

function TCSVFile.GetControl: TWinControl;
begin
  Result := Contents;
end;

function TCSVFile.GetIsReadonly: Boolean;
begin
  Result := False;
end;

procedure TCSVFile.DoLoad(FileName: string);
begin
  Contents.Load(FileName);
end;

procedure TCSVFile.DoSave(FileName: string);
begin
  Contents.Save(FileName);
end;

destructor TCSVFile.Destroy;
begin
  FreeAndNil(FContents);
  inherited;
end;

{ TmneSynPASSyn }

function TSynCSVSyn.GetSampleSource: string;
begin
  Result := 'ID;Name;Company;Phone'#13#10 +
             '1;"John Smith";"No Where";0963157529'#13#10 +
             '2;"Jenna Smith";"No Where";0668157529'#13#10;
end;

class function TSynCSVSyn.GetLanguageName: string;
begin
  Result := 'CSV';
end;

{ TCSVTendency }

procedure TCSVTendency.Init;
begin
  FName := 'CSV';
  FTitle := 'CSV project';
  FDescription := 'Comma seperator values Files, *.csv';
  FImageIndex := -1;
end;

{ TCSVFileCategory }

function TCSVFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynCSVSyn.Create(nil);
end;

procedure TCSVFileCategory.InitMappers;
begin
  with Highlighter as TSynCSVSyn do
  begin
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(KeyAttri, attKeyword);
    Mapper.Add(ConstantAttri, attDataName);
    Mapper.Add(ObjectAttri, attIdentifier);
    Mapper.Add(EntityAttri, attQuotedString);
    Mapper.Add(DotAttri, attIdentifier);
    Mapper.Add(VariableAttri, attVariable);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(PreprocessorAttri, attDirective);
    Mapper.Add(SpaceAttri, attDefault);
    Mapper.Add(StringAttri, attDataType);
    Mapper.Add(SymbolAttri, attSymbol);
  end;
end;

initialization
  with Engine do
  begin
    Tendencies.Add(TCSVTendency);
    Categories.Add(TCSVFileCategory.Create(TCSVTendency, 'csv', 'CSV comma seperated files'));
    Groups.Add(TCSVFile, 'csv', 'CSV', TCSVFileCategory, ['csv'], [fgkBrowsable], []);
  end;
end.
