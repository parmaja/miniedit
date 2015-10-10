unit mneSARDClasses;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics, Contnrs, Dialogs,
  LCLintf, LCLType,
  EditorOptions, EditorDebugger,
  SynEditHighlighter, SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  SynHighlighterSARD, SynHighlighterLFM;

type

  { TmneSynSARDSyn }

  TmneSynSARDSyn = class(TSynSARDSyn)
  public
  end;

  { TSARDFile }

  TSARDFile = class(TSourceEditorFile)
  protected
    procedure NewContent; override;
  public
  end;

  { TSARDFileCategory }

  TSARDFileCategory = class(TTextFileCategory)
  private
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
  public
  end;

  TSardEditorDebugger = class(TEditorDebugger)
  end;

  { TSARDPerspective }

  TSARDTendency = class(TEditorTendency)
  protected
    procedure Init; override;
    function CreateDebugger: TEditorDebugger; override;
  public
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils;

{ TSARDTendency }

procedure TSARDTendency.Init;
begin
  FCapabilities := [capRun, capProjectOptions, capOptions];
  FName := 'SARD';
  FTitle := 'SARD project';
  FDescription := 'SARD Files, *.sard';
  FImageIndex := -1;
  AddGroup('sard', 'sard');
end;

function TSARDTendency.CreateDebugger: TEditorDebugger;
begin
  //Result := TSardEditorDebugger.Create;
  Result := inherited CreateDebugger;
end;

{ TSARDFileCategory }

function TSARDFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TmneSynSARDSyn.Create(nil);
end;

procedure TSARDFileCategory.InitMappers;
begin
  with Highlighter as TSynSARDSyn do
  begin
    Mapper.Add(SpaceAttri, attWhitespace);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(ObjectAttri, attName);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attString);
    Mapper.Add(SymbolAttri, attSymbol);
  end
end;

{ TSARDFile }

procedure TSARDFile.NewContent;
begin
  inherited NewContent;
end;

initialization
  with Engine do
  begin
    Categories.Add(TSARDFileCategory.Create('Sard'));
    Groups.Add(TSARDFile, 'sard', 'SARD Files', 'sard', ['sard'], [fgkAssociated, fgkExecutable, fgkMember, fgkBrowsable], [fgsFolding]);

    Tendencies.Add(TSARDTendency);
  end;
end.
