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
    function GetSampleSource: string; override;
  end;

  { TSARDFile }

  TSARDFile = class(TSourceEditorFile)
  protected
    procedure NewSource; override;
  public
  end;

  { TSARDFileCategory }

  TSARDFileCategory = class(TFileCategory)
  private
  protected
    function CreateHighlighter: TSynCustomHighlighter; override;
  public
  end;

  TSardEditorDebugger = class(TEditorDebugger)
  end;

  { TSARDPerspective }

  TSARDPerspective = class(TEditorPerspective)
  protected
    procedure Init; override;
    function CreateDebugger: TEditorDebugger; override;
  public
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils;

{ TmneSynSARDSyn }

function TmneSynSARDSyn.GetSampleSource: string;
begin
  Result := '/*'
            +'    This examples are worked, and this comment will ignored, not compiled or parsed as we say.'#13
            +'  */'#13
            +''#13
            +'  //Single Line comment'#13
            +'  CalcIt:Integer(p1, p2){'#13
            +'      :=p1 * p2 / 2;'#13
            +'    };'#13
            +''#13
            +'  x := {'#13
            +'        y := 0;'#13
            +'        x := CalcIt(x, y);'#13
            +'        := y + x+ 500 * %10; //this is a result return of the block'#13
            +'    }; //do not forget to add ; here'#13
            +''#13
            +'  f := 10.0;'#13
            +'  f := z + 5.5;'#13
            +''#13
            +'  {* Embeded block comment *};'#13
            +''#13
            +'  := "Result:" + x + '' It is an example:'#13
            +'    Multi Line String'#13
            +'  '';'#13;
end;

{ TSARDPerspective }

procedure TSARDPerspective.Init;
begin
  FName := 'SARD';
  FTitle := 'SARD project';
  FDescription := 'SARD Files, *.sard';
  FImageIndex := -1;
  AddGroup('sard', 'sard');
end;

function TSARDPerspective.CreateDebugger: TEditorDebugger;
begin
  //Result := TSardEditorDebugger.Create;
  Result := inherited CreateDebugger;
end;

{ TSARDFileCategory }

function TSARDFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TmneSynSARDSyn.Create(nil);
end;

{ TSARDFile }

procedure TSARDFile.NewSource;
begin
  inherited NewSource;
end;

initialization
  with Engine do
  begin
    Categories.Add(TSARDFileCategory, 'SARD');
    Groups.Add(TSARDFile, 'sard', 'SARD Files', 'sard', ['sard'], [fgkAssociated, fgkExecutable, fgkMember, fgkBrowsable], [fgsFolding]);

    Perspectives.Add(TSARDPerspective);
  end;
end.
