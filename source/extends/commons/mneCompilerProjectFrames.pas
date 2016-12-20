unit mneCompilerProjectFrames;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditorEngine, SelectFiles, EditorDebugger;

type

  { TCompilerProjectFrame }

  TCompilerProjectFrame = class(TFrame, IEditorOptions, IEditorProjectFrame)
    Bevel1: TBevel;
    RunParamsEdit: TEdit;
    Label4: TLabel;
    ConfigFileEdit: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    OutputFileEdit: TEdit;
    ExpandPathsChk: TCheckBox;
    ExpandPathsChk1: TCheckBox;
    ExpandPathsChk2: TCheckBox;
    PathsLbl: TLabel;
    PathsEdit: TSynEdit;
    procedure Bevel1ChangeBounds(Sender: TObject);
  private
    Options: TEditorProjectOptions;
  protected
    function GetProject: TEditorProject;
  public
    FProject: TEditorProject;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TCompilerProjectFrame }

function TCompilerProjectFrame.GetProject: TEditorProject;
begin
  Result := FProject;
end;

procedure TCompilerProjectFrame.Bevel1ChangeBounds(Sender: TObject);
begin

end;

procedure TCompilerProjectFrame.Apply;
begin
  Options.OutputFile := OutputFileEdit.Text;
  Options.Params := RunParamsEdit.Text;
  Options.ConfigFile := ConfigFileEdit.Text;
  Options.ExpandPaths := ExpandPathsChk.Checked;
  Options.Paths.Assign(PathsEdit.Lines);
end;

procedure TCompilerProjectFrame.Retrieve;
begin
  Options := (FProject.Options as TEditorProjectOptions);
  OutputFileEdit.Text := Options.OutputFile;
  RunParamsEdit.Text := Options.Params;
  ConfigFileEdit.Text := Options.ConfigFile;
  ExpandPathsChk.Checked := Options.ExpandPaths;
  PathsEdit.Lines.Assign(Options.Paths);
end;

end.
