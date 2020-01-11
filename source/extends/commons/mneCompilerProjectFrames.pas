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
    PathsEdit: TSynEdit;
    procedure Bevel1ChangeBounds(Sender: TObject);
  private
  protected
    function GetProject: TEditorProject;
  public
    Project: TEditorProject;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TCompilerProjectFrame }

function TCompilerProjectFrame.GetProject: TEditorProject;
begin
  Result := Project;
end;

procedure TCompilerProjectFrame.Bevel1ChangeBounds(Sender: TObject);
begin

end;

procedure TCompilerProjectFrame.Apply;
begin
  Project.RunOptions.OutputFile := OutputFileEdit.Text;
  Project.RunOptions.Params := RunParamsEdit.Text;
  Project.RunOptions.ConfigFile := ConfigFileEdit.Text;
  Project.RunOptions.ExpandPaths := ExpandPathsChk.Checked;
  Project.RunOptions.Paths.Assign(PathsEdit.Lines);
end;

procedure TCompilerProjectFrame.Retrieve;
begin
  OutputFileEdit.Text := Project.RunOptions.OutputFile;
  RunParamsEdit.Text := Project.RunOptions.Params;
  ConfigFileEdit.Text := Project.RunOptions.ConfigFile;
  ExpandPathsChk.Checked := Project.RunOptions.ExpandPaths;
  PathsEdit.Lines.Assign(Project.RunOptions.Paths);
end;

end.
