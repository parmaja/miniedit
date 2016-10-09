unit mneCompileProjectOptions;
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

  { TCompilerProjectOptionsForm }

  TCompilerProjectOptionsForm = class(TFrame, IEditorOptions, IEditorProjectFrame)
    Bevel1: TBevel;
    Button4: TButton;
    RunParamsEdit: TEdit;
    Label1: TLabel;
    Label4: TLabel;
    ConfigFileEdit: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    OutputFileEdit: TEdit;
    PauseChk: TCheckBox;
    Label3: TLabel;
    RunModeCbo: TComboBox;
    ExpandPathsChk: TCheckBox;
    ExpandPathsChk1: TCheckBox;
    ExpandPathsChk2: TCheckBox;
    PathsLbl: TLabel;
    MainEdit: TEdit;
    Label2: TLabel;
    PathsEdit: TSynEdit;
    procedure Bevel1ChangeBounds(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    Options: TCompilerProjectOptions;
  protected
    function GetProject: TEditorProject;
  public
    FProject: TEditorProject;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TCompilerProjectOptionsForm }

procedure TCompilerProjectOptionsForm.Button4Click(Sender: TObject);
var
  s: string;
begin
  ShowSelectFile(FProject.RootDir, s);
  MainEdit.Text := s;
end;

function TCompilerProjectOptionsForm.GetProject: TEditorProject;
begin
  Result := FProject;
end;

procedure TCompilerProjectOptionsForm.Bevel1ChangeBounds(Sender: TObject);
begin

end;

procedure TCompilerProjectOptionsForm.Apply;
begin
  Options.RunMode := TmneRunMode(RunModeCbo.ItemIndex);
  Options.PauseConsole := PauseChk.Checked;
  Options.MainFile := MainEdit.Text;
  Options.OutputFile := OutputFileEdit.Text;
  Options.RunParams := RunParamsEdit.Text;
  Options.ConfigFile := ConfigFileEdit.Text;
  Options.ExpandPaths := ExpandPathsChk.Checked;
  Options.Paths.Assign(PathsEdit.Lines);
end;

procedure TCompilerProjectOptionsForm.Retrieve;
begin
  Options := (FProject.Options as TCompilerProjectOptions);

  EnumRunMode(RunModeCbo.Items);
  RunModeCbo.ItemIndex := ord(Options.RunMode);

  PauseChk.Checked := Options.PauseConsole;
  MainEdit.Text := Options.MainFile;
  OutputFileEdit.Text := Options.OutputFile;
  RunParamsEdit.Text := Options.RunParams;
  ConfigFileEdit.Text := Options.ConfigFile;
  ExpandPathsChk.Checked := Options.ExpandPaths;
  PathsEdit.Lines.Assign(Options.Paths);
end;

end.

