unit mneRunFrames;
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

  { TRunFrame }

  TRunFrame = class(TFrame, IEditorOptions, IEditorProjectFrame)
    Button3: TButton;
    CompilerEdit: TEdit;
    CompilerLabel: TLabel;
    Label7: TLabel;
    OpenDialog: TOpenDialog;
    PauseChk: TCheckBox;
    RunModeCbo: TComboBox;
    procedure Bevel1ChangeBounds(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
  protected
    function GetProject: TEditorProject;
  public
    FProject: TEditorProject;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TRunFrame }

function TRunFrame.GetProject: TEditorProject;
begin
  Result := FProject;
end;

procedure TRunFrame.Bevel1ChangeBounds(Sender: TObject);
begin
end;

procedure TRunFrame.Button3Click(Sender: TObject);
begin
  OpenDialog.Filter := 'EXE files|*.exe|All files|*.*';
  OpenDialog.FileName := CompilerEdit.Text;
  OpenDialog.InitialDir := ExtractFilePath(OpenDialog.FileName);
  if OpenDialog.Execute then
  begin
    CompilerEdit.Text := OpenDialog.FileName;
  end;
end;

procedure TRunFrame.Apply;
begin
  FProject.Options.RunMode := TmneRunMode(RunModeCbo.ItemIndex);
  FProject.Options.RunPause := PauseChk.Checked;
end;

procedure TRunFrame.Retrieve;
begin
  EnumRunMode(RunModeCbo.Items);
  RunModeCbo.ItemIndex := ord(FProject.Options.RunMode);
  PauseChk.Checked := FProject.Options.RunPause;
end;

end.
