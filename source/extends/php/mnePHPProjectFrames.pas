unit mnePHPProjectFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditorEngine, SelectFiles, DebugClasses;

type

  { TPHPProjectFrame }

  TPHPProjectFrame = class(TFrame, IEditorOptions)
    Button4: TButton;
    Label4: TLabel;
    MainEdit: TEdit;
    Label2: TLabel;
    PauseChk: TCheckBox;
    RunModeCbo: TComboBox;
    procedure Button4Click(Sender: TObject);
  private
  protected
  public
    Project: TEditorProject;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

uses
  mnePHPClasses;

{ TPHPProjectFrame }

procedure TPHPProjectFrame.Button4Click(Sender: TObject);
var
  s: string;
begin
  ShowSelectFile(Project.RootDir, s);
  MainEdit.Text := s;
end;

procedure TPHPProjectFrame.Apply;
begin
  with (Project.Options as TPHPProjectOptions) do
  begin
    RunMode := TmneRunMode(RunModeCbo.ItemIndex);
    PauseConsole := PauseChk.Checked;
    MainFile := MainEdit.Text;
  end;
end;

procedure TPHPProjectFrame.Retrieve;
begin
  RunModeCbo.Items.Add('Console');
  RunModeCbo.Items.Add('Process');
  RunModeCbo.Items.Add('Embedded');
  RunModeCbo.Items.Add('Output');
  RunModeCbo.Items.Add('Browser');

  with (Project.Options as TPHPProjectOptions) do
  begin
    RunModeCbo.ItemIndex := ord(RunMode);
    PauseChk.Checked := PauseConsole;
    MainEdit.Text := MainFile;
  end;
end;

end.

