unit mnePasProjectFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, EditorEngine, SelectFiles, mnePasClasses;

type

  { TPasProjectFrame }

  TPasProjectFrame = class(TFrame, IEditorOptions)
    Button4: TButton;
    CancelBtn: TButton;
    ExpandPathsChk: TCheckBox;
    Label3: TLabel;
    ExeEdit: TEdit;
    Label4: TLabel;
    PauseChk: TCheckBox;
    RunModeCbo: TComboBox;
    UseCFGFileChk: TCheckBox;
    PathsLbl: TLabel;
    MainEdit: TEdit;
    Label2: TLabel;
    OkBtn: TButton;
    PathsEdit: TSynEdit;
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

{ TPasProjectFrame }

procedure TPasProjectFrame.Button4Click(Sender: TObject);
var
  s: string;
begin
  ShowSelectFile(Project.RootDir, s);
  MainEdit.Text := s;
end;

procedure TPasProjectFrame.Apply;
begin
  with (Project.Options as TPasProjectOptions) do
  begin
    RunMode := TmneRunMode(RunModeCbo.ItemIndex);
    PauseConsole := PauseChk.Checked;
    MainFile := MainEdit.Text;
    ExeName := ExeEdit.Text;
    UseCFG := UseCFGFileChk.Checked;
    ExpandPaths := ExpandPathsChk.Checked;
    Paths.Assign(PathsEdit.Lines);
  end;
end;

procedure TPasProjectFrame.Retrieve;
begin
  RunModeCbo.Items.Add('Shell');
  RunModeCbo.Items.Add('Console');
  RunModeCbo.Items.Add('Terminal');
  RunModeCbo.Items.Add('Process');
  RunModeCbo.Items.Add('URL');

  with (Project.Options as TPasProjectOptions) do
  begin
    RunModeCbo.ItemIndex := ord(RunMode);
    PauseChk.Checked := PauseConsole;
    MainEdit.Text := MainFile;
    ExeEdit.Text := ExeName;
    UseCFGFileChk.Checked := UseCFG;
    ExpandPathsChk.Checked := ExpandPaths;
    PathsEdit.Lines.Assign(Paths);
  end;
end;

end.

