unit mneDProjectFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, EditorEngine, SelectFiles, DebugClasses, mneDClasses;

type

  { TDProjectFrame }

  TDProjectFrame = class(TFrame, IEditorOptions)
    Bevel1: TBevel;
    Button4: TButton;
    CancelBtn: TButton;
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
    OkBtn: TButton;
    PathsEdit: TSynEdit;
    procedure Button4Click(Sender: TObject);
  private
    DOptions: TDProjectOptions;
  protected
  public
    //Options: TDProjectOptions;
    Project: TEditorProject;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TDProjectFrame }

procedure TDProjectFrame.Button4Click(Sender: TObject);
var
  s: string;
begin
  ShowSelectFile(Project.RootDir, s);
  MainEdit.Text := s;
end;

procedure TDProjectFrame.Apply;
begin
  DOptions.RunMode := TmneRunMode(RunModeCbo.ItemIndex);
  DOptions.PauseConsole := PauseChk.Checked;
  DOptions.MainFile := MainEdit.Text;
  DOptions.OutputFile := OutputFileEdit.Text;
  DOptions.RunParams := RunParamsEdit.Text;
  DOptions.ConfigFile := ConfigFileEdit.Text;
  DOptions.ExpandPaths := ExpandPathsChk.Checked;
  DOptions.Paths.Assign(PathsEdit.Lines);
end;

procedure TDProjectFrame.Retrieve;
begin
  DOptions := (Project.Options as TDProjectOptions);
  RunModeCbo.Items.Add('Log');
  RunModeCbo.Items.Add('Console');
  RunModeCbo.Items.Add('Terminal');
  RunModeCbo.Items.Add('Process');
  RunModeCbo.Items.Add('URL');

  RunModeCbo.ItemIndex := ord(DOptions.RunMode);
  PauseChk.Checked := DOptions.PauseConsole;
  MainEdit.Text := DOptions.MainFile;
  OutputFileEdit.Text := DOptions.OutputFile;
  RunParamsEdit.Text := DOptions.RunParams;
  ConfigFileEdit.Text := DOptions.ConfigFile;
  ExpandPathsChk.Checked := DOptions.ExpandPaths;
  PathsEdit.Lines.Assign(DOptions.Paths);
end;

end.

