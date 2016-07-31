unit mnePHPTendencyFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditorEngine, mnePHPClasses;

type

  { TPHPTendencyFrame }

  TPHPTendencyFrame = class(TFrame, IEditorOptions)
    Button1: TButton;
    Button3: TButton;
    Button5: TButton;
    HTMLManualEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label8: TLabel;
    OpenDialog: TOpenDialog;
    PHPPathEdit: TEdit;
    PHPManualEdit: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
  protected
  public
    FTendency: TEditorTendency;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TPHPTendencyFrame }

procedure TPHPTendencyFrame.Button3Click(Sender: TObject);
var
  aFolder: String;
begin
  aFolder := PHPPathEdit.Text;
  if SelectFolder('Select PHP directory', '', aFolder) then
  begin
    PHPPathEdit.Text := aFolder;
  end;
end;

procedure TPHPTendencyFrame.Button1Click(Sender: TObject);
begin
  OpenDialog.Title := 'select PHP Help file "php_manual_en.chm"';
  OpenDialog.Filter := 'Help files|*.chm|All files|*.*';
  OpenDialog.FileName := PHPManualEdit.Text;
  OpenDialog.InitialDir := ExtractFilePath(PHPManualEdit.Text);
  if OpenDialog.Execute then
  begin
    PHPManualEdit.Text := OpenDialog.FileName;
  end;
end;

procedure TPHPTendencyFrame.Button5Click(Sender: TObject);
begin
  OpenDialog.Title := 'select HTML Help file';
  OpenDialog.Filter := 'Help files|*.chm|All files|*.*';
  OpenDialog.FileName := HTMLManualEdit.Text;
  OpenDialog.InitialDir := ExtractFilePath(HTMLManualEdit.Text);
  if OpenDialog.Execute then
  begin
    HTMLManualEdit.Text := OpenDialog.FileName;
  end;
end;


procedure TPHPTendencyFrame.Apply;
begin
  with (FTendency as TPHPTendency) do
  begin
    PHPPath := PHPPathEdit.Text;
    PHPHelpFile := PHPManualEdit.Text;
    HTMLHelpFile := HTMLManualEdit.Text;
  end;
end;

procedure TPHPTendencyFrame.Retrieve;
begin
  with (FTendency as TPHPTendency) do
  begin
    PHPPathEdit.Text := PHPPath;
    PHPManualEdit.Text := PHPHelpFile;
    HTMLManualEdit.Text := HTMLHelpFile;
  end;
end;

end.

