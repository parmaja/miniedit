unit mnePHPConfigForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, mnePHPClasses;

type

  { TPHPConfigForm }

  TPHPConfigForm = class(TForm)
    Button1: TButton;
    Button3: TButton;
    Button5: TButton;
    CancelBtn: TButton;
    HTMLManualEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label8: TLabel;
    OkBtn: TButton;
    OpenDialog: TOpenDialog;
    PHPPathEdit: TEdit;
    PHPManualEdit: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
  protected
  public
    FTendency: TPHPTendency;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

uses
  EditorEngine;

{$R *.lfm}

{ TPHPConfigForm }

procedure TPHPConfigForm.Button3Click(Sender: TObject);
var
  aFolder: String;
begin
  aFolder := PHPPathEdit.Text;
  if SelectFolder('Select PHP directory', '', aFolder) then
  begin
    PHPPathEdit.Text := aFolder;
  end;
end;

procedure TPHPConfigForm.Button1Click(Sender: TObject);
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

procedure TPHPConfigForm.Button5Click(Sender: TObject);
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


procedure TPHPConfigForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult = mrOk then
  begin
  end;
end;

procedure TPHPConfigForm.Apply;
begin
  FTendency.PHPPath := PHPPathEdit.Text;
  FTendency.PHPHelpFile := PHPManualEdit.Text;
  FTendency.HTMLHelpFile := HTMLManualEdit.Text;
end;

procedure TPHPConfigForm.Retrieve;
begin
  PHPPathEdit.Text := FTendency.PHPPath;
  PHPManualEdit.Text := FTendency.PHPHelpFile;
  HTMLManualEdit.Text := FTendency.HTMLHelpFile;
end;

end.

