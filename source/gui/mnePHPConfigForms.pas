unit mnePHPConfigForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TPHPConfigForm }

  TPHPConfigForm = class(TForm)
    Button3: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    OkBtn: TButton;
    OpenDialog: TOpenDialog;
    PHPDirEdit: TEdit;
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
  end; 

implementation

uses
  EditorEngine, mnePHPClasses;

{$R *.lfm}

{ TPHPConfigForm }

procedure TPHPConfigForm.Button3Click(Sender: TObject);
var
  aFolder: string;
begin
  aFolder := PHPDirEdit.Text;
  if SelectFolder('Select PHP directory', '', aFolder) then
  begin
    PHPDirEdit.Text := aFolder;
  end;
end;

procedure TPHPConfigForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult = mrOK then
  begin
  end;
end;

initialization
  Engine.Forms.Add(TPHPPerspective, TPHPConfigForm);
end.

