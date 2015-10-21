unit mnePasConfigForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, mnePasClasses;

type

  { TPasConfigForm }

  TPasConfigForm = class(TForm)
    Button3: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    OkBtn: TButton;
    CompilerEdit: TEdit;
    OpenDialog: TOpenDialog;
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
  protected
  public
    FTendency: TPasTendency;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

uses
  EditorEngine;

{$R *.lfm}

{ TPasConfigForm }

procedure TPasConfigForm.Button3Click(Sender: TObject);
begin
  OpenDialog.Filter := 'EXE files|*.exe|All files|*.*';
  OpenDialog.FileName := CompilerEdit.Text;
  OpenDialog.InitialDir := ExtractFilePath(OpenDialog.FileName);
  if OpenDialog.Execute then
  begin
    CompilerEdit.Text := OpenDialog.FileName;
  end;
end;

procedure TPasConfigForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult = mrOk then
  begin
  end;
end;

procedure TPasConfigForm.Apply;
begin
  FTendency.Compiler := CompilerEdit.Text;
end;

procedure TPasConfigForm.Retrieve;
begin
  CompilerEdit.Text := FTendency.Compiler;
end;

end.

