unit mneDConfigForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, mneDClasses;

type

  { TDConfigForm }

  TDConfigForm = class(TForm)
    Button3: TButton;
    CancelBtn: TButton;
    Label1: TLabel;
    OkBtn: TButton;
    DPathEdit: TEdit;
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
  protected
  public
    FTendency: TDTendency;
    procedure Apply;
    procedure Retrive;
  end;

implementation

uses
  EditorEngine;

{$R *.lfm}

{ TDConfigForm }

procedure TDConfigForm.Button3Click(Sender: TObject);
var
  aFolder: String;
begin
  aFolder := DPathEdit.Text;
  if SelectFolder('Select D directory', '', aFolder) then
  begin
    DPathEdit.Text := aFolder;
  end;
end;

procedure TDConfigForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult = mrOk then
  begin
  end;
end;

procedure TDConfigForm.Apply;
begin
  FTendency.DPath := DPathEdit.Text;
end;

procedure TDConfigForm.Retrive;
begin
  DPathEdit.Text := FTendency.DPath;
end;

end.

