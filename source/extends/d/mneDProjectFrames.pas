unit mneDProjectFrames;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditorEngine, mneDClasses;

type

  { TDProjectFrame }

  TDProjectFrame = class(TFrame, IEditorFrame)
    Button4: TButton;
    CancelBtn: TButton;
    MainEdit: TEdit;
    Label2: TLabel;
    OkBtn: TButton;
    procedure Button3Click(Sender: TObject);
  private
  protected
  public
    Options: TDProjectOptions;
    procedure Apply;
    procedure Retrieve;
  end;

implementation

{$R *.lfm}

{ TDProjectFrame }

procedure TDProjectFrame.Button3Click(Sender: TObject);
begin
end;

procedure TDProjectFrame.Apply;
begin
  Options.MainFile := MainEdit.Text;
end;

procedure TDProjectFrame.Retrieve;
begin
  MainEdit.Text := Options.MainFile;
end;

end.

