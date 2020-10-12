unit sqlvConnectServers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, FileUtil, mncDB;

type

  { TConnectDBServerForm }

  TConnectDBServerForm = class(TForm)
    SavePasswordChk: TCheckBox;
    CancelBtn: TButton;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    HostEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    OkBtn: TButton;
    DatabaseEngineCbo: TComboBox;
    PortEdit: TEdit;
    UserEdit: TEdit;
    PasswordEdit: TEdit;
    RoleEdit: TEdit;
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
  public
  end;

implementation

uses
  EditorEngine, sqlvEngines;

{$R *.lfm}

{ TConnectDBServerForm }

procedure TConnectDBServerForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TConnectDBServerForm.FormCreate(Sender: TObject);
begin
  Engines.EnumConnections(DatabaseEngineCbo.Items);
  DatabaseEngineCbo.Sorted := True;
  DatabaseEngineCbo.ItemIndex := 0;
end;

procedure TConnectDBServerForm.OkBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.

