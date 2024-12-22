unit mndOpenDatabases;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, FileUtil, mncDB;

type

  { TOpenDatabaseForm }

  TOpenDatabaseForm = class(TForm)
    AnsiCodePageChk: TCheckBox;
    BrowseBtn: TButton;
    DatabaseEdit: TEdit;
    SavePasswordChk: TCheckBox;
    CancelBtn: TButton;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    HostEdit: TEdit;
    ExclusiveChk: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OkBtn: TButton;
    OpenDialog: TOpenDialog;
    DatabaseEngineCbo: TComboBox;
    PortEdit: TEdit;
    UserEdit: TEdit;
    PasswordEdit: TEdit;
    RoleEdit: TEdit;
    procedure BrowseBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
  public
  end;

implementation

uses
  mndEngines;

{$R *.lfm}

{ TOpenDatabaseForm }

procedure TOpenDatabaseForm.BrowseBtnClick(Sender: TObject);
begin
  OpenDialog.FileName := sFileNameFilter;
  OpenDialog.DefaultExt := sFileExtFilter;
  OpenDialog.Filter := sFileNameFilter;
  if DatabaseEdit.Text <> '' then
    OpenDialog.InitialDir := DatabaseEdit.Text;
  if OpenDialog.Execute then
  begin
    DatabaseEdit.Text := OpenDialog.FileName;
  end;
end;

procedure TOpenDatabaseForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TOpenDatabaseForm.FormCreate(Sender: TObject);
begin
  Engines.EnumConnections(DatabaseEngineCbo.Items);
  DatabaseEngineCbo.Sorted := True;
  DatabaseEngineCbo.ItemIndex := 0;
end;

procedure TOpenDatabaseForm.OkBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.
