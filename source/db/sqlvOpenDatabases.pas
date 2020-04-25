unit sqlvOpenDatabases;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, FileUtil, mncDB,
  sqlvConsts;

type

  { TOpenDatabaseForm }

  TOpenDatabaseForm = class(TForm)
    AnsiCodePageChk: TCheckBox;
    BrowseBtn: TButton;
    CacheMetaChk: TCheckBox;
    CancelBtn: TButton;
    DatabaseCbo: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    ServerEdit: TEdit;
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
    VacuumChk: TCheckBox;
    procedure BrowseBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure DatabaseCboSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
  public
    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  sqlvClasses;

{$R *.lfm}

{ TOpenDatabaseForm }

procedure TOpenDatabaseForm.BrowseBtnClick(Sender: TObject);
begin
  OpenDialog.FileName := sFileNameFilter;
  OpenDialog.DefaultExt := sFileExtFilter;
  OpenDialog.Filter := sFileNameFilter;
  if DatabaseCbo.Text <> '' then
    OpenDialog.InitialDir := DatabaseCbo.Text;
  if OpenDialog.Execute then
  begin
    DatabaseCbo.Text := OpenDialog.FileName;
  end;
end;

procedure TOpenDatabaseForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TOpenDatabaseForm.DatabaseCboSelect(Sender: TObject);
var
  i: Integer;
  EngineName, Resource, Host, User, Password, Role: string;
  aEngine: TmncEngine;
begin
  if DatabaseCbo.ItemIndex >=0 then
  begin
    Engines.DecomposeConnectionString(DBEngine.Recents[DatabaseCbo.ItemIndex], EngineName, Resource, Host, User, Password, Role);
    if EngineName <> '' then
    begin
      aEngine := Engines.Find(EngineName);
      DatabaseEngineCbo.ItemIndex := DatabaseEngineCbo.Items.IndexOfObject(aEngine);
    end;
  end;
end;

procedure TOpenDatabaseForm.FormCreate(Sender: TObject);
begin
  Engines.EnumConnections(DatabaseEngineCbo.Items);
end;

procedure TOpenDatabaseForm.OkBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

constructor TOpenDatabaseForm.Create(TheOwner: TComponent);
var
  i: Integer;
  EngineName, Resource, Host, User, Password, Role: string;
begin
  inherited Create(TheOwner);
  for i := 0 to DBEngine.Recents.Count -1 do
  begin
    Engines.DecomposeConnectionString(DBEngine.Recents[i], EngineName, Resource, Host, User, Password, Role);
    DatabaseCbo.Items.Add(Resource);
  end;
end;

end.

