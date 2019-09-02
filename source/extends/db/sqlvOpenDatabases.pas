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
    AutoCreateChk: TCheckBox;
    BrowseBtn: TButton;
    CacheMetaChk: TCheckBox;
    CancelBtn: TButton;
    DatabaseCbo: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ServerEdit: TEdit;
    ExclusiveChk: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OkBtn: TButton;
    OpenDialog: TOpenDialog;
    DatabaseTypeCbo: TComboBox;
    PortEdit: TEdit;
    UserEdit: TEdit;
    PasswordEdit: TEdit;
    VacuumChk: TCheckBox;
    procedure BrowseBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure DirectoryFoundEvent(FileIterator: TFileIterator);
    procedure FormCreate(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
  public
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

procedure TOpenDatabaseForm.DirectoryFoundEvent(FileIterator: TFileIterator);
begin
  DatabaseCbo.Items.Add(FileIterator.FileName);
end;

procedure TOpenDatabaseForm.FormCreate(Sender: TObject);
begin
  DB.EnumConnectionsModels(DatabaseTypeCbo.Items);
end;

procedure TOpenDatabaseForm.OkBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

end.

