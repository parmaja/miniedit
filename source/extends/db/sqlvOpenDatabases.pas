unit sqlvOpenDatabases;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, FileUtil,
  sqlvConsts;

type

  { TOpenDatabaseForm }

  TOpenDatabaseForm = class(TForm)
    AnsiCodePageChk: TCheckBox;
    AutoCreateChk: TCheckBox;
    BrowseBtn: TButton;
    CacheMetaChk: TCheckBox;
    CancelBtn: TButton;
    DatabasesCbo: TComboBox;
    DataPathCbo: TComboBox;
    ExclusiveChk: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OkBtn: TButton;
    OpenDialog: TOpenDialog;
    RecentsCbo: TComboBox;
    RefreshBtn: TButton;
    RemoveBtn: TButton;
    VacuumChk: TCheckBox;
    procedure BrowseBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure DatabasesCboDropDown(Sender: TObject);
    procedure DataPathCboDropDown(Sender: TObject);
    procedure DataPathCboExit(Sender: TObject);
    procedure DirectoryFoundEvent(FileIterator: TFileIterator);
    procedure OkBtnClick(Sender: TObject);
    procedure RecentsCboSelect(Sender: TObject);
    procedure RefreshBtnClick(Sender: TObject);
    procedure RemoveBtnClick(Sender: TObject);
    procedure SetRealDataPath(FileName: string);
  private
    FLockEnum: Boolean;
    FDataPath: string;
    procedure EnumDatabases;
    procedure FileFoundEvent(FileIterator: TFileIterator);
    procedure SetDataPath(const AValue: string);
  public
    function GetDatabaseName: string;
    property DataPath: string read FDataPath write SetDataPath;
  end;

implementation

uses
  sqlvClasses;

{$R *.lfm}

{ TOpenDatabaseForm }

procedure TOpenDatabaseForm.DatabasesCboDropDown(Sender: TObject);
begin
  DataPath := DataPathCbo.Text;
end;

procedure TOpenDatabaseForm.DataPathCboDropDown(Sender: TObject);
begin
  DataPath := DataPathCbo.Text;
end;

procedure TOpenDatabaseForm.DataPathCboExit(Sender: TObject);
begin
  DataPath := DataPathCbo.Text;
end;

procedure TOpenDatabaseForm.RecentsCboSelect(Sender: TObject);
begin
  if RecentsCbo.ItemIndex >= 0 then
    SetRealDataPath(RecentsCbo.Text);
end;

procedure TOpenDatabaseForm.RefreshBtnClick(Sender: TObject);
begin
  EnumDatabases;
end;

procedure TOpenDatabaseForm.RemoveBtnClick(Sender: TObject);
var
  i: Integer;
begin
  i := sqlvEngine.Recents.IndexOf(RecentsCbo.Text);
  if i >= 0 then
  begin
    sqlvEngine.Recents.Delete(i);
    sqlvEngine.SaveRecents;
    RecentsCbo.Items.Assign(sqlvEngine.Recents);
  end;
end;

procedure TOpenDatabaseForm.SetRealDataPath(FileName: string);
var
  aPath: string;
  i: Integer;
begin
  aPath := ExtractFilePath(FileName);
  DataPathCbo.Text := aPath;
  DataPath := aPath;
  FileName := ExtractFileName(FileName);
  i:= DatabasesCbo.Items.IndexOf(FileName);
  if i >= 0 then
    DatabasesCbo.ItemIndex := i
  else
    DatabasesCbo.Text := FileName;
end;

procedure TOpenDatabaseForm.BrowseBtnClick(Sender: TObject);
begin
  OpenDialog.FileName := sFileNameFilter;
  OpenDialog.DefaultExt := sFileExtFilter;
  OpenDialog.Filter := sFileNameFilter;
  OpenDialog.InitialDir := DataPathCbo.Text;
  if OpenDialog.Execute then
  begin
    SetRealDataPath(OpenDialog.FileName);
  end;
end;

procedure TOpenDatabaseForm.CancelBtnClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TOpenDatabaseForm.FileFoundEvent(FileIterator: TFileIterator);
begin
  DatabasesCbo.Items.Add(FileIterator.FileInfo.Name);
end;

procedure TOpenDatabaseForm.DirectoryFoundEvent(FileIterator: TFileIterator);
begin
  DataPathCbo.Items.Add(FileIterator.FileName);
end;

procedure TOpenDatabaseForm.OkBtnClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TOpenDatabaseForm.EnumDatabases;
var
  aFiles: TStringList;
  aFileSearcher: TFileSearcher;
  OldFile: string;
  aUpPath: string;
begin
  if not FLockEnum then
  begin
    FLockEnum := True;
    try
      aFileSearcher := TFileSearcher.Create;
      aFiles := TStringList.Create;
      OldFile := DatabasesCbo.Text;
      DataPathCbo.Items.BeginUpdate;
      DatabasesCbo.Items.BeginUpdate;
      try
        DatabasesCbo.Items.Clear;
        DataPathCbo.Items.Clear;
        aUpPath := ExpandFileName(IncludeTrailingPathDelimiter(FDataPath) + '..\');
        if aUpPath <> FDataPath then
          DataPathCbo.Items.Add(aUpPath);
        DataPathCbo.Items.Add(FDataPath);
        aFileSearcher.OnDirectoryFound := @DirectoryFoundEvent;
        aFileSearcher.OnFileFound := @FileFoundEvent;
        aFileSearcher.Search(DataPath, sFileNameFilter, False);
        DataPathCbo.Text := FDataPath;
        if DatabasesCbo.Items.IndexOf(OldFile) >=0 then
          DatabasesCbo.Text := OldFile;
      finally
        aFileSearcher.Free;
        aFiles.Free;
        DataPathCbo.Items.EndUpdate;
        DatabasesCbo.Items.EndUpdate;
        DataPathCbo.Refresh;
        DatabasesCbo.Refresh;
      end;
    finally
      FLockEnum := False;
    end;
  end;
end;

procedure TOpenDatabaseForm.SetDataPath(const AValue: string);
begin
  if not FLockEnum then
    if FDataPath <> AValue then
    begin
      FDataPath := AValue;
      EnumDatabases;
    end;
end;

function TOpenDatabaseForm.GetDatabaseName: string;
begin
  Result := FDataPath + DatabasesCbo.Text;
end;

end.

