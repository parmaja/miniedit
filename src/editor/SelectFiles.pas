unit SelectFiles;
{$mode objfpc}{$H+}

{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *}
{bugs
  IsMatch cant match this with "*sea*" "cust_search_form.inc.php"
}
interface

uses
  LCLIntf, LCLProc, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  EditorEngine, Dialogs, ComCtrls, StdCtrls, ExtCtrls;

type

  { TSelectFileForm }

  TSelectFileForm = class(TForm)
    FilesList: TListView;
    OkBtn: TButton;
    CancelBtn: TButton;
    FilterEdit: TEdit;
    Timer: TTimer;
    procedure CancelBtnClick(Sender: TObject);
    procedure FilesListDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FilterEditChange(Sender: TObject);
    procedure FilterEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OkBtnClick(Sender: TObject);
  private
    FFiles: TStringList;
    FLoaded: Boolean;
    FRoot: string;
    procedure EnumFiles;
  public
    procedure ShowFiles;
  end;

function ShowSelectFile(vRoot: string; out FileName: string): Boolean;

implementation

uses
  StrUtils, mneResources, mneClasses;

var
  LastFilter: string = '';

{$R *.lfm}

function ShowSelectFile(vRoot: string; out FileName: string): Boolean;
begin
  with TSelectFileForm.Create(Application) do
  begin
    try
      FRoot := vRoot;
      //FilterEdit.Text := LastFilter;
      FilterEdit.SelectAll;

      Result := ShowModal = mrOK;
      if Result then
      begin
        if FilesList.Selected <> nil then
        begin
          LastFilter := FilterEdit.Text;
          FileName := IncludeTrailingPathDelimiter((IncludeTrailingPathDelimiter(vRoot)) + FilesList.Selected.SubItems[0]) + FilesList.Selected.Caption;
        end
        else
          Result := False;
      end;
      Free;
    finally
    end;
  end;
end;

procedure TSelectFileForm.FilesListDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TSelectFileForm.CancelBtnClick(Sender: TObject);
begin
  CancelSearch;
end;

procedure TSelectFileForm.FormDestroy(Sender: TObject);
begin
  FFiles.Free;
end;

procedure TSelectFileForm.FormCreate(Sender: TObject);
begin
  FFiles := TStringList.Create;
end;

procedure TSelectFileForm.ShowFiles;
var
  s: string;
  i: Integer;
  aFileName: string;
  aItem: TListItem;
begin
  if not FLoaded then
  begin
    FLoaded := True;
    EnumFiles;
  end;
  s := FilterEdit.Text;
  {if (s <> '') and (Pos('*', s) = 0) then
    s := s + '*';}
  FilesList.Items.BeginUpdate;
  try
    FilesList.Clear;
    for i := 0 to FFiles.Count - 1 do
    begin
      aFileName := ExtractFileName(FFiles[i]);
      if (s = '')  or (FindPart(LowerCase(s), LowerCase(aFileName)) > 0) then
      //if (s = '') or IsMatch(s, aFileName) then
      begin
        aItem := FilesList.Items.Add;
        aItem.Caption := aFileName;
        aItem.SubItems.Add(ExtractFilePath(FFiles[i]));
        aItem.ImageIndex := EditorResource.GetFileImageIndex(aItem.Caption, -1);
      end;
    end;
  finally
    FilesList.Items.EndUpdate;
  end;
  if FilesList.Items.Count > 0 then
  begin
    FilesList.Items[0].Selected := True;
  end;
end;

procedure TSelectFileForm.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := False;
  ShowFiles;
end;

procedure TSelectFileForm.FilterEditChange(Sender: TObject);
begin
  Timer.Enabled := False;
  if Length(FilterEdit.Text) >= 3 then
    Timer.Interval := 250
  else
    Timer.Interval := 500;
  Timer.Interval := 300;
  Timer.Enabled := True;
end;

procedure TSelectFileForm.FilterEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_DOWN, VK_UP, VK_NEXT, VK_PRIOR, VK_HOME, VK_END:
        FilesList.SetFocus;
    end;
  end;
end;

procedure TSelectFileForm.OkBtnClick(Sender: TObject);
begin
  if Timer.Enabled then
  begin
    Timer.Enabled := False;
    ShowFiles;
  end;
  if FilesList.Items.Count > 0 then
    ModalResult := mrOK
end;

procedure TSelectFileForm.EnumFiles;
var
  aIgnoreNames: string;
begin
  Screen.Cursor := crHourGlass;
  try
    aIgnoreNames := Engine.Options.IgnoreNames;
    if Engine.Session.Active then
    begin
      if Engine.Session.Project.IgnoreNames <> '' then
      begin
        if (aIgnoreNames <> '') then
          aIgnoreNames := aIgnoreNames + ';';
        aIgnoreNames := aIgnoreNames + Engine.Session.Project.IgnoreNames;
      end;
    end;
    EnumFileList(FRoot, Engine.Groups.CreateFilter(False), aIgnoreNames, FFiles, 1000, 10, False);
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
