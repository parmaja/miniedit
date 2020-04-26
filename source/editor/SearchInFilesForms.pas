unit SearchInFilesForms;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, MsgBox,
  StdCtrls, ExtCtrls, SynEdit, SynEditTypes, SynEditRegexSearch, SynEditMiscClasses,
  SynEditSearch, SearchProgressForms, EditorEngine, ComCtrls, Menus, ntvImgBtns;

type
  TSearchFoundEvent = procedure(Index: Integer; FileName: string; const Line: string; LineNo, Column, FoundLength: Integer) of object;

  { TSearchInFilesForm }

  TSearchInFilesForm = class(TForm)
    InsertBtn: TntvImgBtn;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    DirPopupMenu: TPopupMenu;
    SearchTextEdit: TComboBox;
    SearchOptionsGrp: TGroupBox;
    FindBtn: TButton;
    CancelBtn: TButton;
    SearchCaseSensitiveChk: TCheckBox;
    SearchWholeWordsChk: TCheckBox;
    ReplaceWithEdit: TComboBox;
    ReplaceWithChk: TCheckBox;
    Label2: TLabel;
    SearchFolderEdit: TComboBox;
    SearchFilesGrp: TRadioGroup;
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure ReplaceWithChkClick(Sender: TObject);
  private
    function CreateMask(vGroup: TFileGroup): Boolean;
    procedure UpdateReplace;
    procedure FoundEvent(FileName: string; const Line: string; LineNo, Column, FoundLength: Integer);
    procedure SearchReplaceText;
    procedure SearchInFiles;
    procedure SearchInFile(FileName: string);
  protected
    FSearchCount: Integer;
    FSearchFoundEvent: TSearchFoundEvent;
    FProgressForm: TSearchProgressForm;
    FSearchText: string;
    FReplaceText: string;
    FSearchOptions: TSynSearchOptions;
  public
  end;

function ShowSearchInFilesForm(SearchFoundEvent: TSearchFoundEvent; SearchText, SearchFolder: string; SearchFolderHistory, SearchHistory, ReplaceHistory: TStringList): Boolean;

implementation

uses SearchForms;

{$R *.lfm}

procedure DoSearchInFileCallback(AObject: TObject; const FileName: string; Count, Level:Integer; IsDirectory: Boolean; var Resume: Boolean);
begin
  with (AObject as TSearchInFilesForm) do
  begin
    SearchInFile(ExpandFileName(FileName));
    if (Count mod 25) = 0 then
    begin
      Application.ProcessMessages;
      if FProgressForm.Canceled then
        Resume := False;
      Application.ProcessMessages;
    end;
  end;
end;

procedure TSearchInFilesForm.SearchInFiles;
var
  aMasks: string;
begin
  if Engine.Session.Active and (SearchFilesGrp.ItemIndex = 0) then
    aMasks := Engine.Session.Project.Tendency.Groups.CreateMask(@CreateMask)
  else
    aMasks := Engine.Groups.CreateMask(@CreateMask);

  EnumFileList(IncludeTrailingPathDelimiter(SearchFolderEdit.Text), aMasks, Engine.Options.IgnoreNames, @DoSearchInFileCallback, Self, 1000, 3, True);
end;

procedure TSearchInFilesForm.SearchReplaceText;
begin
  if ReplaceWithChk.Checked then
    FSearchOptions := [ssoReplace, ssoReplaceAll];
  if SearchCaseSensitiveChk.Checked then
    Include(FSearchOptions, ssoMatchCase);
  if SearchWholeWordsChk.Checked then
    Include(FSearchOptions, ssoWholeWord);
  FSearchText := SearchTextEdit.Text;
  FReplaceText := ReplaceWithEdit.Text;
  if (Engine.Files.Count > 0) and (ssoReplace in FSearchOptions) then
  begin
    Engine.Files.CheckingChanged := True; //stop auto check file age, we will reload them all
    if MsgBox.Msg.Yes('Replace in files need to save all changed files, do you want to save it?') then
      Engine.Files.SaveAll(False)
    else
      Abort;
  end;
  FProgressForm := TSearchProgressForm.Create(Application);
  try
    FProgressForm.Show;
    SearchInFiles;
    SetTextSearch(FSearchText, FReplaceText, FSearchOptions);// send text to normal text search
  finally
    FreeAndNil(FProgressForm);
    Engine.Files.CheckingChanged := False;
  end;
  if ssoReplace in FSearchOptions then
  begin
    Engine.Files.ReloadAll;
  end;
end;

function ShowSearchInFilesForm(SearchFoundEvent: TSearchFoundEvent; SearchText, SearchFolder: string; SearchFolderHistory, SearchHistory, ReplaceHistory: TStringList): Boolean;
var
  aForm: TSearchInFilesForm;
  i: Integer;
begin
  aForm := TSearchInFilesForm.Create(Application);
  with aForm do
  try
    // assign search FSearchOptions
    // start with last search text
    FSearchFoundEvent := SearchFoundEvent;
    if SearchHistory <> nil then
      SearchTextEdit.Items.Assign(SearchHistory);
    if ReplaceHistory <> nil then
      ReplaceWithEdit.Items.Assign(ReplaceHistory);
    if SearchFolderHistory <> nil then
      SearchFolderEdit.Items.Assign(SearchFolderHistory);

    UpdateReplace;

    SearchTextEdit.Text := SearchText;
    if SearchHistory.Count > 0 then
    begin
      if SearchText = SearchHistory[0] then
      begin
        ReplaceWithEdit.Text := ReplaceHistory[0]
      end
    end;

    SearchFolderEdit.Text := SearchFolder;

    Result := ShowModal = mrOK;
    if Result then
    begin
      if SearchTextEdit.Text <> '' then
      begin
        SearchReplaceText;

        if SearchHistory <> nil then
        begin
          i := SearchHistory.IndexOf(SearchTextEdit.Text);
          if i >= 0 then
            SearchHistory.Delete(i);
          SearchHistory.Insert(0, SearchTextEdit.Text);
          while SearchHistory.Count > 25 do
          begin
            SearchHistory.Delete(SearchHistory.Count - 1);
          end;
        end;

        if SearchFolderHistory <> nil then
        begin
          i := SearchFolderHistory.IndexOf(SearchFolderEdit.Text);
          if i >= 0 then
            SearchFolderHistory.Delete(i);
          SearchFolderHistory.Insert(0, SearchFolderEdit.Text);
          while SearchFolderHistory.Count > 25 do
          begin
            SearchFolderHistory.Delete(SearchFolderHistory.Count - 1);
          end;
        end;

        if ReplaceWithEdit.Text <> '' then
          if ReplaceHistory <> nil then
          begin
            i := ReplaceHistory.IndexOf(ReplaceWithEdit.Text);
            if i >= 0 then
              ReplaceHistory.Delete(i);
            ReplaceHistory.Insert(0, ReplaceWithEdit.Text);
            while ReplaceHistory.Count > 25 do
            begin
              ReplaceHistory.Delete(ReplaceHistory.Count - 1);
            end;
          end;
      end;
    end;
  finally
    aForm.Free;
  end;
end;

procedure TSearchInFilesForm.ReplaceWithChkClick(Sender: TObject);
begin
  ReplaceWithEdit.Enabled := ReplaceWithChk.Checked;
  UpdateReplace;
end;

function TSearchInFilesForm.CreateMask(vGroup: TFileGroup): Boolean;
begin
  Result := vGroup.Category is TTextFileCategory;
end;

procedure TSearchInFilesForm.MenuItem1Click(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    SearchFolderEdit.Text := ExtractFileDir(Engine.Files.Current.Name)
end;

procedure TSearchInFilesForm.MenuItem2Click(Sender: TObject);
begin
  SearchFolderEdit.Text := Engine.BrowseFolder;
end;

procedure TSearchInFilesForm.UpdateReplace;
begin
  if ReplaceWithChk.Checked then
  begin
    ReplaceWithEdit.Enabled := True;
    ReplaceWithEdit.Color := clWindow;
    ReplaceWithEdit.TabStop := True;
    FindBtn.Caption := 'R&eplace';
  end
  else
  begin
    ReplaceWithEdit.Enabled := False;
    ReplaceWithEdit.Color := clBtnFace;
    ReplaceWithEdit.TabStop := False;
    FindBtn.Caption := '&Find';
  end;
end;

procedure TSearchInFilesForm.FoundEvent(FileName: string; const Line: string; LineNo, Column, FoundLength: Integer);
begin
  FSearchFoundEvent(FSearchCount, FileName,Line, LineNo, Column, FoundLength);
  Inc(FSearchCount);
end;

procedure TSearchInFilesForm.SearchInFile(FileName: string);
var
  Contents: string;
  Size: Integer;
  Stream: TFileStream;
  aStrings: TStringList;
  Mode: TEditorLinesMode;
begin
  FileName := ExpandFileName(FileName);
  if FProgressForm <> nil then
  begin
    FProgressForm.FileNameLbl.Caption := FileName;
    Application.ProcessMessages;
  end;
  aStrings := TStringList.Create;
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      Size := Stream.Size - Stream.Position;
      SetString(Contents, nil, Size);
      Stream.Read(Pointer(Contents)^, Size);
      Mode := DetectLinesMode(Contents);
      aStrings.Text := Contents;
    finally
      Stream.Free;
    end;
    Engine.SearchReplace(FileName, aStrings, FSearchText, FReplaceText, @FoundEvent, FSearchOptions);
    if ssoReplace in FSearchOptions then
      SaveAsMode(FileName, Mode, aStrings);
  finally
    aStrings.Free;
  end;
  if FProgressForm <> nil then
  begin
    FProgressForm.FoundLbl.Caption := IntToStr(FSearchCount);
    Application.ProcessMessages;
  end;
end;

end.
