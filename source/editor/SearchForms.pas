unit SearchForms;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, SynEdit, SynEditTypes, SynEditRegexSearch, SynEditMiscClasses, SynEditSearch;

type

  { TSearchForm }

  TSearchForm = class(TForm)
    AllBtn: TButton;
    Label1: TLabel;
    SearchTextEdit: TComboBox;
    SearchDirectionGrp: TRadioGroup;
    SearchOptionsGrp: TGroupBox;
    FindBtn: TButton;
    CancelBtn: TButton;
    SearchCaseSensitiveChk: TCheckBox;
    SearchWholeWordsChk: TCheckBox;
    SearchFromStartChk: TCheckBox;
    SearchSelectedOnlyChk: TCheckBox;
    ReplaceWithEdit: TComboBox;
    ReplaceWithChk: TCheckBox;
    procedure AllBtnClick(Sender: TObject);
    procedure ReplaceWithChkClick(Sender: TObject);
    procedure SearchDirectionGrpClick(Sender: TObject);
  private
    FAll: Boolean;
    procedure UpdateReplace;
    procedure SearchReplaceText(SynEdit: TSynEdit);
  public
  end;

procedure ShowSearchForm(SynEdit: TSynEdit; SearchHistory, ReplaceHistory: TStringList; ForReplace: Boolean);
procedure SetTextSearch(ASearchText: string; AReplaceText: string = ''; ASearchOptions: TSynSearchOptions = []);
function GetTextSearch: string; 
procedure SearchTextNext(SynEdit: TSynEdit);
procedure SearchTextPrevious(SynEdit: TSynEdit);

implementation

uses
    EditorEngine, EditorClasses;

{$R *.lfm}

var
  FSearchText: string;
  FReplaceText: string;
  FSearchOptions: TSynSearchOptions = [];

procedure SetTextSearch(ASearchText, AReplaceText: string; ASearchOptions: TSynSearchOptions);
begin
  FSearchText := ASearchText;
  FReplaceText := AReplaceText;
  FSearchOptions := ASearchOptions;
end;

function GetTextSearch: string;
begin
  Result := FSearchText;
end;

procedure InternalSearchText(SynEdit: TSynEdit; Options: TSynSearchOptions);
begin
  if SynEdit.SearchReplace(FSearchText, FReplaceText, Options) = 0 then
  begin
    Engine.SendMessage('Phrase not found', msgtEndStatus)
  end;
end;

procedure SearchTextNext(SynEdit: TSynEdit);
begin
  InternalSearchText(SynEdit, FSearchOptions - [ssoEntireScope]);
end;

procedure SearchTextPrevious(SynEdit: TSynEdit);
begin
  InternalSearchText(SynEdit, FSearchOptions + [ssoBackwards] - [ssoEntireScope]);
end;

procedure TSearchForm.SearchReplaceText(SynEdit: TSynEdit);
begin
  FSearchText := SearchTextEdit.Text;
  FReplaceText := ReplaceWithEdit.Text;
  if ReplaceWithChk.Checked then
  begin
    FSearchOptions := [ssoReplace];
    if FAll then
      FSearchOptions := FSearchOptions + [ssoReplaceAll]
    else
      FSearchOptions := FSearchOptions + [ssoPrompt];
  end
  else
    FSearchOptions := [];

  if SearchDirectionGrp.ItemIndex > 0 then
    Include(FSearchOptions, ssoBackwards);

  if SearchCaseSensitiveChk.Checked then
    Include(FSearchOptions, ssoMatchCase);
  if SearchWholeWordsChk.Checked then
    Include(FSearchOptions, ssoWholeWord);
  if SearchFromStartChk.Checked then
    Include(FSearchOptions, ssoEntireScope);
  if SearchSelectedOnlyChk.Checked then
    Include(FSearchOptions, ssoSelectedOnly);
  InternalSearchText(SynEdit, FSearchOptions);
end;

procedure ShowSearchForm(SynEdit: TSynEdit; SearchHistory, ReplaceHistory: TStringList; ForReplace: Boolean);
var
  aForm: TSearchForm;
  i: Integer;
begin
  aForm := TSearchForm.Create(SynEdit.Owner);
  with aForm do
  try
    // assign search FSearchOptions
    // start with last search text
    if SearchHistory <> nil then
      SearchTextEdit.Items.Assign(SearchHistory);
    if ReplaceHistory <> nil then
      ReplaceWithEdit.Items.Assign(ReplaceHistory);

    ReplaceWithChk.Checked := ForReplace;

    UpdateReplace;

    if SynEdit.SelAvail and (SynEdit.BlockBegin.y = SynEdit.BlockEnd.y) then
      SearchTextEdit.Text := SynEdit.SelText
    else
      SearchTextEdit.Text := SynEdit.GetWordAtRowCol(SynEdit.LogicalCaretXY);

    if SearchHistory.Count > 0 then
    begin
      if SearchTextEdit.Text = SearchHistory[0] then
      begin
        ReplaceWithEdit.Text := ReplaceHistory[0]
      end
    end;

    SearchDirectionGrp.ItemIndex := ord(ssoBackwards in FSearchOptions);
    SearchCaseSensitiveChk.Checked := ssoMatchCase in FSearchOptions;
    SearchWholeWordsChk.Checked := ssoWholeWord in FSearchOptions;
    SearchFromStartChk.Checked := ssoEntireScope in FSearchOptions;
    SearchSelectedOnlyChk.Checked := ssoSelectedOnly in FSearchOptions;

    if ShowModal = mrOK then
    begin
      if SearchTextEdit.Text <> '' then
      begin
        SearchReplaceText(SynEdit);

        if SearchTextEdit.Text <> '' then
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

procedure TSearchForm.ReplaceWithChkClick(Sender: TObject);
begin
  ReplaceWithEdit.Enabled := ReplaceWithChk.Checked;
  UpdateReplace;
end;

procedure TSearchForm.SearchDirectionGrpClick(Sender: TObject);
begin

end;

procedure TSearchForm.AllBtnClick(Sender: TObject);
begin
  FAll := True;
  ModalResult := mrOk;
end;

procedure TSearchForm.UpdateReplace;
begin
  if ReplaceWithChk.Checked then
  begin
    ReplaceWithEdit.Enabled := True;
    ReplaceWithEdit.Color := clWindow;
    ReplaceWithEdit.TabStop := True;
    AllBtn.Visible := True;
  end
  else
  begin
    ReplaceWithEdit.Enabled := False;
    ReplaceWithEdit.Color := clBtnFace;
    ReplaceWithEdit.TabStop := False;
    AllBtn.Visible := False;
  end;
end;

end.

