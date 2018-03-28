unit SelectList;
{$mode objfpc}{$H+}

{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  LMessages, LCLIntf, LCLProc, LCLType, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  mnUtils, StrUtils,
  EditorClasses, EditorEngine, Dialogs, ComCtrls, StdCtrls, ExtCtrls;

type
  TSelectListFormStyle = set of (slfSearch, slfIncludeNone, slfUseNameTitle);
  //slfUseNameTitle instead if Title/Description

  { TSelectListForm }

  TSelectListForm = class(TForm)
    SearchEdit: TEdit;
    ItemsList: TListView;
    Label1: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    SearchTimer: TTimer;
    procedure ItemsListDblClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);

    procedure SearchEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SearchEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SearchTimerTimer(Sender: TObject);
  private
    FStyle: TSelectListFormStyle;
  public
    Elements: TEditorElements;
    procedure UpdateStyle;
    procedure ShowItems(vSelect: string; vFilter: string = '');
  end;

function ShowSelectList(ACaption: string; vElements: TEditorElements; Style: TSelectListFormStyle; var vName: string): Boolean;
function ShowSelectList(ACaption: string; vElements: TEditorElements; Style: TSelectListFormStyle; out vIndex: Integer): Boolean;

implementation

uses
  mneResources, mneClasses;

{$R *.lfm}

function ShowSelectList(ACaption: string; vElements: TEditorElements; Style: TSelectListFormStyle; var vName: string): Boolean;
begin
  with TSelectListForm.Create(Application) do
  begin
    try
      Caption := ACaption;
      FStyle := Style;
      UpdateStyle;
      Elements := vElements;
      ShowItems(vName);
      Result := (ShowModal = mrOK) and (ItemsList.Selected <> nil);
      if Result then
      begin
        if ItemsList.Selected.Data <> nil then
          vName := (TObject(ItemsList.Selected.Data) as TEditorElement).Name
        else
          vName := '';
      end;
      Free;
    finally
    end;
  end;
end;

function ShowSelectList(ACaption: string; vElements: TEditorElements; Style: TSelectListFormStyle; out vIndex: Integer): Boolean;
begin
  with TSelectListForm.Create(Application) do
  begin
    try
      Caption := ACaption;
      FStyle := Style;
      UpdateStyle;
      Elements := vElements;
      ShowItems('');
      Result := (ShowModal = mrOK) and (ItemsList.Selected <> nil);
      if Result then
        if ItemsList.Selected.Data <> nil then
          vIndex := vElements.IndexOf((TObject(ItemsList.Selected.Data) as TEditorElement))
        else
          vIndex := -1;
      Free;
    finally
    end;
  end;
end;

procedure TSelectListForm.ItemsListDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TSelectListForm.ShowItems(vSelect: string; vFilter: string);
var
  i, c, t: Integer;
  aItem: TListItem;
  procedure AddItem(vData: Pointer; Name, Title, Description: string; ImageIndex: Integer);
  begin
    if (vFilter = '') or ContainsText(vFilter, Name) or ContainsText(vFilter, Title) or ContainsText(vFilter, Description) then
    begin
      aItem := ItemsList.Items.Add;
      aItem.Data := vData;

      if slfUseNameTitle in FStyle then
      begin
        aItem.Caption := Name;
        aItem.SubItems.Add(Title);
      end
      else
      begin
        aItem.Caption := Title;
        aItem.SubItems.Add(Description);
      end;

      aItem.ImageIndex := ImageIndex;
      //SetLength(Items, c + 1);
      //Items[c] := Name;
      if SameText(vSelect, Name) then
        t := c;
      inc(c);
    end;
  end;
begin
  ItemsList.Items.BeginUpdate;
  with Engine do
  try
    ItemsList.Clear;
    c := 0;
    t := 0;
    if slfIncludeNone in FStyle then
      AddItem(nil, '', 'None', '', -1);
    for i := 0 to Elements.Count - 1 do
      AddItem(Elements[i], Elements[i].Name, Elements[i].Title, Elements[i].Description, Elements[i].ImageIndex);
  finally
    ItemsList.Items.EndUpdate;
  end;
  if ItemsList.Items.Count > 0 then
  begin
    ItemsList.Items[t].Selected := True;
    ItemsList.Items[t].Focused := True;
  end;
end;

procedure TSelectListForm.OkBtnClick(Sender: TObject);
begin
 ModalResult := mrOK
end;

{type
  THackWinControl = class(TWinControl)
  end;}

procedure TSelectListForm.SearchEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_DOWN, VK_UP, VK_NEXT, VK_PRIOR, VK_HOME, VK_END:
        ItemsList.SetFocus;
        //THackWinControl(ItemsList).KeyDown(Key, Shift);
    end;
  end;
end;

procedure TSelectListForm.SearchEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key > VK_SPACE then
    SearchTimer.Enabled := True;
end;

procedure TSelectListForm.SearchTimerTimer(Sender: TObject);
begin
  ShowItems('', SearchEdit.Text);
  SearchTimer.Enabled := False;
end;

procedure TSelectListForm.UpdateStyle;
begin
  if not (slfSearch in FStyle) then
  begin
    SearchEdit.Visible := False;
    ItemsList.BoundsRect := Rect(ItemsList.BoundsRect.Left, SearchEdit.BoundsRect.Top, ItemsList.BoundsRect.Right, ItemsList.BoundsRect.Bottom);
    ActiveControl := ItemsList;
  end
  else
    ActiveControl := SearchEdit;
end;

end.

