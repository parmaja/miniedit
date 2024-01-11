unit SelectList;
{$mode objfpc}{$H+}

{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
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
  protected
    procedure DoShow; override;
		procedure ShowItems(vFilter: string = ''; vSelect: string = '');
  public
    FLastSearch: string;
    Elements: TEditorElements;
    procedure UpdateStyle;
  end;

function ShowSelectList(ACaption: string; vElements: TEditorElements; Style: TSelectListFormStyle; var vName: string; ImgList: TImageList = nil): Boolean;
function ShowSelectList(ACaption: string; vElements: TEditorElements; Style: TSelectListFormStyle; out vIndex: Integer; ImgList: TImageList = nil): Boolean;

implementation

uses
  mneResources, mneClasses;

{$R *.lfm}

function ShowSelectList(ACaption: string; vElements: TEditorElements; Style: TSelectListFormStyle; var vName: string; ImgList: TImageList): Boolean;
begin
  with TSelectListForm.Create(Application) do
  begin
    try
      Caption := ACaption;
      FStyle := Style;
      UpdateStyle;
      Elements := vElements;
      ItemsList.SmallImages := ImgList;
      ShowItems('', vName);
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

function ShowSelectList(ACaption: string; vElements: TEditorElements; Style: TSelectListFormStyle; out vIndex: Integer; ImgList: TImageList): Boolean;
begin
  with TSelectListForm.Create(Application) do
  begin
    try
      Caption := ACaption;
      FStyle := Style;
      UpdateStyle;
      Elements := vElements;
      ItemsList.SmallImages := ImgList;
      ShowItems;
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

procedure TSelectListForm.ShowItems(vFilter: string; vSelect: string);
var
  i, c, t: Integer;
  aItem: TListItem;
  procedure AddItem(vData: Pointer; Name, Title, Description: string; ImageIndex: Integer);
  begin
    if (vFilter = '') or ContainsText(Name, vFilter) or ContainsText(Title, vFilter) or ContainsText(Description, vFilter) then
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
    ItemsList.Items[t].MakeVisible(False);
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
  if ((Key >= VK_DELETE) or (Key >= VK_BACK) or (Key >= VK_SPACE)) and (FLastSearch <> SearchEdit.Text) then //* do not search at show dialog
    SearchTimer.Enabled := True;
end;

procedure TSelectListForm.SearchTimerTimer(Sender: TObject);
begin
  FLastSearch := SearchEdit.Text;
  ShowItems(FLastSearch);
  SearchTimer.Enabled := False;
end;

procedure TSelectListForm.DoShow;
begin
  inherited;
  ItemsList.Selected.MakeVisible(False); //ShowItems can not make it visible while the form not visible yet
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

