unit mneSelectComponents;
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
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Grids, Types, EditorEngine, mnClasses, mneBoardComponents;

type
  { TSelectComponentForm }

  TSelectComponentForm = class(TForm)
    FilterEdit: TEdit;
    OkBtn: TButton;
    CancelBtn: TButton;
    ItemsList: TStringGrid;
    procedure ItemsListDblClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure ItemsListDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
  private
  public
    Elements: TBoardComponents;
    Items: array of string;
    procedure ShowItems(vSelect: string);
  end;

function ShowSelectComponent(ACaption: string; vElements: TBoardComponents; var vName: string): Boolean;
function ShowSelectComponent(ACaption: string; vElements: TBoardComponents; out vIndex: Integer): Boolean;

implementation

uses
  mneResources, mneClasses;

{$R *.lfm}

function ShowSelectComponent(ACaption: string; vElements: TBoardComponents; var vName: string): Boolean;
begin
  with TSelectComponentForm.Create(Application) do
  begin
    try
      Caption := ACaption;
      Elements := vElements;
      ShowItems(vName);
      Result := (ShowModal = mrOK);
      if Result then
        vName := Items[ItemsList.Row];
      Free;
    finally
    end;
  end;
end;

function ShowSelectComponent(ACaption: string; vElements: TBoardComponents; out vIndex: Integer): Boolean;
begin
  with TSelectComponentForm.Create(Application) do
  begin
    try
      Caption := ACaption;
      Elements := vElements;
      ShowItems('');
      Result := (ShowModal = mrOK);
      if Result then
        vIndex := ItemsList.Row;
      Free;
    finally
    end;
  end;
end;

procedure TSelectComponentForm.ItemsListDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TSelectComponentForm.ShowItems(vSelect: string);
var
  i, c, t: Integer;
  procedure AddItem(Name, Title: string);
  begin
    ItemsList.Cells[0, c] := Name;
    ItemsList.Cells[1, c] := Title;

    if SameText(vSelect, Name) then
      t := c;
    inc(c);
  end;
begin
  ItemsList.BeginUpdate;
  try
    ItemsList.Clear;
    ItemsList.RowCount := Elements.Count;
    ItemsList.ColWidths[0] := 64;
    c := 0;
    t := 0;
    for i := 0 to Elements.Count - 1 do
      AddItem(Elements[i].Name, Elements[i].Caption);
  finally
    ItemsList.EndUpdate;
  end;
  if ItemsList.RowCount > 0 then
  begin
    ItemsList.Row := t;
  end;
end;

procedure TSelectComponentForm.OkBtnClick(Sender: TObject);
begin
 ModalResult := mrOK
end;

procedure TSelectComponentForm.ItemsListDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  aCanvas: TCanvas;
  Element: TBoardComponent;
begin
  if (aCol = 0) then
  begin
    aCanvas := ItemsList.Canvas;
    Element := Elements[aRow];
    Element.CheckImage;
    aCanvas.Draw(aRect.Left, aRect.Top, Element.Image);
  end;
end;

end.

