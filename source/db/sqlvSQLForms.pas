unit sqlvSQLForms;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{*TODO
  * No header grid
  * Sort column
  * Options on Tendency
  * View as text
  * Find and Replace
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, FileUtil,
  mncSQL,
  LCLType, Graphics, Menus, Buttons, ComCtrls, EditorEngine, sqlvManagerForms, ParamsForms,
  sqlvEngines, IniFiles, ntvTabSets, mnMsgBox, mnStreams, ntvGrids,
  ntvPageControls, mncConnections, mncCSV;

type

  { TSQLEditForm }

  TSQLEditForm = class(TFrame, IEditorControl)
    DataGrid: TntvGrid;
    FetchCountLbl: TLabel;
    FetchedLbl: TLabel;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    IsRtlMnu: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    GridPopupMenu: TPopupMenu;
    PageControl: TntvPageControl;
    SQLPnl: TPanel;
    DataPnl: TPanel;
    Panel4: TPanel;
    StopBtn: TButton;
    ClearBtn: TButton;
    CommitBtn: TButton;
    RollbackBtn: TButton;
    procedure ConfigFileBtnClick(Sender: TObject);
    procedure ControlPagesChange(Sender: TObject);
    procedure DataGridChanged(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure IsRtlMnuClick(Sender: TObject);
    procedure StopBtn2Click(Sender: TObject);
    procedure OptionsBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
  private
    FOnChanged: TNotifyEvent;
  protected
    FCancel: Boolean;
    IsNumbers: array of boolean;
    FFileName: string;
    procedure Changed;
  public
    SQLEdit: TmnSynEdit;
    IsRTL: Boolean;
    FInteractive: Boolean;
    FLoading: Boolean;
    constructor Create(TheOwner: TComponent); override;
    procedure RefreshControls;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    procedure ClearGrid;
    procedure FillGrid(SQLCMD: TmncCommand; Title: String; MergeColumns: Boolean = False);
    procedure Execute;
  end;

implementation

uses
  mnUtils, CSVOptionsForms, EditorClasses;

{$R *.lfm}

{ TSQLEditForm }

procedure TSQLEditForm.ConfigFileBtnClick(Sender: TObject);
begin
end;

procedure TSQLEditForm.ControlPagesChange(Sender: TObject);
begin

end;

procedure TSQLEditForm.DataGridChanged(Sender: TObject);
begin
  Changed;
end;

procedure TSQLEditForm.MenuItem1Click(Sender: TObject);
var
  r: Integer;
begin
  if not MsgBox.No('Are you sure you want to clear cells') then
  begin
    with DataGrid do
    begin
      BeginUpdate;
      try
        for r := Selected.Start.Row to Selected.Stop.Row do
          begin
            ClearRow(r);
          end;
      finally
        EndUpdate;
      end;
    end;
    Changed;
  end;
end;

procedure TSQLEditForm.MenuItem2Click(Sender: TObject);
var
  s: string;
  i: Integer;
begin
  s := '1';
  if MsgBox.Input(s, 'Enter columns count to add') then
  begin
    for i := 0 to StrToInt(s) -1 do
      DataGrid.AddColumn;
    Changed;
  end;
end;

procedure TSQLEditForm.MenuItem3Click(Sender: TObject);
begin
  DataGrid.DeleteColumn(DataGrid.CurrentColumn.Index);
  //RemoveCols(DataGrid, DataGrid.Selection.Left,DataGrid.Selection.Right - DataGrid.Selection.Left + 1);
  Changed;
end;

procedure TSQLEditForm.MenuItem4Click(Sender: TObject);
var
  s: string;
begin
  s := '1';
  if MsgBox.Input(s, 'Enter rows count to add') then
  begin
    DataGrid.RowsCount := DataGrid.RowsCount + StrToIntDef(s, 0);
    Changed;
  end;
end;

procedure TSQLEditForm.MenuItem5Click(Sender: TObject);
begin
  //DataGrid.DeleteRow(DataGrid.Row);
  //RemoveRows(DataGrid, DataGrid.Selection.Top, DataGrid.Selection.Bottom - DataGrid.Selection.Top + 1);
  Changed;
end;

procedure TSQLEditForm.MenuItem6Click(Sender: TObject);
begin
  //DataGrid.CopyToClipboard(True);
end;

procedure TSQLEditForm.IsRtlMnuClick(Sender: TObject);
begin
  IsRTL := IsRtlMnu.Checked;
  RefreshControls;
  Changed;
end;

procedure TSQLEditForm.StopBtn2Click(Sender: TObject);
begin
  if not MsgBox.No('Are you sure you want to clear it') then
  begin
    ClearGrid;
    Changed;
  end;
end;

procedure TSQLEditForm.OptionsBtnClick(Sender: TObject);
begin
end;

procedure TSQLEditForm.StopBtnClick(Sender: TObject);
begin
  FCancel := True;
end;

procedure TSQLEditForm.Changed;
begin
  if not FLoading and Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TSQLEditForm.RefreshControls;
begin
  IsRTLMnu.Checked := IsRTL;
  if IsRTL then
    DataGrid.BiDiMode := bdRightToLeft
  else
    DataGrid.BiDiMode := bdLeftToRight;
end;

procedure TSQLEditForm.ClearGrid;
begin
  IsNumbers := nil;
  DataGrid.Reset;
end;

procedure TSQLEditForm.FillGrid(SQLCMD: TmncCommand; Title: String; MergeColumns: Boolean);

  function GetTextWidth(Text: String): Integer;
  begin
    Result := DataGrid.Canvas.TextWidth(Text);
  end;

  function GetCharWidth: Integer;
  begin
    Result := (GetTextWidth('Wi') div 2);
  end;

var
  i, r, w: Integer;
  s: String;
  str: string;
  startCol: integer;
  cols: Integer;
  max: array of integer;
  procedure CalcWidths; //stupid idea
  var
    i, m: Integer;
  begin
    for i := 0 to cols - 1 do
    begin
      m := max[i];
      if m > 36 then
        m := 36;
      w := GetTextWidth(StringOfChar('W', m)); //aaaaaaaaaaaaaaaaa
      if w < 40 then
        w := 40;
      DataGrid.Columns[startCol + i].Width := w;
    end;
  end;
var
  Steps: Integer;
  HaveHeader: Boolean;
begin
  StopBtn.Enabled := True;
  Steps := 100;
  if not FInteractive then
    DataGrid.BeginUpdate;
  try
    if not MergeColumns then
    begin
      DataGrid.ColumnsCount := 0;
      DataGrid.Clear;
      DataGrid.Rows.Clear;
    end;
    IsNumbers := nil;
    if Title = '' then
      Caption := 'Data'
    else
      Caption := 'Data: ' + Title;

    FetchedLbl.Caption := 'Fetched: ';
    max := nil;
    FCancel := False;

    cols := SQLCMD.Columns.Count;
    HaveHeader := cols > 0;
    if not HaveHeader then
    begin
      cols := SQLCMD.Columns.Count;
    end;
    setLength(max, cols);
    setLength(IsNumbers, cols);

    startCol := DataGrid.Columns.Count;
    DataGrid.ColumnsCount := startCol + cols;
    DataGrid.Current.Col := startCol;

    if HaveHeader then
    begin
      for i := 0 to cols - 1 do
      begin
        s := SQLCMD.Columns[i].Name;
        max[i] := length(s);
        DataGrid.Columns[startCol + i].Title := s;
        IsNumbers[i] := SQLCMD.Columns[i].IsNumber;
      end;
    end;
    r := 0;
    CalcWidths;
    if FInteractive then
      Application.ProcessMessages;

    while not SQLCMD.Done do
    begin
      if DataGrid.RowsCount <= r then
      begin
        if not FInteractive or (r >= Steps) then
          DataGrid.RowsCount := r + Steps
        else
          DataGrid.RowsCount := r;
      end;
      for i := 0 to cols - 1 do
      begin
        if i < SQLCMD.Fields.Count then
        begin
          str := SQLCMD.Fields.Items[i].AsString;
          if length(str) > max[i] then
            max[i] := length(str);
          DataGrid.Values[startCol + i, r] := str;
        end;
      end;
      Inc(r);
      //before 100 rows will see the grid row by row filled, cheeting the eyes of user
      if (r < Steps) or (Frac(r / Steps) = 0) then
      begin
        if FInteractive then
        begin
          FetchCountLbl.Caption := IntToStr(r);
          CalcWidths;
        end;
        Application.ProcessMessages;
        {if c > 100000 then
          steps := 100000
        else
        if c > 10000 then
          steps := 10000
        else
        if c > 2500 then
          steps := 1000
        else }if r > 500 then
          steps := 500;
      end;
      if FCancel then
        break;
      SQLCMD.Next;
    end;
    CalcWidths;
    if not MergeColumns then
    begin
      DataGrid.RowsCount := r;
      DataGrid.Capacity := r;
    end;
    FetchCountLbl.Caption := IntToStr(r);
  finally
    if not FInteractive then
      DataGrid.EndUpdate;
    StopBtn.Enabled := False;
  end;
end;

constructor TSQLEditForm.Create(TheOwner: TComponent);
begin
  inherited;
  PageControl.ItemIndex := 0;
  SQLEdit := TmnSynEdit.Create(Self);
  SQLEdit.Parent := SQLPnl;
  SQLEdit.Align := alClient;
  SQLEdit.Visible := True;

  Color := Engine.Options.Profile.Attributes.Default.Background;
  Font.Color := Engine.Options.Profile.Attributes.Default.Foreground;
  DataGrid.Color := Engine.Options.Profile.Attributes.Panel.Background;
  DataGrid.Font.Color := Engine.Options.Profile.Attributes.Default.Foreground;
  DataGrid.FixedColor := Engine.Options.Profile.Attributes.Gutter.Background;
  DataGrid.FixedFontColor := Engine.Options.Profile.Attributes.Gutter.Foreground;
  DataGrid.LinesColor := Engine.Options.Profile.Attributes.Separator.Background;
  DataGrid.EvenColor := Engine.Options.Profile.Attributes.Default.Background;
  DataGrid.OddColor := Engine.Options.Profile.Attributes.Default.Background;
end;

procedure TSQLEditForm.Execute;
var
  CMD: TmncSQLCommand;
  Session: TmncSQLSession;
begin
  if DBEngine.DB.IsActive then
  begin
    Session := DBEngine.DB.Connection.CreateSession;
    CMD := Session.CreateCommand;
    CMD.SQL.Text := SQLEdit.Text;
    try
      Session.Start;
      try
        CMD.Prepare;
        if (CMD.Params.Count = 0) or ShowSQLParams(CMD) then
        begin
          CMD.Execute;
          PageControl.ActiveControl := DataPnl;
          FillGrid(CMD, 'Data');
        end;
        Session.Commit;
      except
        on E: Exception do
        begin
          Engine.SendLog(E.Message);
          Session.Rollback;
          raise;
        end;
      end;
    finally
      if CMD.Active then
        CMD.Close;
      CMD.Free;
    end;
  end;
end;

end.

