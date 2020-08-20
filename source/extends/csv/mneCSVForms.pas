unit mneCSVForms;
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
  Classes, SysUtils, Forms, Controls, Grids, ExtCtrls, StdCtrls,
  FileUtil, LCLType, Graphics, Menus, Buttons, EditorEngine, IniFiles,
  MsgBox, mnStreams, ntvGrids, ntvPageControls, mncConnections, mncCSV, ntvTabSets, ntvTabs;

type
  TCSVFileMode = (csvmGrid, csvmText);

  { TCSVForm }

  TCSVForm = class(TFrame, IEditorControl)
    ClearBtn: TButton;
    DataGrid: TntvGrid;
    DelConfigFileBtn: TButton;
    FetchCountLbl: TLabel;
    FetchedLbl: TLabel;
    GridPnl: TPanel;
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
    OptionsBtn: TButton;
    PageControl: TntvPageControl;
    Panel2: TPanel;
    SaveConfigFileBtn: TButton;
    TextPnl: TPanel;
    StopBtn: TButton;
    procedure ConfigFileBtnClick(Sender: TObject);
    procedure DataGridChanged(Sender: TObject);
    procedure DataGridColClick(Sender: TntvCustomGrid; Column: TntvColumn);
    procedure DelConfigFileBtnClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure IsRtlMnuClick(Sender: TObject);
    procedure PageControlTabSelected(Sender: TObject; OldTab, NewTab: TntvTabItem);
    procedure ClearBtnClick(Sender: TObject);
    procedure OptionsBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
  private
    FOnChanged: TNotifyEvent;
    function GetMode: TCSVFileMode;
  protected
    FCancel: Boolean;
    IsNumbers: array of boolean;
    FFileName: string;
    procedure Changed;
    procedure ConvertToTextEdit;
    procedure ConvertToGrid;
  public
    EditorFile: TEditorFile;
    TextEdit: TmnSynEdit;
    IsRTL: Boolean;
    CSVOptions: TmncCSVOptions;
    FInteractive: Boolean;
    FLoading: Boolean;
    constructor Create(TheOwner: TComponent); override;
    procedure RenameHeader(Index: Integer);
    procedure RefreshControls;
    function IsConfigFileExists: Boolean;
    procedure SaveConfigFile;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    procedure ClearGrid;
    procedure FillGrid(SQLCMD: TmncCommand; Title: String);
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    procedure Load(FileName: string);
    procedure Save(FileName: string);
    property Mode: TCSVFileMode read GetMode;
  end;

implementation

uses
  mnUtils, CSVOptionsForms, EditorClasses;

{$R *.lfm}

procedure RemoveRows(Grid: TStringGrid; RowIndex, vCount: Integer);
var
  i: Integer;
begin
  with Grid do
  begin
    BeginUpdate;
    try
      for i := RowIndex to RowCount - vCount - 1 do
        Rows[i] := Rows[i + vCount];
      RowCount := RowCount - vCount;
    finally
      EndUpdate;
    end;
  end;
end;

{ TCSVForm }

procedure TCSVForm.ConfigFileBtnClick(Sender: TObject);
begin
  SaveConfigFile;
  RefreshControls;
  Engine.UpdateState([ecsFolder]);
end;

procedure TCSVForm.DataGridChanged(Sender: TObject);
begin
  Changed;
end;

procedure TCSVForm.DataGridColClick(Sender: TntvCustomGrid; Column: TntvColumn);
begin
  RenameHeader(Column.Index);
end;

procedure TCSVForm.DelConfigFileBtnClick(Sender: TObject);
begin
  DeleteFile(FFileName + '.conf');
  RefreshControls;
  Engine.UpdateState([ecsFolder]);
end;

procedure TCSVForm.MenuItem1Click(Sender: TObject);
var
  r: Integer;
begin
  if not Msg.No('Are you sure you want to clear cells') then
  begin
    with DataGrid do
    begin
      BeginUpdate;
      try
        for r := Selected.StartRow to Selected.EndRow do
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

procedure TCSVForm.MenuItem2Click(Sender: TObject);
var
  s: string;
  i: Integer;
begin
  s := '1';
  if Msg.Input(s, 'Enter columns count to add') then
  begin
    for i := 0 to StrToInt(s) -1 do
      DataGrid.AddColumn;
    Changed;
  end;
end;

procedure TCSVForm.MenuItem3Click(Sender: TObject);
begin
  DataGrid.DeleteColumn(DataGrid.CurrentColumn.Index);
  //RemoveCols(DataGrid, DataGrid.Selection.Left,DataGrid.Selection.Right - DataGrid.Selection.Left + 1);
  Changed;
end;

procedure TCSVForm.MenuItem4Click(Sender: TObject);
var
  s: string;
begin
  s := '1';
  if Msg.Input(s, 'Enter rows count to add') then
  begin
    DataGrid.RowsCount := DataGrid.RowsCount + StrToIntDef(s, 0);
    Changed;
  end;
end;

procedure TCSVForm.MenuItem5Click(Sender: TObject);
begin
  //DataGrid.DeleteRow(DataGrid.Row);
  //RemoveRows(DataGrid, DataGrid.Selection.Top, DataGrid.Selection.Bottom - DataGrid.Selection.Top + 1);
  Changed;
end;

procedure TCSVForm.MenuItem6Click(Sender: TObject);
begin
  //DataGrid.CopyToClipboard(True);
end;

procedure TCSVForm.MenuItem8Click(Sender: TObject);
begin
  RenameHeader(DataGrid.Current.Col);
end;

procedure TCSVForm.IsRtlMnuClick(Sender: TObject);
begin
  IsRTL := IsRtlMnu.Checked;
  RefreshControls;
  Changed;
end;

procedure TCSVForm.PageControlTabSelected(Sender: TObject; OldTab, NewTab: TntvTabItem);
begin
  if Mode = csvmText then
    ConvertToTextEdit
  else
    ConvertToGrid;
end;

procedure TCSVForm.ClearBtnClick(Sender: TObject);
begin
  if not Msg.No('Are you sure you want to clear it') then
  begin
    ClearGrid;
    Changed;
  end;
end;

procedure TCSVForm.OptionsBtnClick(Sender: TObject);
var
  aCSVOptions: TmncCSVOptions;
begin
  aCSVOptions := CSVOptions;
  if ShowCSVOptions('Export CSV', aCSVOptions) then
  begin
    CSVOptions := aCSVOptions;
    Changed;
  end
end;

procedure TCSVForm.StopBtnClick(Sender: TObject);
begin
  FCancel := True;
end;

function TCSVForm.GetMode: TCSVFileMode;
begin
  if PageControl.ActiveControl = TextPnl then
    Result := csvmText
  else if PageControl.ActiveControl = GridPnl then
    Result := csvmGrid
  else
    Result := csvmGrid;
end;

procedure TCSVForm.Changed;
begin
  if not FLoading and Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TCSVForm.ConvertToTextEdit;
begin
  EditorFile.Save;
  EditorFile.Load;
end;

procedure TCSVForm.ConvertToGrid;
begin
  EditorFile.Save;
  EditorFile.Load;
end;

procedure TCSVForm.RenameHeader(Index: Integer);
var
  s: string;
begin
  s := DataGrid.Columns[Index].Title;
  if s = '' then
    s := 'Header' + IntToStr(Index);
  if MsgBox.Msg.Input(s, 'Rename column header') then
  begin
    DataGrid.Columns[Index].Title := s;
    Changed;
  end;
end;

procedure TCSVForm.RefreshControls;
begin
  DelConfigFileBtn.Visible := IsConfigFileExists;
  SaveConfigFileBtn.Visible := not DelConfigFileBtn.Visible;
  IsRTLMnu.Checked := IsRTL;
  if IsRTL then
    DataGrid.BiDiMode := bdRightToLeft
  else
    DataGrid.BiDiMode := bdLeftToRight;
end;

function TCSVForm.IsConfigFileExists: Boolean;
begin
  Result := FileExists(FFileName + '.conf');
end;

procedure TCSVForm.SaveConfigFile;
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(FFileName + '.conf');
  try
    CSVOptions.SaveToIni('options', ini);
    ini.WriteBool('ui', 'rtl', IsRTL);
  finally
    ini.Free;
  end;
end;

procedure TCSVForm.ClearGrid;
begin
  IsNumbers := nil;
  DataGrid.Reset;
end;

procedure TCSVForm.SaveToFile(FileName: string);
var
  aFile: TFileStream;
  csvCnn: TmncCSVConnection;
  csvSes: TmncCSVSession;
  csvCMD: TmncCSVCommand;
  c, r: Integer;
begin
  FFileName := FileName;
  csvCnn := TmncCSVConnection.Create;
  csvSes := TmncCSVSession.Create(csvCnn);
  try
    csvSes.CSVOptions := CSVOptions;
    csvCnn.Connect;
    csvSes.Start;
    aFile := TFileStream.Create(FileName, fmCreate or fmOpenWrite);
    csvCMD := TmncCSVCommand.Create(csvSes, aFile, csvmWrite);
    try
      //adding header, even if we will not save it
      for c := 0 to DataGrid.Columns.Count - 1 do
      begin
        csvCMD.Columns.Add(DataGrid.Columns[c].Title, dtString);
      end;
      csvCMD.Prepare; //generate Params and save header
      r := 0; //first row of data
      while r < DataGrid.RowsCount do
      begin
        for c := 0 to DataGrid.Columns.Count - 1 do
        begin
          csvCMD.Params.Items[c].Value := DataGrid.Values[c, r];
        end;
        csvCMD.Execute;
        r := r + 1;
      end;

    finally
      aFile.Free;
    end
  finally
    csvSes.Free;
    csvCnn.Free;
  end;
  if IsConfigFileExists then
    SaveConfigFile;
end;

procedure TCSVForm.Load(FileName: string);
begin
  LoadFromFile(FileName);
end;

procedure TCSVForm.Save(FileName: string);
begin
  SaveToFile(FileName);
end;

procedure TCSVForm.LoadFromFile(FileName: string);
var
  aFile: TFileStream;
  b: Boolean;
  csvCnn: TmncCSVConnection;
  csvSes: TmncCSVSession;
  csvCMD: TmncCSVCommand;
  Ini: TIniFile;
begin
  FFileName := FileName;

  FLoading := True;
  try
    ClearGrid;
    csvCnn := TmncCSVConnection.Create;
    csvSes := TmncCSVSession.Create(csvCnn);
    try
      b := IsConfigFileExists;
      if b then
        Ini := TIniFile.Create(FFileName + '.conf')
      else
        Ini := TIniFile.Create(Engine.WorkSpace + 'mne-csv-options.ini');
      try
        CSVOptions.LoadFromIni('options', Ini);
        IsRTL := Ini.ReadBool('ui', 'rtl', false);
      finally
        Ini.Free;
      end;

      RefreshControls;

      if b or ShowCSVOptions('Export CSV', CSVOptions) then
      begin
        csvSes.CSVOptions := CSVOptions;
        csvCnn.Connect;
        csvSes.Start;
        aFile := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
        csvCMD := TmncCSVCommand.Create(csvSes, aFile, csvmRead);
        csvCMD.EmptyLine := elSkip;
        try
          if csvCMD.Execute then //not empty, or eof
            FillGrid(csvCMD, 'File: ' + FileName);
        finally
          aFile.Free;
          csvCMD.Free;
        end;

        Ini := TIniFile.Create(Engine.WorkSpace + 'mne-csv-options.ini');
        try
          CSVOptions.SaveToIni('options', Ini);
        finally
          Ini.Free;
        end;
      end;
    finally
      csvSes.Free;
      csvCnn.Free;
    end;
  finally
    FLoading := False;
  end;
  {if DataGrid.Columns.Count >0 then
    DataGrid.Columns[1].Alignment := taRightJustify;}
end;

procedure TCSVForm.FillGrid(SQLCMD: TmncCommand; Title: String);

  function GetTextWidth(Text: String): Integer;
  begin
    Result := DataGrid.Canvas.TextWidth(Text);
  end;

  function GetCharWidth: Integer;
  begin
    Result := (GetTextWidth('Wi') div 2);
  end;

var
  i, c, w: Integer;
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
      cols := SQLCMD.Fields.Count;
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
    c := 0;
    CalcWidths;

    if FInteractive then
      Application.ProcessMessages;

    while not SQLCMD.Done do
    begin
      if DataGrid.RowsCount <= (c + 1) then
      begin
        if not FInteractive or (c >= Steps) then
          DataGrid.RowsCount := c + Steps
        else
          DataGrid.RowsCount := c + 1;
      end;
      DataGrid.Values[0, c] := IntToStr(c);
      for i := 0 to cols - 1 do
      begin
        if i < SQLCMD.Fields.Count then
        begin
          str := SQLCMD.Fields.Items[i].AsString;
          if length(str) > max[i] then
            max[i] := length(str);
          DataGrid.Values[startCol + i, c] := str;
        end;
      end;
      Inc(c);
      //before 100 rows will see the grid row by row filled, cheeting the eyes of user
      if (c < Steps) or (Frac(c / Steps) = 0) then
      begin
        if FInteractive then
        begin
          FetchCountLbl.Caption := IntToStr(c - 1);
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
        else }if c > 500 then
          steps := 500;
      end;
      if FCancel then
        break;
      SQLCMD.Next;
    end;
    CalcWidths;
    DataGrid.RowsCount := c;
    FetchCountLbl.Caption := IntToStr(c - 1);
  finally
    if not FInteractive then
      DataGrid.EndUpdate;
    StopBtn.Enabled := False;
  end;
end;

constructor TCSVForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  PageControl.ItemIndex := 0;
  TextEdit := TmnSynEdit.Create(Self);
  TextEdit.Parent := TextPnl;
  TextEdit.Align := alClient;
  TextEdit.Visible := True;

  Initialize(CSVOptions);
  CSVOptions.HeaderLine := hdrNormal;
  CSVOptions.DelimiterChar := ',';
  CSVOptions.EndOfLine := sUnixEndOfLine;
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

end.

