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
  mnMsgBox, mnStreams, ntvGrids, ntvPageControls, mncConnections, mncCSV, ntvTabSets, ntvTabs;

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
    procedure PageControlTabSelect(Sender: TObject; OldTab, NewTab: TntvTabItem; var CanSelect: boolean);
    procedure PageControlTabSelected(Sender: TObject; OldTab, NewTab: TntvTabItem);
    procedure ClearBtnClick(Sender: TObject);
    procedure OptionsBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
  private
    FOnChanged: TNotifyEvent;
    function GetMode: TCSVFileMode;
  protected
    FCancel: Boolean;
    FGridChanged: Boolean;
    IsNumbers: array of boolean;
    procedure Changed;
    procedure Reload;
  public
    FLoading: Boolean;
    EditorFile: TEditorFile;
    TextEdit: TmnSynEdit;
    IsRTL: Boolean;
    CSVOptions: TmncCSVOptions;
    FInteractive: Boolean;
    FConfigLoaded: Boolean;
    constructor Create(TheOwner: TComponent); override;
    procedure RenameHeader(Index: Integer);
    procedure RefreshControls;
    function IsConfigFileExists: Boolean;
    procedure SaveConfigFile;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;

    procedure ClearGrid;
    procedure FillGrid(SQLCMD: TmncCommand; Title: String);

    procedure LoadFromStream(AStream: TStream);
    procedure SaveToStream(AStream: TStream);
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
  Engine.Update([ecsFolder]);
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
  DeleteFile(EditorFile.Name + '.conf');
  RefreshControls;
  Engine.Update([ecsFolder]);
end;

procedure TCSVForm.MenuItem1Click(Sender: TObject);
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

procedure TCSVForm.MenuItem2Click(Sender: TObject);
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
  if MsgBox.Input(s, 'Enter rows count to add') then
  begin
    DataGrid.Count := DataGrid.Count + StrToIntDef(s, 0);
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

procedure TCSVForm.PageControlTabSelect(Sender: TObject; OldTab, NewTab: TntvTabItem; var CanSelect: boolean);
begin
  CanSelect := not FLoading;
end;

procedure TCSVForm.PageControlTabSelected(Sender: TObject; OldTab, NewTab: TntvTabItem);
var
  Mem: TMemoryStream;
begin
  if OldTab <> nil then //not first time changed, only when end user changed the tab/pagecontrol
  begin
    if Mode = csvmText then //New mode is Text, converting it from Grid
    begin
      Mem := TMemoryStream.Create;
      try
        SaveToStream(Mem);
        Mem.Position := 0;
        EditorFile.ContentsLoadFromStream(EditorFile.SynEdit, Mem);
      finally
        Mem.Free;
      end;
    end
    else
    begin
      Mem := TMemoryStream.Create;
      try
        EditorFile.ContentsSaveToStream(EditorFile.SynEdit, Mem);
        Mem.Position := 0;
        LoadFromStream(Mem);
      finally
        Mem.Free;
      end;
    end;
  end;
end;

procedure TCSVForm.ClearBtnClick(Sender: TObject);
begin
  if not MsgBox.No('Are you sure you want to clear it') then
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
  begin
    FOnChanged(Self);
    FGridChanged := True;
  end;
end;

procedure TCSVForm.Reload;
begin
end;

procedure TCSVForm.RenameHeader(Index: Integer);
var
  s: string;
begin
  s := DataGrid.Columns[Index].Title;
  if s = '' then
    s := 'Header' + IntToStr(Index);
  if MsgBox.Input(s, 'Rename column header') then
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
  Result := FileExists(EditorFile.Name + '.conf');
end;

procedure TCSVForm.SaveConfigFile;
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(EditorFile.Name + '.conf');
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
  DataGrid.ColumnsCount := 0;
end;

procedure TCSVForm.SaveToFile(FileName: string);
var
  aFile: TFileStream;
begin
  aFile := TFileStream.Create(EditorFile.Name, fmCreate or fmOpenWrite);
  try
    SaveToStream(aFile);
  finally
    aFile.Free;
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
  Ini: TIniFile;
begin
  if FConfigLoaded then
    b := True
  else
  begin
    b := IsConfigFileExists;
    if b then
      Ini := TIniFile.Create(EditorFile.Name + '.conf')
    else
      Ini := TIniFile.Create(Engine.WorkSpace + 'mne-csv-options.ini');
    try
      CSVOptions.LoadFromIni('options', Ini);
      IsRTL := Ini.ReadBool('ui', 'rtl', false);
    finally
      Ini.Free;
    end;
    FConfigLoaded := True;
  end;

  RefreshControls;

  if b or ShowCSVOptions('Export CSV', CSVOptions) then
  begin
    aFile := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      LoadFromStream(aFile);
    finally
      aFile.Free;
    end;

    Ini := TIniFile.Create(Engine.WorkSpace + 'mne-csv-options.ini');
    try
      CSVOptions.SaveToIni('options', Ini);
    finally
      Ini.Free;
    end;
  end;
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
  FGridChanged := False;
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
      if DataGrid.Count <= (c + 1) then
      begin
        if not FInteractive or (c >= Steps) then
          DataGrid.Count := c + Steps
        else
          DataGrid.Count := c + 1;
      end;

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
    DataGrid.Count := c;
    FetchCountLbl.Caption := IntToStr(c - 1);
  finally
    if not FInteractive then
      DataGrid.EndUpdate;
    StopBtn.Enabled := False;
  end;
end;

procedure TCSVForm.LoadFromStream(AStream: TStream);
var
  csvCnn: TmncCSVConnection;
  csvSes: TmncCSVSession;
  csvCMD: TmncCSVCommand;
  t: Int64;
begin
  t := GetTickCount64;
  FLoading := True;
  Screen.Cursor := crHourGlass;
  try
    ClearGrid;
    csvCnn := TmncCSVConnection.Create;
    csvSes := TmncCSVSession.Create(csvCnn);
    try
      csvSes.CSVOptions := CSVOptions;
      csvCnn.Connect;
      csvSes.Start;
      csvCMD := TmncCSVCommand.Create(csvSes, AStream, csvmRead);
      try
        if CSVOptions.SkipEmptyLines then
          csvCMD.EmptyLine := elSkip
        else
          csvCMD.EmptyLine := elFetch;

        if csvCMD.Execute then //not empty, or eof
          FillGrid(csvCMD, 'File: ' + EditorFile.Name);
      finally
        csvCMD.Free;
      end;
    finally
      csvSes.Free;
      csvCnn.Free;
    end;
  finally
    FLoading := False;
    Screen.Cursor := crDefault;
  end;
  {if DataGrid.Columns.Count >0 then
    DataGrid.Columns[1].Alignment := taRightJustify;}
  t := GetTickCount64 - t;
  Engine.SendLog(TicksToString(t));
end;

procedure TCSVForm.SaveToStream(AStream: TStream);
var
  csvCnn: TmncCSVConnection;
  csvSes: TmncCSVSession;
  csvCMD: TmncCSVCommand;
  c, r: Integer;
begin
  csvCnn := TmncCSVConnection.Create;
  csvSes := TmncCSVSession.Create(csvCnn);
  Screen.Cursor := crHourGlass;
  try
    csvSes.CSVOptions := CSVOptions;
    csvCnn.Connect;
    csvSes.Start;
    csvCMD := TmncCSVCommand.Create(csvSes, AStream, csvmWrite);
    try
      //adding header, even if we will not save it
      for c := 0 to DataGrid.Columns.Count - 1 do
      begin
        csvCMD.Columns.Add(DataGrid.Columns[c].Title, dtString);
      end;
      csvCMD.Prepare; //generate Params and save header
      r := 0; //first row of data
      while r < DataGrid.Count do
      begin
        for c := 0 to DataGrid.Columns.Count - 1 do
        begin
          csvCMD.Params.Items[c].Value := DataGrid.Values[c, r];
        end;
        csvCMD.Execute;
        r := r + 1;
      end;
    finally
      csvCMD.Free;
    end;
  finally
    csvSes.Free;
    csvCnn.Free;
    Screen.Cursor := crDefault;
  end;
  if IsConfigFileExists then
    SaveConfigFile;
end;

constructor TCSVForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  //FInteractive := True; //ewww

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

