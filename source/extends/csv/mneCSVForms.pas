unit mneCSVForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Grids, ExtCtrls, StdCtrls,
  LCLType, Graphics, Menus, EditorEngine,
  mnStreams, mncConnections, mncCSV;

type

  { TCSVForm }

  TCSVForm = class(TFrame)
    DataGrid: TStringGrid;
    FetchCountLbl: TLabel;
    FetchedLbl: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    Panel2: TPanel;
    GridPopupMenu: TPopupMenu;
    StopBtn: TButton;
    StopBtn2: TButton;

    procedure DataGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure DataGridEditingDone(Sender: TObject);

    procedure DataGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure StopBtn2Click(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
  private
    FOnChanged: TNotifyEvent;
  protected
    FCancel: Boolean;
    IsNumbers: array of boolean;
    procedure Changed;
  public
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    procedure ClearGrid;
    procedure Load(FileName: string);
    procedure Save(FileName: string);
    procedure FillGrid(SQLCMD: TmncCommand; Title: String; Append: Boolean = False);

  end;

implementation

uses
  CSVOptionsForms;

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

procedure RemoveCols(Grid: TStringGrid; ColIndex, vCount: Integer);
var
  i: Integer;
begin
  with Grid do
  begin
    BeginUpdate;
    try
      for i := ColIndex to ColCount - vCount - 1 do
        Cols[i] := Cols[i + vCount];
      ColCount := ColCount - vCount;
    finally
      EndUpdate;
    end;
  end;
end;

{ TCSVForm }

procedure TCSVForm.DataGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  //aState := aState + [gdSelected];
  if (aRow < DataGrid.FixedRows) or (aCol < DataGrid.FixedCols) then
  begin
    DataGrid.Canvas.Brush.Color := DataGrid.FixedColor;
    DataGrid.Canvas.FillRect(aRect);
  end
  else if ((aRow = DataGrid.Row) and (aCol = DataGrid.Col)) or
          ((aRow >= DataGrid.Selection.Top) and (aRow <= DataGrid.Selection.Bottom) and (aCol >= DataGrid.Selection.Left) and (aCol <= DataGrid.Selection.Right))
    then
  begin
    DataGrid.Canvas.Brush.Color := $00D8A276;
    DataGrid.Canvas.FillRect(aRect);
    //DataGrid.Canvas.Font.Color := clWhite;
  end
  else if (aRow = DataGrid.Row) then
  begin
    DataGrid.Canvas.Brush.Color := $00E0C6A3;
    DataGrid.Canvas.FillRect(aRect);
  end;
  DataGrid.DefaultDrawCell(aCol, aRow, aRect, aState);
end;

procedure TCSVForm.DataGridEditingDone(Sender: TObject);
begin
  Changed;
end;

procedure TCSVForm.DataGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_C) and (Shift = [ssCtrl]) then
    DataGrid.CopyToClipboard(True)
end;

procedure TCSVForm.MenuItem1Click(Sender: TObject);
begin
  ClearGrid;
end;

procedure TCSVForm.MenuItem2Click(Sender: TObject);
begin

end;

procedure TCSVForm.MenuItem3Click(Sender: TObject);
begin
  //DataGrid.DeleteCol(DataGrid.Col);
  RemoveCols(DataGrid, DataGrid.Selection.Left,DataGrid.Selection.Right - DataGrid.Selection.Left + 1);
  Engine.Files.Edited;
end;

procedure TCSVForm.MenuItem5Click(Sender: TObject);
begin
  //DataGrid.DeleteRow(DataGrid.Row);
  RemoveRows(DataGrid, DataGrid.Selection.Top,DataGrid.Selection.Bottom - DataGrid.Selection.Top + 1);
  Engine.Files.Edited;
end;

procedure TCSVForm.MenuItem6Click(Sender: TObject);
begin
  DataGrid.CopyToClipboard(True);
end;

procedure TCSVForm.StopBtn2Click(Sender: TObject);
begin
  ClearGrid;
end;

procedure TCSVForm.StopBtnClick(Sender: TObject);
begin
  FCancel := True;
end;

procedure TCSVForm.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TCSVForm.ClearGrid;
begin
  IsNumbers := nil;
  DataGrid.ColWidths[0] := 20;
  DataGrid.Row := 1;
  DataGrid.Col := 1;
  DataGrid.FixedCols := 1;
  DataGrid.FixedRows := 1;
  DataGrid.ColCount := 1;
  DataGrid.RowCount := 1;
  DataGrid.Cells[0, 0] := '';
  Engine.Files.Edited;
end;

procedure TCSVForm.Save(FileName: string);
var
  aFile: TFileStream;
  r, c:Integer;
  s: string;
begin
  aFile := TFileStream.Create(FileName, fmCreate or fmOpenWrite);
  try
    for r := 0 to DataGrid.RowCount -1 do
    begin
      s := '';
      for c := 1 to DataGrid.ColCount -1 do
      begin
        if c>1 then
          s := s + ';';
        s := s + DataGrid.Cells[c, r];
      end;

      s := s + sWinEndOfLine;
      aFile.WriteBuffer(Pointer(S)^, length(s));
    end;
  finally
    aFile.Free;
  end;
end;

procedure TCSVForm.Load(FileName: string);
var
  aFile: TFileStream;
  r, c:Integer;
  s: string;
  csvCnn: TmncCSVConnection;
  csvSes: TmncCSVSession;
  csvCMD: TmncCSVCommand;
  aCSVOptions: TmncCSVOptions;
begin
  ClearGrid;
  csvCnn := TmncCSVConnection.Create;
  csvSes := TmncCSVSession.Create(csvCnn);
  try
    FillByte(aCSVOptions, Sizeof(aCSVOptions), 0);
    aCSVOptions.HeaderLine := hdrNormal;
    aCSVOptions.DelimiterChar := ',';
    aCSVOptions.EndOfLine := sUnixEndOfLine;
    if ShowCSVOptions('Export CSV', aCSVOptions) then
    begin
      csvSes.CSVOptions := aCSVOptions;
      csvCnn.Connect;
      csvSes.Start;
      aFile := TFileStream.Create(FileName, fmOpenRead);
      csvCMD := TmncCSVCommand.Create(csvSes, aFile, csvmRead);
      csvCMD.EmptyLine := elSkip;
      try
        csvCMD.Execute;
        FillGrid(csvCMD, 'File: ' + FileName);
      finally
        aFile.Free;
        csvCMD.Free;
      end;
    end;
  finally
    csvSes.Free;
    csvCnn.Free;
  end;
end;

procedure TCSVForm.FillGrid(SQLCMD: TmncCommand; Title: String; Append: Boolean);

  function GetTextWidth(Text: String): Integer;
  begin
    DataGrid.Canvas.Font := DataGrid.Font;
    Result := DataGrid.Canvas.TextWidth(Text);
  end;

  function GetCharWidth: Integer;
  begin
    Result := (GetTextWidth('Wi') div 2);
  end;

var
  i, c, w: Integer;
  s: String;
  b: Boolean;
  str: utf8string;
  startCol: integer;
  cols: Integer;
  max: array of integer;
  procedure CalcWidths;
  var
    i: Integer;
  begin
    max[0] := length(IntToStr(c));
    for i := 0 to cols do
    begin
      w := GetTextWidth(StringOfChar('W', max[i])) + 10;
      if w < 10 then
        w := 10;
      DataGrid.ColWidths[startCol + i] := w;
    end;
  end;
var
  Steps: Integer;
begin
  DataGrid.FocusColor := clBlack;
  StopBtn.Enabled := True;
  Steps := 100;
  try
    IsNumbers := nil;
    if Title = '' then
      Caption := 'Data'
    else
      Caption := 'Data: ' + Title;

    FetchedLbl.Caption := 'Fetched:';
    max := nil;
    FCancel := False;

    cols := SQLCMD.Columns.Count;
    setLength(max, cols + 1);
    setLength(IsNumbers, cols + 1);

    if Append then
    begin
      startCol := 0;
      DataGrid.Col := startCol;
      c := DataGrid.RowCount;
      for i := 0 to cols - 1 do
      begin
        s := SQLCMD.Columns[i].Name;
        max[i + 1] := length(s);
        IsNumbers[i] := SQLCMD.Columns[i].IsNumber;
      end;
    end
    else
    begin
      startCol := DataGrid.ColCount - 1;
      DataGrid.ColCount := startCol + cols + 1; //1 for fixed col
      DataGrid.Col := startCol;

      for i := 0 to cols - 1 do
      begin
        s := SQLCMD.Columns[i].Name;
        max[i + 1] := length(s);
        DataGrid.Cells[startCol + i + 1, 0] := s;
        b := SQLCMD.Columns[i].IsNumber;
        IsNumbers[i] := b;
      end;
      c := 1;
      CalcWidths;
    end;
    Application.ProcessMessages;

    while not SQLCMD.Done do
    begin
      if DataGrid.RowCount <= (c + 1) then
      begin
        if (c >= Steps) then
          DataGrid.RowCount := c + Steps
        else
          DataGrid.RowCount := c + 1;
      end;
      DataGrid.Cells[0, c] := IntToStr(c);
      for i := 0 to cols - 1 do
      begin
        if i < SQLCMD.Fields.Count then
        begin
          str := SQLCMD.Fields.Items[i].AsString;
          if length(str) > max[i + 1] then
            max[i + 1] := length(str);
          DataGrid.Cells[startCol + i + 1, c] := str;
        end;
      end;
      Inc(c);
      //before 100 rows will see the grid row by row filled, cheeting the eyes of user
      if (c < Steps) or (Frac(c / Steps) = 0) then
      begin
        FetchCountLbl.Caption := IntToStr(c - 1);
        CalcWidths;
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
    DataGrid.RowCount := c;
    FetchCountLbl.Caption := IntToStr(c - 1);
  finally
    StopBtn.Enabled := False;
  end;
end;

end.

