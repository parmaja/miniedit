unit mneSettings;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *}

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  EditorEngine, EditorClasses, ntvGrids, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls;

type

  { TEditorSettingForm }

  TEditorSettingForm = class(TForm)
    AutoOpenProjectChk: TCheckBox;
    Button3: TButton;
    MainPathEdit: TEdit;
    FallbackToTxtChk: TCheckBox;
    CodePagesCbo: TComboBox;
    IgnoreNamesEdit: TEdit;
    Label1: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ExtensionsGrid: TntvGrid;
    Label6: TLabel;
    Label7: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    PageControl: TPageControl;
    AutoStartDebugGrp: TRadioGroup;
    TabSheet2: TTabSheet;
    Label3: TLabel;
    CollectTimeoutEdit: TEdit;
    CollectTimeoutSpn: TUpDown;
    CollectAutoCompleteChk: TCheckBox;
    PHPManualEdit: TEdit;
    Label2: TLabel;
    Button1: TButton;
    TabSheet4: TTabSheet;
    Label9: TLabel;
    Label10: TLabel;
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure INIEditChange(Sender: TObject);
    procedure ExtensionsGridClick(Sender: TObject);
  private
    FEngine: TEditorEngine;
    FExtraExtensions: array of string;
    GroupCol: TntvStandardColumn;
    ExtensionCol: TntvStandardColumn;
    procedure SelectPathFolder;
    procedure Retrieve;
    procedure Apply;
  public
  end;


function ShowSettingForm(Engine: TEditorEngine): Boolean;

implementation

uses
  LConvEncoding;

var
  CurrentPage: Integer;

{$R *.lfm}
type
  TCodePageEntry = record
    cp: TSystemCodePage;
    name: PAnsiChar;
  end;
const
  CodePageNames: array[0..71] of TCodePageEntry = (
    (cp: 37; name: 'ibm037'),
    (cp: 437; name: 'ibm437'),
    (cp: 500; name: 'IBM500'),
    (cp: 708; name: 'asmo-708'),
    (cp: 720; name: 'DOS-720'),
    (cp: 737; name: 'ibm737'),
    (cp: 775; name: 'ibm775'),
    (cp: 850; name: 'ibm850'),
    (cp: 852; name: 'ibm852'),
    (cp: 855; name: 'IBM855'),
    (cp: 857; name: 'ibm857'),
    (cp: 858; name: 'ibm00858'),
    (cp: 860; name: 'IBM860'),
    (cp: 861; name: 'ibm861'),
    (cp: 862; name: 'DOS-862'),
    (cp: 863; name: 'IBM863'),
    (cp: 864; name: 'IBM864'),
    (cp: 865; name: 'IBM865'),
    (cp: 866; name: 'cp866'),
    (cp: 869; name: 'ibm869'),
    (cp: 870; name: 'IBM870'),
    (cp: 874; name: 'windows-874'),
    (cp: 875; name: 'cp875'),
    (cp: 1026; name: 'ibm1026'),
    (cp: 1047; name: 'ibm01047'),
    (cp: 1140; name: 'ibm01140'),
    (cp: 1141; name: 'IBM01141'),
    (cp: 1142; name: 'IBM01142'),
    (cp: 1143; name: 'IBM01143'),
    (cp: 1144; name: 'IBM01144'),
    (cp: 1145; name: 'ibm01145'),
    (cp: 1146; name: 'ibm01146'),
    (cp: 1147; name: 'ibm01147'),
    (cp: 1148; name: 'IBM01148'),
    (cp: 1149; name: 'IBM01149'),
    (cp: 1200; name: 'utf-16'),
    (cp: 1201; name: 'unicodefffe'),
    (cp: 1250; name: 'windows-1250'),
    (cp: 1251; name: 'windows-1251'),
    (cp: 1252; name: 'windows-1252'),
    (cp: 1253; name: 'windows-1253'),
    (cp: 1254; name: 'windows-1254'),
    (cp: 1255; name: 'windows-1255'),
    (cp: 1256; name: 'windows-1256'),
    (cp: 1257; name: 'windows-1257'),
    (cp: 1258; name: 'windows-1258'),
    (cp: 20273; name: 'ibm273'),
    (cp: 20277; name: 'ibm277'),
    (cp: 20278; name: 'ibm278'),
    (cp: 20280; name: 'ibm280'),
    (cp: 20284; name: 'ibm284'),
    (cp: 20285; name: 'IBM285'),
    (cp: 20290; name: 'IBM290'),
    (cp: 20297; name: 'IBM297'),
    (cp: 20420; name: 'ibm420'),
    (cp: 20423; name: 'ibm423'),
    (cp: 20424; name: 'IBM424'),
    (cp: 20871; name: 'ibm871'),
    (cp: 20880; name: 'ibm880'),
    (cp: 20905; name: 'ibm905'),
    (cp: 20924; name: 'IBM00924'),
    (cp: 28591; name: 'iso-8859-1'),
    (cp: 28592; name: 'iso-8859-2'),
    (cp: 28593; name: 'iso-8859-3'),
    (cp: 28594; name: 'iso-8859-4'),
    (cp: 28595; name: 'iso-8859-5'),
    (cp: 28596; name: 'iso-8859-6'),
    (cp: 28597; name: 'iso-8859-7'),
    (cp: 28598; name: 'iso-8859-8'),
    (cp: 28599; name: 'iso-8859-9'),
    (cp: 28603; name: 'iso-8859-13'),
    (cp: 28605; name: 'iso-8859-15')
  );

function ShowSettingForm(Engine: TEditorEngine): Boolean;
begin
  with TEditorSettingForm.Create(Application) do
  begin
    FEngine := Engine;
    Retrieve;
    Result := ShowModal = mrOk;
    if Result then
    begin
      Apply;
      Engine.Options.Save(Engine.WorkSpace);
    end;
  end;
end;

procedure TEditorSettingForm.Apply;
var
  i, c: Integer;
begin
  FEngine.Options.MainPath := MainPathEdit.Text;
  FEngine.Options.FallbackToText := FallbackToTxtChk.Checked;
  FEngine.Options.AutoOpenProject := AutoOpenProjectChk.Checked;
  FEngine.Options.CollectAutoComplete := CollectAutoCompleteChk.Checked;
  FEngine.Options.CollectTimeout := CollectTimeoutSpn.Position;
  FEngine.Options.AutoStartDebug := TAutoStartDebug(AutoStartDebugGrp.ItemIndex);
  c := 0;
  for i := 0 to FEngine.Groups.Count - 1 do
  begin
    //if not FEngine.Groups[i].Category.Tendency.IsDefault then
    begin
      ExtensionsGrid.ActiveIndex := c;
      FEngine.Options.ExtraExtensions.Values[FExtraExtensions[c]] := ExtensionCol.AsString;
      Inc(c);
    end;
  end;
  FEngine.Options.IgnoreNames := IgnoreNamesEdit.Text;
  FEngine.Options.AnsiCodePage := CodePageNames[CodePagesCbo.ItemIndex].cp;
  Engine.UpdatePath;
  Engine.UpdateOptions;
  Engine.Update([ecsOptions]);
end;

procedure TEditorSettingForm.Retrieve;
var
  i, c: Integer;
begin
  MainPathEdit.Text := FEngine.Options.MainPath;
  FallbackToTxtChk.Checked := FEngine.Options.FallbackToText;
  AutoOpenProjectChk.Checked := FEngine.Options.AutoOpenProject;
  CollectAutoCompleteChk.Checked := FEngine.Options.CollectAutoComplete;
  CollectTimeoutSpn.Position := FEngine.Options.CollectTimeout;
  AutoStartDebugGrp.ItemIndex := Integer(FEngine.Options.AutoStartDebug);
  c := 0;
  for i := 0 to FEngine.Groups.Count - 1 do
  begin
  //  if not FEngine.Groups[i].Category.Tendency.IsDefault then
    begin
      ExtensionsGrid.ActiveIndex := c;
      GroupCol.AsString := FEngine.Groups[i].Title;
      ExtensionCol.AsString := FEngine.Options.ExtraExtensions.Values[FEngine.Groups[i].Name];
      SetLength(FExtraExtensions, c + 1);
      FExtraExtensions[c] := FEngine.Groups[i].Name;
      Inc(c);
    end;
  end;
  IgnoreNamesEdit.Text := FEngine.Options.IgnoreNames;
  for i := 0 to Length(CodePageNames) - 1 do
    CodePagesCbo.Items.Add(CodePageNames[i].name);
  i := CodePagesCbo.Items.IndexOf(CodePageToCodePageName(FEngine.Options.AnsiCodePage));
  if i >= 0 then
    CodePagesCbo.ItemIndex := i;
end;


procedure TEditorSettingForm.FormCreate(Sender: TObject);
begin
  GroupCol := TntvStandardColumn.Create(ExtensionsGrid, 'Group', 'GroupCol');
  GroupCol.ReadOnly := True;
  ExtensionCol := TntvStandardColumn.Create(ExtensionsGrid, 'Extension', 'ExtensionCol');
  ExtensionCol.AutoFit := True;
  PageControl.ActivePageIndex := CurrentPage;
end;

procedure TEditorSettingForm.Button3Click(Sender: TObject);
begin
  SelectPathFolder;
end;

procedure TEditorSettingForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CurrentPage := PageControl.ActivePageIndex;
end;

procedure TEditorSettingForm.INIEditChange(Sender: TObject);
begin
end;

procedure TEditorSettingForm.ExtensionsGridClick(Sender: TObject);
begin

end;

procedure TEditorSettingForm.SelectPathFolder;
var
  aFolder: string;
begin
  aFolder := MainPathEdit.Text;
  if (aFolder = '') and (Engine.Files.Current <> nil) then
    aFolder := ExtractFilePath(Engine.Files.Current.FileName);
  if SelectFolder('Select root directory for your project', '', aFolder) then
  begin
    MainPathEdit.Text := aFolder;
  end;
end;

end.

