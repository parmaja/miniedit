unit AboutForms;
{$mode delphi}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Shellapi;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Image1: TImage;
    Memo1: TMemo;
    SiteLbl: TLabel;
    VersionLbl: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SiteLblClick(Sender: TObject);
    procedure TabControl1Changing(Sender: TObject; var AllowChange: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses FileInfo;

{$R *.lfm}

procedure TAboutForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
var
  FileVersionInfo:TFileVersionInfo;
begin
  FileVersionInfo:=TFileVersionInfo.Create(nil);
  FileVersionInfo.fileName := Application.ExeName;
  VersionLbl.Caption := FileVersionInfo.getVersionSetting('FileVersion');
  FileVersionInfo.Free;
end;

procedure TAboutForm.SiteLblClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar((Sender as TLabel).Caption), nil, nil, SW_SHOWNORMAL);
end;

procedure TAboutForm.TabControl1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin

end;

end.

