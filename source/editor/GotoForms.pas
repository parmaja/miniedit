unit GotoForms;
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TGotoLineForm = class(TForm)
    NumberEdit: TEdit;
    Label1: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.lfm}

end.