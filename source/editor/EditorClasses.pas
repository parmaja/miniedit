unit EditorClasses;
{$mode objfpc}{$H+}
{$INTERFACES CORBA} //Needed for interfaces without guid, see below
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Classes, SysUtils;

  type
    IFileEditor = interface
    end;

    IClipboardEditor = interface(IFileEditor)
      function CanPaste: Boolean;
      procedure Paste;
      procedure Cut;
      function CanCopy: Boolean;
      procedure Copy;
      procedure SelectAll;
    end;

    ITextEditor = interface(IClipboardEditor)
    end;

    IExecuteEditor = interface
    end;

    IWatchEditor = interface
    end;

    IFormEditor = interface
    end;

implementation

end.

