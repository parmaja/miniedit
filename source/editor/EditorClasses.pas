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

    ITextEditor = interface
      function CanCopy: Boolean;
      function CanPaste: Boolean;
      procedure Copy;
      procedure Paste;
      procedure Cut;
      procedure SelectAll;
    end;

    ISourceEditor = interface(ITextEditor)
    end;

    IExecuteEditor = interface
    end;

    IWatchEditor = interface
    end;

    IFormEditor = interface
    end;

implementation

end.

