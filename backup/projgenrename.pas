unit projgenrename;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel;

type

  { TRenameForm }

  TRenameForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    EntityName: TEdit;
    Label1: TLabel;
  private

  public

  end;

var
  RenameForm: TRenameForm;

implementation

{$R *.lfm}

end.

