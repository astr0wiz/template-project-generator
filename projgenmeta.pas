unit projgenmeta;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel;

type

  { TMetaForm }

  TMetaForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    FileNameOriginal: TStaticText;
    DirectorySource: TStaticText;
    DirectoryDestination: TStaticText;
    FileNameTokenized: TStaticText;
    CanTokenizeContents: TStaticText;
  private

  public

  end;

var
  MetaForm: TMetaForm;

implementation

{$R *.lfm}

end.

