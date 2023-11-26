Unit projgenhelpform;

{$mode objfpc}{$H+}

Interface

Uses
  ButtonPanel,
  Classes,
  ComCtrls,
  Controls,
  Dialogs,
  Forms,
  FramView,
  Graphics,
  StdCtrls,
  SysUtils;

Type

  { THelpForm }

  THelpForm = Class(TForm)
    ButtonPanel1: TButtonPanel;
    Frame:        TFrameViewer;
    Tabs:         TTabControl;
    Procedure FormShow(Sender: TObject);
    procedure TabsChange(Sender: TObject);
  Private
    Procedure LoadHelpPage(Page: Integer);
  Public

  End;

Var
  HelpForm: THelpForm;

Implementation

{$R *.lfm}

{ THelpForm }

Procedure THelpForm.FormShow(Sender: TObject);
Begin
  Tabs.TabIndex := 0;
  LoadHelpPage(0);
End;

procedure THelpForm.TabsChange(Sender: TObject);
begin
  LoadHelpPage(Tabs.TabIndex);
end;

Procedure THelpForm.LoadHelpPage(Page: Integer);
Var
  fname: widestring;
Begin
  Case Page Of
    0: fname := 'Docs/template_help.html';
    1: fname := 'Docs/token_help.html';
    2: fname := 'Docs/fileinfo_help.html';
  End;
  Frame.LoadFromFile(fname);
End;

End.
