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
    Procedure TabsChange(Sender: TObject);
  Private
    Procedure LoadHelpPage(Page: Integer);
  Public

  End;

Const
  HELPLOCATION = 'Docs/';

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

Procedure THelpForm.TabsChange(Sender: TObject);
Begin
  LoadHelpPage(Tabs.TabIndex);
End;

Procedure THelpForm.LoadHelpPage(Page: Integer);
Var
  fname: WideString;
Begin
  Case Page Of
    0: fname := 'template_help.html';
    1: fname := 'token_help.html';
    2: fname := 'fileinfo_help.html';
  End;
  Frame.LoadFromFile(HELPLOCATION + fname);
End;

End.
