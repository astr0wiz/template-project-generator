unit projgenpreview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  SynHighlighterPas, SynEdit;

type

  { TPreviewForm }

  TPreviewForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    DisplayPane: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    //DisplayPane: TMemo;
    procedure FormResize(Sender: TObject);
  private

  public

  end;

var
  PreviewForm: TPreviewForm;

implementation

{$R *.lfm}

{ TPreviewForm }

procedure TPreviewForm.FormResize(Sender: TObject);
begin
  DisplayPane.Height:=self.Height-64;
  DisplayPane.Width:=self.Width-18;
end;

end.

