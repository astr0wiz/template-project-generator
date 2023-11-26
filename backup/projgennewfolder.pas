Unit projgennewfolder;

{$mode objfpc}{$H+}

Interface

Uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ButtonPanel,
  CommonUtils;

Type

  { TNewFolderForm }

  TNewFolderForm = Class(TForm)
    ButtonPanel1: TButtonPanel;
    FolderName:   TEdit;
    Label1:       TLabel;
    Procedure FolderNameEditingDone(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  Private

  Public

  End;

Var
  NewFolder: TNewFolderForm;

Implementation

{$R *.lfm}

{ TNewFolderForm }

Procedure TNewFolderForm.FolderNameEditingDone(Sender: TObject);
  Begin
    If IsValidFilesystemName(FolderName.Text) Then
      ButtonPanel1.OkButton.Enabled := True
    Else
      Begin
      Buttonpanel1.OkButton.Enabled := False;
      ShowMessage('Done.');
      End;
  End;

Procedure TNewFolderForm.FormShow(Sender: TObject);
  Begin
  End;

procedure TNewFolderForm.OKButtonClick(Sender: TObject);
begin

end;

End.
