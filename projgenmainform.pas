Unit projgenMainform;

{$mode objfpc}{$H+}

Interface

Uses
  ButtonPanel,
  Buttons,
  Classes,
  commonutils,
  Controls,
  Dialogs,
  EditBtn,
  ExtCtrls,
  Forms,
  Graphics,
  Grids,
  IniFiles,
  projgenGenform,
  projgenhelpform,
  StdCtrls,
  SysUtils,
  Types;

Const
  COL_NAM          = 0;
  COL_DSC          = 1;
  COL_CRE          = 2;
  COL_AUT          = 3;
  COL_DEF          = 2;
  COL_NEW          = 3;
  APP_INI_FILENAME = 'JJProjGen.ini';
  APPSEC_LOC       = 'Locations';
  APPVAL_TDIR      = 'TemplateDirectory';
  APPVAL_LASTPROJDIR = 'LastProjectDirectory';
  TMP_INI_FILENAME = 'Template.ini';
  TMPSEC_META      = 'Meta';
  TMPSEC_TOKENS    = 'Tokens';
  TMPSEC_DESCS     = 'TokenDescriptions';
  TMPSEC_DEFS      = 'TokenDefaults';
  TMPSEC_NEWS      = 'TokenNewValues';

Type

  TToken = Record
    Name:         String;
    Description:  String;
    DefaultValue: String;
    NewValue:     String;
  End;

  TTokenList = Array Of TToken;

  TTemplateMeta = Record
    Name:         String;
    Description:  String;
    CreationDate: String;
    Author:       String;
  End;

  { TProjGenForm }

  TProjGenForm = Class(TForm)
    AddToken:         TBitBtn;
    ButtonPanel1:     TButtonPanel;
    DeleteToken:      TBitBtn;
    ExitButton:       TPanelBitBtn;
    GenerateButton:   TPanelBitBtn;
    EqualityWarning:  TLabel;
    ProjectDirectory: TDirectoryEdit;
    Label1:           TLabel;
    Label4:           TLabel;
    Label6:           TLabel;
    TokenList:        TStringGrid;
    TemplateAuthorLabel: TLabel;
    Label2:           TLabel;
    Label3:           TLabel;
    TemplateNameLabel: TLabel;
    Label5:           TLabel;
    TemplateDescriptionLabel: TLabel;
    Label7:           TLabel;
    TemplateCreationDateLabel: TLabel;
    Label9:           TLabel;
    Panel1:           TPanel;
    TemplateList:     TStringGrid;
    Procedure AddTokenClick(Sender: TObject);
    Procedure DeleteTokenClick(Sender: TObject);
    Procedure ExitButtonClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure GenerateButtonClick(Sender: TObject);
    Procedure HelpButtonClick(Sender: TObject);
    Procedure ProjectDirectoryAcceptDirectory(Sender: TObject; Var Value: String);
    Procedure ProjectDirectoryExit(Sender: TObject);
    Procedure TemplateListSelectCell(Sender: TObject; aCol, aRow: Integer; Var CanSelect: Boolean);
    Procedure TokenListDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    Procedure TokenListValidateEntry(Sender: TObject; aCol, aRow: Integer; Const OldValue: String; Var NewValue: String);
  Private
    Procedure GetProjectSettings;
    Procedure SaveProjectSettings(NewTemplateDir: String; NewProjectDir: String);
    Procedure ClearGenerationDetails;
    Procedure LoadTemplateList;
    Procedure SaveTheTokens;
    Function GetAllDirectories(BaseDirectory: String): TStringList;
    Function GetTemplateMetaData(DirName: String): TTemplateMeta;
    Procedure GetTemplateTokenList(DirName: String);
    Function DefOrNew(aRow: Integer): String;
    Function TokensNeedAttention: Boolean;
    Function TokenEqualsSub(aRow: Integer): Boolean;
    Function RemoveTrailingSeparators(path: String): String;
  Public

  End;

Var
  ProjGenForm: TProjGenForm;
  VerifyForm:  TGenForm;
  HelpForm:    THelpForm;

Implementation

Var
  AppIniFile: TIniFile;
  TemplateBaseDirectory: String;

{$R *.lfm}

{ TProjGenForm }

Procedure TProjGenForm.FormCreate(Sender: TObject);
Begin
  GetProjectSettings;
  ClearGenerationDetails;
End;

Procedure TProjGenForm.FormShow(Sender: TObject);
Begin
  LoadTemplateList;
End;

Procedure TProjGenForm.GenerateButtonClick(Sender: TObject);
Var
  firstDir: String;
  subvalue: String;
  i:        Integer;
Begin
  ProjectDirectory.Directory := RemoveTrailingSeparators(ProjectDirectory.Directory);
  If Not DirectoryExists(ProjectDirectory.Directory) Then
  Begin
    // See if last path segment is legal.  If not, then fail.
    If Not IsValidFilesystemName(ExtractFileName(ProjectDirectory.Directory)) Then
      If MessageDlg('Project Directory Error', 'The project directory name is illegal or contains illegal characters.  Please rectify this before generating the project.', mtError, [mbOK], 0, mbOK) = mrOk Then
        exit;
    // Check path up to last segment.  If not exists, then fail.
    If Not DirectoryExists(ExtractFileDir(ProjectDirectory.Directory)) Then
      If MessageDlg('Project Directory Error', 'The directory path is missing a segment.  Please ensure all segments in the full path exist before generating the project.', mtError, [mbOK], 0, mbOK) = mrOk Then
        Exit;
  End;
  VerifyForm := TGenForm.Create(self);
  Try
    For i := 1 To TokenList.RowCount - 1 Do
    Begin
      subvalue := TokenList.Cells[COL_NEW, i];
      If subvalue = '' Then
        subvalue := TokenList.Cells[COL_DEF, i];
      VerifyForm.AddToken(TokenList.Cells[COL_NAM, i], subvalue);
    End;
    VerifyForm.SetDirLocation(ProjectDirectory.Directory);
    firstDir := IncludeTrailingPathDelimiter(TemplateBaseDirectory) + IncludeTrailingPathDelimiter(TemplateList.Cells[0, TemplateList.Row]);
    VerifyForm.DiscoverFileStructure(ExtractFileName(ExcludeTrailingPathDelimiter(ProjectDirectory.Directory)), firstDir, nil);
    VerifyForm.ShowModal;
  Finally
    VerifyForm.Free;
  End;
End;

Procedure TProjGenForm.HelpButtonClick(Sender: TObject);
Begin
  HelpForm := THelpForm.Create(self);
  Try
    HelpForm.ShowModal;
  Finally
    FreeAndNil(HelpForm);
  End;
End;

Procedure TProjGenForm.ProjectDirectoryAcceptDirectory(Sender: TObject; Var Value: String);
Begin
  SaveProjectSettings(TemplateBaseDirectory, Value);
End;

Procedure TProjGenForm.ProjectDirectoryExit(Sender: TObject);
Begin
  SaveProjectSettings(TemplateBaseDirectory, ProjectDirectory.Directory);
End;

Procedure TProjGenForm.TemplateListSelectCell(Sender: TObject; aCol, aRow: Integer; Var CanSelect: Boolean);
Begin
  ClearGenerationDetails;
  TemplateNameLabel.Caption         := TemplateList.Cells[COL_NAM, aRow];
  TemplateDescriptionLabel.Caption  := TemplateList.Cells[COL_DSC, aRow];
  TemplateCreationDateLabel.Caption := TemplateList.Cells[COL_CRE, aRow];
  TemplateAuthorLabel.Caption       := TemplateList.Cells[COL_AUT, aRow];
  GetTemplateTokenList(TemplateList.Cells[COL_NAM, aRow]);
End;

Function TProjGenForm.TokenEqualsSub(aRow: Integer): Boolean;
Begin
  Result := ((TokenList.Cells[COL_NAM, aRow] = TokenList.Cells[COL_DEF, aRow]) Or
    (TokenList.Cells[COL_NAM, aRow] = TokenList.Cells[COL_NEW, aRow])) And (TokenList.Cells[COL_NAM, aRow] <> '');
End;

Function TProjGenForm.RemoveTrailingSeparators(path: String): String;
Begin
  Result := path;
  Repeat
    Result := ExcludeTrailingPathDelimiter(Result);
  Until (Result = '') Or (RightStr(Result, 1) <> DirectorySeparator);
End;

Procedure TProjGenForm.TokenListDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
Begin
  If (aRow > 0) Then
  Begin
    If ((aCol = COL_DEF) Or (aCol = COL_NEW)) Then
      If (TokenList.Cells[COL_DEF, aRow] = '') And (TokenList.Cells[COL_NEW, aRow] = '') Then
        With TStringGrid(Sender) Do
        Begin
          Canvas.Brush.Color := TColor($8080FF);
          Canvas.FillRect(aRect);
        End;
    If (aCol = COL_NAM) Then
    Begin
      EqualityWarning.Visible := False;
      If TokenEqualsSub(aRow) Then
        With TStringGrid(Sender) Do
        Begin
          EqualityWarning.Visible := True;
          Canvas.Brush.Color      := clYellow;
          Canvas.FillRect(aRect);
          Canvas.TextOut(aRect.Left + 2, aRect.Top + 2,
            TokenList.Cells[COL_NAM, aRow]);
        End;
    End;
  End;
End;

Procedure TProjGenForm.TokenListValidateEntry(Sender: TObject; aCol, aRow: Integer; Const OldValue: String; Var NewValue: String);
Begin
  If OldValue <> NewValue Then
    SaveTheTokens;
  TokenList.Repaint;
End;

Procedure TProjGenForm.AddTokenClick(Sender: TObject);
Begin
  TokenList.RowCount  := TokenList.RowCount + 1;
  TokenList.Row       := TokenList.RowCount - 1;
  DeleteToken.Enabled := True;
End;

Procedure TProjGenForm.DeleteTokenClick(Sender: TObject);
Var
  rowident: String;
Begin
  rowident := TokenList.Cells[0, TokenList.Row];
  If rowident <> '' Then
    If MessageDlg('Verify Token Removal', Format('Are you sure you want to remove the token "%s"?', [rowident]),
      mtWarning, [mbYes, mbAbort], 0, mbAbort) = mrAbort Then
      Exit;
  TokenList.DeleteRow(TokenList.Row);
  SaveTheTokens;
  DeleteToken.Enabled := (TokenList.RowCount > 1);
End;

Procedure TProjGenForm.ExitButtonClick(Sender: TObject);
Begin
  Close;
End;

Procedure TProjGenForm.GetProjectSettings;
Var
  lastprojdir: String;
Begin
  AppIniFile := TIniFile.Create(APP_INI_FILENAME, []);
  Try
    TemplateBaseDirectory := AppIniFile.ReadString(APPSEC_LOC, APPVAL_TDIR, '');
    If TemplateBaseDirectory = '' Then
    Begin
      TemplateBaseDirectory := 'H:\PascalTemplatesXX';
      AppIniFile.WriteString(APPSEC_LOC, APPVAL_TDIR, TemplateBaseDirectory);
    End;
    lastprojdir := AppIniFile.ReadString(APPSEC_LOC, APPVAL_LASTPROJDIR, '');
    If lastprojdir = '' Then
    Begin
      lastprojdir := 'H:\Development\FreePascal\Projects';
      AppIniFile.WriteString(APPSEC_LOC, APPVAL_LASTPROJDIR, lastprojdir);
    End;
    ProjectDirectory.Directory := lastprojdir;
  Finally
    AppIniFile.Free;
  End;
End;

Procedure TProjGenForm.SaveProjectSettings(NewTemplateDir: String; NewProjectDir: String);
Begin
  AppIniFile := TIniFile.Create(APP_INI_FILENAME, []);
  Try
    If NewTemplateDir <> '' Then
      AppIniFile.WriteString(APPSEC_LOC, APPVAL_TDIR, NewTemplateDir);
    If NewProjectDir <> '' Then
      AppIniFile.WriteString(APPSEC_LOC, APPVAL_LASTPROJDIR, NewProjectDir);
  Finally
    AppIniFile.Free;
  End;
End;

Procedure TProjGenForm.ClearGenerationDetails;
Begin
  TemplateNameLabel.Caption := '';
  TemplateDescriptionLabel.Caption := '';
  TemplateCreationDateLabel.Caption := '';
  TemplateAuthorLabel.Caption := '';
  TokenList.RowCount := 1;
End;

Procedure TProjGenForm.LoadTemplateList;
Var
  dirlist:   TStringList;
  dir:       String;
  meta:      TTemplateMeta;
  throwaway: Boolean;
Begin
  If DirectoryExists(TemplateBaseDirectory) Then
  Begin
    Try
      dirlist := GetAllDirectories(TemplateBaseDirectory);
      For dir In dirlist Do
      Begin
        meta := GetTemplateMetaData(IncludeTrailingPathDelimiter(TemplateBaseDirectory) +
          IncludeTrailingPathDelimiter(dir));
        With meta Do
          TemplateList.InsertRowWithValues(TemplateList.RowCount,
            [dir, Description, CreationDate, Author]);
      End;
    Finally
      dirlist.Free;
    End;
    throwaway := True;
    TemplateListSelectCell(self, 0, 1, throwaway);
  End
  Else
  Begin
    If MessageDlg('Location Error', Format(
      'The base template directory, %s, does not exist!  Either create this directory or change the value of the base directory in the generator INI file.',
      [TemplateBaseDirectory]), mtError, [mbAbort], 0) = mrAbort Then
      Close;
  End;
End;

Procedure TProjGenForm.SaveTheTokens;
Var
  i:       Integer;
  fulldir: String;
  tmpini:  TIniFile;
Begin
  fulldir := IncludeTrailingPathDelimiter(TemplateBaseDirectory) +
    IncludeTrailingPathDelimiter(TemplateList.Cells[0, TemplateList.Row]);
  If FileExists(fulldir + TMP_INI_FILENAME) Then
  Begin
    tmpini := TIniFile.Create(fulldir + TMP_INI_FILENAME);
    Try
      tmpini.EraseSection(TMPSEC_TOKENS);
      tmpini.EraseSection(TMPSEC_DESCS);
      tmpini.EraseSection(TMPSEC_DEFS);
      tmpini.EraseSection(TMPSEC_NEWS);
      For i := 1 To TokenList.Rowcount - 1 Do
      Begin
        tmpini.WriteString(TMPSEC_TOKENS, IntToStr(i), TokenList.Cells[COL_NAM, i]);
        tmpini.WriteString(TMPSEC_DESCS, IntToStr(i), TokenList.Cells[COL_DSC, i]);
        tmpini.WriteString(TMPSEC_DEFS, IntToStr(i), TokenList.Cells[COL_DEF, i]);
        tmpini.WriteString(TMPSEC_NEWS, IntToStr(i), TokenList.Cells[COL_NEW, i]);
      End;
    Finally
      tmpini.Free;
    End;
  End;
End;

Function TProjGenForm.GetAllDirectories(BaseDirectory: String): TStringList;
Var
  searchInfo: TSearchRec;
Begin
  Result := TStringList.Create;
  Try
    If FindFirst(IncludeTrailingPathDelimiter(BaseDirectory) + '*', faDirectory, searchInfo) = 0 Then
      Repeat
        If (searchInfo.Attr And faDirectory) = faDirectory Then
          If (searchInfo.Name <> '.') And (searchInfo.Name <> '..') Then
            Result.Add(searchInfo.Name);
      Until FindNext(searchInfo) <> 0
  Finally
    FindClose(searchInfo);
  End;
End;

Function TProjGenForm.GetTemplateMetaData(DirName: String): TTemplateMeta;
Var
  tmpini: TIniFile;
Begin
  Result.Name := '* Empty *';
  If FileExists(DirName + TMP_INI_FILENAME) Then
  Begin
    tmpini := TIniFile.Create(IncludeTrailingPathDelimiter(DirName) + TMP_INI_FILENAME);
    Try
      Result.Name         := tmpini.ReadString(TMPSEC_META, 'ProjectName', 'No Name');
      Result.Description  :=
        tmpini.ReadString(TMPSEC_META, 'Description', 'No Description');
      Result.CreationDate := tmpini.ReadString(TMPSEC_META, 'CreationDate', 'No Date');
      Result.Author       := tmpini.ReadString(TMPSEC_META, 'Author', 'No Author');
    Finally
      tmpini.Free;
    End;
  End;
End;

Procedure TProjGenForm.GetTemplateTokenList(DirName: String);
Var
  tmpini:    TIniFile;
  fulldir:   String;
  rawTokens: TStringList;
  rawtoken:  String;
  ident:     String;
  val:       String;
  desc:      String;
  def:       String;
  newval:    String;
  p:         Integer;
  row:       Integer;
Begin
  fulldir := IncludeTrailingPathDelimiter(TemplateBaseDirectory) + IncludeTrailingPathDelimiter(DirName);
  If FileExists(fulldir + TMP_INI_FILENAME) Then
  Begin
    tmpini    := TIniFile.Create(fulldir + TMP_INI_FILENAME);
    rawTokens := TStringList.Create;
    row       := 1;
    Try
      If tmpini.SectionExists(TMPSEC_TOKENS) Then
      Begin
        tmpini.ReadSectionRaw(TMPSEC_TOKENS, rawTokens);
        For rawtoken In rawTokens Do
          If copy(rawtoken, 1, 1) <> ';' Then
            If Pos('=', rawtoken) > 1 Then
            Begin
              p      := pos('=', rawtoken);
              ident  := Copy(rawtoken, 1, p - 1);
              val    := copy(rawtoken, p + 1, Length(rawtoken) - p);
              desc   := tmpini.ReadString(TMPSEC_DESCS, ident, '');
              def    := tmpini.ReadString(TMPSEC_DEFS, ident, '');
              newval := tmpini.ReadString(TMPSEC_NEWS, ident, '');
              TokenList.InsertRowWithValues(row, [val, desc, def, newval]);
              Inc(row);
            End;
      End;
    Finally
      tmpini.Free;
      rawTokens.Free;
    End;
  End;
  DeleteToken.Enabled := (TokenList.RowCount > 1);
End;

Function TProjGenForm.DefOrNew(aRow: Integer): String;
Begin
  Result := TokenList.Cells[COL_NEW, aRow];
  If TokenList.Cells[COL_NEW, aRow] = '' Then
    Result := TokenList.Cells[COL_DEF, aRow];
End;

Function TProjGenForm.TokensNeedAttention: Boolean;
Var
  i: Integer;
Begin
  Result := False;
  For i := 1 To TokenList.RowCount - 1 Do
    If (TokenList.Cells[COL_DEF, i] = '') And (TokenList.Cells[COL_NEW, i] = '') Then
      Result := True;
End;




End.












