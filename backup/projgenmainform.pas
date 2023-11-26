Unit projgenMainform;

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
  Grids,
  ExtCtrls,
  EditBtn,
  Buttons, ButtonPanel,
  IniFiles,
  Types,
  commonutils,
  projgenGenform,
  projgenhelpform;

Const
  COL_NAM            = 0;
  COL_DSC            = 1;
  COL_CRE            = 2;
  COL_AUT            = 3;
  COL_DEF            = 2;
  COL_NEW            = 3;
  APP_INI_FILENAME   = 'JJProjGen.ini';
  APPSEC_LOC         = 'Locations';
  APPVAL_TDIR        = 'TemplateDirectory';
  APPVAL_LASTPROJDIR = 'LastProjectDirectory';
  TMP_INI_FILENAME   = 'Template.ini';
  TMPSEC_META        = 'Meta';
  TMPSEC_TOKENS      = 'Tokens';
  TMPSEC_DESCS       = 'TokenDescriptions';
  TMPSEC_DEFS        = 'TokenDefaults';
  TMPSEC_NEWS        = 'TokenNewValues';

Type

  TToken = Record
    Name:         string;
    Description:  string;
    DefaultValue: string;
    NewValue:     string;
  End;

  TTokenList = Array Of TToken;

  TTemplateMeta = Record
    Name:         string;
    Description:  string;
    CreationDate: string;
    Author:       string;
  End;

  { TProjGenForm }

  TProjGenForm = Class(TForm)
    AddToken:                  TBitBtn;
    ButtonPanel1: TButtonPanel;
    DeleteToken:               TBitBtn;
    ExitButton: TPanelBitBtn;
    GenerateButton: TPanelBitBtn;
    EqualityWarning:           TLabel;
    ProjectDirectory:          TDirectoryEdit;
    Label1:                    TLabel;
    Label4:                    TLabel;
    Label6:                    TLabel;
    TokenList:                 TStringGrid;
    TemplateAuthorLabel:       TLabel;
    Label2:                    TLabel;
    Label3:                    TLabel;
    TemplateNameLabel:         TLabel;
    Label5:                    TLabel;
    TemplateDescriptionLabel:  TLabel;
    Label7:                    TLabel;
    TemplateCreationDateLabel: TLabel;
    Label9:                    TLabel;
    Panel1:                    TPanel;
    TemplateList:              TStringGrid;
    Procedure AddTokenClick(Sender: TObject);
    Procedure DeleteTokenClick(Sender: TObject);
    procedure ExitButtonClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    procedure GenerateButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    Procedure ProjectDirectoryAcceptDirectory(Sender: TObject; Var Value: string);
    Procedure ProjectDirectoryExit(Sender: TObject);
    Procedure TemplateListSelectCell(Sender: TObject; aCol, aRow: integer; Var CanSelect: boolean);
    Procedure TokenListDrawCell(Sender: TObject; aCol, aRow: integer; aRect: TRect; aState: TGridDrawState);
    Procedure TokenListValidateEntry(Sender: TObject; aCol, aRow: integer; Const OldValue: string; Var NewValue: string);
  Private
    Procedure GetProjectSettings;
    Procedure SaveProjectSettings(NewTemplateDir: string; NewProjectDir: string);
    Procedure ClearGenerationDetails;
    Procedure LoadTemplateList;
    Procedure SaveTheTokens;
    Function GetAllDirectories(BaseDirectory: string): TStringList;
    Function GetTemplateMetaData(DirName: string): TTemplateMeta;
    Procedure GetTemplateTokenList(DirName: string);
    Function DefOrNew(aRow: integer): string;
    Function TokensNeedAttention: boolean;
    Function TokenEqualsSub(aRow: integer): boolean;
    Function RemoveTrailingSeparators(path: string):string;
  Public

  End;

Var
  ProjGenForm: TProjGenForm;
  VerifyForm:  TGenForm;
  HelpForm: THelpForm;

Implementation

Var
  AppIniFile:            TIniFile;
  TemplateBaseDirectory: string;

{$R *.lfm}

{ TProjGenForm }

procedure TProjGenForm.FormCreate(Sender: TObject);
  Begin
    GetProjectSettings;
    ClearGenerationDetails;
  End;

procedure TProjGenForm.FormShow(Sender: TObject);
  Begin
    LoadTemplateList;
  End;

procedure TProjGenForm.GenerateButtonClick(Sender: TObject);
  Var
    firstDir: string;
    subvalue: string;
    i:        integer;
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

procedure TProjGenForm.HelpButtonClick(Sender: TObject);
begin
  HelpForm:=THelpForm.Create(self);
  try
  HelpForm.ShowModal;
  finally
  FreeAndNil(HelpForm);
  end;
end;

procedure TProjGenForm.ProjectDirectoryAcceptDirectory(Sender: TObject;
  var Value: string);
  Begin
    SaveProjectSettings(TemplateBaseDirectory, Value);
  End;

procedure TProjGenForm.ProjectDirectoryExit(Sender: TObject);
  Begin
    SaveProjectSettings(TemplateBaseDirectory, ProjectDirectory.Directory);
  End;

procedure TProjGenForm.TemplateListSelectCell(Sender: TObject; aCol,
  aRow: integer; var CanSelect: boolean);
  Begin
    ClearGenerationDetails;
    TemplateNameLabel.Caption         := TemplateList.Cells[COL_NAM, aRow];
    TemplateDescriptionLabel.Caption  := TemplateList.Cells[COL_DSC, aRow];
    TemplateCreationDateLabel.Caption := TemplateList.Cells[COL_CRE, aRow];
    TemplateAuthorLabel.Caption       := TemplateList.Cells[COL_AUT, aRow];
    GetTemplateTokenList(TemplateList.Cells[COL_NAM, aRow]);
  End;

function TProjGenForm.TokenEqualsSub(aRow: integer): boolean;
  Begin
    Result := ((TokenList.Cells[COL_NAM, aRow] = TokenList.Cells[COL_DEF, aRow]) Or (TokenList.Cells[COL_NAM, aRow] = TokenList.Cells[COL_NEW, aRow])) And (TokenList.Cells[COL_NAM, aRow] <> '');
  End;

function TProjGenForm.RemoveTrailingSeparators(path: string): string;
begin
  Result := path;
  repeat
    Result := ExcludeTrailingPathDelimiter(Result);
  until (Result = '') or (RightStr(Result,1) <> DirectorySeparator);
end;

procedure TProjGenForm.TokenListDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
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
            Canvas.TextOut(aRect.Left + 2, aRect.Top + 2, TokenList.Cells[COL_NAM, aRow]);
            End;
        End;
      End;
  End;

procedure TProjGenForm.TokenListValidateEntry(Sender: TObject; aCol,
  aRow: integer; const OldValue: string; var NewValue: string);
  Begin
    If OldValue <> NewValue Then
      SaveTheTokens;
    TokenList.Repaint;
  End;

procedure TProjGenForm.AddTokenClick(Sender: TObject);
  Begin
    TokenList.RowCount  := TokenList.RowCount + 1;
    TokenList.Row       := TokenList.RowCount - 1;
    DeleteToken.Enabled := True;
  End;

procedure TProjGenForm.DeleteTokenClick(Sender: TObject);
  Var
    rowident: string;
  Begin
    rowident := TokenList.Cells[0, TokenList.Row];
    If rowident <> '' Then
      If MessageDlg('Verify Token Removal', Format('Are you sure you want to remove the token "%s"?', [rowident]), mtWarning, [mbYes, mbAbort], 0, mbAbort) = mrAbort Then
        Exit;
    TokenList.DeleteRow(TokenList.Row);
    SaveTheTokens;
    DeleteToken.Enabled := (TokenList.RowCount > 1);
  End;

procedure TProjGenForm.ExitButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TProjGenForm.GetProjectSettings;
  Var
    lastprojdir: string;
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

procedure TProjGenForm.SaveProjectSettings(NewTemplateDir: string;
  NewProjectDir: string);
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

procedure TProjGenForm.ClearGenerationDetails;
  Begin
    TemplateNameLabel.Caption         := '';
    TemplateDescriptionLabel.Caption  := '';
    TemplateCreationDateLabel.Caption := '';
    TemplateAuthorLabel.Caption       := '';
    TokenList.RowCount                := 1;
  End;

procedure TProjGenForm.LoadTemplateList;
  Var
    dirlist:   TStringList;
    dir:       string;
    meta:      TTemplateMeta;
    throwaway: boolean;
  Begin
    If DirectoryExists(TemplateBaseDirectory) Then
      Begin
        Try
        dirlist := GetAllDirectories(TemplateBaseDirectory);
        For dir In dirlist Do
          Begin
          meta := GetTemplateMetaData(IncludeTrailingPathDelimiter(TemplateBaseDirectory) + IncludeTrailingPathDelimiter(dir));
          With meta Do
            TemplateList.InsertRowWithValues(TemplateList.RowCount, [dir, Description, CreationDate, Author]);
          End;
        Finally
        dirlist.Free;
        End;
      throwaway := True;
      TemplateListSelectCell(self, 0, 1, throwaway);
      End
    Else
      Begin
      If MessageDlg('Location Error', Format('The base template directory, %s, does not exist!  Either create this directory or change the value of the base directory in the generator INI file.', [TemplateBaseDirectory]), mtError, [mbAbort], 0) = mrAbort Then
        Close;
      End;
  End;

procedure TProjGenForm.SaveTheTokens;
  Var
    i:               integer;
    fulldir:         string;
    tmpini:          TIniFile;
  Begin
    fulldir := IncludeTrailingPathDelimiter(TemplateBaseDirectory) + IncludeTrailingPathDelimiter(TemplateList.Cells[0, TemplateList.Row]);
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

function TProjGenForm.GetAllDirectories(BaseDirectory: string): TStringList;
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

function TProjGenForm.GetTemplateMetaData(DirName: string): TTemplateMeta;
  Var
    tmpini: TIniFile;
  Begin
    Result.Name := '* Empty *';
    If FileExists(DirName + TMP_INI_FILENAME) Then
      Begin
      tmpini := TIniFile.Create(IncludeTrailingPathDelimiter(DirName) + TMP_INI_FILENAME);
        Try
        Result.Name         := tmpini.ReadString(TMPSEC_META, 'ProjectName', 'No Name');
        Result.Description  := tmpini.ReadString(TMPSEC_META, 'Description', 'No Description');
        Result.CreationDate := tmpini.ReadString(TMPSEC_META, 'CreationDate', 'No Date');
        Result.Author       := tmpini.ReadString(TMPSEC_META, 'Author', 'No Author');
        Finally
        tmpini.Free;
        End;
      End;
  End;

procedure TProjGenForm.GetTemplateTokenList(DirName: string);
  Var
    tmpini:    TIniFile;
    fulldir:   string;
    rawTokens: TStringList;
    rawtoken:  string;
    ident:     string;
    val:       string;
    desc:      string;
    def:       string;
    newval:    string;
    p:         integer;
    row:       integer;
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

function TProjGenForm.DefOrNew(aRow: integer): string;
  Begin
    Result := TokenList.Cells[COL_NEW, aRow];
    If TokenList.Cells[COL_NEW, aRow] = '' Then
      Result := TokenList.Cells[COL_DEF, aRow];
  End;

function TProjGenForm.TokensNeedAttention: boolean;
  Var
    i: integer;
  Begin
    Result := False;
    For i := 1 To TokenList.RowCount - 1 Do
      If (TokenList.Cells[COL_DEF, i] = '') And (TokenList.Cells[COL_NEW, i] = '') Then
        Result := True;
  End;


















End.
